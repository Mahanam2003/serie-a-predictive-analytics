# Weekly Match Predictions for Serie A
#
# The combined model predicts season-end point totals.
# This script does something different: it predicts individual
# UPCOMING fixtures using the Poisson approach from script 04.
#
# The key insight: once you know each team's attack and defense
# strength from historical data, you can plug in ANY matchup
# and get probabilities for home win / draw / away win.
#
# Data source: football-data.co.uk I1.csv updates throughout
# the season. Upcoming fixtures have blank FTHG/FTAG columns.
# We use that to automatically separate "played" from "upcoming".

rm(list = ls())

library(ggplot2)


# ---- LOAD TRAINING AVERAGES ----
# We keep using the training-data averages as our baseline
# so predictions stay calibrated on historical Serie A patterns.
# The Poisson formula needs these to scale team strengths:
#   lambda_home = avg_home_goals × home_attack × away_defense

all_matches    <- read.csv("data/serie_a_all_matches.csv")
train          <- all_matches[all_matches$Season != "2024", ]
avg_home_goals <- mean(train$FTHG)
avg_away_goals <- mean(train$FTAG)

cat("=== League averages from training data ===\n")
cat("Average home goals per match:", round(avg_home_goals, 3), "\n")
cat("Average away goals per match:", round(avg_away_goals, 3), "\n\n")


# ---- LOAD TEAM STRENGTHS ----
# These were saved by 04_poisson_model.R.
# HomeAttack / HomeDefense = home performance vs league average
# AwayAttack / AwayDefense = away performance vs league average
# A value of 1.4 means 40% above average; 0.8 means 20% below.

team_stats <- read.csv("data/serie_a_team_strengths.csv")


# PART 1: The Prediction + Confidence Function
#
# This is the same predict_match() from script 04, but with two upgrades:
#   1. Graceful fallback for promoted teams not in training data
#      (we assume league-average strength rather than crashing)
#   2. A confidence score attached to every prediction
#
# CONFIDENCE FORMULA:
# With three possible outcomes (home win / draw / away win), a model
# with zero information would give each outcome 1/3 = 33.3%.
# Our confidence is how much our TOP probability exceeds that
# "I have no idea" baseline, scaled to a 0-100 range:
#
#   confidence = (max_prob - 1/3) / (1 - 1/3) × 100
#
# Intuition with examples:
#   max_prob = 0.333  → (0.333 - 0.333) / 0.667 = 0%   (pure coin flip)
#   max_prob = 0.50   → (0.50  - 0.333) / 0.667 = 25%  (slight edge)
#   max_prob = 0.60   → (0.60  - 0.333) / 0.667 = 40%  (good edge)
#   max_prob = 0.75   → (0.75  - 0.333) / 0.667 = 63%  (strong edge)
#   max_prob = 0.90   → (0.90  - 0.333) / 0.667 = 85%  (heavy favorite)
#
# We clamp to [5, 95] — a model is never useless and never certain.

predict_match <- function(home_team, away_team, team_stats_df,
                          avg_hg, avg_ag) {

  h_idx <- which(team_stats_df$Team == home_team)
  a_idx <- which(team_stats_df$Team == away_team)

  # If a team is newly promoted and not in our historical data,
  # we assign league-average attack and defense (all ratings = 1.0).
  # This is a conservative fallback — better than crashing.
  if (length(h_idx) == 0) {
    cat("  Note: '", home_team, "' not in training data — using league average\n", sep = "")
    avg_row <- data.frame(Team = home_team,
                          HomeGoalsScored = NA, HomeGoalsConceded = NA, HomeGames = NA,
                          AwayGoalsScored = NA, AwayGoalsConceded = NA, AwayGames = NA,
                          HomeAttack = 1.0, HomeDefense = 1.0,
                          AwayAttack = 1.0, AwayDefense = 1.0)
    team_stats_df <<- rbind(team_stats_df, avg_row)
    h_idx <- nrow(team_stats_df)
  }
  if (length(a_idx) == 0) {
    cat("  Note: '", away_team, "' not in training data — using league average\n", sep = "")
    avg_row <- data.frame(Team = away_team,
                          HomeGoalsScored = NA, HomeGoalsConceded = NA, HomeGames = NA,
                          AwayGoalsScored = NA, AwayGoalsConceded = NA, AwayGames = NA,
                          HomeAttack = 1.0, HomeDefense = 1.0,
                          AwayAttack = 1.0, AwayDefense = 1.0)
    team_stats_df <<- rbind(team_stats_df, avg_row)
    a_idx <- nrow(team_stats_df)
  }

  # Expected goals = league average × attacking team's strength × defending team's weakness
  # "Away defense" = how many goals they let in per away game vs the average
  # A high AwayDefense means they concede a lot away → the home team should score more
  lambda_home <- avg_hg * team_stats_df$HomeAttack[h_idx] * team_stats_df$AwayDefense[a_idx]
  lambda_away <- avg_ag * team_stats_df$AwayAttack[a_idx] * team_stats_df$HomeDefense[h_idx]

  # Sum Poisson probabilities for all scorelines from 0-0 to 6-6
  # dpois(k, lambda) = P(exactly k goals when average rate is lambda)
  max_goals     <- 6
  home_win_prob <- 0
  draw_prob     <- 0
  away_win_prob <- 0

  for (hg in 0:max_goals) {
    for (ag in 0:max_goals) {
      p_score <- dpois(hg, lambda_home) * dpois(ag, lambda_away)
      if      (hg > ag) home_win_prob <- home_win_prob + p_score
      else if (hg < ag) away_win_prob <- away_win_prob + p_score
      else              draw_prob     <- draw_prob     + p_score
    }
  }

  # Confidence: how far above "random" is our best prediction?
  max_prob   <- max(home_win_prob, draw_prob, away_win_prob)
  confidence <- (max_prob - (1/3)) / (1 - (1/3)) * 100
  confidence <- max(5, min(95, round(confidence)))

  # Pick the most likely outcome as our prediction
  pred_result <- ifelse(home_win_prob > draw_prob & home_win_prob > away_win_prob, "H",
                        ifelse(away_win_prob > draw_prob, "A", "D"))

  return(list(
    lambda_home  = round(lambda_home, 2),
    lambda_away  = round(lambda_away, 2),
    prob_home    = round(home_win_prob, 4),
    prob_draw    = round(draw_prob,     4),
    prob_away    = round(away_win_prob, 4),
    pred_result  = pred_result,
    confidence   = confidence
  ))
}


# PART 2: Fetch the Current Season Fixture List
#
# football-data.co.uk keeps I1.csv updated throughout the season.
# Rows with FTHG and FTAG filled in = already played.
# Rows where those columns are blank/NA = upcoming fixtures.
#
# URL pattern: /mmz4281/[YYZZ]/I1.csv
# 2526 = 2025-26 season

cat("=== Fetching 2025-26 fixture list from football-data.co.uk ===\n")

fixtures_raw <- tryCatch({
  df <- read.csv("https://www.football-data.co.uk/mmz4281/2526/I1.csv")
  cat("Fetched", nrow(df), "rows\n")
  df
}, error = function(e) {
  cat("Could not reach football-data.co.uk:", conditionMessage(e), "\n")
  cat("Falling back to local copy if available...\n")
  if (file.exists("data/fixtures_2526.csv")) {
    read.csv("data/fixtures_2526.csv")
  } else {
    stop("No fixture data available. Run with internet access first.")
  }
})

# Save a local backup so the script can run offline next time
write.csv(fixtures_raw, "data/fixtures_2526.csv", row.names = FALSE)

# Split into played vs upcoming.
# A match is "upcoming" if FTHG (full-time home goals) is NA or empty.
played   <- fixtures_raw[!is.na(fixtures_raw$FTHG) & fixtures_raw$FTHG != "", ]
upcoming <- fixtures_raw[ is.na(fixtures_raw$FTHG) | fixtures_raw$FTHG == "", ]

cat("Matches played so far:", nrow(played), "\n")
cat("Upcoming fixtures:    ", nrow(upcoming), "\n\n")

if (nrow(upcoming) == 0) {
  cat("No upcoming fixtures found — season may be complete.\n")
  quit(save = "no")
}

# Take the next matchweek.
# If a Date column exists and is populated, group by the earliest upcoming date.
# Otherwise just take the first 10 rows (one full matchweek in Serie A).

if ("Date" %in% colnames(upcoming) &&
    any(!is.na(upcoming$Date) & upcoming$Date != "")) {
  next_date      <- upcoming$Date[!is.na(upcoming$Date) & upcoming$Date != ""][1]
  next_matchweek <- upcoming[upcoming$Date == next_date, ]
  cat("Next matchweek date:", next_date, "\n")
} else {
  next_matchweek <- head(upcoming, 10)
  cat("No date column found — using first", nrow(next_matchweek), "upcoming fixtures\n")
}

cat("Fixtures in this matchweek:", nrow(next_matchweek), "\n\n")


# PART 3: Run Predictions on Every Upcoming Fixture
#
# For each game we call predict_match() and collect the output
# into a clean data frame. The function returns:
#   - ExpHome / ExpAway: expected goals (the Poisson rate, lambda)
#   - ProbHome / ProbDraw / ProbAway: outcome probabilities
#   - PredResult: which outcome the model favors (H, D, or A)
#   - Confidence: how certain the model is (our scaled formula above)

cat("=== NEXT MATCHWEEK PREDICTIONS ===\n\n")

predictions <- data.frame(
  HomeTeam   = character(nrow(next_matchweek)),
  AwayTeam   = character(nrow(next_matchweek)),
  ExpHome    = numeric(nrow(next_matchweek)),
  ExpAway    = numeric(nrow(next_matchweek)),
  ProbHome   = numeric(nrow(next_matchweek)),
  ProbDraw   = numeric(nrow(next_matchweek)),
  ProbAway   = numeric(nrow(next_matchweek)),
  PredResult = character(nrow(next_matchweek)),
  Confidence = integer(nrow(next_matchweek)),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(next_matchweek)) {
  home <- next_matchweek$HomeTeam[i]
  away <- next_matchweek$AwayTeam[i]

  res <- predict_match(home, away, team_stats, avg_home_goals, avg_away_goals)

  predictions$HomeTeam[i]   <- home
  predictions$AwayTeam[i]   <- away
  predictions$ExpHome[i]    <- res$lambda_home
  predictions$ExpAway[i]    <- res$lambda_away
  predictions$ProbHome[i]   <- res$prob_home
  predictions$ProbDraw[i]   <- res$prob_draw
  predictions$ProbAway[i]   <- res$prob_away
  predictions$PredResult[i] <- res$pred_result
  predictions$Confidence[i] <- res$confidence

  # Human-readable prediction label
  outcome_label <- switch(res$pred_result,
    "H" = paste(home, "win"),
    "D" = "Draw",
    "A" = paste(away, "win")
  )

  cat(sprintf("  %-22s  vs  %-22s\n", home, away))
  cat(sprintf("  Expected scoreline: %.1f – %.1f\n", res$lambda_home, res$lambda_away))
  cat(sprintf("  Home: %4.1f%%   Draw: %4.1f%%   Away: %4.1f%%\n",
              res$prob_home * 100, res$prob_draw * 100, res$prob_away * 100))
  cat(sprintf("  Prediction: %-25s Confidence: %d%%\n\n", outcome_label, res$confidence))
}


# PART 4: Matchweek Summary
#
# How confident is the model overall for this matchweek?
# A low average confidence = lots of toss-up games (exciting week!).
# A high average = the model sees clear favorites across the board.

avg_conf  <- round(mean(predictions$Confidence))
n_high    <- sum(predictions$Confidence >= 50)
n_home    <- sum(predictions$PredResult == "H")
n_draw    <- sum(predictions$PredResult == "D")
n_away    <- sum(predictions$PredResult == "A")

cat("=== MATCHWEEK SUMMARY ===\n")
cat(sprintf("Average confidence:          %d%%\n", avg_conf))
cat(sprintf("High-confidence games (50%%+): %d of %d\n", n_high, nrow(predictions)))
cat(sprintf("Predicted outcomes:          %d home wins, %d draws, %d away wins\n\n",
            n_home, n_draw, n_away))


# PART 5: Visualize the Matchweek
#
# Horizontal stacked bar chart — one bar per fixture.
# Segments show home/draw/away probabilities.
# The number in the middle of each bar is the confidence score.
# Matches are sorted so the most confident prediction sits at the top,
# making it easy to spot the "banker" picks vs the coin flips.

plot_data <- data.frame(
  match     = paste(predictions$HomeTeam, "vs", predictions$AwayTeam),
  home_p    = predictions$ProbHome * 100,
  draw_p    = predictions$ProbDraw * 100,
  away_p    = predictions$ProbAway * 100,
  conf      = predictions$Confidence,
  stringsAsFactors = FALSE
)

# Sort so most confident games appear at top of the chart
plot_data$match <- factor(plot_data$match,
                          levels = plot_data$match[order(plot_data$conf)])

# ggplot needs long format: one row per outcome per match
long <- rbind(
  data.frame(match = plot_data$match, outcome = "Home Win", prob = plot_data$home_p, conf = plot_data$conf),
  data.frame(match = plot_data$match, outcome = "Draw",     prob = plot_data$draw_p,  conf = plot_data$conf),
  data.frame(match = plot_data$match, outcome = "Away Win", prob = plot_data$away_p,  conf = plot_data$conf)
)
long$outcome <- factor(long$outcome, levels = c("Away Win", "Draw", "Home Win"))

p <- ggplot(long, aes(x = match, y = prob, fill = outcome)) +
  geom_col(width = 0.65) +
  # Confidence label sits in the middle of each bar at 50%
  geom_text(data = plot_data,
            aes(x = match, y = 50, label = paste0(conf, "%"), fill = NULL),
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c(
    "Home Win" = "#2563EB",
    "Draw"     = "#64748B",
    "Away Win" = "#DC2626"
  )) +
  coord_flip() +
  labs(
    title    = "Serie A — Next Matchweek Predictions",
    subtitle = "Label = model confidence in predicted outcome. Sorted highest to lowest confidence.",
    x        = NULL,
    y        = "Probability (%)",
    fill     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "gray60", size = 10),
    legend.position  = "top",
    panel.grid.minor = element_blank()
  )

ggsave("plots/weekly_predictions.png", p,
       width = 10, height = max(5, nrow(predictions) * 0.65 + 2), dpi = 300)
cat("Matchweek chart saved to plots/weekly_predictions.png\n")

# Save to CSV — useful for updating the website manually
write.csv(predictions, "data/weekly_predictions.csv", row.names = FALSE)
cat("Predictions saved to data/weekly_predictions.csv\n\n")
cat("To update the website, copy the numbers from weekly_predictions.csv\n")
cat("into the weeklyMatches array in index.html\n")
