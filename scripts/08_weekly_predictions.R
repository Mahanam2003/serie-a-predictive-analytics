# Weekly Match Predictions v2 — Recency-Weighted Multi-Season Training
rm(list = ls())
library(ggplot2)

# PART 1: Load Historical Data (last 4 seasons)
historical <- read.csv("data/serie_a_all_matches.csv")
cat("Historical data:", nrow(historical), "matches\n")

# PART 2: Fetch Current 2025/26 Season
cat("=== Fetching 2025/26 season data ===\n")
current_raw <- tryCatch({
  read.csv("https://www.football-data.co.uk/mmz4281/2526/I1.csv")
}, error = function(e) {
  if (file.exists("data/fixtures_2526.csv")) {
    read.csv("data/fixtures_2526.csv")
  } else {
    stop("No fixture data available.")
  }
})

write.csv(current_raw, "data/fixtures_2526.csv", row.names = FALSE)
played_2526 <- current_raw[!is.na(current_raw$FTHG) & current_raw$FTHG != "", ]
cat("2025/26 games played:", nrow(played_2526), "\n\n")

# PART 3: Combine All Data With Recency Weights
played_2526$Season <- "2025"
common_cols <- intersect(names(historical), names(played_2526))
all_data <- rbind(historical[, common_cols], played_2526[, common_cols])

season_weights <- data.frame(
  Season = c("2021", "2022", "2023", "2024", "2025"),
  Weight = c(0.1, 0.25, 0.50, 1.0, 2.0),
  stringsAsFactors = FALSE
)

all_data <- merge(all_data, season_weights, by = "Season", all.x = TRUE)
all_data$Weight[is.na(all_data$Weight)] <- 0.1

# PART 4: Compute Weighted Team Strengths
weighted_mean <- function(x, w) sum(x * w) / sum(w)

avg_home_goals <- weighted_mean(all_data$FTHG, all_data$Weight)
avg_away_goals <- weighted_mean(all_data$FTAG, all_data$Weight)

cat("=== Weighted league averages ===\n")
cat("Home goals/match:", round(avg_home_goals, 3), "\n")
cat("Away goals/match:", round(avg_away_goals, 3), "\n\n")

all_teams <- unique(c(all_data$HomeTeam, all_data$AwayTeam))

team_stats <- data.frame()
for (team in all_teams) {
  home_games <- all_data[all_data$HomeTeam == team, ]
  away_games <- all_data[all_data$AwayTeam == team, ]
  
  if (nrow(home_games) < 3 || nrow(away_games) < 3) next
  
  ha_raw <- weighted_mean(home_games$FTHG, home_games$Weight)
  hd_raw <- weighted_mean(home_games$FTAG, home_games$Weight)
  aa_raw <- weighted_mean(away_games$FTAG, away_games$Weight)
  ad_raw <- weighted_mean(away_games$FTHG, away_games$Weight)
  
  team_stats <- rbind(team_stats, data.frame(
    Team        = team,
    HomeAttack  = round(ha_raw / avg_home_goals, 3),
    HomeDefense = round(hd_raw / avg_away_goals, 3),
    AwayAttack  = round(aa_raw / avg_away_goals, 3),
    AwayDefense = round(ad_raw / avg_home_goals, 3),
    HomeGames   = nrow(home_games),
    AwayGames   = nrow(away_games),
    stringsAsFactors = FALSE
  ))
}

cat("Team strengths computed for", nrow(team_stats), "teams\n")

team_stats$NetStrength <- round((team_stats$HomeAttack + team_stats$AwayAttack) -
                                (team_stats$HomeDefense + team_stats$AwayDefense), 3)
cat("\n=== Top 5 teams by net strength ===\n")
print(head(team_stats[order(-team_stats$NetStrength),
                      c("Team", "HomeAttack", "AwayAttack", "NetStrength")], 5))
cat("\n")

# PART 5: Derive Upcoming Fixtures
current_teams <- unique(c(played_2526$HomeTeam, played_2526$AwayTeam))
all_possible <- expand.grid(HomeTeam = current_teams, AwayTeam = current_teams, stringsAsFactors = FALSE)
all_possible <- all_possible[all_possible$HomeTeam != all_possible$AwayTeam, ]
played_keys <- paste(played_2526$HomeTeam, played_2526$AwayTeam)
all_possible$key <- paste(all_possible$HomeTeam, all_possible$AwayTeam)
upcoming <- all_possible[!all_possible$key %in% played_keys, ]
upcoming$key <- NULL

cat("Upcoming fixtures derived:", nrow(upcoming), "\n\n")
next_mw <- head(upcoming, 10)

# PART 6: Prediction Function
predict_match <- function(home, away, stats_df, avg_hg, avg_ag) {
  h_idx <- which(stats_df$Team == home)
  a_idx <- which(stats_df$Team == away)
  if (length(h_idx) == 0 || length(a_idx) == 0) {
    return(list(lambda_home=avg_hg, lambda_away=avg_ag,
                prob_home=0.33, prob_draw=0.33, prob_away=0.33,
                pred_result="D", confidence=5))
  }
  lambda_h <- avg_hg * stats_df$HomeAttack[h_idx] * stats_df$AwayDefense[a_idx]
  lambda_a <- avg_ag * stats_df$AwayAttack[a_idx] * stats_df$HomeDefense[h_idx]
  ph <- 0; pd <- 0; pa <- 0
  for (hg in 0:6) for (ag in 0:6) {
    p <- dpois(hg, lambda_h) * dpois(ag, lambda_a)
    if (hg > ag) ph <- ph + p else if (hg < ag) pa <- pa + p else pd <- pd + p
  }
  max_p <- max(ph, pd, pa)
  conf <- max(5, min(95, round((max_p - 1/3) / (2/3) * 100)))
  pred <- if (ph > pd & ph > pa) "H" else if (pa > pd) "A" else "D"
  list(lambda_home=round(lambda_h,2), lambda_away=round(lambda_a,2),
       prob_home=round(ph,4), prob_draw=round(pd,4), prob_away=round(pa,4),
       pred_result=pred, confidence=conf)
}

# PART 7: Run Predictions
cat("=== NEXT MATCHWEEK PREDICTIONS ===\n\n")
predictions <- data.frame()
for (i in 1:nrow(next_mw)) {
  home <- next_mw$HomeTeam[i]
  away <- next_mw$AwayTeam[i]
  r <- predict_match(home, away, team_stats, avg_home_goals, avg_away_goals)
  
  predictions <- rbind(predictions, data.frame(
    HomeTeam=home, AwayTeam=away,
    ExpHome=r$lambda_home, ExpAway=r$lambda_away,
    ProbHome=r$prob_home, ProbDraw=r$prob_draw, ProbAway=r$prob_away,
    PredResult=r$pred_result, Confidence=r$confidence,
    stringsAsFactors=FALSE
  ))
  
  label <- switch(r$pred_result, "H"=paste(home,"win"), "D"="Draw", "A"=paste(away,"win"))
  cat(sprintf("  %-20s vs %-20s\n", home, away))
  cat(sprintf("  Expected: %.1f - %.1f | H: %.1f%% D: %.1f%% A: %.1f%%\n",
              r$lambda_home, r$lambda_away, r$prob_home*100, r$prob_draw*100, r$prob_away*100))
  cat(sprintf("  Prediction: %s (%d%% confidence)\n\n", label, r$confidence))
}

cat("=== MATCHWEEK SUMMARY ===\n")
cat(sprintf("Avg confidence: %d%%\n", round(mean(predictions$Confidence))))
cat(sprintf("High confidence games: %d of %d\n", sum(predictions$Confidence >= 50), nrow(predictions)))
cat(sprintf("Predicted: %d home, %d draw, %d away\n\n",
            sum(predictions$PredResult=="H"), sum(predictions$PredResult=="D"), sum(predictions$PredResult=="A")))

write.csv(predictions, "data/weekly_predictions.csv", row.names=FALSE)
cat("Saved to data/weekly_predictions.csv\n")