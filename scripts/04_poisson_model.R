# Poisson Match Prediction Model for Serie A
#
# The Pythagorean model predicts season totals.
# This model is more powerful because it predicts
# INDIVIDUAL MATCH scores using the Poisson distribution.
#
# The Poisson distribution describes how often rare events
# happen in a fixed time period. Goals in soccer are rare
# events (usually 0-3 per game) in a fixed time (90 min).
# Betting companies use this exact math as their starting point.

rm(list = ls())

library(ggplot2)
library(gridExtra)

# Loading my Serie A match data
all_matches <- read.csv("data/serie_a_all_matches.csv")

# I will use seasons 2021-2023 to TRAIN the model
# and 2024 season to TEST it. This way I know the model
# works on data it has never seen before (no overfitting).
train <- all_matches[all_matches$Season != "2024", ]
test  <- all_matches[all_matches$Season == "2024", ]

cat("Training matches:", nrow(train), "\n")
cat("Testing matches:", nrow(test), "\n\n")


# PART 1: Calculate Attack and Defense Strength
#
# Every team gets two numbers:
#   Attack strength = how good they are at scoring
#   Defense strength = how bad they are at defending
#
# These are relative to the league average.
# If a team has attack strength of 1.5, they score
# 50% more goals than the average team.
# If a team has defense strength of 0.8, they concede
# 20% fewer goals than the average team.

# First I need the league average goals per home and away game
avg_home_goals <- mean(train$FTHG)
avg_away_goals <- mean(train$FTAG)

cat("Average home goals per match:", round(avg_home_goals, 3), "\n")
cat("Average away goals per match:", round(avg_away_goals, 3), "\n\n")

# Now calculate each teams attack and defense strength
# I need to look at each team as both home and away

teams <- unique(c(train$HomeTeam, train$AwayTeam))

team_stats <- data.frame(Team = teams)
team_stats$HomeGoalsScored <- 0
team_stats$HomeGoalsConceded <- 0
team_stats$HomeGames <- 0
team_stats$AwayGoalsScored <- 0
team_stats$AwayGoalsConceded <- 0
team_stats$AwayGames <- 0

for (i in 1:nrow(train)) {
  home <- train$HomeTeam[i]
  away <- train$AwayTeam[i]
  
  h_idx <- which(team_stats$Team == home)
  a_idx <- which(team_stats$Team == away)
  
  # Home team stats
  team_stats$HomeGoalsScored[h_idx] <- team_stats$HomeGoalsScored[h_idx] + train$FTHG[i]
  team_stats$HomeGoalsConceded[h_idx] <- team_stats$HomeGoalsConceded[h_idx] + train$FTAG[i]
  team_stats$HomeGames[h_idx] <- team_stats$HomeGames[h_idx] + 1
  
  # Away team stats
  team_stats$AwayGoalsScored[a_idx] <- team_stats$AwayGoalsScored[a_idx] + train$FTAG[i]
  team_stats$AwayGoalsConceded[a_idx] <- team_stats$AwayGoalsConceded[a_idx] + train$FTHG[i]
  team_stats$AwayGames[a_idx] <- team_stats$AwayGames[a_idx] + 1
}

# Attack strength = how many goals per game vs league average
# Defense strength = how many goals conceded per game vs league average
#
# Example: if Inter scores 2.1 goals per home game
# and the league average is 1.5, then Inter's
# home attack strength = 2.1 / 1.5 = 1.4
# meaning they score 40% more than average at home.

team_stats$HomeAttack <- (team_stats$HomeGoalsScored / team_stats$HomeGames) / avg_home_goals
team_stats$HomeDefense <- (team_stats$HomeGoalsConceded / team_stats$HomeGames) / avg_away_goals
team_stats$AwayAttack <- (team_stats$AwayGoalsScored / team_stats$AwayGames) / avg_away_goals
team_stats$AwayDefense <- (team_stats$AwayGoalsConceded / team_stats$AwayGames) / avg_home_goals

# Show the top teams by attack strength
cat("=== Team Strengths (from training data) ===\n")
display <- team_stats[, c("Team", "HomeAttack", "HomeDefense", "AwayAttack", "AwayDefense")]
display[, 2:5] <- round(display[, 2:5], 3)
print(display[order(-display$HomeAttack), ])


# PART 2: Predict Matches Using Poisson Distribution
#
# The Poisson formula gives the probability of scoring
# exactly k goals:
#
#   P(k goals) = (lambda^k * e^(-lambda)) / k!
#
# Where lambda is the expected number of goals.
# For a home team:
#   lambda_home = avg_home_goals * home_attack * away_defense
#
# This makes intuitive sense:
#   start with the average, then adjust up if the home
#   team has a strong attack, and adjust up again if the
#   away team has a weak defense.

predict_match <- function(home_team, away_team, team_stats, avg_hg, avg_ag) {
  
  h_idx <- which(team_stats$Team == home_team)
  a_idx <- which(team_stats$Team == away_team)
  
  # If a team is not in training data, use league average (1.0)
  if (length(h_idx) == 0 | length(a_idx) == 0) {
    return(c(NA, NA, NA, NA, NA))
  }
  
  # Expected goals for each team
  lambda_home <- avg_hg * team_stats$HomeAttack[h_idx] * team_stats$AwayDefense[a_idx]
  lambda_away <- avg_ag * team_stats$AwayAttack[a_idx] * team_stats$HomeDefense[h_idx]
  
  # Calculate probability of each possible scoreline
  # I check scores from 0-0 up to 6-6
  # (anything above 6 goals is extremely rare)
  max_goals <- 6
  home_win_prob <- 0
  draw_prob <- 0
  away_win_prob <- 0
  
  for (hg in 0:max_goals) {
    for (ag in 0:max_goals) {
      # Poisson probability for each team scoring exactly this many
      # dpois() is R's built in Poisson formula
      # dpois(k, lambda) = (lambda^k * e^(-lambda)) / k!
      p_home <- dpois(hg, lambda_home)
      p_away <- dpois(ag, lambda_away)
      
      # Joint probability of this exact scoreline
      # I assume home and away goals are independent
      # (this is a known weakness, Dixon-Coles fixes this)
      p_score <- p_home * p_away
      
      if (hg > ag) {
        home_win_prob <- home_win_prob + p_score
      } else if (hg < ag) {
        away_win_prob <- away_win_prob + p_score
      } else {
        draw_prob <- draw_prob + p_score
      }
    }
  }
  
  return(c(round(lambda_home, 2), round(lambda_away, 2),
           round(home_win_prob, 4), round(draw_prob, 4), 
           round(away_win_prob, 4)))
}


# PART 3: Test the Model on 2024/25 Season
#
# I run the model on every match in the test set and
# see if it predicts the correct outcome.

cat("\n=== Running predictions on 2024/25 season ===\n")

results <- data.frame(
  HomeTeam = test$HomeTeam,
  AwayTeam = test$AwayTeam,
  ActualHome = test$FTHG,
  ActualAway = test$FTAG,
  ActualResult = test$FTR,
  PredHomeGoals = NA,
  PredAwayGoals = NA,
  ProbHome = NA,
  ProbDraw = NA,
  ProbAway = NA
)

for (i in 1:nrow(test)) {
  pred <- predict_match(test$HomeTeam[i], test$AwayTeam[i],
                        team_stats, avg_home_goals, avg_away_goals)
  results$PredHomeGoals[i] <- pred[1]
  results$PredAwayGoals[i] <- pred[2]
  results$ProbHome[i] <- pred[3]
  results$ProbDraw[i] <- pred[4]
  results$ProbAway[i] <- pred[5]
}

# Remove any matches with teams not in training data
results <- results[complete.cases(results), ]

# The predicted result is whichever outcome has the highest probability
results$PredResult <- ifelse(results$ProbHome > results$ProbDraw & 
                               results$ProbHome > results$ProbAway, "H",
                             ifelse(results$ProbAway > results$ProbDraw, "A", "D"))

# Show first 10 predictions
cat("\nFirst 10 match predictions:\n")
print(head(results[, c("HomeTeam", "AwayTeam", "ActualResult", 
                       "PredResult", "ProbHome", "ProbDraw", "ProbAway")], 10))


# PART 4: How Accurate Is the Model?
#
# I check what percentage of matches the model
# predicted the correct outcome (home win, draw, or away win).
#
# Note: my model predicted 0 draws correctly. This is a known
# weakness of the basic Poisson model because it treats home
# and away goals as independent events. In reality, when one
# team sits back and defends, BOTH teams score less, making
# draws more common than the model expects. The Dixon-Coles
# model fixes this by adding a correlation parameter.

correct <- sum(results$ActualResult == results$PredResult)
total <- nrow(results)
accuracy <- round(correct / total * 100, 1)

cat("\n=== Model Accuracy ===\n")
cat("Correct predictions:", correct, "out of", total, "\n")
cat("Accuracy:", accuracy, "%\n\n")

# Break down accuracy by result type
cat("Home wins predicted correctly:", 
    sum(results$ActualResult == "H" & results$PredResult == "H"), "out of",
    sum(results$ActualResult == "H"), "\n")
cat("Draws predicted correctly:", 
    sum(results$ActualResult == "D" & results$PredResult == "D"), "out of",
    sum(results$ActualResult == "D"), "\n")
cat("Away wins predicted correctly:", 
    sum(results$ActualResult == "A" & results$PredResult == "A"), "out of",
    sum(results$ActualResult == "A"), "\n\n")


# PART 5: Compare My Model to Bookmakers
#
# The test data has betting odds from Bet365 (B365H, B365D, B365A).
# Lower odds = bookmaker thinks its more likely to happen.
# I convert odds to probabilities, then compare accuracy.
#
# Odds to probability formula:
#   probability = 1 / odds
#
# Note: bookmaker probabilities add up to more than 1.0
# because they include their profit margin (called the overround
# or vigorish). I normalize them so they add up to exactly 1.0.

original_test <- read.csv("https://www.football-data.co.uk/mmz4281/2425/I1.csv")

results$B365H <- original_test$B365H[1:nrow(results)]
results$B365D <- original_test$B365D[1:nrow(results)]
results$B365A <- original_test$B365A[1:nrow(results)]

# Convert odds to probabilities
results$BookProbH <- 1 / results$B365H
results$BookProbD <- 1 / results$B365D
results$BookProbA <- 1 / results$B365A

# Normalize so they add up to 1
total_prob <- results$BookProbH + results$BookProbD + results$BookProbA
results$BookProbH <- round(results$BookProbH / total_prob, 4)
results$BookProbD <- round(results$BookProbD / total_prob, 4)
results$BookProbA <- round(results$BookProbA / total_prob, 4)

# Bookmaker predicted result = highest probability
results$BookResult <- ifelse(results$BookProbH > results$BookProbD & 
                               results$BookProbH > results$BookProbA, "H",
                             ifelse(results$BookProbA > results$BookProbD, "A", "D"))

book_correct <- sum(results$ActualResult == results$BookResult)
book_accuracy <- round(book_correct / nrow(results) * 100, 1)

cat("=== My Model vs Bookmakers ===\n")
cat("My model accuracy:    ", accuracy, "%\n")
cat("Bookmaker accuracy:   ", book_accuracy, "%\n")
cat("Difference:           ", round(accuracy - book_accuracy, 1), "percentage points\n\n")


# PART 6: Visualization
#
# I plot my predicted probabilities against the bookmaker
# probabilities. If my model agrees with the bookmakers,
# the dots should sit on the red diagonal line.
# Dots far from the line are matches where we disagree.
# I label the 5 biggest disagreements on each plot.

# Find the biggest disagreements
results$HomeDisagree <- abs(results$ProbHome - results$BookProbH)
results$DrawDisagree <- abs(results$ProbDraw - results$BookProbD)
results$AwayDisagree <- abs(results$ProbAway - results$BookProbA)

# Create match labels like "Inter v Napoli"
results$MatchLabel <- paste(results$HomeTeam, "v", results$AwayTeam)

# Only label the top 5 biggest disagreements on each plot
top_home <- results[order(-results$HomeDisagree), ][1:5, ]
top_draw <- results[order(-results$DrawDisagree), ][1:5, ]
top_away <- results[order(-results$AwayDisagree), ][1:5, ]

p1 <- ggplot(results, aes(x = ProbHome, y = BookProbH)) +
  geom_point(color = "#2563EB", alpha = 0.4, size = 2) +
  geom_point(data = top_home, color = "#1E3A5F", size = 3) +
  geom_text(data = top_home, aes(label = MatchLabel), 
            size = 2.5, nudge_y = 0.03, fontface = "bold") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Home Win Probability",
       x = "My Poisson Model", y = "Bet365 Bookmaker") +
  theme_minimal() +
  xlim(0, 1) + ylim(0, 1)

p2 <- ggplot(results, aes(x = ProbDraw, y = BookProbD)) +
  geom_point(color = "#16A34A", alpha = 0.4, size = 2) +
  geom_point(data = top_draw, color = "#0B5D1E", size = 3) +
  geom_text(data = top_draw, aes(label = MatchLabel), 
            size = 2.5, nudge_y = 0.015, fontface = "bold") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Draw Probability",
       x = "My Poisson Model", y = "Bet365 Bookmaker") +
  theme_minimal() +
  xlim(0, 0.5) + ylim(0, 0.5)

p3 <- ggplot(results, aes(x = ProbAway, y = BookProbA)) +
  geom_point(color = "#DC2626", alpha = 0.4, size = 2) +
  geom_point(data = top_away, color = "#7F1D1D", size = 3) +
  geom_text(data = top_away, aes(label = MatchLabel), 
            size = 2.5, nudge_y = 0.03, fontface = "bold") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Away Win Probability",
       x = "My Poisson Model", y = "Bet365 Bookmaker") +
  theme_minimal() +
  xlim(0, 1) + ylim(0, 1)

# Save the plot
ggsave("plots/poisson_vs_bookmakers.png", 
       grid.arrange(p1, p2, p3, ncol = 3),
       width = 15, height = 5, dpi = 300)

cat("Comparison plot saved to plots/poisson_vs_bookmakers.png\n")

# Save all results
write.csv(results, "data/serie_a_poisson_predictions.csv", row.names = FALSE)
write.csv(team_stats, "data/serie_a_team_strengths.csv", row.names = FALSE)
cat("Results saved to data/ folder\n")