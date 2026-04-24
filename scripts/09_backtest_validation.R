# Backtest Validation: Did My Model Actually Work?
#
# GOAL: Prove the model can predict match outcomes BEFORE they happen.
#
# METHOD:
#   1. Train on data through matchweek 32 (pretend week 33 hasn't happened yet)
#   2. Predict matchweek 33 results
#   3. Compare predictions to ACTUAL results from week 33
#   4. Calculate accuracy metrics
#
# This is how you validate a predictive model in real-world conditions.

rm(list = ls())

library(ggplot2)

# PART 1: Load Current Season Data

current_raw <- read.csv("https://www.football-data.co.uk/mmz4281/2526/I1.csv")
played <- current_raw[!is.na(current_raw$FTHG) & current_raw$FTHG != "", ]

cat("Total matches played:", nrow(played), "\n")
cat("Total matchweeks:", max(played$MW, na.rm = TRUE), "\n\n")

# PART 2: Split Into Training (weeks 1-32) and Test (week 33)

# Check if we have matchweek column
if (!"MW" %in% colnames(played)) {
  # If no MW column, estimate it based on date
  played <- played[order(played$Date), ]
  played$MW <- rep(1:ceiling(nrow(played)/10), each=10, length.out=nrow(played))
}

latest_week <- max(played$MW, na.rm = TRUE)
test_week <- latest_week  # Most recent completed week
train_weeks <- 1:(test_week - 1)

train_data <- played[played$MW %in% train_weeks, ]
test_data <- played[played$MW == test_week, ]

cat("Training data: matchweeks", min(train_weeks), "-", max(train_weeks), 
    "(", nrow(train_data), "matches )\n")
cat("Test data: matchweek", test_week, "(", nrow(test_data), "matches )\n\n")

if (nrow(test_data) == 0) {
  cat("No test data available - need at least 2 completed matchweeks.\n")
  quit(save = "no")
}

# PART 3: Calculate Team Strengths Using ONLY Training Data

weighted_mean <- function(x, w) sum(x * w) / sum(w)

# Load historical data
historical <- read.csv("data/serie_a_all_matches.csv")
historical$Weight <- 0.5  # Historical seasons get lower weight

train_data$Season <- "2025"
train_data$Weight <- 2.0  # Current season training data gets high weight

common_cols <- intersect(names(historical), names(train_data))
all_data <- rbind(historical[, common_cols], train_data[, common_cols])

avg_home_goals <- weighted_mean(all_data$FTHG, all_data$Weight)
avg_away_goals <- weighted_mean(all_data$FTAG, all_data$Weight)

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
    Team = team,
    HomeAttack = round(ha_raw / avg_home_goals, 3),
    HomeDefense = round(hd_raw / avg_away_goals, 3),
    AwayAttack = round(aa_raw / avg_away_goals, 3),
    AwayDefense = round(ad_raw / avg_home_goals, 3),
    stringsAsFactors = FALSE
  ))
}

cat("Team strengths calculated for", nrow(team_stats), "teams\n")

# PART 4: Prediction Function

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

# PART 5: Generate Predictions for Test Week

cat("\n=== PREDICTIONS vs ACTUAL RESULTS (Matchweek", test_week, ") ===\n\n")

results <- data.frame()
for (i in 1:nrow(test_data)) {
  home <- test_data$HomeTeam[i]
  away <- test_data$AwayTeam[i]
  actual_result <- test_data$FTR[i]
  
  pred <- predict_match(home, away, team_stats, avg_home_goals, avg_away_goals)
  
  correct <- ifelse(pred$pred_result == actual_result, "✓ CORRECT", "✗ WRONG")
  
  results <- rbind(results, data.frame(
    HomeTeam = home,
    AwayTeam = away,
    ActualScore = paste(test_data$FTHG[i], "-", test_data$FTAG[i]),
    ActualResult = actual_result,
    PredResult = pred$pred_result,
    Confidence = pred$confidence,
    Correct = (pred$pred_result == actual_result),
    stringsAsFactors = FALSE
  ))
  
  cat(sprintf("%-18s vs %-18s", home, away))
  cat(sprintf("  Actual: %s  Predicted: %s  %s", 
              actual_result, pred$pred_result, correct))
  cat(sprintf("  (Confidence: %d%%)\n", pred$confidence))
}

# PART 6: Calculate Accuracy Metrics

n_correct <- sum(results$Correct)
n_total <- nrow(results)
accuracy <- round(n_correct / n_total * 100, 1)

# Baseline: always predict home win
baseline_correct <- sum(results$ActualResult == "H")
baseline_accuracy <- round(baseline_correct / n_total * 100, 1)

cat("\n=== ACCURACY METRICS ===\n")
cat("Matches predicted:   ", n_total, "\n")
cat("Correct predictions: ", n_correct, "\n")
cat("Accuracy:            ", accuracy, "%\n\n")

cat("Baseline (always predict home win):", baseline_accuracy, "%\n")
cat("Model improvement over baseline:   ", round(accuracy - baseline_accuracy, 1), 
    "percentage points\n\n")

# Break down by confidence level
high_conf <- results[results$Confidence >= 30, ]
low_conf <- results[results$Confidence < 30, ]

if (nrow(high_conf) > 0) {
  high_acc <- round(sum(high_conf$Correct) / nrow(high_conf) * 100, 1)
  cat("High confidence predictions (30%+): ", high_acc, "% accurate (", 
      sum(high_conf$Correct), "/", nrow(high_conf), ")\n")
}
if (nrow(low_conf) > 0) {
  low_acc <- round(sum(low_conf$Correct) / nrow(low_conf) * 100, 1)
  cat("Low confidence predictions (<30%):  ", low_acc, "% accurate (", 
      sum(low_conf$Correct), "/", nrow(low_conf), ")\n")
}

# PART 7: Visualize Results

results$Result <- ifelse(results$Correct, "Correct", "Wrong")
results$ConfBucket <- cut(results$Confidence, 
                          breaks = c(0, 20, 40, 60, 100),
                          labels = c("0-20%", "20-40%", "40-60%", "60%+"))

accuracy_by_conf <- aggregate(Correct ~ ConfBucket, data = results, FUN = mean)
accuracy_by_conf$Accuracy <- round(accuracy_by_conf$Correct * 100, 1)

p1 <- ggplot(accuracy_by_conf, aes(x = ConfBucket, y = Accuracy, fill = ConfBucket)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(Accuracy, "%")), vjust = -0.5, fontface = "bold") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
  labs(title = "Model Accuracy by Confidence Level",
       subtitle = paste("Backtest on matchweek", test_week, "predictions"),
       x = "Model Confidence", y = "Accuracy (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#EF4444", "#F59E0B", "#10B981", "#3B82F6")) +
  ylim(0, 100)

ggsave("plots/backtest_accuracy.png", p1, width = 8, height = 6, dpi = 300)
cat("\nPlot saved to plots/backtest_accuracy.png\n")

# Save results
write.csv(results, "data/backtest_results.csv", row.names = FALSE)
cat("Results saved to data/backtest_results.csv\n")

# PART 8: Statistical Significance Test

cat("\n=== STATISTICAL ANALYSIS ===\n")
cat("Is this accuracy better than random guessing?\n\n")

# Binomial test: if model has no skill, it should get 33% correct (3 outcomes)
binom_test <- binom.test(n_correct, n_total, p = 1/3, alternative = "greater")

cat("Null hypothesis: Model is guessing randomly (33% accuracy expected)\n")
cat("P-value:", round(binom_test$p.value, 4), "\n")

if (binom_test$p.value < 0.05) {
  cat("RESULT: Model is statistically significantly better than random! ✓\n")
} else {
  cat("RESULT: Not enough evidence that model beats random guessing.\n")
  cat("(This is expected with small sample sizes - need more weeks to test)\n")
}

cat("\n=== CONCLUSION ===\n")
if (accuracy > 40) {
  cat("Model shows predictive skill on unseen data.\n")
  cat("Next step: Test on multiple weeks to confirm consistency.\n")
} else {
  cat("Model accuracy is close to random.\n")
  cat("Consider: (1) more training data, (2) feature engineering, (3) different algorithms.\n")
}