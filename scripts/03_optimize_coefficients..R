# Optimizing Pythagorean Model Coefficients for Serie A
#
# The original model uses coefficients from the EPL
# (a=2.78, b=1.24, c=1.24, d=1.25) which gave me
# an MAE of 3.54 points. I want to find better values
# that are specifically tuned for Italian football.
#
# My approach: try thousands of different combinations
# and keep whichever one gives the smallest error.

rm(list = ls())

# Loading my data
league_table <- read.csv("data/serie_a_league_table_2425.csv")

# This function takes any values of a, b, c, d and
# calculates the MAE (mean absolute error) for Serie A.
# Lower MAE = better prediction = better coefficients.

calc_mae <- function(a, b, c, d, data) {
  pythag_frac <- (data$GF^b) / ((data$GF^c) + (data$GA^d))
  pythag_pts <- a * pythag_frac * data$PLD
  mae <- mean(abs(data$PTS - pythag_pts))
  return(mae)
}

# First let me confirm the original EPL coefficients error
original_mae <- calc_mae(2.78, 1.24, 1.24, 1.25, league_table)
cat("Original EPL coefficients MAE:", round(original_mae, 4), "\n\n")


# METHOD 1: Grid Search
# I create a range of possible values for each coefficient
# and test every combination. This is brute force but
# its simple and guaranteed to find the best values
# within the ranges I specify.

cat("Running grid search... this may take a minute\n")

# Setting up ranges to search through
a_range <- seq(2.4, 3.2, by = 0.02)   # multiplier
b_range <- seq(1.0, 2.0, by = 0.02)   # GF exponent
c_range <- seq(1.0, 2.0, by = 0.02)   # GF exponent in denominator
d_range <- seq(1.0, 2.0, by = 0.02)   # GA exponent

# Since testing ALL combinations of 4 variables would take
# forever, I optimize in two stages.

# STAGE 1: Fix a=2.78 and optimize b, c, d
cat("Stage 1: Optimizing exponents b, c, d...\n")

best_mae <- Inf  # start with infinity so anything is better
best_b <- 1.24
best_c <- 1.24
best_d <- 1.25

for (b in b_range) {
  for (c_val in c_range) {
    for (d in d_range) {
      current_mae <- calc_mae(2.78, b, c_val, d, league_table)
      if (current_mae < best_mae) {
        best_mae <- current_mae
        best_b <- b
        best_c <- c_val
        best_d <- d
      }
    }
  }
}

cat("Best exponents found: b =", best_b, 
    " c =", best_c, " d =", best_d, "\n")
cat("MAE with optimized exponents:", round(best_mae, 4), "\n\n")

# STAGE 2: Now optimize the multiplier 'a' using the best exponents
cat("Stage 2: Optimizing multiplier a...\n")

best_a <- 2.78
best_mae_final <- Inf

for (a in a_range) {
  current_mae <- calc_mae(a, best_b, best_c, best_d, league_table)
  if (current_mae < best_mae_final) {
    best_mae_final <- current_mae
    best_a <- a
  }
}

cat("Best multiplier found: a =", best_a, "\n\n")


# FINAL RESULTS: Compare old vs new coefficients

cat("=== COMPARISON ===\n")
cat("EPL coefficients:      a=2.78  b=1.24  c=1.24  d=1.25\n")
cat("Serie A optimized:     a=", best_a, " b=", best_b, 
    " c=", best_c, " d=", best_d, "\n\n")

cat("EPL MAE:              ", round(original_mae, 4), "points\n")
cat("Serie A optimized MAE:", round(best_mae_final, 4), "points\n")
cat("Improvement:          ", 
    round(original_mae - best_mae_final, 4), "points\n\n")


# Apply the optimized model to the league table

league_table$OptPythagFrac <- (league_table$GF^best_b) / 
  ((league_table$GF^best_c) + (league_table$GA^best_d))
league_table$OptPythagPTS <- round(best_a * league_table$OptPythagFrac * league_table$PLD, 2)
league_table$OptPythagDiff <- round(league_table$PTS - league_table$OptPythagPTS, 2)

# Show the comparison side by side
# Old = EPL coefficients, New = Serie A optimized
old_frac <- (league_table$GF^1.24) / ((league_table$GF^1.24) + (league_table$GA^1.25))
league_table$OldPythagPTS <- round(2.78 * old_frac * league_table$PLD, 2)

cat("=== Old vs New Predictions ===\n")
comparison <- league_table[, c("Club", "PTS", "OldPythagPTS", "OptPythagPTS", "OptPythagDiff")]
print(comparison)

# Correlation check on optimized model
cor_new <- cor.test(league_table$PTS, league_table$OptPythagPTS)
cat("\nOptimized model correlation:", round(cor_new$estimate, 4), "\n")
cat("Optimized model R-squared:", round(cor_new$estimate^2, 4), "\n")

# Save a plot comparing old vs new model
png("plots/old_vs_optimized_pythagorean.png", width = 900, height = 600)
par(mfrow = c(1, 2))

# Left plot: old model
plot(league_table$PTS, league_table$OldPythagPTS,
     col = "darkblue", pch = 19, cex = 1.3,
     xlab = "Actual Points", ylab = "Predicted Points",
     main = paste0("EPL Coefficients (MAE = ", round(original_mae, 2), ")"),
     xlim = c(10, 90), ylim = c(10, 90))
abline(0, 1, lty = 3, col = "gray50")
text(league_table$PTS, league_table$OldPythagPTS,
     labels = league_table$Club, cex = 0.5, pos = 3)

# Right plot: new model
plot(league_table$PTS, league_table$OptPythagPTS,
     col = "darkred", pch = 19, cex = 1.3,
     xlab = "Actual Points", ylab = "Predicted Points",
     main = paste0("Serie A Optimized (MAE = ", round(best_mae_final, 2), ")"),
     xlim = c(10, 90), ylim = c(10, 90))
abline(0, 1, lty = 3, col = "gray50")
text(league_table$PTS, league_table$OptPythagPTS,
     labels = league_table$Club, cex = 0.5, pos = 3)

par(mfrow = c(1, 1))
dev.off()

cat("\nSide-by-side plot saved to plots/old_vs_optimized_pythagorean.png\n")

# Save results
write.csv(league_table, "data/serie_a_optimized_results.csv", row.names = FALSE)
cat("Results saved to data/serie_a_optimized_results.csv\n")

cat("\nNote: these coefficients are optimized on one season.\n")
cat("To make them more robust, I should test them on other\n")
cat("seasons to make sure they generalize and Im not just\n")
cat("overfitting to this specific year.\n")