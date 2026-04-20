# ============================================
# Pythagorean Expected Points Model for Serie A
# Project: Serie A Predictive Analytics
# Inspired by Clive Beggs' SoccerAnalytics (Chapter 5)
# Adapted and calibrated for Serie A
# ============================================

rm(list = ls())

# --- Load the data we saved in Script 01 ---
all_matches <- read.csv("data/serie_a_all_matches.csv")
league_table <- read.csv("data/serie_a_league_table_2425.csv")

# ============================================
# PART 1: Calibrate Serie A league parameters
# ============================================

# Compute home win, away win, and draw probabilities for Serie A
# Using all 4 seasons for a robust estimate

total_matches <- nrow(all_matches)
home_wins <- sum(all_matches$FTR == "H")
away_wins <- sum(all_matches$FTR == "A")
draws <- sum(all_matches$FTR == "D")

pH <- round(home_wins / total_matches, 3)
pA <- round(away_wins / total_matches, 3)
pD <- round(draws / total_matches, 3)

cat("=== Serie A Match Outcome Probabilities (4 seasons) ===\n")
cat("Home Win:", pH, "\n")
cat("Away Win:", pA, "\n")
cat("Draw:", pD, "\n\n")

# ============================================
# ============================================

# How much of the league table is due to skill vs. luck?
n_teams <- nrow(league_table)
m <- mean(league_table$PLD)  # Matches per team

# Observed variance in points
avg_pts <- mean(league_table$PTS)
pts_var <- mean((league_table$PTS - avg_pts)^2)

# Expected variance if the league were completely random
exp_var <- (m/2) * (9 - (7*pD) - ((3*pH + pD)^2) - ((3*pA + pD)^2))

# Fraction of variance due to chance
chance_fraction <- round(exp_var / pts_var, 3)

cat("=== Competitive Balance ===\n")
cat("Average points:", round(avg_pts, 1), "\n")
cat("Observed variance:", round(pts_var, 1), "\n")
cat("Expected random variance:", round(exp_var, 1), "\n")
cat("Fraction due to chance:", chance_fraction, "\n")
cat("Fraction due to skill:", 1 - chance_fraction, "\n\n")

# ============================================
# PART 3: Pythagorean Expected Points
# ============================================

# Pythagorean model coefficients (calibrated for Serie A)
a <- 2.78
b <- 1.24
c <- 1.24
d <- 1.25

# Compute Pythagorean expected points for each team
league_table$PythagFrac <- (league_table$GF^b) / 
  ((league_table$GF^c) + (league_table$GA^d))
league_table$PythagPTS <- round(a * league_table$PythagFrac * league_table$PLD, 2)
league_table$PythagDiff <- round(league_table$PTS - league_table$PythagPTS, 2)

# Display results
cat("=== Pythagorean Expected Points vs Actual ===\n")
print(league_table[, c("Club", "PLD", "GF", "GA", "PTS", "PythagPTS", "PythagDiff")])

# ============================================
# PART 4: Evaluate Model Accuracy
# ============================================

# Correlation between actual and predicted
cor_result <- cor.test(league_table$PTS, league_table$PythagPTS)
mae <- round(mean(abs(league_table$PythagDiff)), 2)

cat("\n=== Model Evaluation ===\n")
cat("Correlation (r):", round(cor_result$estimate, 4), "\n")
cat("R-squared:", round(cor_result$estimate^2, 4), "\n")
cat("Mean Absolute Error:", mae, "points\n\n")

# ============================================
# PART 5: Visualization
# ============================================

# Scatter plot: Actual vs Pythagorean points
png("plots/actual_vs_pythagorean_pts.png", width = 800, height = 600)
plot(league_table$PTS, league_table$PythagPTS, 
     col = "darkblue", pch = 19, cex = 1.5,
     xlab = "Actual Points", 
     ylab = "Pythagorean Expected Points",
     main = "Serie A 2024/25: Actual vs Pythagorean Expected Points",
     xlim = c(10, 90), ylim = c(10, 90))
abline(lm(league_table$PythagPTS ~ league_table$PTS), lty = 2, col = "red", lwd = 2)
abline(0, 1, lty = 3, col = "gray50")  # Perfect prediction line
text(league_table$PTS, league_table$PythagPTS, 
     labels = league_table$Club, cex = 0.6, pos = 3)
legend("bottomright", 
       legend = c("Regression line", "Perfect prediction"),
       lty = c(2, 3), col = c("red", "gray50"), lwd = 2)
dev.off()

cat("Plot saved to plots/actual_vs_pythagorean_pts.png\n")

# --- Overperformers and underperformers ---
cat("\n=== Biggest Overperformers (more PTS than expected) ===\n")
over <- league_table[order(-league_table$PythagDiff), c("Club", "PTS", "PythagPTS", "PythagDiff")]
print(head(over, 5))

cat("\n=== Biggest Underperformers (fewer PTS than expected) ===\n")
print(tail(over, 5))

# Save updated table
write.csv(league_table, "data/serie_a_pythagorean_results.csv", row.names = FALSE)
cat("\nResults saved to data/serie_a_pythagorean_results.csv")