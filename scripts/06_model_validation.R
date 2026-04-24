# Combined Predictive Model for Serie A
#
# This model combines the best ideas from all my previous models:
#   1. Pythagorean framework (goals ratio predicts points)
#   2. Serie A optimized coefficients (not EPL defaults)
#   3. xG data (shot quality, not just results)
#   4. PPDA pressing intensity (how aggressively teams press)
#
# The key innovation is blending actual goals with xG.
# Actual goals include luck (deflections, goalkeeper mistakes).
# xG strips out luck but misses clinical finishing ability.
# By blending them, I capture both skill AND chance quality.

rm(list = ls())

library(ggplot2)

# Load data
league_table <- read.csv("data/serie_a_league_table_2425.csv")
xg_data <- read.csv("data/serie_a_xg_stats.csv.csv", sep = ";")

# Clean team names to match between datasets
xg_data$team <- gsub("AC Milan", "Milan", xg_data$team)
xg_data$team <- gsub("Parma Calcio 1913", "Parma", xg_data$team)
xg_data$team <- gsub("Hellas Verona", "Verona", xg_data$team)

# Merge datasets
combined <- merge(league_table, xg_data, by.x = "Club", by.y = "team")
cat("Teams matched:", nrow(combined), "out of 20\n\n")


# PART 1: Define the Combined Model Function

avg_ppda <- mean(combined$ppda)

calc_combined_mae <- function(params, data, avg_ppda) {
  w1 <- params[1]  # weight on actual goals
  w2 <- params[2]  # weight on xG
  w3 <- params[3]  # weight on actual GA
  w4 <- params[4]  # weight on xGA
  w5 <- params[5]  # pressing adjustment strength
  a  <- params[6]  # Pythagorean multiplier
  b  <- params[7]  # GF exponent
  d  <- params[8]  # GA exponent
  
  # Blend actual goals with expected goals
  blended_gf <- w1 * data$GF + w2 * data$xG
  blended_ga <- w3 * data$GA + w4 * data$xGA
  
  # Pressing adjustment
  press_adj <- 1 + w5 * (avg_ppda - data$ppda) / avg_ppda
  
  # Pythagorean prediction with pressing adjustment
  pythag_frac <- (blended_gf^b) / ((blended_gf^b) + (blended_ga^d))
  pred_pts <- a * pythag_frac * data$PLD * press_adj
  
  # Mean absolute error
  mae <- mean(abs(data$PTS - pred_pts))
  return(mae)
}


# PART 2: Optimize All Parameters

start_params <- c(
  w1 = 0.5,    # equal weight on actual goals
  w2 = 0.5,    # equal weight on xG
  w3 = 0.5,    # equal weight on actual GA
  w4 = 0.5,    # equal weight on xGA
  w5 = 0.05,   # small pressing adjustment to start
  a  = 2.78,   # Pythagorean multiplier
  b  = 1.34,   # Serie A optimized exponent
  d  = 1.38    # Serie A optimized exponent
)

cat("Optimizing 8 parameters simultaneously...\n")
result <- optim(
  par = start_params,
  fn = calc_combined_mae,
  data = combined,
  avg_ppda = avg_ppda,
  method = "Nelder-Mead",
  control = list(maxit = 50000)
)

best <- result$par
cat("\n=== Optimized Parameters ===\n")
cat("w1 (actual goals weight): ", round(best[1], 4), "\n")
cat("w2 (xG weight):           ", round(best[2], 4), "\n")
cat("w3 (actual GA weight):    ", round(best[3], 4), "\n")
cat("w4 (xGA weight):          ", round(best[4], 4), "\n")
cat("w5 (pressing adjustment): ", round(best[5], 4), "\n")
cat("a  (multiplier):          ", round(best[6], 4), "\n")
cat("b  (GF exponent):         ", round(best[7], 4), "\n")
cat("d  (GA exponent):         ", round(best[8], 4), "\n\n")


# PART 3: Apply the Optimized Model

combined$BlendedGF <- best[1] * combined$GF + best[2] * combined$xG
combined$BlendedGA <- best[3] * combined$GA + best[4] * combined$xGA
combined$PressAdj <- 1 + best[5] * (avg_ppda - combined$ppda) / avg_ppda

pythag_frac <- (combined$BlendedGF^best[7]) / 
  ((combined$BlendedGF^best[7]) + (combined$BlendedGA^best[8]))
combined$CombinedPred <- round(best[6] * pythag_frac * combined$PLD * combined$PressAdj, 2)
combined$CombinedDiff <- round(combined$PTS - combined$CombinedPred, 2)


# PART 4: Compare ALL Models

# Model 1: Original Pythagorean (EPL coefficients)
frac1 <- (combined$GF^1.24) / ((combined$GF^1.24) + (combined$GA^1.25))
combined$Model1_EPL <- round(2.78 * frac1 * combined$PLD, 2)
mae1 <- round(mean(abs(combined$PTS - combined$Model1_EPL)), 2)

# Model 2: Optimized Pythagorean (Serie A coefficients)
frac2 <- (combined$GF^1.34) / ((combined$GF^1.32) + (combined$GA^1.38))
combined$Model2_OptPythag <- round(2.78 * frac2 * combined$PLD, 2)
mae2 <- round(mean(abs(combined$PTS - combined$Model2_OptPythag)), 2)

# Model 3: xG Pythagorean
frac3 <- (combined$xG^1.34) / ((combined$xG^1.32) + (combined$xGA^1.38))
combined$Model3_xG <- round(2.78 * frac3 * combined$PLD, 2)
mae3 <- round(mean(abs(combined$PTS - combined$Model3_xG)), 2)

# Model 4: Combined model (this script)
mae4 <- round(mean(abs(combined$CombinedDiff)), 2)

# Model 5: Understat xPTS for reference
mae5 <- round(mean(abs(combined$PTS - combined$xPTS)), 2)

cat("=== FINAL MODEL COMPARISON ===\n")
cat("Model 1 - Pythagorean (EPL defaults):     MAE =", mae1, "points\n")
cat("Model 2 - Pythagorean (Serie A optimized): MAE =", mae2, "points\n")
cat("Model 3 - Pythagorean (xG based):          MAE =", mae3, "points\n")
cat("Model 4 - COMBINED MODEL (new!):           MAE =", mae4, "points\n")
cat("Understat xPTS (benchmark):                MAE =", mae5, "points\n\n")

cat("Improvement from Model 1 to Model 4:", round(mae1 - mae4, 2), "points\n")
cat("Improvement percentage:", round((mae1 - mae4) / mae1 * 100, 1), "%\n\n")

# Show predictions for every team
cat("=== Combined Model Predictions ===\n")
final_table <- combined[order(-combined$PTS), 
  c("Club", "PTS", "Model1_EPL", "Model2_OptPythag", "Model3_xG", "CombinedPred", "CombinedDiff")]
print(final_table)

# Visualizations
model_names <- c("EPL Pythag", "Serie A Pythag", "xG Pythag", "Combined", "Understat xPTS")
mae_values <- c(mae1, mae2, mae3, mae4, mae5)
model_df <- data.frame(Model = factor(model_names, levels = model_names), MAE = mae_values)

p1 <- ggplot(model_df, aes(x = Model, y = MAE, fill = Model)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = MAE), vjust = -0.5, size = 4, fontface = "bold") +
  labs(title = "Model Evolution: Reducing Prediction Error",
       subtitle = "Mean Absolute Error (lower is better)",
       y = "MAE (points)", x = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#94A3B8", "#64748B", "#3B82F6", "#DC2626", "#9CA3AF")) +
  ylim(0, max(mae_values) + 1)

ggsave("plots/model_evolution.png", p1, width = 10, height = 6, dpi = 300)

# Save everything
write.csv(combined, "data/serie_a_combined_model_results.csv", row.names = FALSE)
cat("\nResults saved to data/serie_a_combined_model_results.csv\n")

cat("\n=== THE COMBINED MODEL EQUATION ===\n")
cat("BlendedGF =", round(best[1],4), "x Goals +", round(best[2],4), "x xG\n")
cat("BlendedGA =", round(best[3],4), "x GoalsAgainst +", round(best[4],4), "x xGA\n")
cat("PressAdj  = 1 +", round(best[5],4), "x (AvgPPDA - TeamPPDA) / AvgPPDA\n")
cat("PredPTS   =", round(best[6],4), "x (BlendedGF^", round(best[7],4), 
    "/ (BlendedGF^", round(best[7],4), "+ BlendedGA^", round(best[8],4), 
    ")) x 38 x PressAdj\n")