# Live Prediction: Serie A 2025/26 Season
#
# The season is currently in progress (33 games played).
# I apply my combined model to the current season to:
#   1. Test accuracy on live data with new teams (Pisa, Cremonese)
#   2. Predict final end-of-season standings

rm(list = ls())

library(ggplot2)

# Load current season data
xg_2526 <- read.csv("data/serie_a_xg_2526.csv", sep = ";")

# Clean team names
xg_2526$team <- gsub("AC Milan", "Milan", xg_2526$team)
xg_2526$team <- gsub("Parma Calcio 1913", "Parma", xg_2526$team)
xg_2526$team <- gsub("Hellas Verona", "Verona", xg_2526$team)

# These are the optimized parameters from my combined model
best <- c(0.6908, -0.0439, 0.6345, 0.0775, 0.155, 2.9805, 1.3325, 1.3542)

avg_ppda <- mean(xg_2526$ppda)


# PART 1: Test Model on Current Standings (33 games)

predict_combined <- function(data, params, avg_ppda) {
  w1 <- params[1]
  w2 <- params[2]
  w3 <- params[3]
  w4 <- params[4]
  w5 <- params[5]
  a  <- params[6]
  b  <- params[7]
  d  <- params[8]
  
  blended_gf <- w1 * data$goals + w2 * data$xG
  blended_ga <- w3 * data$ga + w4 * data$xGA
  press_adj <- 1 + w5 * (avg_ppda - data$ppda) / avg_ppda
  
  pythag_frac <- (blended_gf^b) / ((blended_gf^b) + (blended_ga^d))
  pred_pts <- a * pythag_frac * data$matches * press_adj
  
  return(pred_pts)
}

pred_33 <- predict_combined(xg_2526, best, avg_ppda)
xg_2526$Predicted33 <- round(pred_33, 1)
xg_2526$Error33 <- round(xg_2526$points - pred_33, 1)

mae_2526 <- round(mean(abs(xg_2526$Error33)), 2)

cat("=== Model Test on 2025/26 Season (33 games played) ===\n")
cat("MAE:", mae_2526, "points\n\n")

cat("For reference:\n")
cat("  2024/25 (training): 2.58\n")
cat("  2023/24 (test):     2.97\n")
cat("  2022/23 (test):     2.34\n")
cat("  2025/26 (LIVE):    ", mae_2526, "\n\n")

cat("=== Current Standings vs Model Prediction ===\n")
current <- xg_2526[order(-xg_2526$points), 
                    c("team", "matches", "points", "Predicted33", "Error33")]
print(current)


# PART 2: Predict End-of-Season Standings

remaining <- 38 - xg_2526$matches
scale_factor <- 38 / xg_2526$matches

projected <- xg_2526
projected$goals <- round(xg_2526$goals * scale_factor, 1)
projected$ga <- round(xg_2526$ga * scale_factor, 1)
projected$xG <- round(xg_2526$xG * scale_factor, 2)
projected$xGA <- round(xg_2526$xGA * scale_factor, 2)
projected$matches <- 38

pred_38 <- predict_combined(projected, best, avg_ppda)
projected$PredictedFinal <- round(pred_38, 1)
projected$LinearProjection <- round(xg_2526$points * scale_factor, 1)

cat("\n=== PREDICTED FINAL STANDINGS 2025/26 ===\n")

final <- projected[order(-projected$PredictedFinal), 
                    c("team", "PredictedFinal", "LinearProjection")]
final$ModelRank <- 1:nrow(final)
final$CurrentRank <- rank(-xg_2526$points, ties.method = "min")[order(-projected$PredictedFinal)]
final$CurrentPTS <- xg_2526$points[order(-projected$PredictedFinal)]

print(final[, c("ModelRank", "team", "CurrentPTS", "PredictedFinal", "LinearProjection")])

cat("\n=== KEY PREDICTIONS ===\n")
cat("Champion:", final$team[1], "with", final$PredictedFinal[1], "points\n")
cat("Relegation zone:\n")
bottom3 <- tail(final, 3)
for (i in 1:3) {
  cat("  ", bottom3$team[i], "-", bottom3$PredictedFinal[i], "points\n")
}


# PART 3: Model vs Linear Projection

projected$ModelVsLinear <- round(projected$PredictedFinal - projected$LinearProjection, 1)

cat("\n=== Model vs Simple Projection ===\n")
insight <- projected[order(-projected$ModelVsLinear), 
                      c("team", "LinearProjection", "PredictedFinal", "ModelVsLinear")]
print(insight)

# Save predictions
write.csv(projected[order(-projected$PredictedFinal), ], 
          "data/serie_a_2526_predictions.csv", row.names = FALSE)
cat("\nPredictions saved!\n")