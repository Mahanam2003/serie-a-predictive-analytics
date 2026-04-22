# Combined Predictive Model for Serie A — with Confidence Scores
#
# This script blends three data sources into one prediction:
#   1. Actual goals (GF and GA)
#   2. Expected goals (xG and xGA from Understat)
#   3. Pressing intensity (PPDA from Understat)
#
# Why blend all three? Each source captures something different:
#   - Actual goals = what literally happened (includes lucky finishes)
#   - xG = whether goals came from genuinely good chances or not
#   - PPDA = how hard a team works without the ball (lower = more pressing)
#
# The optimizer in script 03 found the weights that minimize
# prediction error on 3 seasons of Serie A data. Those exact
# weights are hardcoded here as constants.
#
# NEW in this version: confidence scores for every prediction.
# See PART 3 for the explanation.

rm(list = ls())

library(ggplot2)


# ---- Load Data ----

league_table <- read.csv("data/serie_a_pythagorean_results.csv")

# Note: the xG file has a double .csv extension because of how
# it was originally saved. We use the exact filename as-is.
xg_data <- read.csv("data/serie_a_xg_stats.csv.csv")

# Merge league table with xG / pressing data on team name
# all.x = TRUE keeps all 20 teams; xG rows that don't match get NA
combined <- merge(league_table, xg_data, by.x = "Club", by.y = "Team", all.x = TRUE)

# Drop any teams missing xG or pressing data
# (this can happen for newly promoted clubs not on Understat)
combined <- combined[complete.cases(combined[, c("xG", "xGA", "PPDA")]), ]

cat("Teams with complete data:", nrow(combined), "\n\n")


# PART 1: The Combined Model Formula
#
# The optimizer tested over 130,000 parameter combinations and
# found these 8 values minimize prediction error across seasons:
#
#   w1 =  0.6908  weight for actual goals scored (GF)
#   w2 = -0.0439  weight for expected goals (xG)   ← slightly negative!
#   w3 =  0.6345  weight for actual goals conceded (GA)
#   w4 =  0.0775  weight for expected goals against (xGA)
#   w5 =  0.155   pressing coefficient
#   a  =  2.9805  Pythagorean multiplier
#   b  =  1.3325  GF exponent
#   d  =  1.3542  GA exponent
#
# Why is w2 negative? The model is saying: "if your xG is high
# but your actual goals are also high, you probably didn't just
# get lucky — you're genuinely clinical." So xG gets slightly
# discounted in favour of actual output. Serie A rewards finishing.

w1 <- 0.6908;  w2 <- -0.0439
w3 <- 0.6345;  w4 <-  0.0775
w5 <- 0.155;   a  <-  2.9805
b  <- 1.3325;  d  <-  1.3542

avg_ppda <- mean(combined$PPDA)

# Step 1: Blend actual goals with expected goals.
# BlendedGF = weighted sum of GF and xG.
# Real goals dominate (0.69 weight) but xG adds a small correction.
combined$BlendedGF <- w1 * combined$GF + w2 * combined$xG
combined$BlendedGA <- w3 * combined$GA + w4 * combined$xGA

# Step 2: Pressing adjustment.
# PressAdj > 1.0 → team presses more than average → slight boost
# PressAdj < 1.0 → team presses less than average → slight penalty
# Lower PPDA = more aggressive pressing.
# So (avg_ppda - team_ppda) is positive for high-pressure teams.
combined$PressAdj <- 1 + w5 * (avg_ppda - combined$PPDA) / avg_ppda

# Step 3: Pythagorean formula with pressing baked in.
# The Pythagorean fraction (blended_gf^b / (blended_gf^b + blended_ga^d))
# is a number from 0 to 1 representing how dominant a team is.
# Multiply by the Pythagorean multiplier (a) and games played to
# convert that dominance score into predicted season points.
combined$PredPTS <- a * (combined$BlendedGF^b /
                           (combined$BlendedGF^b + combined$BlendedGA^d)) *
                    combined$PLD * combined$PressAdj

combined$PredPTS <- round(combined$PredPTS, 1)
combined$Diff    <- round(combined$PTS - combined$PredPTS, 1)

cat("=== Combined Model Results (2024/25) ===\n")
print(combined[order(-combined$PTS),
               c("Club", "PTS", "PredPTS", "Diff")])


# PART 2: Model Accuracy
#
# MAE = 2.58 means on average the model is within 2.58 points
# per team — that's the headline number on the website.
# Correlation and R-squared tell us how well it ranks teams
# in the right order, not just how close the numbers are.

mae   <- round(mean(abs(combined$Diff)), 2)
r_val <- cor(combined$PTS, combined$PredPTS)

cat("\n=== Model Accuracy ===\n")
cat("Correlation (r):", round(r_val, 4), "\n")
cat("R-squared:      ", round(r_val^2, 4), "\n")
cat("MAE:            ", mae, "points per team\n\n")


# PART 3: Confidence Scores for Season Predictions
#
# Not all predictions are equally reliable. Consider two scenarios:
#
# Scenario A: Team X predicted at 70 pts, nearest rival at 64 pts.
#   Even if the model is off by ±3 points, team X still finishes above.
#   We should be CONFIDENT about their position.
#
# Scenario B: Teams X, Y, Z all predicted within 2 points of each other.
#   Any small model error could swap their order entirely.
#   We should have LOW CONFIDENCE about any individual ranking here.
#
# FORMULA:
#   gap = distance in predicted points to the nearest rival
#   confidence = clamp( gap / (2 × MAE) × 100, 40, 90 )
#
# Where:
#   2 × MAE = 5.16 pts (our "one standard error" width)
#
#   gap < 2.58 pts  (inside 1 MAE) → confidence < 50%
#   gap = 5.16 pts  (exactly 2 MAE) → confidence = 100% → clamped to 90%
#   gap > 5.16 pts  → confidence = 90% (ceiling)
#
# We floor at 40% because even in tight races the model beats random
# (random would give every outcome equal probability, that's worse).

cat("=== Confidence Scores for Each Position Prediction ===\n\n")

sorted_preds <- sort(combined$PredPTS)
combined$Confidence <- 0

for (i in 1:nrow(combined)) {
  pred_pts <- combined$PredPTS[i]

  # Find the nearest other team's predicted points
  others <- sorted_preds[sorted_preds != pred_pts]

  if (length(others) == 0) {
    combined$Confidence[i] <- 75
    next
  }

  gap <- min(abs(others - pred_pts))

  # Scale: gap of 2×MAE (5.16 pts) → 100%, then clamp to [40, 90]
  conf_raw <- (gap / (2 * mae)) * 100
  combined$Confidence[i] <- max(40, min(90, round(conf_raw)))
}

# Readable label alongside the number
combined$ConfLabel <- ifelse(combined$Confidence >= 70, "HIGH",
                      ifelse(combined$Confidence >= 50, "MED",  "LOW"))

print(combined[order(-combined$PredPTS),
               c("Club", "PredPTS", "PTS", "Diff", "Confidence", "ConfLabel")])

cat("\nMean confidence:          ", round(mean(combined$Confidence)), "%\n")
cat("HIGH confidence (70%+):   ", sum(combined$ConfLabel == "HIGH"), "teams\n")
cat("MED  confidence (50-69%): ", sum(combined$ConfLabel == "MED"),  "teams\n")
cat("LOW  confidence (<50%):   ", sum(combined$ConfLabel == "LOW"),  "teams\n\n")

cat("Interpretation: LOW confidence means multiple teams are predicted\n")
cat("within the model's ±2.58 point error margin. Any of them could\n")
cat("realistically finish in each other's position.\n\n")


# PART 4: Visualize Predictions with Confidence
#
# Each team is a dot on a vertical chart sorted by predicted points.
# The error bar shows ±1 MAE (2.58 pts) — the model's typical miss range.
# Dot color = confidence tier:
#   Green  = HIGH: team is clearly separated from nearest rival
#   Amber  = MED:  one MAE could shift their position by ±1 place
#   Red    = LOW:  tight cluster, ranking is genuinely uncertain
#
# Note: clusters of red dots = contested spots in the table.
# In 2024/25 you can expect red in the 6th-9th range (Europa spots).

combined$ConfColor <- ifelse(combined$Confidence >= 70, "#22c55e",
                      ifelse(combined$Confidence >= 50, "#f59e0b", "#ef4444"))

combined$Club <- factor(combined$Club,
                        levels = combined$Club[order(combined$PredPTS)])

p <- ggplot(combined, aes(x = Club, y = PredPTS, color = ConfColor)) +
  geom_errorbar(aes(ymin = PredPTS - mae, ymax = PredPTS + mae),
                width = 0.35, alpha = 0.45, linewidth = 0.8) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(Confidence, "%")),
            size = 2.8, vjust = -1.4, fontface = "bold") +
  scale_color_identity() +
  coord_flip() +
  labs(
    title    = "Combined Model Predictions with Confidence Scores",
    subtitle = paste0("Error bars = ±1 MAE (", mae, " pts). Label = confidence in predicted table position."),
    x        = NULL,
    y        = "Predicted Points"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "gray60", size = 10),
    panel.grid.minor = element_blank()
  )

ggsave("plots/combined_model_predictions.png", p,
       width = 10, height = 7.5, dpi = 300)
cat("Plot saved to plots/combined_model_predictions.png\n")


# Save results with confidence column
out_cols <- c("Club", "PTS", "PredPTS", "Diff",
              "BlendedGF", "BlendedGA", "PressAdj",
              "Confidence", "ConfLabel")

write.csv(combined[, out_cols], "data/serie_a_combined_model_results.csv",
          row.names = FALSE)
cat("Results saved to data/serie_a_combined_model_results.csv\n")
