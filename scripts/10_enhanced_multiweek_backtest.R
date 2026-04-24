# Enhanced Model: Multi-Week Backtest + Improvements
#
# IMPROVEMENTS TESTED:
#   1. Home advantage factor (beyond attack/defense split)
#   2. Recent form weighting (last 5 games weighted higher)
#   3. Head-to-head history
#   4. More historical data (if available)
#
# VALIDATION:
#   - Test on weeks 28, 29, 30, 31, 32, 33
#   - Compare enhanced model vs baseline Poisson
#   - Show consistency across multiple weeks

rm(list = ls())

library(ggplot2)

# PART 1: Load All Available Data

current_raw <- read.csv("https://www.football-data.co.uk/mmz4281/2526/I1.csv")
played <- current_raw[!is.na(current_raw$FTHG) & current_raw$FTHG != "", ]

# Estimate matchweek from chronological order
played <- played[order(played$Date), ]
played$MW <- rep(1:ceiling(nrow(played)/10), each=10, length.out=nrow(played))

historical <- read.csv("data/serie_a_all_matches.csv")

cat("=== DATA SUMMARY ===\n")
cat("Current season matches:", nrow(played), "\n")
cat("Latest matchweek:", max(played$MW), "\n")
cat("Historical matches:", nrow(historical), "\n")
cat("Historical seasons:", paste(unique(historical$Season), collapse=", "), "\n\n")


# PART 2: Enhanced Model Functions

# Calculate team strengths with recency weighting and form
calc_team_strengths_enhanced <- function(matches, current_season_weight=2.0, 
                                         recent_form_boost=1.5) {
  # Assign weights
  matches$Weight <- ifelse(matches$Season == "2025", current_season_weight, 0.5)
  
  # Recent form: boost last 5 games for current season
  if ("MW" %in% colnames(matches)) {
    max_week <- max(matches$MW[matches$Season == "2025"], na.rm=TRUE)
    recent_weeks <- (max_week - 4):max_week
    matches$Weight[matches$Season == "2025" & matches$MW %in% recent_weeks] <- 
      matches$Weight[matches$Season == "2025" & matches$MW %in% recent_weeks] * recent_form_boost
  }
  
  weighted_mean <- function(x, w) sum(x * w) / sum(w)
  
  avg_home_goals <- weighted_mean(matches$FTHG, matches$Weight)
  avg_away_goals <- weighted_mean(matches$FTAG, matches$Weight)
  
  # Home advantage factor
  home_advantage <- avg_home_goals / avg_away_goals
  
  all_teams <- unique(c(matches$HomeTeam, matches$AwayTeam))
  
  team_stats <- data.frame()
  for (team in all_teams) {
    home_games <- matches[matches$HomeTeam == team, ]
    away_games <- matches[matches$AwayTeam == team, ]
    
    if (nrow(home_games) < 3 || nrow(away_games) < 3) next
    
    ha_raw <- weighted_mean(home_games$FTHG, home_games$Weight)
    hd_raw <- weighted_mean(home_games$FTAG, home_games$Weight)
    aa_raw <- weighted_mean(away_games$FTAG, away_games$Weight)
    ad_raw <- weighted_mean(away_games$FTHG, away_games$Weight)
    
    team_stats <- rbind(team_stats, data.frame(
      Team = team,
      HomeAttack = ha_raw / avg_home_goals,
      HomeDefense = hd_raw / avg_away_goals,
      AwayAttack = aa_raw / avg_away_goals,
      AwayDefense = ad_raw / avg_home_goals,
      stringsAsFactors = FALSE
    ))
  }
  
  list(
    team_stats = team_stats,
    avg_home_goals = avg_home_goals,
    avg_away_goals = avg_away_goals,
    home_advantage = home_advantage
  )
}

# Enhanced prediction with head-to-head
predict_match_enhanced <- function(home, away, stats_df, avg_hg, avg_ag, 
                                   h2h_matches=NULL, home_adv_factor=1.0) {
  h_idx <- which(stats_df$Team == home)
  a_idx <- which(stats_df$Team == away)
  
  if (length(h_idx) == 0 || length(a_idx) == 0) {
    return(list(lambda_home=avg_hg, lambda_away=avg_ag,
                prob_home=0.33, prob_draw=0.33, prob_away=0.33,
                pred_result="D", confidence=5))
  }
  
  # Base Poisson rates
  lambda_h <- avg_hg * stats_df$HomeAttack[h_idx] * stats_df$AwayDefense[a_idx]
  lambda_a <- avg_ag * stats_df$AwayAttack[a_idx] * stats_df$HomeDefense[h_idx]
  
  # Apply home advantage boost
  lambda_h <- lambda_h * home_adv_factor
  
  # Head-to-head adjustment (if data available)
  if (!is.null(h2h_matches) && nrow(h2h_matches) > 0) {
    h2h_home_goals <- mean(h2h_matches$FTHG[h2h_matches$HomeTeam == home])
    h2h_away_goals <- mean(h2h_matches$FTAG[h2h_matches$AwayTeam == away])
    
    if (!is.na(h2h_home_goals) && !is.na(h2h_away_goals)) {
      # Blend 80% model, 20% h2h history
      lambda_h <- 0.8 * lambda_h + 0.2 * h2h_home_goals
      lambda_a <- 0.8 * lambda_a + 0.2 * h2h_away_goals
    }
  }
  
  # Calculate probabilities
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

# Baseline Poisson (no enhancements)
predict_baseline <- function(home, away, stats_df, avg_hg, avg_ag) {
  h_idx <- which(stats_df$Team == home)
  a_idx <- which(stats_df$Team == away)
  
  if (length(h_idx) == 0 || length(a_idx) == 0) {
    return("D")
  }
  
  lambda_h <- avg_hg * stats_df$HomeAttack[h_idx] * stats_df$AwayDefense[a_idx]
  lambda_a <- avg_ag * stats_df$AwayAttack[a_idx] * stats_df$HomeDefense[h_idx]
  
  ph <- 0; pd <- 0; pa <- 0
  for (hg in 0:6) for (ag in 0:6) {
    p <- dpois(hg, lambda_h) * dpois(ag, lambda_a)
    if (hg > ag) ph <- ph + p else if (hg < ag) pa <- pa + p else pd <- pd + p
  }
  
  if (ph > pd & ph > pa) return("H") else if (pa > pd) return("A") else return("D")
}


# PART 3: Multi-Week Backtest

test_weeks <- 28:33
all_results <- data.frame()

cat("=== MULTI-WEEK BACKTEST ===\n\n")

for (test_week in test_weeks) {
  cat("Testing Week", test_week, "...\n")
  
  # Split data
  train_data <- played[played$MW < test_week, ]
  test_data <- played[played$MW == test_week, ]
  
  if (nrow(test_data) == 0) {
    cat("  No data for week", test_week, "- skipping\n\n")
    next
  }
  
  # Combine with historical
  train_data$Season <- "2025"
  common_cols <- intersect(names(historical), names(train_data))
  all_train <- rbind(historical[, common_cols], train_data[, common_cols])
  
  # Calculate strengths - ENHANCED
  enhanced <- calc_team_strengths_enhanced(all_train, current_season_weight=2.0, 
                                           recent_form_boost=1.5)
  
  # Calculate strengths - BASELINE
  all_train$Weight <- ifelse(all_train$Season == "2025", 2.0, 0.5)
  weighted_mean <- function(x, w) sum(x * w) / sum(w)
  avg_hg_base <- weighted_mean(all_train$FTHG, all_train$Weight)
  avg_ag_base <- weighted_mean(all_train$FTAG, all_train$Weight)
  
  all_teams <- unique(c(all_train$HomeTeam, all_train$AwayTeam))
  baseline_stats <- data.frame()
  for (team in all_teams) {
    home_games <- all_train[all_train$HomeTeam == team, ]
    away_games <- all_train[all_train$AwayTeam == team, ]
    if (nrow(home_games) < 3 || nrow(away_games) < 3) next
    
    ha <- weighted_mean(home_games$FTHG, home_games$Weight)
    hd <- weighted_mean(home_games$FTAG, home_games$Weight)
    aa <- weighted_mean(away_games$FTAG, away_games$Weight)
    ad <- weighted_mean(away_games$FTHG, away_games$Weight)
    
    baseline_stats <- rbind(baseline_stats, data.frame(
      Team=team, HomeAttack=ha/avg_hg_base, HomeDefense=hd/avg_ag_base,
      AwayAttack=aa/avg_ag_base, AwayDefense=ad/avg_hg_base,
      stringsAsFactors=FALSE
    ))
  }
  
  # Make predictions
  for (i in 1:nrow(test_data)) {
    home <- test_data$HomeTeam[i]
    away <- test_data$AwayTeam[i]
    actual <- test_data$FTR[i]
    
    # Get head-to-head history
    h2h <- all_train[(all_train$HomeTeam == home & all_train$AwayTeam == away) |
                     (all_train$HomeTeam == away & all_train$AwayTeam == home), ]
    
    # Enhanced prediction
    pred_enh <- predict_match_enhanced(home, away, enhanced$team_stats, 
                                       enhanced$avg_home_goals, enhanced$avg_away_goals,
                                       h2h, enhanced$home_advantage^0.15)
    
    # Baseline prediction
    pred_base <- predict_baseline(home, away, baseline_stats, avg_hg_base, avg_ag_base)
    
    all_results <- rbind(all_results, data.frame(
      Week = test_week,
      HomeTeam = home,
      AwayTeam = away,
      Actual = actual,
      Enhanced = pred_enh$pred_result,
      Baseline = pred_base,
      Confidence = pred_enh$confidence,
      EnhancedCorrect = (pred_enh$pred_result == actual),
      BaselineCorrect = (pred_base == actual),
      stringsAsFactors = FALSE
    ))
  }
  
  week_enh_acc <- round(sum(all_results$EnhancedCorrect[all_results$Week == test_week]) / 
                        nrow(test_data) * 100, 1)
  week_base_acc <- round(sum(all_results$BaselineCorrect[all_results$Week == test_week]) / 
                         nrow(test_data) * 100, 1)
  
  cat("  Enhanced: ", week_enh_acc, "%  |  Baseline: ", week_base_acc, "%\n\n")
}


# PART 4: Overall Results

cat("=== OVERALL RESULTS ===\n")
cat("Total matches tested:", nrow(all_results), "\n\n")

enh_correct <- sum(all_results$EnhancedCorrect)
base_correct <- sum(all_results$BaselineCorrect)
total <- nrow(all_results)

enh_acc <- round(enh_correct / total * 100, 1)
base_acc <- round(base_correct / total * 100, 1)
home_baseline <- round(sum(all_results$Actual == "H") / total * 100, 1)

cat("ENHANCED MODEL:        ", enh_acc, "% (", enh_correct, "/", total, ")\n")
cat("BASELINE POISSON:      ", base_acc, "% (", base_correct, "/", total, ")\n")
cat("NAIVE (always home):   ", home_baseline, "%\n\n")

cat("Improvement over baseline: +", round(enh_acc - base_acc, 1), " percentage points\n\n")


# PART 5: Consistency Check

week_summary <- aggregate(cbind(EnhancedCorrect, BaselineCorrect) ~ Week, 
                         data=all_results, FUN=mean)
week_summary$EnhancedAcc <- round(week_summary$EnhancedCorrect * 100, 1)
week_summary$BaselineAcc <- round(week_summary$BaselineCorrect * 100, 1)

cat("=== WEEK-BY-WEEK BREAKDOWN ===\n")
print(week_summary[, c("Week", "EnhancedAcc", "BaselineAcc")])
cat("\n")

# Calculate standard deviation (consistency metric)
enh_sd <- round(sd(week_summary$EnhancedAcc), 1)
base_sd <- round(sd(week_summary$BaselineAcc), 1)

cat("Consistency (lower is more stable):\n")
cat("Enhanced SD:  ", enh_sd, "\n")
cat("Baseline SD:  ", base_sd, "\n\n")

if (enh_sd < base_sd) {
  cat("✓ Enhanced model is MORE CONSISTENT week-to-week\n\n")
} else {
  cat("✗ Baseline is more stable (enhanced model may be overfitting)\n\n")
}


# PART 6: Confidence Calibration

cat("=== CONFIDENCE CALIBRATION ===\n")
cat("Are high-confidence picks actually more accurate?\n\n")

all_results$ConfBucket <- cut(all_results$Confidence, 
                               breaks=c(0, 20, 30, 40, 100),
                               labels=c("0-20%", "20-30%", "30-40%", "40%+"))

conf_analysis <- aggregate(EnhancedCorrect ~ ConfBucket, data=all_results, 
                          FUN=function(x) c(Accuracy=round(mean(x)*100,1), Count=length(x)))
conf_matrix <- do.call(data.frame, conf_analysis)
names(conf_matrix) <- c("ConfBucket", "Accuracy", "Count")

print(conf_matrix)
cat("\n")

if (conf_matrix$Accuracy[4] > conf_matrix$Accuracy[1]) {
  cat("✓ YES - Confidence is calibrated (high conf = higher accuracy)\n\n")
} else {
  cat("✗ NO - Confidence is not well calibrated\n\n")
}


# PART 7: Statistical Significance

binom_enh <- binom.test(enh_correct, total, p=1/3, alternative="greater")
binom_base <- binom.test(base_correct, total, p=1/3, alternative="greater")

cat("=== STATISTICAL TESTS ===\n")
cat("Null hypothesis: Model guesses randomly (33% expected)\n\n")

cat("Enhanced p-value: ", round(binom_enh$p.value, 4), 
    ifelse(binom_enh$p.value < 0.05, " ✓ SIGNIFICANT", " ✗ not significant"), "\n")
cat("Baseline p-value: ", round(binom_base$p.value, 4),
    ifelse(binom_base$p.value < 0.05, " ✓ SIGNIFICANT", " ✗ not significant"), "\n\n")


# PART 8: Visualizations

# Accuracy comparison
comp_df <- data.frame(
  Model = c("Enhanced", "Baseline", "Always Home"),
  Accuracy = c(enh_acc, base_acc, home_baseline)
)
comp_df$Model <- factor(comp_df$Model, levels=c("Always Home", "Baseline", "Enhanced"))

p1 <- ggplot(comp_df, aes(x=Model, y=Accuracy, fill=Model)) +
  geom_col(width=0.6) +
  geom_text(aes(label=paste0(Accuracy, "%")), vjust=-0.5, fontface="bold", size=5) +
  labs(title="Model Comparison: Enhanced vs Baseline",
       subtitle=paste("Tested on", length(test_weeks), "matchweeks (", total, "matches )"),
       y="Accuracy (%)", x="") +
  theme_minimal() +
  theme(legend.position="none") +
  scale_fill_manual(values=c("#94A3B8", "#3B82F6", "#10B981")) +
  ylim(0, 100)

ggsave("plots/enhanced_vs_baseline.png", p1, width=10, height=6, dpi=300)

# Week-by-week consistency
week_long <- rbind(
  data.frame(Week=week_summary$Week, Model="Enhanced", Accuracy=week_summary$EnhancedAcc),
  data.frame(Week=week_summary$Week, Model="Baseline", Accuracy=week_summary$BaselineAcc)
)

p2 <- ggplot(week_long, aes(x=Week, y=Accuracy, color=Model, group=Model)) +
  geom_line(size=1.2) +
  geom_point(size=3) +
  geom_hline(yintercept=33.3, linetype="dashed", color="gray50") +
  labs(title="Model Consistency Across Weeks",
       subtitle="Does accuracy hold up or fluctuate wildly?",
       y="Accuracy (%)", x="Matchweek") +
  theme_minimal() +
  scale_color_manual(values=c("#3B82F6", "#10B981")) +
  ylim(0, 100)

ggsave("plots/week_by_week_consistency.png", p2, width=10, height=6, dpi=300)

cat("Plots saved!\n")


# PART 9: Save Results

write.csv(all_results, "data/multiweek_backtest_results.csv", row.names=FALSE)
cat("Full results saved to data/multiweek_backtest_results.csv\n\n")


# PART 10: Final Verdict

cat("=== FINAL VERDICT ===\n\n")

if (enh_acc > base_acc + 3) {
  cat("✓ ENHANCED MODEL IS BETTER\n")
  cat("  Improvements (form, h2h, home adv) add real predictive value\n")
  cat("  Recommendation: Use enhanced model for predictions\n")
} else if (enh_acc > base_acc) {
  cat("~ ENHANCED MODEL SLIGHTLY BETTER\n")
  cat("  Marginal improvement - added complexity may not be worth it\n")
  cat("  Recommendation: Either model is fine\n")
} else {
  cat("✗ BASELINE IS BETTER\n")
  cat("  Enhancements add noise, not signal (overfitting)\n")
  cat("  Recommendation: Stick with simple Poisson baseline\n")
}