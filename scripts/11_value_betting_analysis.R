# Model v3: More Seasons + Betting Odds Comparison
#
# TWO UPGRADES:
#
# 1. MORE DATA — Training on 9 seasons instead of 5
#    Why this helps: with 5 seasons, one weird season (Covid 2020,
#    Napoli's freak title year) can distort team strength estimates.
#    With 9 seasons, these wash out. Less overfitting, more stable.
#
#    Seasons fetched automatically from football-data.co.uk:
#      2016/17, 2017/18, 2018/19, 2019/20, 2020/21 (new)
#      2021/22, 2022/23, 2023/24, 2024/25 (already have)
#      2025/26 (current, live)
#
# 2. BETTING ODDS COMPARISON
#    football-data.co.uk already includes Bet365 odds (B365H/D/A)
#    in the same CSV we download. No scraping needed.
#
#    VALUE = when your model's probability is HIGHER than what the
#    bookmaker implies. Example:
#      Your model: Inter win = 75%
#      Bet365 odds: 1.60 → implied prob = 62.5%
#      VALUE GAP: +12.5% → your model sees edge the market doesn't
#
#    If your model consistently finds value AND those value bets
#    win, that's proof your model captures signal the market misses.

rm(list = ls())

library(ggplot2)


# PART 1: Fetch All Seasons From Football-Data.co.uk
#
# URL pattern: /mmz4281/[YYZZ]/I1.csv
# Italy Serie A = I1
# 1617 = 2016/17 season

fetch_season <- function(code, season_label) {
  url <- paste0("https://www.football-data.co.uk/mmz4281/", code, "/I1.csv")
  cache_file <- paste0("data/fd_", code, ".csv")
  
  if (file.exists(cache_file)) {
    df <- read.csv(cache_file)
    cat("  Loaded from cache:", season_label, "(", nrow(df), "matches)\n")
    return(df)
  }
  
  df <- tryCatch({
    d <- read.csv(url)
    cat("  Fetched:", season_label, "(", nrow(d), "matches)\n")
    d
  }, error = function(e) {
    cat("  Failed:", season_label, "-", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (!is.null(df)) write.csv(df, cache_file, row.names=FALSE)
  return(df)
}

cat("=== FETCHING HISTORICAL SEASONS ===\n")

season_codes <- list(
  "1617" = "2016/17",
  "1718" = "2017/18",
  "1819" = "2018/19",
  "1920" = "2019/20",
  "2021" = "2020/21",
  "2122" = "2021/22",
  "2223" = "2022/23",
  "2324" = "2023/24",
  "2425" = "2024/25"
)

all_historical <- data.frame()
for (code in names(season_codes)) {
  df <- fetch_season(code, season_codes[[code]])
  if (!is.null(df) && nrow(df) > 0) {
    df$Season <- season_codes[[code]]
    df$SeasonCode <- as.numeric(substr(code, 1, 2)) + 2000
    all_historical <- rbind(all_historical, df[, intersect(names(df),
      c("HomeTeam","AwayTeam","FTHG","FTAG","FTR","B365H","B365D","B365A",
        "Season","SeasonCode"))])
  }
}

# Also fetch current 2025/26 season
cat("  Fetching current season...\n")
current_raw <- read.csv("https://www.football-data.co.uk/mmz4281/2526/I1.csv")
current_raw$Season <- "2025/26"
current_raw$SeasonCode <- 2025
played_2526 <- current_raw[!is.na(current_raw$FTHG) & current_raw$FTHG != "", ]
played_2526 <- played_2526[order(played_2526$Date), ]
played_2526$MW <- rep(1:ceiling(nrow(played_2526)/10), each=10, length.out=nrow(played_2526))

all_historical <- rbind(all_historical,
  played_2526[, intersect(names(played_2526),
    c("HomeTeam","AwayTeam","FTHG","FTAG","FTR","B365H","B365D","B365A",
      "Season","SeasonCode","MW"))])

cat("\nTotal historical matches:", nrow(all_historical), "\n")
cat("Seasons:", paste(unique(all_historical$Season), collapse=", "), "\n\n")


# PART 2: Recency-Weighted Team Strengths
#
# 9-season weights (oldest to newest):
#   2016/17 → 0.05  (ancient history)
#   2017/18 → 0.08
#   2018/19 → 0.12
#   2019/20 → 0.18  (Covid season — slightly weird)
#   2020/21 → 0.25
#   2021/22 → 0.35
#   2022/23 → 0.50
#   2023/24 → 0.75
#   2024/25 → 1.00
#   2025/26 → 2.00  (current — most important)

season_weight_map <- c(
  "2016/17" = 0.05,
  "2017/18" = 0.08,
  "2018/19" = 0.12,
  "2019/20" = 0.18,
  "2020/21" = 0.25,
  "2021/22" = 0.35,
  "2022/23" = 0.50,
  "2023/24" = 0.75,
  "2024/25" = 1.00,
  "2025/26" = 2.00
)

all_historical$Weight <- season_weight_map[all_historical$Season]
all_historical$Weight[is.na(all_historical$Weight)] <- 0.05

cat("=== WEIGHT DISTRIBUTION ===\n")
weight_table <- aggregate(list(Matches=all_historical$FTHG),
                         by=list(Season=all_historical$Season,
                                 Weight=all_historical$Weight), FUN=length)
print(weight_table[order(weight_table$Weight), ])
cat("\n")


# PART 3: Calculate Team Strengths (Full 9-Season Training)

weighted_mean <- function(x, w) sum(x * w, na.rm=TRUE) / sum(w[!is.na(x)])

train_data <- all_historical[!is.na(all_historical$FTHG), ]

avg_home_goals <- weighted_mean(train_data$FTHG, train_data$Weight)
avg_away_goals <- weighted_mean(train_data$FTAG, train_data$Weight)

cat("=== LEAGUE AVERAGES (9-season weighted) ===\n")
cat("Home goals/match:", round(avg_home_goals, 3), "\n")
cat("Away goals/match:", round(avg_away_goals, 3), "\n\n")

all_teams <- unique(c(train_data$HomeTeam, train_data$AwayTeam))
team_stats <- data.frame()

for (team in all_teams) {
  home_games <- train_data[train_data$HomeTeam == team, ]
  away_games <- train_data[train_data$AwayTeam == team, ]
  
  if (nrow(home_games) < 5 || nrow(away_games) < 5) next
  
  team_stats <- rbind(team_stats, data.frame(
    Team = team,
    HomeAttack = weighted_mean(home_games$FTHG, home_games$Weight) / avg_home_goals,
    HomeDefense = weighted_mean(home_games$FTAG, home_games$Weight) / avg_away_goals,
    AwayAttack = weighted_mean(away_games$FTAG, away_games$Weight) / avg_away_goals,
    AwayDefense = weighted_mean(away_games$FTHG, away_games$Weight) / avg_home_goals,
    stringsAsFactors = FALSE
  ))
}

cat("Team strengths computed for", nrow(team_stats), "teams\n")

team_stats$NetStrength <- round(
  (team_stats$HomeAttack + team_stats$AwayAttack) -
  (team_stats$HomeDefense + team_stats$AwayDefense), 3)

cat("\n=== TOP 5 TEAMS BY NET STRENGTH (9-season model) ===\n")
print(head(team_stats[order(-team_stats$NetStrength),
           c("Team","HomeAttack","AwayAttack","NetStrength")], 5))
cat("\n")

write.csv(team_stats, "data/team_strengths_9season.csv", row.names=FALSE)


# PART 4: Prediction Function (Poisson)

predict_poisson <- function(home, away, stats, avg_hg, avg_ag) {
  h <- which(stats$Team == home)
  a <- which(stats$Team == away)
  
  if (length(h)==0 || length(a)==0) {
    return(list(prob_home=1/3, prob_draw=1/3, prob_away=1/3, pred="D", conf=5))
  }
  
  lh <- avg_hg * stats$HomeAttack[h] * stats$AwayDefense[a]
  la <- avg_ag * stats$AwayAttack[a] * stats$HomeDefense[h]
  
  ph <- 0; pd <- 0; pa <- 0
  for (hg in 0:6) for (ag in 0:6) {
    p <- dpois(hg, lh) * dpois(ag, la)
    if (hg > ag) ph <- ph + p else if (hg < ag) pa <- pa + p else pd <- pd + p
  }
  
  max_p <- max(ph, pd, pa)
  conf <- max(5, min(95, round((max_p - 1/3) / (2/3) * 100)))
  pred <- if (ph > pd & ph > pa) "H" else if (pa > pd) "A" else "D"
  
  list(prob_home=round(ph,4), prob_draw=round(pd,4), prob_away=round(pa,4),
       pred=pred, conf=conf)
}


# PART 5: Betting Odds Comparison
#
# Convert bookmaker odds to implied probabilities:
#   implied_prob = 1 / decimal_odds
#
# But bookmakers build in a margin (typically 5-8% for Serie A).
# We remove the margin by normalizing:
#   normalized_prob = raw_implied / sum(all_raw_implied)
#
# VALUE = your model probability - bookmaker normalized probability
# Positive value = model thinks outcome more likely than market does

cat("=== BETTING ODDS COMPARISON ===\n\n")

# Use 2024/25 season for comparison (we have full odds data)
season_for_odds <- all_historical[
  all_historical$Season == "2024/25" &
  !is.na(all_historical$B365H) &
  !is.na(all_historical$FTHG), ]

cat("Matches with Bet365 odds (2024/25):", nrow(season_for_odds), "\n\n")

odds_results <- data.frame()

for (i in 1:nrow(season_for_odds)) {
  row <- season_for_odds[i, ]
  home <- row$HomeTeam
  away <- row$AwayTeam
  actual <- row$FTR
  
  # Model prediction
  pred <- predict_poisson(home, away, team_stats, avg_home_goals, avg_away_goals)
  
  # Bookmaker implied probabilities (normalized to remove margin)
  raw_h <- 1 / row$B365H
  raw_d <- 1 / row$B365D
  raw_a <- 1 / row$B365A
  total_raw <- raw_h + raw_d + raw_a  # This > 1 = bookmaker margin
  
  bookie_overround <- round((total_raw - 1) * 100, 1)
  
  bookie_h <- raw_h / total_raw
  bookie_d <- raw_d / total_raw
  bookie_a <- raw_a / total_raw
  
  # Value = model probability - bookmaker probability
  value_h <- round(pred$prob_home - bookie_h, 4)
  value_d <- round(pred$prob_draw - bookie_d, 4)
  value_a <- round(pred$prob_away - bookie_a, 4)
  
  # The outcome with most value according to model
  max_value <- max(abs(value_h), abs(value_d), abs(value_a))
  model_picks <- pred$pred
  
  odds_results <- rbind(odds_results, data.frame(
    HomeTeam = home,
    AwayTeam = away,
    Actual = actual,
    ModelPred = pred$pred,
    Confidence = pred$conf,
    # Model probabilities
    ModelHome = pred$prob_home,
    ModelDraw = pred$prob_draw,
    ModelAway = pred$prob_away,
    # Bookmaker probabilities
    BookieHome = round(bookie_h, 4),
    BookieDraw = round(bookie_d, 4),
    BookieAway = round(bookie_a, 4),
    # Value gaps
    ValueHome = value_h,
    ValueDraw = value_d,
    ValueAway = value_a,
    # Bookmaker margin
    BookieMargin = bookie_overround,
    # Bet365 odds
    OddsH = row$B365H,
    OddsD = row$B365D,
    OddsA = row$B365A,
    stringsAsFactors = FALSE
  ))
}

cat("Bookie margin (average):", round(mean(odds_results$BookieMargin), 1), "%\n")
cat("(Typical = 5-8%. Higher = worse deal for bettors)\n\n")


# PART 6: Value Betting Analysis
#
# A "value bet" = when your model says an outcome is MORE likely
# than the bookmaker's odds imply.
#
# Strategy: only bet when ModelProb > BookieImpliedProb + threshold

threshold <- 0.05  # Only bet when you have 5%+ edge

# Find value bets for home, draw, away separately
home_value <- odds_results[odds_results$ValueHome > threshold, ]
draw_value <- odds_results[odds_results$ValueDraw > threshold, ]
away_value <- odds_results[odds_results$ValueAway > threshold, ]

cat("=== VALUE BET ANALYSIS ===\n")
cat("Value threshold:", threshold * 100, "%\n\n")

# How often do value bets actually win?
calc_value_accuracy <- function(value_bets, outcome_col, actual_outcome) {
  if (nrow(value_bets) == 0) return(list(n=0, acc=NA, profit=NA))
  correct <- sum(value_bets$Actual == actual_outcome)
  total <- nrow(value_bets)
  
  # Simulated profit: bet 1 unit on each, win = odds - 1, lose = -1
  profits <- ifelse(value_bets$Actual == actual_outcome,
                    value_bets[[paste0("Odds", substr(actual_outcome,1,1))]] - 1, -1)
  
  list(n=total, acc=round(correct/total*100,1), profit=round(sum(profits),2))
}

home_stats <- calc_value_accuracy(home_value, "ValueHome", "H")
draw_stats <- calc_value_accuracy(draw_value, "ValueDraw", "D")
away_stats <- calc_value_accuracy(away_value, "ValueAway", "A")

cat("HOME value bets:  ", home_stats$n, "bets |",
    home_stats$acc, "% accuracy |",
    ifelse(!is.na(home_stats$profit),
           paste0("profit: ", home_stats$profit, " units"), "N/A"), "\n")
cat("DRAW value bets:  ", draw_stats$n, "bets |",
    draw_stats$acc, "% accuracy |",
    ifelse(!is.na(draw_stats$profit),
           paste0("profit: ", draw_stats$profit, " units"), "N/A"), "\n")
cat("AWAY value bets:  ", away_stats$n, "bets |",
    away_stats$acc, "% accuracy |",
    ifelse(!is.na(away_stats$profit),
           paste0("profit: ", away_stats$profit, " units"), "N/A"), "\n\n")

total_value_bets <- nrow(home_value) + nrow(away_value)
total_profit <- sum(c(home_stats$profit, away_stats$profit), na.rm=TRUE)
cat("TOTAL (home+away): ", total_value_bets, "bets | Net profit:",
    round(total_profit, 2), "units\n\n")

# Biggest value disagreements (where model and market differ most)
odds_results$MaxValueGap <- pmax(abs(odds_results$ValueHome),
                                  abs(odds_results$ValueAway),
                                  abs(odds_results$ValueDraw))

cat("=== TOP 10 BIGGEST MODEL vs MARKET DISAGREEMENTS ===\n")
top_disagreements <- head(odds_results[order(-odds_results$MaxValueGap),
  c("HomeTeam","AwayTeam","Actual","ModelPred","ValueHome","ValueDraw","ValueAway")], 10)
print(top_disagreements)
cat("\n")


# PART 7: Multi-Week Backtest With 9-Season Model
#
# Now let's see if the 9-season model is more accurate than 5-season

cat("=== MULTI-WEEK BACKTEST (9-Season Model) ===\n\n")

test_weeks <- 28:33
backtest_results <- data.frame()

for (test_week in test_weeks) {
  test_data <- played_2526[played_2526$MW == test_week, ]
  if (nrow(test_data) == 0) next
  
  for (i in 1:nrow(test_data)) {
    home <- test_data$HomeTeam[i]
    away <- test_data$AwayTeam[i]
    actual <- test_data$FTR[i]
    
    pred <- predict_poisson(home, away, team_stats, avg_home_goals, avg_away_goals)
    
    backtest_results <- rbind(backtest_results, data.frame(
      Week = test_week,
      HomeTeam = home,
      AwayTeam = away,
      Actual = actual,
      Predicted = pred$pred,
      Confidence = pred$conf,
      Correct = (pred$pred == actual),
      stringsAsFactors = FALSE
    ))
  }
  
  week_acc <- round(sum(backtest_results$Correct[backtest_results$Week == test_week]) /
                    nrow(test_data) * 100, 1)
  cat("  Week", test_week, ":", week_acc, "%\n")
}

total_correct <- sum(backtest_results$Correct)
total_matches <- nrow(backtest_results)
overall_acc <- round(total_correct / total_matches * 100, 1)

cat("\nOVERALL (9-season model): ", overall_acc, "% (",
    total_correct, "/", total_matches, ")\n")
cat("PREVIOUS (5-season model): 56.7% (34/60)\n")
cat("IMPROVEMENT: +", round(overall_acc - 56.7, 1), " percentage points\n\n")

# Confidence calibration on backtest
cat("=== CONFIDENCE CALIBRATION (9-Season) ===\n")
backtest_results$ConfBucket <- cut(backtest_results$Confidence,
                                   breaks=c(0,20,30,40,100),
                                   labels=c("0-20%","20-30%","30-40%","40%+"))
conf_cal <- aggregate(Correct ~ ConfBucket, data=backtest_results, FUN=mean)
conf_cal$Accuracy <- round(conf_cal$Correct * 100, 1)
conf_cal$Count <- aggregate(Correct ~ ConfBucket, data=backtest_results, FUN=length)$Correct
print(conf_cal[, c("ConfBucket", "Accuracy", "Count")])


# PART 8: Visualization — Value vs Market

# Model vs Bookie probability scatter
p1 <- ggplot(odds_results, aes(x=BookieHome, y=ModelHome)) +
  geom_point(aes(color=Actual=="H"), alpha=0.6, size=2) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="gray50") +
  geom_smooth(method="lm", se=FALSE, color="#3B82F6") +
  scale_color_manual(values=c("FALSE"="#EF4444", "TRUE"="#10B981"),
                     labels=c("Home lost/draw", "Home won"),
                     name="Actual Result") +
  labs(title="Model vs Bet365: Home Win Probability",
       subtitle="Points above dashed line = model more bullish than market",
       x="Bet365 Implied Probability", y="Model Probability") +
  theme_minimal()

ggsave("plots/model_vs_bookie.png", p1, width=10, height=7, dpi=300)

# Value betting profit by confidence level
if (nrow(home_value) + nrow(away_value) > 0) {
  all_value <- rbind(
    if(nrow(home_value)>0) data.frame(home_value, Type="Home", 
      Odds=home_value$OddsH, Won=(home_value$Actual=="H")) else data.frame(),
    if(nrow(away_value)>0) data.frame(away_value, Type="Away",
      Odds=away_value$OddsA, Won=(away_value$Actual=="A")) else data.frame()
  )
  
  if (nrow(all_value) > 0) {
    all_value$Profit <- ifelse(all_value$Won, all_value$Odds - 1, -1)
    all_value$CumProfit <- cumsum(all_value$Profit)
    all_value$BetNumber <- 1:nrow(all_value)
    
    p2 <- ggplot(all_value, aes(x=BetNumber, y=CumProfit)) +
      geom_line(color="#3B82F6", size=1.2) +
      geom_hline(yintercept=0, linetype="dashed", color="gray50") +
      labs(title="Cumulative Profit: Value Bets (2024/25 Season)",
           subtitle=paste("Betting 1 unit when model finds", threshold*100,
                         "% edge vs Bet365 |", nrow(all_value), "bets total"),
           x="Bet Number", y="Profit (units)") +
      theme_minimal()
    
    ggsave("plots/value_betting_profit.png", p2, width=10, height=6, dpi=300)
    cat("\nValue betting profit chart saved!\n")
  }
}

# Save everything
write.csv(odds_results, "data/model_vs_bookie_2425.csv", row.names=FALSE)
write.csv(backtest_results, "data/backtest_9season.csv", row.names=FALSE)

cat("\nAll files saved!\n\n")
cat("=== FINAL SUMMARY ===\n")
cat("9-Season Model Accuracy (backtest): ", overall_acc, "%\n")
cat("5-Season Model Accuracy (previous): 56.7%\n")
cat("Value bets (2024/25):               ", total_value_bets, "bets\n")
cat("Net value bet profit:               ", round(total_profit, 2), "units\n")
cat("Bookie margin (avg):                ", round(mean(odds_results$BookieMargin),1), "%\n")