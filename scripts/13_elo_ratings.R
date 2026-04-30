# Elo Rating System — Final Clean Version
#
# KEY FIX: Don't rely on date parsing across different CSV formats.
# Instead, sort by season order + row index within each season.
# This guarantees correct chronological order regardless of date format.
#
# KEY FINDING FROM PREVIOUS RUNS:
#   When Poisson AND Elo AGREE → 62.1% accuracy
#   This is the strongest signal in the entire model suite.

rm(list = ls())
library(ggplot2)


# PART 1: Load All Seasons — Sort By Season Order, Not Dates

seasons_ordered <- c(
  "2016/17","2017/18","2018/19","2019/20","2020/21",
  "2021/22","2022/23","2023/24","2024/25","2025/26"
)

season_files <- c(
  "2016/17"="data/fd_1617.csv",
  "2017/18"="data/fd_1718.csv",
  "2018/19"="data/fd_1819.csv",
  "2019/20"="data/fd_1920.csv",
  "2020/21"="data/fd_2021.csv",
  "2021/22"="data/fd_2122.csv",
  "2022/23"="data/fd_2223.csv",
  "2023/24"="data/fd_2324.csv",
  "2024/25"="data/fd_2425.csv"
)

all_matches <- data.frame()
for (season in seasons_ordered[1:9]) {
  file <- season_files[season]
  if (file.exists(file)) {
    df <- read.csv(file)
    df <- df[!is.na(df$FTHG) & df$FTHG != "", ]
    df$Season <- season
    df$SeasonOrder <- which(seasons_ordered == season)
    df$GameIndex <- 1:nrow(df)  # row order within season = time order
    all_matches <- rbind(all_matches,
      df[, intersect(names(df),
         c("HomeTeam","AwayTeam","FTHG","FTAG","FTR",
           "Season","SeasonOrder","GameIndex"))])
  }
}

# Add 2025/26 live data
current_raw <- read.csv("https://www.football-data.co.uk/mmz4281/2526/I1.csv")
played_2526 <- current_raw[!is.na(current_raw$FTHG) & current_raw$FTHG != "", ]

# Parse date for matchweek assignment only
played_2526$DateParsed <- as.Date(played_2526$Date, format="%d/%m/%Y")
if (sum(is.na(played_2526$DateParsed)) > nrow(played_2526)/2) {
  played_2526$DateParsed <- as.Date(played_2526$Date, format="%d/%m/%y")
}
played_2526 <- played_2526[order(played_2526$DateParsed, na.last=TRUE), ]
played_2526$MW <- rep(1:ceiling(nrow(played_2526)/10), each=10,
                      length.out=nrow(played_2526))

played_2526$Season <- "2025/26"
played_2526$SeasonOrder <- 10
played_2526$GameIndex <- 1:nrow(played_2526)

all_matches <- rbind(all_matches,
  played_2526[, intersect(names(played_2526),
    c("HomeTeam","AwayTeam","FTHG","FTAG","FTR",
      "Season","SeasonOrder","GameIndex","MW"))])

# Sort by season order, then game index within season
all_matches <- all_matches[order(all_matches$SeasonOrder, all_matches$GameIndex), ]
all_matches$MatchIndex <- 1:nrow(all_matches)

cat("=== DATA LOADED ===\n")
cat("Total matches:", nrow(all_matches), "\n")
cat("Seasons:", paste(unique(all_matches$Season), collapse=", "), "\n\n")


# PART 2: Elo Engine

calc_expected <- function(rating_a, rating_b, home_adv=50) {
  1 / (1 + 10^((rating_b - (rating_a + home_adv)) / 400))
}

run_elo <- function(matches, k=32, home_adv=50, season_carryover=0.75,
                    start_rating=1500) {
  ratings <- list()
  match_elos <- data.frame()
  current_season <- ""

  for (i in 1:nrow(matches)) {
    home   <- matches$HomeTeam[i]
    away   <- matches$AwayTeam[i]
    result <- matches$FTR[i]
    season <- matches$Season[i]

    # Season reset at start of each new season
    if (season != current_season) {
      current_season <- season
      for (team in names(ratings)) {
        ratings[[team]] <- start_rating +
          season_carryover * (ratings[[team]] - start_rating)
      }
    }

    if (is.null(ratings[[home]])) ratings[[home]] <- start_rating
    if (is.null(ratings[[away]])) ratings[[away]] <- start_rating

    r_home <- ratings[[home]]
    r_away <- ratings[[away]]
    exp_home <- calc_expected(r_home, r_away, home_adv)

    act_home <- if(result=="H") 1 else if(result=="A") 0 else 0.5

    ratings[[home]] <- r_home + k * (act_home - exp_home)
    ratings[[away]] <- r_away + k * (act_home - exp_home) * (-1)

    match_elos <- rbind(match_elos, data.frame(
      Season=season, MatchIndex=matches$MatchIndex[i],
      HomeTeam=home, AwayTeam=away,
      HomeElo=round(r_home), AwayElo=round(r_away),
      EloDiff=round(r_home - r_away),
      WinProbHome=round(exp_home,4),
      Actual=result,
      EloPred=ifelse(exp_home>0.5,"H",ifelse(exp_home<0.45,"A","D")),
      stringsAsFactors=FALSE
    ))
  }
  list(ratings=ratings, history=match_elos)
}


# PART 3: K-Factor Optimization

cat("=== OPTIMIZING K-FACTOR ===\n")

train_matches <- all_matches[all_matches$Season != "2024/25" &
                              all_matches$Season != "2025/26", ]
test_matches  <- all_matches[all_matches$Season == "2024/25", ]

k_values <- c(16, 24, 32, 40, 48, 56, 64)
k_results <- data.frame()

for (k in k_values) {
  combined  <- rbind(train_matches, test_matches)
  combined  <- combined[order(combined$SeasonOrder, combined$GameIndex), ]
  combined$MatchIndex <- 1:nrow(combined)
  
  elo_out  <- run_elo(combined, k=k)
  test_hist <- elo_out$history[
    grepl("2024/25", elo_out$history$Season), ]
  acc <- mean(test_hist$EloPred == test_hist$Actual, na.rm=TRUE)
  k_results <- rbind(k_results, data.frame(K=k, Accuracy=round(acc*100,1)))
  cat("  K =", k, "| Accuracy:", round(acc*100,1), "%\n")
}

best_k   <- k_results$K[which.max(k_results$Accuracy)]
best_acc <- max(k_results$Accuracy)
cat("\nBest K:", best_k, "| Accuracy:", best_acc, "%\n\n")


# PART 4: Final Elo Model

cat("=== FINAL ELO MODEL (K =", best_k,") ===\n")
elo_final <- run_elo(all_matches, k=best_k)

current_teams <- unique(c(played_2526$HomeTeam, played_2526$AwayTeam))
current_ratings <- data.frame(
  Team      = names(elo_final$ratings),
  EloRating = round(unlist(elo_final$ratings))
)
current_2526 <- current_ratings[current_ratings$Team %in% current_teams, ]
current_2526 <- current_2526[order(-current_2526$EloRating), ]
current_2526$Rank <- 1:nrow(current_2526)

cat("\n=== CURRENT ELO RATINGS ===\n")
print(current_2526)
cat("\n")

write.csv(current_ratings, "data/elo_ratings_current.csv", row.names=FALSE)


# PART 5: Elo vs Poisson — Proper Backtest

cat("=== ELO vs POISSON BACKTEST (Weeks 28-33) ===\n\n")

team_stats_poisson <- read.csv("data/team_strengths_9season.csv")
promoted_prior     <- read.csv("data/promoted_team_prior.csv")
avg_home_goals     <- 1.44
avg_away_goals     <- 1.242

predict_poisson <- function(home, away, stats, avg_hg, avg_ag, prior) {
  h  <- which(stats$Team == home)
  a  <- which(stats$Team == away)
  ha <- if(length(h)==0) prior$HomeAttack  else stats$HomeAttack[h]
  hd <- if(length(h)==0) prior$HomeDefense else stats$HomeDefense[h]
  aa <- if(length(a)==0) prior$AwayAttack  else stats$AwayAttack[a]
  ad <- if(length(a)==0) prior$AwayDefense else stats$AwayDefense[a]
  lh <- avg_hg * ha * ad
  la <- avg_ag * aa * hd
  ph <- 0; pd <- 0; pa <- 0
  for (hg in 0:6) for (ag in 0:6) {
    p <- dpois(hg, lh) * dpois(ag, la)
    if(hg>ag) ph<-ph+p else if(hg<ag) pa<-pa+p else pd<-pd+p
  }
  list(ph=ph, pd=pd, pa=pa,
       pred=if(ph>pd&ph>pa)"H" else if(pa>pd)"A" else "D")
}

predict_elo_3way <- function(home, away, ratings_list, home_adv=50) {
  r_h <- if(!is.null(ratings_list[[home]])) ratings_list[[home]] else 1500
  r_a <- if(!is.null(ratings_list[[away]])) ratings_list[[away]] else 1500
  exp_h <- calc_expected(r_h, r_a, home_adv)
  draw_adj <- 0.1 * (1 - abs(exp_h - 0.5) / 0.5)
  pd <- 0.25 + draw_adj
  ph <- exp_h * (1 - pd)
  pa <- (1 - exp_h) * (1 - pd)
  pred <- if(ph>pd&ph>pa)"H" else if(pa>pd)"A" else "D"
  list(ph=round(ph,4), pd=round(pd,4), pa=round(pa,4), pred=pred)
}

comparison <- data.frame()
test_weeks <- 28:33

for (test_week in test_weeks) {
  test_data <- played_2526[played_2526$MW == test_week, ]
  if (nrow(test_data) == 0) next

  # Elo trained on everything BEFORE this matchweek
  cutoff_idx <- min(all_matches$MatchIndex[
    all_matches$Season == "2025/26" & 
    !is.na(all_matches$MW) & all_matches$MW == test_week])
  
  train_elo <- all_matches[all_matches$MatchIndex < cutoff_idx, ]
  elo_pre   <- run_elo(train_elo, k=best_k)

  for (i in 1:nrow(test_data)) {
    home   <- test_data$HomeTeam[i]
    away   <- test_data$AwayTeam[i]
    actual <- test_data$FTR[i]

    p_pred <- predict_poisson(home, away, team_stats_poisson,
                               avg_home_goals, avg_away_goals, promoted_prior)
    e_pred <- predict_elo_3way(home, away, elo_pre$ratings)

    ens_h <- 0.5 * p_pred$ph + 0.5 * e_pred$ph
    ens_d <- 0.5 * p_pred$pd + 0.5 * e_pred$pd
    ens_a <- 0.5 * p_pred$pa + 0.5 * e_pred$pa
    ens_pred <- if(ens_h>ens_d&ens_h>ens_a)"H" else if(ens_a>ens_d)"A" else "D"
    max_p <- max(ens_h, ens_d, ens_a)
    conf  <- max(5, min(95, round((max_p - 1/3) / (2/3) * 100)))

    comparison <- rbind(comparison, data.frame(
      Week=test_week, HomeTeam=home, AwayTeam=away, Actual=actual,
      PoissonPred=p_pred$pred, EloPred=e_pred$pred, EnsemblePred=ens_pred,
      PoissonCorrect=(p_pred$pred==actual),
      EloCorrect=(e_pred$pred==actual),
      EnsembleCorrect=(ens_pred==actual),
      BothAgree=(p_pred$pred==e_pred$pred),
      Confidence=conf,
      stringsAsFactors=FALSE
    ))
  }

  wp <- round(mean(comparison$PoissonCorrect[comparison$Week==test_week])*100,1)
  we <- round(mean(comparison$EloCorrect[comparison$Week==test_week])*100,1)
  wn <- round(mean(comparison$EnsembleCorrect[comparison$Week==test_week])*100,1)
  cat(sprintf("  Week %d: Poisson %s%%  |  Elo %s%%  |  Ensemble %s%%\n",
              test_week, wp, we, wn))
}

poisson_acc  <- round(mean(comparison$PoissonCorrect)*100,1)
elo_acc      <- round(mean(comparison$EloCorrect)*100,1)
ensemble_acc <- round(mean(comparison$EnsembleCorrect)*100,1)
n_agree      <- sum(comparison$BothAgree)
agree_acc    <- round(mean(comparison$PoissonCorrect[comparison$BothAgree])*100,1)
n_disagree   <- sum(!comparison$BothAgree)
disagree_p   <- round(mean(comparison$PoissonCorrect[!comparison$BothAgree])*100,1)
disagree_e   <- round(mean(comparison$EloCorrect[!comparison$BothAgree])*100,1)

cat("\n=== FINAL RESULTS ===\n")
cat("Poisson:               ", poisson_acc, "%\n")
cat("Elo:                   ", elo_acc, "%\n")
cat("Ensemble (50/50):      ", ensemble_acc, "%\n\n")
cat("When BOTH AGREE (", n_agree, "matches):", agree_acc, "% accurate ← KEY\n")
cat("When DISAGREE (", n_disagree, "matches):\n")
cat("  Poisson:", disagree_p, "%  |  Elo:", disagree_e, "%\n\n")

best_model <- which.max(c(poisson_acc, elo_acc, ensemble_acc))
cat("WINNER:", c("Poisson","Elo","Ensemble")[best_model], "\n\n")

cat("=== CONFIDENCE CALIBRATION ===\n")
comparison$ConfBucket <- cut(comparison$Confidence,
                              breaks=c(0,20,30,40,100),
                              labels=c("0-20%","20-30%","30-40%","40%+"))
conf_cal <- aggregate(EnsembleCorrect ~ ConfBucket, data=comparison, FUN=mean)
conf_cal$Accuracy <- round(conf_cal$EnsembleCorrect * 100, 1)
conf_cal$Count    <- aggregate(EnsembleCorrect ~ ConfBucket,
                               data=comparison, FUN=length)$EnsembleCorrect
print(conf_cal[, c("ConfBucket","Accuracy","Count")])


# PART 6: Visualizations

# Elo ratings
top_teams <- head(current_2526[order(-current_2526$EloRating),], 10)
top_teams$Team <- factor(top_teams$Team,
                          levels=rev(top_teams$Team))

p1 <- ggplot(top_teams, aes(x=Team, y=EloRating, fill=EloRating)) +
  geom_col(width=0.7) +
  geom_text(aes(label=EloRating), hjust=-0.2, fontface="bold", size=4) +
  coord_flip() +
  scale_fill_gradient(low="#94A3B8", high="#DC2626") +
  labs(title="Serie A Elo Ratings — Current 2025/26",
       subtitle=paste("10 seasons | K =", best_k),
       x=NULL, y="Elo Rating") +
  theme_minimal() +
  theme(legend.position="none") +
  expand_limits(y=max(top_teams$EloRating) + 100)

ggsave("plots/elo_ratings_current.png", p1, width=10, height=7, dpi=300)

# Model comparison
comp_df <- data.frame(
  Model    = factor(c("Poisson","Elo","Ensemble","Both Agree"),
                    levels=c("Poisson","Elo","Ensemble","Both Agree")),
  Accuracy = c(poisson_acc, elo_acc, ensemble_acc, agree_acc)
)

p2 <- ggplot(comp_df, aes(x=Model, y=Accuracy, fill=Model)) +
  geom_col(width=0.6) +
  geom_text(aes(label=paste0(Accuracy,"%")), vjust=-0.5,
            fontface="bold", size=5) +
  labs(title="Model Comparison: Poisson vs Elo vs Ensemble",
       subtitle="'Both Agree' = only predict when both models agree",
       y="Accuracy (%)", x="") +
  theme_minimal() +
  theme(legend.position="none") +
  scale_fill_manual(values=c("#3B82F6","#10B981","#DC2626","#F59E0B")) +
  ylim(0, 100)

ggsave("plots/model_comparison_final.png", p2, width=10, height=6, dpi=300)

# Save
write.csv(comparison, "data/final_model_comparison.csv", row.names=FALSE)
