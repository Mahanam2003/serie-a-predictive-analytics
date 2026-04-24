# Promoted Teams Fix
#
# PROBLEM: Newly promoted teams (Como, Pisa, Cremonese) have no
# Serie A history in our dataset. They get assigned league-average
# strength (1.0) which is WRONG — promoted teams are typically weaker.
#
# SOLUTION: Calculate the average first-season performance of ALL
# teams that were promoted over the last 9 seasons. Use that as the
# prior for any new team with insufficient data.
#
# Historical evidence: Promoted teams concede more, score less.
# We'll quantify exactly HOW MUCH weaker they are on average.

rm(list = ls())

library(ggplot2)

# PART 1: Load All 9 Seasons

season_files <- list(
  "data/fd_1617.csv" = "2016/17",
  "data/fd_1718.csv" = "2017/18",
  "data/fd_1819.csv" = "2018/19",
  "data/fd_1920.csv" = "2019/20",
  "data/fd_2021.csv" = "2020/21",
  "data/fd_2122.csv" = "2021/22",
  "data/fd_2223.csv" = "2022/23",
  "data/fd_2324.csv" = "2023/24",
  "data/fd_2425.csv" = "2024/25"
)

all_seasons <- data.frame()
for (file in names(season_files)) {
  if (file.exists(file)) {
    df <- read.csv(file)
    df$Season <- season_files[[file]]
    all_seasons <- rbind(all_seasons,
      df[, intersect(names(df),
         c("HomeTeam","AwayTeam","FTHG","FTAG","FTR","Season"))])
  }
}

cat("Total matches loaded:", nrow(all_seasons), "\n")
cat("Seasons:", paste(unique(all_seasons$Season), collapse=", "), "\n\n")


# PART 2: Identify Promoted Teams Each Season
#
# A team is "promoted" in season N if they did NOT appear in season N-1.
# We compare team lists between consecutive seasons.

seasons_ordered <- c("2016/17","2017/18","2018/19","2019/20",
                     "2020/21","2021/22","2022/23","2023/24","2024/25")

promoted_teams_log <- data.frame()

for (i in 2:length(seasons_ordered)) {
  prev_season <- seasons_ordered[i-1]
  curr_season <- seasons_ordered[i]
  
  prev_teams <- unique(c(
    all_seasons$HomeTeam[all_seasons$Season == prev_season],
    all_seasons$AwayTeam[all_seasons$Season == prev_season]
  ))
  curr_teams <- unique(c(
    all_seasons$HomeTeam[all_seasons$Season == curr_season],
    all_seasons$AwayTeam[all_seasons$Season == curr_season]
  ))
  
  # Teams in current but NOT in previous = promoted
  promoted <- curr_teams[!curr_teams %in% prev_teams]
  
  if (length(promoted) > 0) {
    promoted_teams_log <- rbind(promoted_teams_log, data.frame(
      Team = promoted,
      FirstSeason = curr_season,
      stringsAsFactors = FALSE
    ))
  }
}

cat("=== PROMOTED TEAMS IDENTIFIED ===\n")
print(promoted_teams_log)
cat("\n")


# PART 3: Calculate First-Season Stats for Each Promoted Team
#
# For each promoted team, look at their FIRST season in Serie A
# and compute their attack/defense strength vs league average.

weighted_mean <- function(x, w) sum(x * w, na.rm=TRUE) / sum(w[!is.na(x)])

promoted_stats <- data.frame()

for (i in 1:nrow(promoted_teams_log)) {
  team <- promoted_teams_log$Team[i]
  season <- promoted_teams_log$FirstSeason[i]
  
  season_data <- all_seasons[all_seasons$Season == season & !is.na(all_seasons$FTHG), ]
  
  avg_hg <- mean(season_data$FTHG)
  avg_ag <- mean(season_data$FTAG)
  
  home_games <- season_data[season_data$HomeTeam == team, ]
  away_games <- season_data[season_data$AwayTeam == team, ]
  
  if (nrow(home_games) < 5 || nrow(away_games) < 5) next
  
  ha <- mean(home_games$FTHG) / avg_hg
  hd <- mean(home_games$FTAG) / avg_ag
  aa <- mean(away_games$FTAG) / avg_ag
  ad <- mean(away_games$FTHG) / avg_hg
  
  promoted_stats <- rbind(promoted_stats, data.frame(
    Team = team,
    FirstSeason = season,
    HomeAttack = round(ha, 3),
    HomeDefense = round(hd, 3),
    AwayAttack = round(aa, 3),
    AwayDefense = round(ad, 3),
    NetStrength = round((ha + aa) - (hd + ad), 3),
    stringsAsFactors = FALSE
  ))
}

cat("=== FIRST-SEASON PERFORMANCE OF PROMOTED TEAMS ===\n")
print(promoted_stats[order(promoted_stats$NetStrength), 
      c("Team","FirstSeason","HomeAttack","HomeDefense",
        "AwayAttack","AwayDefense","NetStrength")])
cat("\n")


# PART 4: Calculate the "Promoted Team Prior"
#
# Average the first-season stats across all promoted teams.
# This is what we assign to ANY new team with no Serie A history.

prior_ha <- round(mean(promoted_stats$HomeAttack), 3)
prior_hd <- round(mean(promoted_stats$HomeDefense), 3)
prior_aa <- round(mean(promoted_stats$AwayAttack), 3)
prior_ad <- round(mean(promoted_stats$AwayDefense), 3)
prior_net <- round(mean(promoted_stats$NetStrength), 3)

cat("=== PROMOTED TEAM PRIOR (vs league average = 1.0) ===\n")
cat("HomeAttack: ", prior_ha, "(", round((prior_ha-1)*100,1), "% vs avg)\n")
cat("HomeDefense:", prior_hd, "(", round((prior_hd-1)*100,1), "% vs avg) — higher = worse\n")
cat("AwayAttack: ", prior_aa, "(", round((prior_aa-1)*100,1), "% vs avg)\n")
cat("AwayDefense:", prior_ad, "(", round((prior_ad-1)*100,1), "% vs avg) — higher = worse\n")
cat("NetStrength:", prior_net, "(baseline 1.0 would be 0.0)\n\n")

cat("INTERPRETATION:\n")
cat("A promoted team scores", round((1-prior_ha)*100,1), "% fewer goals at home\n")
cat("A promoted team concedes", round((prior_hd-1)*100,1), "% more goals at home\n")
cat("A promoted team scores", round((1-prior_aa)*100,1), "% fewer goals away\n")
cat("A promoted team concedes", round((prior_ad-1)*100,1), "% more goals away\n\n")

# Save the prior
promoted_prior <- data.frame(
  HomeAttack = prior_ha,
  HomeDefense = prior_hd,
  AwayAttack = prior_aa,
  AwayDefense = prior_ad
)
write.csv(promoted_prior, "data/promoted_team_prior.csv", row.names=FALSE)
cat("Prior saved to data/promoted_team_prior.csv\n\n")


# PART 5: Updated Prediction Function Using Promoted Prior
#
# Now ANY team with insufficient history gets the promoted prior
# instead of league average (1.0).

team_stats <- read.csv("data/team_strengths_9season.csv")
current_raw <- read.csv("https://www.football-data.co.uk/mmz4281/2526/I1.csv")
played_2526 <- current_raw[!is.na(current_raw$FTHG) & current_raw$FTHG != "", ]
played_2526 <- played_2526[order(played_2526$Date), ]
played_2526$MW <- rep(1:ceiling(nrow(played_2526)/10), each=10, length.out=nrow(played_2526))

avg_home_goals <- 1.44  # From 9-season weighted calculation
avg_away_goals <- 1.242

predict_with_prior <- function(home, away, stats, avg_hg, avg_ag, prior) {
  h <- which(stats$Team == home)
  a <- which(stats$Team == away)
  
  # Use promoted prior for teams with no history
  if (length(h) == 0) {
    cat("  Note:", home, "not in history — using promoted team prior\n")
    ha <- prior$HomeAttack
    hd <- prior$HomeDefense
  } else {
    ha <- stats$HomeAttack[h]
    hd <- stats$HomeDefense[h]
  }
  
  if (length(a) == 0) {
    cat("  Note:", away, "not in history — using promoted team prior\n")
    aa <- prior$AwayAttack
    ad <- prior$AwayDefense
  } else {
    aa <- stats$AwayAttack[a]
    ad <- stats$AwayDefense[a]
  }
  
  lh <- avg_hg * ha * ad
  la <- avg_ag * aa * hd
  
  ph <- 0; pd <- 0; pa <- 0
  for (hg in 0:6) for (ag in 0:6) {
    p <- dpois(hg, lh) * dpois(ag, la)
    if (hg > ag) ph <- ph + p else if (hg < ag) pa <- pa + p else pd <- pd + p
  }
  
  max_p <- max(ph, pd, pa)
  conf <- max(5, min(95, round((max_p - 1/3) / (2/3) * 100)))
  pred <- if (ph > pd & ph > pa) "H" else if (pa > pd) "A" else "D"
  
  list(prob_home=round(ph,4), prob_draw=round(pd,4), prob_away=round(pa,4),
       pred=pred, conf=conf,
       lambda_home=round(lh,2), lambda_away=round(la,2))
}


# PART 6: Derive All Remaining Fixtures + Predict Them

current_teams <- unique(c(played_2526$HomeTeam, played_2526$AwayTeam))
all_possible <- expand.grid(HomeTeam=current_teams, AwayTeam=current_teams,
                             stringsAsFactors=FALSE)
all_possible <- all_possible[all_possible$HomeTeam != all_possible$AwayTeam, ]
played_keys <- paste(played_2526$HomeTeam, played_2526$AwayTeam)
all_possible$key <- paste(all_possible$HomeTeam, all_possible$AwayTeam)
remaining <- all_possible[!all_possible$key %in% played_keys, ]
remaining$key <- NULL

cat("=== REMAINING FIXTURES THIS SEASON ===\n")
cat("Total remaining:", nrow(remaining), "matches\n\n")

all_predictions <- data.frame()
for (i in 1:nrow(remaining)) {
  home <- remaining$HomeTeam[i]
  away <- remaining$AwayTeam[i]
  
  pred <- predict_with_prior(home, away, team_stats,
                              avg_home_goals, avg_away_goals, promoted_prior)
  
  all_predictions <- rbind(all_predictions, data.frame(
    HomeTeam = home,
    AwayTeam = away,
    ExpHome = pred$lambda_home,
    ExpAway = pred$lambda_away,
    ProbHome = pred$prob_home,
    ProbDraw = pred$prob_draw,
    ProbAway = pred$prob_away,
    PredResult = pred$pred,
    Confidence = pred$conf,
    stringsAsFactors = FALSE
  ))
}

# Show top 10 most confident predictions
cat("=== TOP 10 MOST CONFIDENT REMAINING PREDICTIONS ===\n")
top_preds <- head(all_predictions[order(-all_predictions$Confidence), ], 10)
for (i in 1:nrow(top_preds)) {
  r <- top_preds[i, ]
  label <- switch(r$PredResult,
    "H"=paste(r$HomeTeam,"win"),
    "D"="Draw",
    "A"=paste(r$AwayTeam,"win"))
  cat(sprintf("  %-18s vs %-18s  → %-25s (%d%% conf)\n",
              r$HomeTeam, r$AwayTeam, label, r$Confidence))
}

# Save all remaining predictions
write.csv(all_predictions, "data/all_remaining_predictions.csv", row.names=FALSE)
cat("\nAll", nrow(all_predictions), "remaining predictions saved!\n")


# PART 7: Visualize Promoted Team Performance

promoted_stats$Category <- "Promoted Team"
avg_row <- data.frame(Team="League Average", FirstSeason="",
                       HomeAttack=1.0, HomeDefense=1.0,
                       AwayAttack=1.0, AwayDefense=1.0,
                       NetStrength=0.0, Category="League Average")
plot_df <- rbind(promoted_stats, avg_row)

p1 <- ggplot(promoted_stats, aes(x=reorder(Team, NetStrength), y=NetStrength,
                                  fill=NetStrength > 0)) +
  geom_col(width=0.7) +
  geom_hline(yintercept=0, linetype="dashed", color="gray50", size=1) +
  coord_flip() +
  scale_fill_manual(values=c("TRUE"="#10B981","FALSE"="#EF4444")) +
  labs(title="First-Season Performance of Promoted Teams",
       subtitle="Net strength vs league average (0 = average, negative = below average)",
       x=NULL, y="Net Strength vs League Average") +
  theme_minimal() +
  theme(legend.position="none")

ggsave("plots/promoted_teams_analysis.png", p1,
       width=10, height=max(6, nrow(promoted_stats)*0.4+2), dpi=300)

cat("Plot saved: plots/promoted_teams_analysis.png\n\n")
cat("=== SUMMARY ===\n")
cat("Promoted teams analyzed:", nrow(promoted_stats), "\n")
cat("Average promoted team net strength:", prior_net, "\n")
cat("(League average = 0.0, inter = ~1.86)\n")
cat("New teams (Como, Pisa, Cremonese) now use data-driven prior\n")
cat("instead of incorrect league-average assumption\n")