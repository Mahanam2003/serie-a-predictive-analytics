
R version 4.4.3 (2025-02-28 ucrt) -- "Trophy Case"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> next_mw <- head(upcoming, 10)
Error: object 'upcoming' not found
> 
> predict_match <- function(home, away, stats_df, avg_hg, avg_ag) {
+   h_idx <- which(stats_df$Team == home)
+   a_idx <- which(stats_df$Team == away)
$rn(list(lambda_home=avg_hg, lambda_away=avg_ag, pr                        $, lambda_away=avg_ag, prob_home=0.33, prob_draw=0.                        $_home=0.33, prob_draw=0.33, prob_away=0.33, pred_r                          if (length(h_idx) == 0 || length(a_idx) == 0) return(list(lambda_home=avg_$
  lambda_h <- avg_hg * stats_df$HomeAttack[h_idx] * stats_df$AwayDefense[a_i$
  lambda_a <- avg_ag * stats_df$AwayAttack[a_idx] * stats_df$HomeDefense[h_i$
+   ph <- 0; pd <- 0; pa <- 0
+   for (hg in 0:6) for (ag in 0:6) {
+     p <- dpois(hg, lambda_h) * dpois(ag, lambda_a)
    if (hg > ag) ph <- ph + p else if (hg < ag) pa <- pa + p else pd <- pd +$
+   }
+   max_p <- max(ph, pd, pa)
+   conf <- max(5, min(95, round((max_p - 1/3) / (2/3) * 100)))
+   pred <- if (ph > pd & ph > pa) "H" else if (pa > pd) "A" else "D"
$und(lambda_a,2), prob_home=round(ph,4), prob_draw=                        $=round(ph,4), prob_draw=round(pd,4), prob_away=rou                        $und(pd,4), prob_away=round(pa,4), pred_result=pred                          list(lambda_home=round(lambda_h,2), lambda_away=round(lambda_a,2), prob_ho$
+ }
> 
$ck + team_stats$AwayAttack) - (team_stats$HomeDefe                        $) - (team_stats$HomeDefense + team_stats$AwayDefen                        team_stats$NetStrength <- round((team_stats$HomeAttack + team_stats$AwayAtta$
Error: object 'team_stats' not found
> cat("=== Top 5 by net strength ===\n")
=== Top 5 by net strength ===
$, c("Team","HomeAttack","AwayAttack","NetStrength"                        print(head(team_stats[order(-team_stats$NetStrength), c("Team","HomeAttack",$
Error: object 'team_stats' not found
> 
> predictions <- data.frame()
> for (i in 1:nrow(next_mw)) {
+   home <- next_mw$HomeTeam[i]
+   away <- next_mw$AwayTeam[i]
  r <- predict_match(home, away, team_stats, avg_home_goals, avg_away_goals)
$eam=home, AwayTeam=away, ExpHome=r$lambda_home, Ex                        $xpHome=r$lambda_home, ExpAway=r$lambda_away, ProbH                        $way=r$lambda_away, ProbHome=r$prob_home, ProbDraw=                        $e=r$prob_home, ProbDraw=r$prob_draw, ProbAway=r$pr                        $prob_draw, ProbAway=r$prob_away, PredResult=r$pred                        $_away, PredResult=r$pred_result, Confidence=r$conf                        $esult, Confidence=r$confidence, stringsAsFactors=F                          predictions <- rbind(predictions, data.frame(HomeTeam=home, AwayTeam=away,$
  label <- switch(r$pred_result, "H"=paste(home,"win"), "D"="Draw", "A"=past$
+   cat(sprintf("  %-20s vs %-20s\n", home, away))
$: %.1f%% A: %.1f%%\n", r$lambda_home, r$lambda_awa                        $ambda_home, r$lambda_away, r$prob_home*100, r$prob                        $ r$prob_home*100, r$prob_draw*100, r$prob_away*100                          cat(sprintf("  Expected: %.1f - %.1f | H: %.1f%% D: %.1f%% A: %.1f%%\n", r$
  cat(sprintf("  Prediction: %s (%d%% confidence)\n\n", label, r$confidence))
+ }
Error: object 'next_mw' not found
> write.csv(predictions, "data/weekly_predictions.csv", row.names=FALSE)