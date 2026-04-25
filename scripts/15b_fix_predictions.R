# Script 15b: Fix Draw Predictions + Update Website
#
# FIX 1: Draw threshold
# The ensemble never predicts draws because we pick the MAX probability.
# But draws are 26% of Serie A results. We need a draw zone:
#   If max outcome probability < 45%, AND draw prob > 22% → predict draw
#
# FIX 2: Website update
# We need to add weeklyMatches to index.html properly.

rm(list = ls())
library(ggplot2)

# Load predictions we already made
all_predictions <- read.csv("data/unified_predictions.csv",
                             stringsAsFactors=FALSE)

cat("=== FIXING DRAW PREDICTIONS ===\n\n")
cat("Before fix:\n")
cat("  Home:", sum(all_predictions$PredResult=="H"), "\n")
cat("  Draw:", sum(all_predictions$PredResult=="D"), "\n")
cat("  Away:", sum(all_predictions$PredResult=="A"), "\n\n")

# Apply draw zone logic
# If the top probability < 45% AND draw prob > 22% → it's a draw
fix_prediction <- function(ph, pd, pa) {
  max_p <- max(ph, pd, pa)
  if (max_p < 45 && pd > 22) return("D")
  if (ph >= pa) return("H")
  return("A")
}

all_predictions$PredResult <- mapply(fix_prediction,
                                      all_predictions$ProbHome,
                                      all_predictions$ProbDraw,
                                      all_predictions$ProbAway)

all_predictions$Label <- ifelse(
  all_predictions$PredResult == "H",
  paste(all_predictions$HomeTeam, "win"),
  ifelse(all_predictions$PredResult == "D", "Draw",
         paste(all_predictions$AwayTeam, "win"))
)

cat("After fix:\n")
cat("  Home:", sum(all_predictions$PredResult=="H"), "\n")
cat("  Draw:", sum(all_predictions$PredResult=="D"), "\n")
cat("  Away:", sum(all_predictions$PredResult=="A"), "\n\n")

# Expected: ~13 draws (26% of 50)
draw_pct <- round(sum(all_predictions$PredResult=="D") / nrow(all_predictions) * 100, 1)
cat("Draw rate:", draw_pct, "% (expected ~26% for Serie A)\n\n")

# Save fixed predictions
write.csv(all_predictions, "data/unified_predictions.csv", row.names=FALSE)

# Get next matchweek
next_mw <- all_predictions[all_predictions$MatchweekEst == 1, ]
write.csv(next_mw, "data/next_matchweek.csv", row.names=FALSE)

cat("=== NEXT MATCHWEEK (FIXED) ===\n")
for (i in 1:nrow(next_mw)) {
  r <- next_mw[i,]
  rain_tag <- if(isTRUE(r$RainWarning)) " ⚠ RAIN" else ""
  cat(sprintf("  %-18s vs %-18s  → %-20s (%d%%%s)\n",
              r$HomeTeam, r$AwayTeam, r$Label, r$Confidence, rain_tag))
}

cat("\n=== HIGH CONFIDENCE PICKS ===\n")
high_conf <- all_predictions[all_predictions$Confidence >= 50 &
                              all_predictions$BothAgree, ]
for (i in 1:nrow(high_conf)) {
  r <- high_conf[i,]
  cat(sprintf("  %-18s vs %-18s  → %-22s (%d%%)\n",
              r$HomeTeam, r$AwayTeam, r$Label, r$Confidence))
}


# ============================================================
# FIX 2: UPDATE index.html
# ============================================================

cat("\n=== UPDATING index.html ===\n")

if (!file.exists("index.html")) {
  cat("index.html not found\n")
} else {
  html_content <- readLines("index.html", warn=FALSE)

  # Build the JS array
  js_array <- paste0(
    "const weeklyMatches = [\n",
    paste(sapply(1:nrow(next_mw), function(i) {
      r <- next_mw[i,]
      rain_str <- if(isTRUE(r$RainWarning)) ", rain: true" else ""
      stakes_str <- if(isTRUE(r$Importance > 0.6)) ", highStakes: true" else ""
      sprintf(
        '  { home: "%s", away: "%s", predResult: "%s", confidence: %d, probHome: %.1f, probDraw: %.1f, probAway: %.1f, expHome: %.1f, expAway: %.1f%s%s }',
        r$HomeTeam, r$AwayTeam, r$PredResult, r$Confidence,
        r$ProbHome, r$ProbDraw, r$ProbAway,
        r$ExpHome, r$ExpAway,
        rain_str, stakes_str
      )
    }), collapse=",\n"),
    "\n];"
  )

  # Look for existing weeklyMatches OR a good injection point
  match_line <- grep("weeklyMatches", html_content)
  script_line <- grep("<script>", html_content)

  if (length(match_line) > 0) {
    # Find the array end
    end_candidates <- grep("^\\s*\\];", html_content)
    end_idx <- end_candidates[end_candidates > match_line[1]][1]

    if (!is.na(end_idx)) {
      new_html <- c(
        html_content[1:(match_line[1]-1)],
        strsplit(js_array, "\n")[[1]],
        html_content[(end_idx+1):length(html_content)]
      )
      writeLines(new_html, "index.html")
      cat("✓ Updated existing weeklyMatches array\n")
    }
  } else if (length(script_line) > 0) {
    # Inject after first <script> tag
    insert_at <- script_line[1]
    new_html <- c(
      html_content[1:insert_at],
      strsplit(js_array, "\n")[[1]],
      "",
      html_content[(insert_at+1):length(html_content)]
    )
    writeLines(new_html, "index.html")
    cat("✓ Injected weeklyMatches after <script> tag\n")
  } else {
    cat("Could not find injection point in index.html\n")
    cat("Paste this manually just inside your first <script> tag:\n\n")
    cat(js_array, "\n\n")
  }
  cat("  → Push to GitHub to see it live!\n\n")
}


# ============================================================
# VISUALIZATION WITH FIXED PREDICTIONS
# ============================================================

plot_data <- next_mw
plot_data$Match <- paste(plot_data$HomeTeam, "vs", plot_data$AwayTeam)
plot_data <- plot_data[order(plot_data$Confidence), ]
plot_data$Match <- factor(plot_data$Match, levels=plot_data$Match)

long <- rbind(
  data.frame(Match=plot_data$Match, Outcome="Home Win",
             Prob=plot_data$ProbHome),
  data.frame(Match=plot_data$Match, Outcome="Draw",
             Prob=plot_data$ProbDraw),
  data.frame(Match=plot_data$Match, Outcome="Away Win",
             Prob=plot_data$ProbAway)
)
long$Outcome <- factor(long$Outcome, levels=c("Away Win","Draw","Home Win"))

# Confidence labels as separate data frame to avoid aes conflicts
conf_labels <- plot_data[, c("Match","Confidence","PredResult")]
conf_labels$y_pos <- 50
conf_labels$label <- paste0(conf_labels$Confidence, "%")

p1 <- ggplot(long, aes(x=Match, y=Prob, fill=Outcome)) +
  geom_col(width=0.65) +
  geom_text(data=conf_labels,
            aes(x=Match, y=y_pos, label=label),
            color="white", fontface="bold", size=3.5, inherit.aes=FALSE) +
  scale_fill_manual(values=c("Home Win"="#2563EB",
                              "Draw"="#64748B",
                              "Away Win"="#DC2626")) +
  coord_flip() +
  labs(title="Unified Model: Next Matchweek Predictions",
       subtitle="Poisson + Elo + Weather + Importance | Label = confidence",
       x=NULL, y="Probability (%)", fill=NULL) +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold"),
        legend.position="top",
        panel.grid.minor=element_blank())

ggsave("plots/unified_predictions.png", p1,
       width=11, height=max(6, nrow(next_mw)*0.7+2), dpi=300)
cat("Chart saved: plots/unified_predictions.png\n\n")

