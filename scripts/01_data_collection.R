
rm(list = ls())  # Clear workspace


seasons <- c("2021" = "2122", "2022" = "2223", 
             "2023" = "2324", "2024" = "2425")

all_matches <- data.frame()  # Empty dataframe to store everything

for (s in names(seasons)) {
  url <- paste0("https://www.football-data.co.uk/mmz4281/", seasons[s], "/I1.csv")
  temp <- read.csv(url)
  temp$Season <- s  # Tag each match with its season
  
  # Keep only the columns we need
  temp <- temp[, c("Season", "Date", "HomeTeam", "AwayTeam", 
                   "FTHG", "FTAG", "FTR", "HS", "AS", 
                   "HST", "AST", "HC", "AC", "HF", "AF")]
  
  all_matches <- rbind(all_matches, temp)
  cat("Loaded season:", s, "- Matches:", nrow(temp), "\n")
}

cat("\nTotal matches loaded:", nrow(all_matches), "\n")

# --- Build a league table for the most recent season (2024/25) ---

current <- all_matches[all_matches$Season == "2024", ]

teams <- unique(c(current$HomeTeam, current$AwayTeam))

league_table <- data.frame(
  Club = teams,
  PLD = 0, W = 0, D = 0, L = 0,
  GF = 0, GA = 0, GD = 0, PTS = 0
)

for (i in 1:nrow(current)) {
  home <- current$HomeTeam[i]
  away <- current$AwayTeam[i]
  hg <- current$FTHG[i]
  ag <- current$FTAG[i]
  
  h_idx <- which(league_table$Club == home)
  a_idx <- which(league_table$Club == away)
  
  # Games played
  league_table$PLD[h_idx] <- league_table$PLD[h_idx] + 1
  league_table$PLD[a_idx] <- league_table$PLD[a_idx] + 1
  
  # Goals
  league_table$GF[h_idx] <- league_table$GF[h_idx] + hg
  league_table$GA[h_idx] <- league_table$GA[h_idx] + ag
  league_table$GF[a_idx] <- league_table$GF[a_idx] + ag
  league_table$GA[a_idx] <- league_table$GA[a_idx] + hg
  
  # Results
  if (hg > ag) {
    league_table$W[h_idx] <- league_table$W[h_idx] + 1
    league_table$L[a_idx] <- league_table$L[a_idx] + 1
  } else if (hg < ag) {
    league_table$L[h_idx] <- league_table$L[h_idx] + 1
    league_table$W[a_idx] <- league_table$W[a_idx] + 1
  } else {
    league_table$D[h_idx] <- league_table$D[h_idx] + 1
    league_table$D[a_idx] <- league_table$D[a_idx] + 1
  }
}

# Calculate goal difference and points
league_table$GD <- league_table$GF - league_table$GA
league_table$PTS <- (league_table$W * 3) + league_table$D

# Sort by points (descending)
league_table <- league_table[order(-league_table$PTS, -league_table$GD), ]
rownames(league_table) <- 1:nrow(league_table)

# Display the table
print(league_table)

# --- Save data ---
write.csv(all_matches, "data/serie_a_all_matches.csv", row.names = FALSE)
write.csv(league_table, "data/serie_a_league_table_2425.csv", row.names = FALSE)

cat("\nData saved to /data folder!")