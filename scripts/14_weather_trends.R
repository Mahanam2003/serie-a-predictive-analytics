# Script 14: Weather + Google Trends Features
#
# TWO NEW FEATURES:
#
# 1. WEATHER (Open-Meteo API - completely free, no key needed)
#    Serie A stadiums are spread across Italy from Turin to Palermo.
#    Weather varies massively - Milan in November vs Naples in April.
#    Research shows:
#      - Heavy rain (-30% goals vs dry conditions in some studies)
#      - High wind reduces long passes and crosses
#      - Extreme cold/heat affects stamina = more errors late in games
#
#    We fetch historical weather for each match city on match date.
#    Then adjust Poisson lambda (expected goals) based on conditions.
#
# 2. GOOGLE TRENDS (gtrendsR package)
#    Pre-match search interest = proxy for match importance/pressure.
#    High search volume before a match means:
#      - It's a high-stakes game (title race, derby, relegation)
#      - More pressure on players → more mistakes → upsets more likely
#      - Underdogs may raise their game (cup final effect)
#
#    We query search interest for each team in the week before their match.
#    A "buzz ratio" = how much more interest than their average.

rm(list = ls())
library(ggplot2)
library(httr)
library(jsonlite)
library(gtrendsR)


# PART 1: Stadium Coordinates
# We need lat/lon to fetch weather from Open-Meteo API

stadium_coords <- data.frame(
  Team = c("Inter","Milan","Juventus","Napoli","Roma","Lazio",
           "Atalanta","Fiorentina","Bologna","Torino",
           "Genoa","Sampdoria","Udinese","Sassuolo","Lecce",
           "Cagliari","Verona","Empoli","Spezia","Venezia",
           "Salernitana","Monza","Cremonese","Frosinone",
           "Como","Pisa","Parma","Brescia","Benevento","Spal",
           "Crotone","Cosenza","Reggina","Ascoli","Palermo"),
  City = c("Milan","Milan","Turin","Naples","Rome","Rome",
           "Bergamo","Florence","Bologna","Turin",
           "Genoa","Genoa","Udine","Sassuolo","Lecce",
           "Cagliari","Verona","Empoli","La Spezia","Venice",
           "Salerno","Monza","Cremona","Frosinone",
           "Como","Pisa","Parma","Brescia","Benevento","Ferrara",
           "Crotone","Cosenza","Reggio Calabria","Ascoli","Palermo"),
  Lat  = c(45.478, 45.478, 45.110, 40.828, 41.934, 41.934,
           45.709, 43.780, 44.493, 45.110,
           44.416, 44.416, 46.060, 44.640, 40.356,
           39.225, 45.439, 43.681, 44.106, 45.438,
           40.682, 45.584, 45.138, 41.645,
           45.810, 43.716, 44.801, 45.571, 41.130, 44.838,
           39.080, 39.298, 38.110, 42.855, 38.120),
  Lon  = c(9.124, 9.124, 7.641, 14.193, 12.454, 12.454,
           9.680, 11.283, 11.340, 7.641,
           8.918, 8.918, 13.236, 10.781, 18.175,
           9.136, 11.013, 10.946, 9.825, 12.332,
           14.776, 9.278, 10.022, 13.342,
           9.085, 10.401, 10.339, 10.218, 14.783, 11.619,
           17.125, 16.252, 15.647, 13.575, 13.362),
  stringsAsFactors = FALSE
)

cat("Stadium coordinates loaded for", nrow(stadium_coords), "teams\n\n")


# PART 2: Weather Fetching Function
#
# Open-Meteo historical API:
# https://archive-api.open-meteo.com/v1/archive
# No API key needed. Free forever.
#
# We fetch for the match date:
#   - precipitation_sum: total rain in mm
#   - wind_speed_10m_max: max wind speed km/h
#   - temperature_2m_mean: average temperature °C

fetch_weather <- function(lat, lon, date_str) {
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", lat, "&longitude=", lon,
    "&start_date=", date_str, "&end_date=", date_str,
    "&daily=precipitation_sum,wind_speed_10m_max,temperature_2m_mean",
    "&timezone=Europe/Rome"
  )

  result <- tryCatch({
    resp <- GET(url, timeout(10))
    if (status_code(resp) == 200) {
      data <- fromJSON(content(resp, "text", encoding="UTF-8"))
      data.frame(
        precipitation = data$daily$precipitation_sum[1],
        wind_speed    = data$daily$wind_speed_10m_max[1],
        temperature   = data$daily$temperature_2m_mean[1],
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(precipitation=NA, wind_speed=NA, temperature=NA)
    }
  }, error = function(e) {
    data.frame(precipitation=NA, wind_speed=NA, temperature=NA)
  })

  return(result)
}


# PART 3: Weather Adjustment Formula
#
# HOW WEATHER AFFECTS GOALS (based on research):
#
# Rain:
#   Dry (0mm):           no adjustment
#   Light rain (1-5mm):  -5% goals
#   Heavy rain (5-15mm): -12% goals
#   Very heavy (15mm+):  -20% goals
#
# Wind:
#   Calm (<20 km/h):     no adjustment
#   Moderate (20-40):    -5% goals
#   Strong (40-60):      -12% goals
#   Very strong (60+):   -18% goals
#
# Temperature extremes:
#   Cold (<5°C):         -5% goals (players less fluid)
#   Hot (>30°C):         -8% goals (fatigue)
#   Ideal (10-25°C):     no adjustment

calc_weather_factor <- function(precip, wind, temp) {
  factor <- 1.0

  if (!is.na(precip)) {
    if      (precip >= 15) factor <- factor * 0.80
    else if (precip >= 5)  factor <- factor * 0.88
    else if (precip >= 1)  factor <- factor * 0.95
  }

  if (!is.na(wind)) {
    if      (wind >= 60) factor <- factor * 0.82
    else if (wind >= 40) factor <- factor * 0.88
    else if (wind >= 20) factor <- factor * 0.95
  }

  if (!is.na(temp)) {
    if      (temp < 5)  factor <- factor * 0.95
    else if (temp > 30) factor <- factor * 0.92
  }

  return(factor)
}


# PART 4: Google Trends Function
#
# We query how much each team is being searched in Italy
# in the 7 days BEFORE the match.
# "Buzz ratio" = current week interest / team's average interest
# High buzz = high-stakes match, more pressure

fetch_trends_safe <- function(team_names, geo="IT") {
  # Map team names to Italian search terms
  search_terms <- team_names

  result <- tryCatch({
    trends <- gtrends(
      keyword  = search_terms[1:min(2, length(search_terms))],
      geo      = geo,
      time     = "today 1-m",  # last month
      onlyInterest = TRUE
    )
    if (!is.null(trends$interest_over_time)) {
      # Return average interest score (0-100)
      avg_interest <- tapply(trends$interest_over_time$hits,
                             trends$interest_over_time$keyword, mean, na.rm=TRUE)
      return(avg_interest)
    }
    return(NULL)
  }, error = function(e) {
    cat("  Google Trends unavailable:", conditionMessage(e), "\n")
    return(NULL)
  })

  return(result)
}


# PART 5: Test on Recent 2024/25 Matches With Weather Data
#
# Strategy: fetch weather for a sample of 2024/25 matches
# and test if weather-adjusted model is more accurate

cat("=== TESTING WEATHER ADJUSTMENT ===\n\n")

# Load 2024/25 data with dates
season_2425 <- read.csv("data/fd_2425.csv")
season_2425$DateParsed <- as.Date(season_2425$Date, format="%d/%m/%y")
if (sum(is.na(season_2425$DateParsed)) > nrow(season_2425)/2) {
  season_2425$DateParsed <- as.Date(season_2425$Date, format="%d/%m/%Y")
}
season_2425 <- season_2425[!is.na(season_2425$FTHG) & !is.na(season_2425$DateParsed), ]
season_2425 <- season_2425[order(season_2425$DateParsed), ]

# Load team stats
team_stats <- read.csv("data/team_strengths_9season.csv")
promoted_prior <- read.csv("data/promoted_team_prior.csv")
avg_home_goals <- 1.44
avg_away_goals <- 1.242

# Sample 30 matches for weather testing (fetching 380 would take too long)
set.seed(42)
sample_idx <- sort(sample(1:nrow(season_2425), min(30, nrow(season_2425))))
sample_matches <- season_2425[sample_idx, ]

cat("Testing weather effect on", nrow(sample_matches), "matches from 2024/25\n")
cat("Fetching weather data (may take 30-60 seconds)...\n\n")

weather_results <- data.frame()

for (i in 1:nrow(sample_matches)) {
  home <- sample_matches$HomeTeam[i]
  away <- sample_matches$AwayTeam[i]
  date_str <- format(sample_matches$DateParsed[i], "%Y-%m-%d")
  actual <- sample_matches$FTR[i]

  # Get stadium coordinates for home team
  coord_idx <- which(stadium_coords$Team == home)
  if (length(coord_idx) == 0) {
    lat <- 41.9; lon <- 12.5  # Default: Rome
  } else {
    lat <- stadium_coords$Lat[coord_idx]
    lon <- stadium_coords$Lon[coord_idx]
  }

  # Fetch weather
  weather <- fetch_weather(lat, lon, date_str)

  # Calculate weather factor
  w_factor <- calc_weather_factor(
    weather$precipitation, weather$wind, weather$temperature)

  # Base Poisson prediction
  h_idx <- which(team_stats$Team == home)
  a_idx <- which(team_stats$Team == away)
  ha <- if(length(h_idx)==0) promoted_prior$HomeAttack  else team_stats$HomeAttack[h_idx]
  hd <- if(length(h_idx)==0) promoted_prior$HomeDefense else team_stats$HomeDefense[h_idx]
  aa <- if(length(a_idx)==0) promoted_prior$AwayAttack  else team_stats$AwayAttack[a_idx]
  ad <- if(length(a_idx)==0) promoted_prior$AwayDefense else team_stats$AwayDefense[a_idx]

  # Base lambdas
  lh_base <- avg_home_goals * ha * ad
  la_base <- avg_away_goals * aa * hd

  # Weather-adjusted lambdas
  lh_adj <- lh_base * w_factor
  la_adj <- la_base * w_factor

  # Calculate probabilities (base)
  ph_b<-0; pd_b<-0; pa_b<-0
  for (hg in 0:6) for (ag in 0:6) {
    p <- dpois(hg,lh_base)*dpois(ag,la_base)
    if(hg>ag) ph_b<-ph_b+p else if(hg<ag) pa_b<-pa_b+p else pd_b<-pd_b+p
  }

  # Calculate probabilities (weather-adjusted)
  ph_w<-0; pd_w<-0; pa_w<-0
  for (hg in 0:6) for (ag in 0:6) {
    p <- dpois(hg,lh_adj)*dpois(ag,la_adj)
    if(hg>ag) ph_w<-ph_w+p else if(hg<ag) pa_w<-pa_w+p else pd_w<-pd_w+p
  }

  pred_base <- if(ph_b>pd_b&ph_b>pa_b)"H" else if(pa_b>pd_b)"A" else "D"
  pred_weather <- if(ph_w>pd_w&ph_w>pa_w)"H" else if(pa_w>pd_w)"A" else "D"

  weather_results <- rbind(weather_results, data.frame(
    HomeTeam    = home,
    AwayTeam    = away,
    Date        = date_str,
    Actual      = actual,
    Precip_mm   = round(weather$precipitation, 1),
    Wind_kmh    = round(weather$wind_speed, 1),
    Temp_C      = round(weather$temperature, 1),
    WeatherFactor = round(w_factor, 3),
    BasePred    = pred_base,
    WeatherPred = pred_weather,
    BaseCorrect = (pred_base == actual),
    WeatherCorrect = (pred_weather == actual),
    stringsAsFactors = FALSE
  ))

  if (i %% 5 == 0) cat("  Processed", i, "/", nrow(sample_matches), "matches\n")
}

cat("\n=== WEATHER ANALYSIS RESULTS ===\n")
cat("Matches tested:", nrow(weather_results), "\n\n")

# Overall accuracy
base_acc    <- round(mean(weather_results$BaseCorrect)*100, 1)
weather_acc <- round(mean(weather_results$WeatherCorrect)*100, 1)

cat("Base model accuracy:    ", base_acc, "%\n")
cat("Weather-adjusted model: ", weather_acc, "%\n")
cat("Change:                 ", round(weather_acc - base_acc, 1), "pp\n\n")

# Weather vs no weather matches
rainy <- weather_results[!is.na(weather_results$Precip_mm) &
                          weather_results$Precip_mm >= 5, ]
dry   <- weather_results[!is.na(weather_results$Precip_mm) &
                          weather_results$Precip_mm < 5, ]

if (nrow(rainy) > 0) {
  cat("Rainy matches (5mm+):\n")
  cat("  Count:", nrow(rainy), "\n")
  cat("  Avg goals (actual):", round(mean(rainy$Actual == "D")*100,1),
      "% draw rate\n")
  cat("  Weather model accuracy:", round(mean(rainy$WeatherCorrect)*100,1), "%\n")
  cat("  Base model accuracy:   ", round(mean(rainy$BaseCorrect)*100,1), "%\n\n")
}

if (nrow(dry) > 0) {
  cat("Dry matches (<5mm):\n")
  cat("  Count:", nrow(dry), "\n")
  cat("  Weather model accuracy:", round(mean(dry$WeatherCorrect)*100,1), "%\n")
  cat("  Base model accuracy:   ", round(mean(dry$BaseCorrect)*100,1), "%\n\n")
}

# Weather stats distribution
cat("=== WEATHER CONDITIONS IN SAMPLE ===\n")
cat("Avg precipitation:", round(mean(weather_results$Precip_mm, na.rm=TRUE),1), "mm\n")
cat("Avg wind speed:   ", round(mean(weather_results$Wind_kmh, na.rm=TRUE),1), "km/h\n")
cat("Avg temperature:  ", round(mean(weather_results$Temp_C, na.rm=TRUE),1), "°C\n")
cat("Matches with rain:", sum(weather_results$Precip_mm >= 1, na.rm=TRUE), "\n")
cat("Matches dry:      ", sum(weather_results$Precip_mm < 1, na.rm=TRUE), "\n\n")


# PART 6: Google Trends Test
#
# Test on a few current teams to see if the API works
# Then we'll use it to adjust confidence for upcoming matches

cat("=== TESTING GOOGLE TRENDS ===\n")
cat("Querying search interest for top Serie A teams...\n\n")

# Test with top 4 teams
test_teams <- c("Inter Milan", "Napoli", "Juventus", "Atalanta")
trends_data <- list()

for (team in test_teams) {
  cat("  Fetching trends for:", team, "...")
  result <- tryCatch({
    t <- gtrends(keyword=team, geo="IT", time="today 3-m", onlyInterest=TRUE)
    if (!is.null(t$interest_over_time)) {
      avg <- mean(as.numeric(t$interest_over_time$hits), na.rm=TRUE)
      cat(" avg interest:", round(avg,1), "/100\n")
      trends_data[[team]] <- avg
    } else {
      cat(" no data\n")
    }
    Sys.sleep(2)  # Avoid rate limiting
  }, error = function(e) {
    cat(" failed:", conditionMessage(e), "\n")
  })
}

if (length(trends_data) > 0) {
  cat("\n=== GOOGLE TRENDS RESULTS ===\n")
  trends_df <- data.frame(
    Team = names(trends_data),
    AvgInterest = round(unlist(trends_data), 1)
  )
  trends_df <- trends_df[order(-trends_df$AvgInterest), ]
  print(trends_df)

  cat("\nINSIGHT: Higher search interest = more media attention = higher pressure\n")
  cat("Teams above their average: potential upset candidates\n\n")
} else {
  cat("Google Trends unavailable (rate limited or no connection)\n")
  cat("This is expected - Google blocks automated requests\n")
  cat("Alternative: use match importance score instead (see below)\n\n")
}


# PART 7: Match Importance Score (Trends Alternative)
#
# If Google Trends is blocked, we calculate match importance ourselves:
#   - Position gap between teams (close = important)
#   - Points from safe zone (relegation pressure)
#   - Derby indicator (same city)
#   - Title race (top 3 teams playing each other)

cat("=== MATCH IMPORTANCE SCORE ===\n")
cat("(Proxy for Google Trends when API is unavailable)\n\n")

# Current 2025/26 standings proxy from our Elo ratings
elo_ratings <- read.csv("data/elo_ratings_current.csv")
elo_2526 <- elo_ratings[elo_ratings$Team %in% c(
  "Inter","Napoli","Juventus","Atalanta","Milan","Roma","Lazio",
  "Fiorentina","Bologna","Como","Genoa","Torino","Parma","Udinese",
  "Cagliari","Lecce","Verona","Cremonese","Pisa","Sassuolo"), ]
elo_2526 <- elo_2526[order(-elo_2526$EloRating), ]
elo_2526$Rank <- 1:nrow(elo_2526)

calc_importance <- function(home, away, standings) {
  h_rank <- standings$Rank[standings$Team == home]
  a_rank <- standings$Rank[standings$Team == away]

  if (length(h_rank)==0 || length(a_rank)==0) return(0.5)

  n_teams <- nrow(standings)

  # Title race: both teams in top 4
  title_race <- (h_rank <= 4 & a_rank <= 4)

  # Relegation battle: either team in bottom 4
  relegation <- (h_rank >= (n_teams-3) | a_rank >= (n_teams-3))

  # Position closeness (0-1 scale)
  rank_diff <- abs(h_rank - a_rank)
  closeness <- 1 - (rank_diff / n_teams)

  # Derby (same city - simplified)
  derbies <- list(
    c("Inter","Milan"),
    c("Roma","Lazio"),
    c("Juventus","Torino"),
    c("Genoa","Sampdoria")
  )
  is_derby <- any(sapply(derbies, function(d)
    (home %in% d & away %in% d)))

  # Importance score (0-1)
  score <- closeness * 0.4 +
           as.numeric(title_race) * 0.3 +
           as.numeric(relegation) * 0.2 +
           as.numeric(is_derby) * 0.1

  return(round(min(1, score), 3))
}

# Test importance for upcoming matches
upcoming_test <- data.frame(
  HomeTeam = c("Inter","Napoli","Lecce","Como","Atalanta"),
  AwayTeam = c("Juventus","Atalanta","Inter","Cagliari","Milan")
)

cat("Sample match importance scores:\n")
for (i in 1:nrow(upcoming_test)) {
  imp <- calc_importance(upcoming_test$HomeTeam[i],
                         upcoming_test$AwayTeam[i], elo_2526)
  cat(sprintf("  %-12s vs %-12s  Importance: %.2f\n",
              upcoming_test$HomeTeam[i], upcoming_test$AwayTeam[i], imp))
}

cat("\nHigh importance (>0.7) = expect tighter game, upsets more likely\n\n")


# PART 8: Save Results + Combined Feature Set

write.csv(weather_results, "data/weather_analysis.csv", row.names=FALSE)
write.csv(elo_2526, "data/standings_with_importance.csv", row.names=FALSE)

# Visualize weather factor distribution
if (nrow(weather_results) > 0 && any(!is.na(weather_results$WeatherFactor))) {
  p1 <- ggplot(weather_results[!is.na(weather_results$Precip_mm),],
               aes(x=Precip_mm, y=WeatherFactor)) +
    geom_point(aes(color=Actual), size=3, alpha=0.7) +
    geom_smooth(method="loess", se=FALSE, color="#3B82F6") +
    geom_hline(yintercept=1, linetype="dashed", color="gray50") +
    scale_color_manual(values=c("H"="#3B82F6","D"="#94A3B8","A"="#DC2626")) +
    labs(title="Weather Impact on Expected Goals",
         subtitle="Points below 1.0 line = weather reduces goals",
         x="Precipitation (mm)", y="Weather Adjustment Factor",
         color="Actual Result") +
    theme_minimal()

  ggsave("plots/weather_impact.png", p1, width=10, height=6, dpi=300)
  cat("Weather plot saved!\n\n")
}

cat("=== SCRIPT 14 COMPLETE ===\n\n")
cat("SUMMARY OF FINDINGS:\n")
cat("Base model accuracy (sample):    ", base_acc, "%\n")
cat("Weather-adjusted accuracy:       ", weather_acc, "%\n")
cat("Weather improvement:             ", round(weather_acc-base_acc,1), "pp\n\n")
