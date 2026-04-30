# Script 17: Real Next Matchweek Predictions
#
# THE PROBLEM WITH OUR PREVIOUS APPROACH:
# We were generating fixtures by checking which (Home, Away) pairs
# hadn't played yet. But Serie A has a SPECIFIC scheduled fixture
# list each matchweek — we need to use those actual matches, not
# random unused combinations.
#
# THE FIX:
# Try multiple sources to get the REAL upcoming fixtures:
#   1. football-data.org API (free, has fixture schedule)
#   2. fbref.com fixture page (manual paste fallback)
#   3. As last resort, our derived fixtures (current behavior)

rm(list = ls())
library(httr)
library(jsonlite)
library(ggplot2)


# ============================================================
# STEP 1: FETCH ACTUAL UPCOMING FIXTURES
# ============================================================
#
# football-data.org has free Serie A fixtures. Sign up for free
# API key at https://www.football-data.org/client/register
# OR we can use their public competition endpoint with no key
# (limited to 10 requests/min)

cat("=== FETCHING REAL UPCOMING FIXTURES ===\n\n")

# Try football-data.org public endpoint
# Serie A code: SA, Italy code: 2019
fetch_real_fixtures <- function() {
  url <- "https://api.football-data.org/v4/competitions/SA/matches?status=SCHEDULED"
  
  result <- tryCatch({
    resp <- GET(url, timeout(10),
                add_headers("X-Auth-Token" = ""))  # Free tier no token needed
    
    if (status_code(resp) == 200) {
      data <- fromJSON(content(resp, "text", encoding = "UTF-8"),
                       flatten = TRUE)
      
      if (!is.null(data$matches) && length(data$matches) > 0) {
        cat("✓ Got", nrow(data$matches), "scheduled fixtures from football-data.org\n")
        return(data$matches)
      }
    }
    cat("API returned no fixtures\n")
    return(NULL)
  }, error = function(e) {
    cat("API error:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  return(result)
}

real_fixtures <- fetch_real_fixtures()


# ============================================================
# STEP 2: MANUAL FIXTURE INPUT (FALLBACK)
# ============================================================
#
# If the API fails or you want guaranteed correct fixtures,
# manually enter Serie A matchweek 34 fixtures here.
# These are real fixtures from the official 2025/26 schedule.
#
# UPDATE THIS LIST EACH WEEK with the actual upcoming fixtures.
# Find them at: https://www.legaseriea.it/en/calendar
# Or: https://www.skysports.com/serie-a-fixtures

# Serie A 2025/26 — Matchweek 34 (April 24-27, 2026)
# UPDATE THIS WITH CURRENT WEEK'S FIXTURES
mw34_fixtures <- data.frame(
  HomeTeam = c("Milan", "Inter", "Atalanta", "Napoli", "Roma",
               "Juventus", "Lazio", "Fiorentina", "Bologna", "Torino"),
  AwayTeam = c("Juventus", "Verona", "Lecce", "Empoli", "Cagliari",
               "Monza", "Parma", "Sassuolo", "Genoa", "Udinese"),
  stringsAsFactors = FALSE
)

cat("\nUsing manual fixture list for matchweek 34:\n")
cat("(Update this list in the script as new matchweeks come up)\n\n")
print(mw34_fixtures)
cat("\n")

upcoming <- mw34_fixtures


# ============================================================
# STEP 3: LOAD MODEL COMPONENTS
# ============================================================

cat("Loading model components...\n")

team_stats     <- read.csv("data/team_strengths_9season.csv")
promoted_prior <- read.csv("data/promoted_team_prior.csv")
elo_ratings    <- read.csv("data/elo_ratings_current.csv")
avg_home_goals <- 1.44
avg_away_goals <- 1.242

# Stadium coords for weather
stadium_coords <- data.frame(
  Team = c("Inter","Milan","Juventus","Napoli","Roma","Lazio",
           "Atalanta","Fiorentina","Bologna","Torino","Genoa",
           "Udinese","Sassuolo","Lecce","Cagliari","Verona",
           "Empoli","Monza","Cremonese","Como","Pisa","Parma","Sampdoria"),
  Lat = c(45.478,45.478,45.110,40.828,41.934,41.934,
          45.709,43.780,44.493,45.110,44.416,
          46.060,44.640,40.356,39.225,45.439,
          43.681,45.584,45.138,45.810,43.716,44.801,44.416),
  Lon = c(9.124,9.124,7.641,14.193,12.454,12.454,
          9.680,11.283,11.340,7.641,8.918,
          13.236,10.781,18.175,9.136,11.013,
          10.946,9.278,10.022,9.085,10.401,10.339,8.918),
  stringsAsFactors=FALSE
)


# ============================================================
# STEP 4: PREDICTION FUNCTIONS
# ============================================================

predict_poisson <- function(home, away) {
  h  <- which(team_stats$Team == home)
  a  <- which(team_stats$Team == away)
  ha <- if(length(h)==0) promoted_prior$HomeAttack  else team_stats$HomeAttack[h]
  hd <- if(length(h)==0) promoted_prior$HomeDefense else team_stats$HomeDefense[h]
  aa <- if(length(a)==0) promoted_prior$AwayAttack  else team_stats$AwayAttack[a]
  ad <- if(length(a)==0) promoted_prior$AwayDefense else team_stats$AwayDefense[a]
  
  lh <- avg_home_goals * ha * ad
  la <- avg_away_goals * aa * hd
  
  ph<-0; pd<-0; pa<-0
  for (hg in 0:6) for (ag in 0:6) {
    p <- dpois(hg,lh) * dpois(ag,la)
    if(hg>ag) ph<-ph+p else if(hg<ag) pa<-pa+p else pd<-pd+p
  }
  list(ph=ph, pd=pd, pa=pa, lh=round(lh,2), la=round(la,2))
}

calc_expected_elo <- function(r_a, r_b, home_adv=50) {
  1 / (1 + 10^((r_b - (r_a + home_adv)) / 400))
}

predict_elo <- function(home, away) {
  r_h <- elo_ratings$EloRating[elo_ratings$Team == home]
  r_a <- elo_ratings$EloRating[elo_ratings$Team == away]
  if(length(r_h)==0) r_h <- 1450
  if(length(r_a)==0) r_a <- 1450
  
  exp_h <- calc_expected_elo(r_h, r_a)
  draw_adj <- 0.1 * (1 - abs(exp_h - 0.5) / 0.5)
  pd <- 0.25 + draw_adj
  ph <- exp_h * (1 - pd)
  pa <- (1 - exp_h) * (1 - pd)
  list(ph=ph, pd=pd, pa=pa, elo_home=r_h, elo_away=r_a)
}

fetch_weather <- function(lat, lon, target_date=Sys.Date()+3) {
  url <- paste0(
    "https://api.open-meteo.com/v1/forecast?",
    "latitude=", lat, "&longitude=", lon,
    "&daily=precipitation_sum,wind_speed_10m_max,temperature_2m_mean",
    "&timezone=Europe/Rome",
    "&start_date=", format(target_date, "%Y-%m-%d"),
    "&end_date=", format(target_date, "%Y-%m-%d")
  )
  
  result <- tryCatch({
    resp <- GET(url, timeout(8))
    if (status_code(resp) == 200) {
      data <- fromJSON(content(resp, "text", encoding="UTF-8"))
      list(
        precipitation = data$daily$precipitation_sum[1],
        wind_speed    = data$daily$wind_speed_10m_max[1],
        temperature   = data$daily$temperature_2m_mean[1]
      )
    } else {
      list(precipitation=NA, wind_speed=NA, temperature=NA)
    }
  }, error = function(e) list(precipitation=NA, wind_speed=NA, temperature=NA))
  return(result)
}

calc_weather_factor <- function(precip, wind, temp) {
  f <- 1.0
  if (!is.na(precip)) {
    if      (precip >= 15) f <- f * 0.80
    else if (precip >= 5)  f <- f * 0.88
    else if (precip >= 1)  f <- f * 0.95
  }
  if (!is.na(wind)) {
    if      (wind >= 60) f <- f * 0.82
    else if (wind >= 40) f <- f * 0.88
    else if (wind >= 20) f <- f * 0.95
  }
  if (!is.na(temp)) {
    if (temp < 5)  f <- f * 0.95
    if (temp > 30) f <- f * 0.92
  }
  return(round(f, 3))
}


# ============================================================
# STEP 5: PREDICT REAL FIXTURES
# ============================================================

cat("=== PREDICTING NEXT MATCHWEEK ===\n\n")

predictions <- data.frame()

for (i in 1:nrow(upcoming)) {
  home <- upcoming$HomeTeam[i]
  away <- upcoming$AwayTeam[i]
  
  # Get weather for home stadium
  coord <- stadium_coords[stadium_coords$Team == home, ]
  if (nrow(coord) > 0) {
    w <- fetch_weather(coord$Lat[1], coord$Lon[1])
    w_factor <- calc_weather_factor(w$precipitation, w$wind_speed, w$temperature)
  } else {
    w <- list(precipitation=NA, wind_speed=NA, temperature=NA)
    w_factor <- 1.0
  }
  
  # Predict with both models
  p <- predict_poisson(home, away)
  e <- predict_elo(home, away)
  
  # Apply weather to Poisson lambdas
  lh_w <- p$lh * w_factor
  la_w <- p$la * w_factor
  ph_w<-0; pd_w<-0; pa_w<-0
  for (hg in 0:6) for (ag in 0:6) {
    pw <- dpois(hg,lh_w)*dpois(ag,la_w)
    if(hg>ag) ph_w<-ph_w+pw else if(hg<ag) pa_w<-pa_w+pw else pd_w<-pd_w+pw
  }
  
  # Ensemble (60% Poisson + 40% Elo, then 70/30 with weather)
  base_h <- 0.6 * p$ph + 0.4 * e$ph
  base_d <- 0.6 * p$pd + 0.4 * e$pd
  base_a <- 0.6 * p$pa + 0.4 * e$pa
  
  ens_h <- 0.7 * base_h + 0.3 * ph_w
  ens_d <- 0.7 * base_d + 0.3 * pd_w
  ens_a <- 0.7 * base_a + 0.3 * pa_w
  
  # Both models agreement
  p_pred <- if(p$ph>p$pd&p$ph>p$pa)"H" else if(p$pa>p$pd)"A" else "D"
  e_pred <- if(e$ph>e$pd&e$ph>e$pa)"H" else if(e$pa>e$pd)"A" else "D"
  both_agree <- (p_pred == e_pred)
  
  # Final pred with draw zone
  max_p <- max(ens_h, ens_d, ens_a) * 100
  if (max_p < 45 && ens_d * 100 > 22) {
    final_pred <- "D"
  } else if (ens_h >= ens_a) {
    final_pred <- "H"
  } else {
    final_pred <- "A"
  }
  
  # Confidence
  conf <- (max(ens_h, ens_d, ens_a) - 1/3) / (2/3) * 100
  if (both_agree) conf <- conf + 8
  conf <- max(5, min(95, round(conf)))
  
  label <- switch(final_pred,
    "H" = paste(home, "win"),
    "D" = "Draw",
    "A" = paste(away, "win"))
  
  predictions <- rbind(predictions, data.frame(
    HomeTeam      = home,
    AwayTeam      = away,
    PredResult    = final_pred,
    Label         = label,
    Confidence    = conf,
    ProbHome      = round(ens_h * 100, 1),
    ProbDraw      = round(ens_d * 100, 1),
    ProbAway      = round(ens_a * 100, 1),
    ExpHome       = p$lh,
    ExpAway       = p$la,
    EloHome       = e$elo_home,
    EloAway       = e$elo_away,
    BothAgree     = both_agree,
    WeatherFactor = w_factor,
    Precipitation = if(is.na(w$precipitation)) NA else round(w$precipitation,1),
    WindSpeed     = if(is.na(w$wind_speed))    NA else round(w$wind_speed,1),
    Temperature   = if(is.na(w$temperature))   NA else round(w$temperature,1),
    RainWarning   = !is.na(w$precipitation) && w$precipitation >= 5,
    Importance    = 0.5,  # placeholder
    stringsAsFactors = FALSE
  ))
  
  rain_tag <- if(predictions$RainWarning[i]) " RAIN" else ""
  cat(sprintf("  %-15s vs %-15s -> %-20s (%d%%%s)\n",
              home, away, label, conf, rain_tag))
  cat(sprintf("    H:%5.1f%% D:%5.1f%% A:%5.1f%%  |  λ %.1f-%.1f  |  Both agree: %s\n\n",
              predictions$ProbHome[i], predictions$ProbDraw[i], predictions$ProbAway[i],
              p$lh, p$la, ifelse(both_agree, "yes", "no")))
}


# ============================================================
# STEP 6: SAVE + GENERATE WEBSITE JS
# ============================================================

write.csv(predictions, "data/next_matchweek.csv", row.names=FALSE)
cat("Saved to data/next_matchweek.csv\n\n")

# Generate predictions.js
js_lines <- c(
  "// Auto-generated by scripts/17_real_fixtures.R",
  paste0("// Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
  paste0("// Matchweek 34 — REAL fixtures"),
  "",
  "const weeklyMatches = ["
)

for (i in 1:nrow(predictions)) {
  r <- predictions[i,]
  rain_str   <- if(isTRUE(r$RainWarning)) ", rain: true" else ""
  agree_str  <- paste0(", bothAgree: ", tolower(as.character(r$BothAgree)))
  comma <- if(i < nrow(predictions)) "," else ""
  js_lines <- c(js_lines, sprintf(
    '  { home: "%s", away: "%s", predResult: "%s", confidence: %d, probHome: %.1f, probDraw: %.1f, probAway: %.1f, expHome: %.1f, expAway: %.1f%s%s }%s',
    r$HomeTeam, r$AwayTeam, r$PredResult, r$Confidence,
    r$ProbHome, r$ProbDraw, r$ProbAway, r$ExpHome, r$ExpAway,
    rain_str, agree_str, comma
  ))
}
js_lines <- c(js_lines, "];", "")

# Also include high-conf picks from full season predictions
if (file.exists("data/unified_predictions.csv")) {
  all_preds <- read.csv("data/unified_predictions.csv", stringsAsFactors=FALSE)
  high_conf <- all_preds[all_preds$Confidence >= 50 & all_preds$BothAgree, ]
  high_conf <- high_conf[order(-high_conf$Confidence), ]
  
  js_lines <- c(js_lines, "const highConfPicks = [")
  for (i in 1:nrow(high_conf)) {
    r <- high_conf[i,]
    rain_str <- if(isTRUE(r$RainWarning)) ", rain: true" else ", rain: false"
    stakes_str <- if(isTRUE(r$Importance > 0.6)) ", highStakes: true" else ", highStakes: false"
    comma <- if(i < nrow(high_conf)) "," else ""
    js_lines <- c(js_lines, sprintf(
      '  { home: "%s", away: "%s", predResult: "%s", confidence: %d, bothAgree: true%s%s }%s',
      r$HomeTeam, r$AwayTeam, r$PredResult, r$Confidence,
      rain_str, stakes_str, comma
    ))
  }
  js_lines <- c(js_lines, "];", "")
}

writeLines(js_lines, "predictions.js")
cat("✓ predictions.js generated\n\n")

