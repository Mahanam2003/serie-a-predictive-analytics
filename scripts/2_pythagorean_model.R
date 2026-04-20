# Pythagorean Expected Points Model for Serie A
# I'm using this model to predict how many points each
# Serie A team SHOULD have based on their goals.
# Then I compare the prediction to reality to find
# which teams got lucky and which got unlucky.

rm(list = ls())

# Loading the data I saved from my first script
all_matches <- read.csv("data/serie_a_all_matches.csv")
league_table <- read.csv("data/serie_a_league_table_2425.csv")


# PART 1: Serie A Match Outcome Probabilities

# Every match ends one of three ways:
#   H = Home team wins
#   A = Away team wins
#   D = Draw
#
# I'm counting how often each outcome happens across 4 seasons
# (1,520 matches) to get a reliable probability for each.
# I used 4 seasons instead of 1 because a bigger sample
# gives more stable estimates, like flipping a coin 1000
# times instead of 10.

total_matches <- nrow(all_matches)
home_wins <- sum(all_matches$FTR == "H")
away_wins <- sum(all_matches$FTR == "A")
draws <- sum(all_matches$FTR == "D")

# Probability = count / total
# These three must add up to 1.0 because there is no
# fourth possible outcome in a football match.
pH <- round(home_wins / total_matches, 3)
pA <- round(away_wins / total_matches, 3)
pD <- round(draws / total_matches, 3)

cat("=== Serie A Match Outcome Probabilities (4 seasons) ===\n")
cat("Home Win:", pH, "\n")
cat("Away Win:", pA, "\n")
cat("Draw:", pD, "\n\n")


# PART 2: Competitive Balance (Skill vs Luck)

# I want to know how much of the Serie A table is explained
# by team quality vs random luck.
#
# Even if all 20 teams were EQUALLY good, some would still
# finish with more points than others just by chance.
# Think of it like flipping a coin 38 times. Sometimes you
# get 22 heads, sometimes 16. That spread is pure luck.
#
# So I compare the ACTUAL spread in the table to the spread
# I would EXPECT from luck alone. If the actual spread is
# way bigger than the luck spread, that means skill matters.

n_teams <- nrow(league_table)
m <- mean(league_table$PLD)

# STEP 1: Measure the actual spread (variance) in the table.
#
# Variance tells me how far teams are from the average.
# I subtract each teams points from the average,
# square the result (so negatives dont cancel positives),
# then take the mean of all those squared differences.
#
# Example:
#   Average points = 51.6
#   Napoli has 82:  (82 - 51.6)^2 = 924.2
#   Monza has 18:   (18 - 51.6)^2 = 1129.0
#   Big numbers = big spread = big variance
avg_pts <- mean(league_table$PTS)
pts_var <- mean((league_table$PTS - avg_pts)^2)

# STEP 2: Calculate how much spread I would expect from
# pure luck if every team was equally good.
#
# Football Pointing System:
#   Win  = 3 points
#   Draw = 1 point
#   Loss = 0 points
#
# In a HOME game, I can earn:
#   3 points if I win     (happens with probability pH)
#   1 point if I draw     (happens with probability pD)
#   0 points if I lose    (happens with probability pA)
#   My expected points = 3 x pH + 1 x pD + 0 x pA
#
# In an AWAY game, I can earn:
#   3 points if I win     (happens with probability pA)
#   1 point if I draw     (happens with probability pD)
#   0 points if I lose    (happens with probability pH)
#   My expected points = 3 x pA + 1 x pD + 0 x pH
#
# The 0 x pH (or 0 x pA) term is there because when the
# OTHER team wins, I get zero. I dont need to count
# losses separately because a home win IS an away loss.
# They are the same game viewed from different sides.
#
# The formula below uses the statistical rule:
#   Variance = E(X squared) minus E(X) squared
# to calculate how much the points would spread out
# over a full season (19 home + 19 away games) if
# every result was basically a coin flip.
exp_var <- (m/2) * (9 - (7*pD) - ((3*pH + pD)^2) - ((3*pA + pD)^2))

# STEP 3: Compare actual spread to luck spread.
# A low number like 0.18 means only 18% is luck, 82% is skill.
# A high number like 0.50 would mean half the table is random.
chance_fraction <- round(exp_var / pts_var, 3)

cat("=== Competitive Balance ===\n")
cat("Average points:", round(avg_pts, 1), "\n")
cat("Observed variance:", round(pts_var, 1), "\n")
cat("Expected random variance:", round(exp_var, 1), "\n")
cat("Fraction due to chance:", chance_fraction, "\n")
cat("Fraction due to skill:", 1 - chance_fraction, "\n\n")


# PART 3: Pythagorean Expected Points

# The Pythagorean model was invented for baseball by Bill James
# in the 1980s!
#
# The core idea is simple:
#   If you score a lot and concede a little, you should win a lot.
#   The formula turns that common sense into a number.
#
# PythagFrac = GF^b / (GF^b + GA^d)
#
# This gives a number between 0 and 1 that represents how
# dominant a team is based on their goals. For example:
#   Napoli scored 59, conceded 27 so PythagFrac = about 0.72
#   This means their goals suggest they should win about 72%
#   of their matches.
#
# I then multiply by 2.78 and by games played to convert
# that fraction into expected points for the full season.
#
# The constants (a=2.78, b=1.24, c=1.24, d=1.25) were found
# by testing thousands of combinations on historical data
# and picking the values that gave the most accurate predictions.
# A future improvement would be to optimize these specifically
# for Serie A instead of using the defaults.

a <- 2.78
b <- 1.24
c <- 1.24
d <- 1.25

# Calculating the "win fraction" for each team based on goals
league_table$PythagFrac <- (league_table$GF^b) / 
  ((league_table$GF^c) + (league_table$GA^d))

# Converting the fraction to expected points over the full season
league_table$PythagPTS <- round(a * league_table$PythagFrac * league_table$PLD, 2)

# How many points did each team get above or below expectation?
# Positive = overperformer (lucky, may drop next season)
# Negative = underperformer (unlucky, may rise next season)
league_table$PythagDiff <- round(league_table$PTS - league_table$PythagPTS, 2)

cat("=== Pythagorean Expected Points vs Actual ===\n")
print(league_table[, c("Club", "PLD", "GF", "GA", "PTS", "PythagPTS", "PythagDiff")])


# PART 4: How Good Is My Model?

# I measure accuracy two different ways because each one
# tells me something different:
#
# CORRELATION (r):
#   Does the model rank teams in the right order?
#   r = 1.0 means perfect ranking
#   r = 0.0 means no relationship at all
#   Our r = 0.978 which is excellent
#
# R SQUARED:
#   What percentage of the variation does my model explain?
#   0.957 means 95.7% which is very strong
#
# MEAN ABSOLUTE ERROR (MAE):
#   On average how many points off is each prediction?
#   I take each teams difference, ignore the sign (thats
#   what absolute value means), and average them all.
#   MAE of 3.54 means the model is typically within about
#   3.5 points of reality which is solid for 38 game seasons.

cor_result <- cor.test(league_table$PTS, league_table$PythagPTS)
mae <- round(mean(abs(league_table$PythagDiff)), 2)

cat("\n=== Model Evaluation ===\n")
cat("Correlation (r):", round(cor_result$estimate, 4), "\n")
cat("R-squared:", round(cor_result$estimate^2, 4), "\n")
cat("Mean Absolute Error:", mae, "points\n\n")


# PART 5: Scatter Plot

# Each dot on this plot is one team.
# X axis = how many points they actually got
# Y axis = how many points the model predicted
#
# If the model were perfect, every dot would sit exactly
# on the gray diagonal line. Teams above the line got
# fewer points than predicted (underperformers). Teams
# below the line got more points than predicted (overperformers).

# Saving plot to file so I can put it in the GitHub README
png("plots/actual_vs_pythagorean_pts.png", width = 800, height = 600)
plot(league_table$PTS, league_table$PythagPTS, 
     col = "darkblue", pch = 19, cex = 1.5,
     xlab = "Actual Points", 
     ylab = "Pythagorean Expected Points",
     main = "Serie A 2024/25: Actual vs Pythagorean Expected Points",
     xlim = c(10, 90), ylim = c(10, 90))
abline(lm(league_table$PythagPTS ~ league_table$PTS), lty = 2, col = "red", lwd = 2)
abline(0, 1, lty = 3, col = "gray50")
text(league_table$PTS, league_table$PythagPTS, 
     labels = league_table$Club, cex = 0.6, pos = 3)
legend("bottomright", 
       legend = c("Regression line", "Perfect prediction"),
       lty = c(2, 3), col = c("red", "gray50"), lwd = 2)
dev.off()

cat("Plot saved to plots/actual_vs_pythagorean_pts.png\n")


# PART 6: Who Got Lucky? Who Got Unlucky?

# Sorting teams by PythagDiff to find the biggest overperformers
# (teams that got way more points than their goals suggest)
# and underperformers (teams that should have more points
# based on how many goals they scored and conceded).
#
# This is useful for scouting because overperformers often
# drop the next season when their luck runs out, and
# underperformers often bounce back.

cat("\n=== Biggest Overperformers (more PTS than expected) ===\n")
over <- league_table[order(-league_table$PythagDiff), c("Club", "PTS", "PythagPTS", "PythagDiff")]
print(head(over, 5))

cat("\n=== Biggest Underperformers (fewer PTS than expected) ===\n")
print(tail(over, 5))

# Saving the full results table with Pythagorean columns added
write.csv(league_table, "data/serie_a_pythagorean_results.csv", row.names = FALSE)
cat("\nResults saved to data/serie_a_pythagorean_results.csv")