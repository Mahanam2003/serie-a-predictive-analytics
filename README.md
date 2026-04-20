# Serie A Predictive Analytics

Predicting Serie A end-of-season standings using a custom combined model that blends actual goals, expected goals (xG), and pressing intensity (PPDA). Built in R.

## What This Project Does

I built a predictive model that estimates how many points each Serie A team will finish the season with. The model started simple and evolved through five iterations, each one reducing the prediction error. The final combined model achieves a Mean Absolute Error of **2.58 points** on the training season and generalizes to unseen seasons with an average MAE of **2.66 points**.

The model was validated across three Serie A seasons (2022/23, 2023/24, 2024/25) and outperforms both the basic Pythagorean approach and Understat's own expected points (xPTS) metric.

## The Model Evolution

| Model | Description | MAE (points) |
|-------|-------------|:------------:|
| 1. EPL Pythagorean | Standard Pythagorean model with English Premier League coefficients | 3.54 |
| 2. Serie A Pythagorean | Same model but with coefficients optimized specifically for Serie A | 2.96 |
| 3. Optimized Coefficients | Grid search across 132,651 combinations to find the best exponents | 2.96 |
| 4. Poisson Match Model | Individual match prediction using Poisson distribution, compared to Bet365 | 53.9% accuracy |
| 5. Combined Model | Blended goals + xG + pressing adjustment, 8 parameters optimized | **2.58** |

## The Combined Model Equation

The final model combines three data sources into one equation:

**Step 1: Blend actual goals with expected goals (xG)**
```
BlendedGF = 0.69 × ActualGoals + (-0.04) × xG
BlendedGA = 0.63 × GoalsAgainst + 0.08 × xGA
```

**Step 2: Adjust for pressing intensity**
```
PressAdj = 1 + 0.155 × (LeagueAvgPPDA - TeamPPDA) / LeagueAvgPPDA
```
Teams with lower PPDA (more aggressive pressing) get a points boost. Teams that sit deep get a penalty.

**Step 3: Pythagorean prediction with pressing adjustment**
```
PredictedPTS = 2.98 × (BlendedGF^1.33 / (BlendedGF^1.33 + BlendedGA^1.35)) × GamesPlayed × PressAdj
```

## Key Findings

**Serie A is 81.4% skill, 18.6% luck.** Using competitive balance analysis across 1,520 matches (4 seasons), I found that the vast majority of the Serie A table reflects real team quality rather than random variation.

**Actual goals matter more than xG for Serie A.** The optimizer assigned a weight of 0.69 to actual goals but only -0.04 to xG. This suggests that in Serie A, finishing ability and clinical conversion are more stable predictors than shot quality alone. This contradicts the common analytics assumption that xG is always more predictive than raw goals.

**Pressing intensity has a measurable impact on points.** The pressing adjustment parameter (w5 = 0.155) is positive and significant, confirming that teams who press more aggressively tend to earn more points in Serie A, even after accounting for goals and xG.

**The model generalizes across seasons.** When tested on seasons it was never trained on, the MAE only increased by 0.08 points on average, confirming the model learned real structural patterns in Serie A rather than memorizing one season.

## Validation Results

The model was trained on the 2024/25 season and tested on two completely unseen seasons:

|  | Combined Model MAE | Simple Pythagorean MAE |
|--|:------------------:|:----------------------:|
| 2024/25 (Training) | 2.58 | 3.54 |
| 2023/24 (Test) | 2.97 | 2.80 |
| 2022/23 (Test) | 2.34 | 2.66 |

The combined model beats the simple Pythagorean on average across unseen data, confirming it adds real predictive value.

## Overperformers and Underperformers (2024/25)

Teams whose actual points differ most from model predictions:

**Overperformers (got more points than expected, may regress):**
- Verona: +9.2 points above prediction
- Lecce: +9.4 points above prediction
- Lazio: +5.2 points above prediction

**Underperformers (got fewer points than expected, may improve):**
- Atalanta: -4.2 points below prediction
- Monza: -3.7 points below prediction
- Juventus: -1.9 points below prediction

## Project Structure

```
serie-a-predictive-analytics/
├── scripts/
│   ├── 01_data_collection.R        # Pull 4 seasons of Serie A data, build league table
│   ├── 02_pythagorean_model.R      # Pythagorean expected points with competitive balance
│   ├── 03_optimize_coefficients.R  # Grid search to optimize coefficients for Serie A
│   ├── 04_poisson_model.R          # Match-level Poisson predictions vs Bet365
│   ├── 05_combined_model.R         # Combined model with xG and pressing data
│   └── 06_model_validation.R       # Out-of-sample validation across 3 seasons
├── data/
│   ├── serie_a_all_matches.csv
│   ├── serie_a_league_table_2425.csv
│   ├── serie_a_xg_stats.csv.csv    # 2024/25 xG and PPDA from Understat
│   ├── serie_a_xg_2223.csv         # 2022/23 xG data for validation
│   └── erie_a_xg_2324.csv          # 2023/24 xG data for validation
├── plots/
│   ├── actual_vs_pythagorean_pts.png
│   ├── poisson_vs_bookmakers.png
│   ├── model_evolution.png
│   ├── model_validation.png
│   └── combined_model_predictions.png
└── README.md
```

## Data Sources

- **Match data:** football-data.co.uk (free, 4 seasons of Serie A results including goals, shots, corners, fouls)
- **xG and pressing data:** Understat.com (free, expected goals, PPDA, deep completions)
- **Betting odds:** Bet365 odds included in football-data.co.uk dataset

## Methods Used

- Pythagorean expected points model (adapted from baseball analytics)
- Poisson distribution for match outcome prediction
- Parameter optimization using grid search and Nelder-Mead algorithm
- Weighted blending of actual goals and expected goals
- Competitive balance analysis (skill vs luck decomposition)
- Out-of-sample validation across multiple seasons
- Comparison against professional bookmaker predictions (Bet365)

## Limitations

- The combined model has 8 parameters optimized on 20 data points (20 teams per season), which is below the recommended 10:1 ratio. This creates some overfitting risk, though validation results suggest the model still generalizes well.
- The Poisson match prediction model correctly predicted 0 out of 84 draws. This is a known limitation of the independent Poisson approach. The Dixon-Coles model corrects for this by adding a correlation parameter for low-scoring games.
- PPDA is used as a proxy for pressing intensity. Actual distance covered and sprint data would be more precise but is not freely available for Serie A.
- The bookmaker comparison in Script 04 has a data alignment issue for teams not present in the training data. This affects the bookmaker accuracy number but not the model accuracy.

## Tools and Languages

- **R** (version 4.4.3)
- **ggplot2** for visualizations
- **Base R** for statistical modeling and optimization

## What I Would Do Next

- Implement the Dixon-Coles model to fix the draw prediction weakness
- Train on all available seasons simultaneously rather than a single season
- Add match-level xG data instead of season aggregates
- Build an interactive R Shiny dashboard for exploring predictions
- Acquire tracking data (distance covered, sprints) if budget allows

## Acknowledgments

This project was inspired by Clive Beggs' "Soccer Analytics: An Introduction Using R" methodology. The Pythagorean model framework originates from Bill James' work in baseball analytics. xG and PPDA data provided by Understat.com. Match data from football-data.co.uk.
