# Churn-Customer-Analysis with R

<img width="1280" height="640" alt="Image" src="https://github.com/user-attachments/assets/d9d8feee-073d-4c9d-a3a0-0aacca088f79" />


# Customer Churn Analysis — Subscription Service

**Author:** Leena A. Elsheikh · Data Analyst


**Tools:** Power BI (Power Query) · R 4.6.1


**Date:** October 2026

A statistical analysis of what is associated with subscription cancellation, built for an imbalanced dataset where fewer than one in ten customers churned. The project covers data cleaning, descriptive comparisons, multivariable logistic regression, model evaluation suited to class imbalance, and one actionable recommendation.



---

## Business question

Which factors are most strongly associated with customers cancelling their subscription, how large is each effect, and what should the company do about it?

## Data

| Item | Value |
|---|---|
| Subscribers | 2,400 (one row each) |
| Period | Subscriptions from Feb 2024 to Nov 2025 |
| Outcome | Churned: 223 (9.3%), so the classes are imbalanced |
| Factors | Plan, billing cycle, payment method, onboarding attendance, discount, support tickets, average monthly sessions |
| Quality | No missing values, no duplicate subscriber IDs |

The dataset is not included in this repository.

## Approach

1. **Cleaning (Power BI / Power Query):** converted yes/no flags to 1/0 and the discount to a decimal rate, renamed columns, and checked quality on the full dataset rather than the default 1,000-row profile.
2. **Data quality review:** found that tenure is inconsistent with the subscription date (67% of tenures exceed the time elapsed since subscribing). Tenure was therefore used descriptively only, and the issue is documented.
3. **Descriptive comparison:** churned vs retained on every variable.
   - Numeric variables: Mann-Whitney U with rank-biserial effect sizes.
   - Categorical variables: chi-square with Cramér's V.
   - A Bonferroni threshold is reported for the eight tests.
4. **Churn rates by category:** each rate with a 95% Wilson confidence interval, plus the difference from the reference category in percentage points.
5. **Multivariable logistic regression:** sessions and tickets enter as natural splines because their relationship with churn is non-linear (U-shaped for sessions). The spline model fits significantly better than the linear one (LRT = 33.6, 4 df, p < 0.001).
6. **Effect estimates:** average marginal effects (percentage points) and odds ratios, each with a 95% CI. Factors are ranked by the AIC increase when each one is dropped.
7. **Evaluation:** stratified 5-fold cross-validation.
   - Discrimination: PR-AUC, ROC-AUC.
   - Classification: recall, precision and lift at practical thresholds.
   - Calibration: Brier score.

### Why not accuracy?

A model that predicts "no churn" for everyone reaches **90.7% accuracy** while finding no churners at all. The evaluation therefore focuses on the minority class:

| Rule | Flagged | Recall | Precision | Accuracy |
|---|---|---|---|---|
| Nobody churns (baseline) | 0 | 0% | — | 90.7% |
| Default threshold 0.5 | 2 | 0% | 0% | 90.6% |
| Top 20% risk | 480 | 40.4% | 18.8% | 78.2% |

No resampling (such as SMOTE) was applied: the goal is to explain factors, and resampling would distort true churn rates and regression estimates.

## Key findings

**Three strongest factors** (adjusted average marginal effect on churn probability):

| Rank | Factor | Comparison | Effect | 95% CI |
|---|---|---|---|---|
| 1 | Monthly sessions | 30 vs 10 sessions | −7.0 pp | −9.3 to −4.6 |
| 2 | Support tickets | 8 vs 2 tickets | +7.9 pp | +4.6 to +11.2 |
| 3 | Onboarding attendance | Attended vs not | −4.1 pp | −6.4 to −1.8 |

Plan and annual billing show smaller effects. Payment method and discount show no association with churn.

**Model performance (cross-validated):**

| Metric | Value | Baseline |
|---|---|---|
| ROC-AUC | 0.682 (95% CI 0.646–0.719) | 0.5 |
| PR-AUC | 0.180 | 0.093 |
| Precision in top 20% risk | 18.8% (lift 2.0) | 9.3% |

The model is useful for prioritising retention efforts, at about twice the efficiency of random targeting, but not for precise individual prediction.

## Recommendation

**Make onboarding a default step for every new subscriber and test it with a randomised trial before full rollout.** Onboarding is the only one of the three strongest factors the company controls directly.

Because 55% of customers already attend, the expected difference between trial groups is small (about 1.8 pp). Detecting it on churn would take years at the current sign-up volume, so the decision to roll out rests on early indicators (attendance and first-month sessions), while churn is monitored over the long term.

## Limitations

- **Associational, not causal:** the data are cross-sectional and observational.
- **Undocumented definitions:** the source does not define sessions, onboarding or the subscription date.
- **Timing of churn cannot be analysed:** there is no cancellation date, and tenure is inconsistent with the subscription date.
- **Possibly synthetic data:** near-zero correlation between factors (VIF ≈ 1.00) suggests the data may be generated, so findings need confirmation on real data.

## Repository structure

```
├── README.md
├── report/
│   └── churn_analysis_report.pdf      # Final report (Arabic)
├── R/
│   └── churn_analysis.R               # Full analysis script, steps 1–6
├── docs/
│   └── R_code_documentation.pdf       # Code documentation and analytical decisions
└── figures/                           # Charts exported from R
```

## How to run

1. Install R (4.2 or later, for UTF-8 Arabic text on Windows) and RStudio.
2. Install the packages:
   ```r
   install.packages(c("tidyverse", "broom", "binom", "car", "pROC",
                      "PRROC", "marginaleffects", "effectsize"))
   ```
3. Place `subscriptions-churn.csv` on your machine and update the file path in Step 1 of `R/churn_analysis.R`.
4. Run the script from top to bottom. Each step depends on the ones before it.


