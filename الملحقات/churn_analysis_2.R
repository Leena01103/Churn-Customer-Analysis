# =============================================================================
# Customer Churn Analysis — Subscriptions Dataset
# Author : Leena A. Elsheikh
# R      : 4.6.1
# Purpose: Describe differences between churned and retained subscribers,
#          identify the three strongest factors associated with churn,
#          estimate each factor's effect with confidence intervals, and
#          evaluate the model with metrics suited to imbalanced data.
#
# Data   : subscriptions-churn.csv — 2,400 subscribers, one row each,
#          12 columns (Arabic headers and Arabic category values).
#
# One-time setup (run once in the Console, not every time):
# install.packages(c("tidyverse", "broom", "binom", "car", "pROC",
#                    "PRROC", "marginaleffects", "effectsize"))
# =============================================================================


# =============================================================================
# STEP 1 — Data preparation
# Mirrors the cleaning done in Power Query, so results match Power BI.
# =============================================================================
library(tidyverse)

# 1.1 Read the file (UTF-8 so Arabic text is read correctly)
df <- read_csv("C:/Users/lina0/Downloads/subscriptions-churn.csv",
               locale = locale(encoding = "UTF-8"))

# 1.2 English column names (same order as the columns in the file)
names(df) <- c("SubscriberID", "SubscriptionDate", "Plan", "MonthlyPrice",
               "BillingCycle", "TenureMonths", "PaymentMethod", "SupportTickets",
               "AvgMonthlySessions", "DiscountRate", "AttendedOnboarding", "Churned")

# 1.3 Transformations
#   - DiscountRate: 0–25 -> 0–0.25
#   - Yes/No (نعم/لا) flags -> 1/0, so the mean of each column is a rate
#   - Factor level order sets the reference category in the regression:
#     Plan = Basic (أساسي), BillingCycle = Monthly (شهري), PaymentMethod = Card (بطاقة)
df <- df %>%
  mutate(
    DiscountRate       = DiscountRate / 100,
    AttendedOnboarding = as.integer(AttendedOnboarding == "نعم"),
    Churned            = as.integer(Churned == "نعم"),
    Plan          = factor(Plan, levels = c("أساسي", "متقدم", "أعمال")),
    BillingCycle  = factor(BillingCycle, levels = c("شهري", "سنوي")),
    PaymentMethod = factor(PaymentMethod, levels = c("بطاقة", "محفظة", "تحويل"))
  )

# 1.4 Checks (expected: 2400 rows, 223 churned = 0.0929, 1314 attended onboarding)
glimpse(df)
nrow(df)
sum(df$Churned); mean(df$Churned)
sum(df$AttendedOnboarding)
table(df$Plan); table(df$BillingCycle); table(df$PaymentMethod)
sort(unique(df$DiscountRate))
class(df$SubscriptionDate)


# =============================================================================
# STEP 2 — Descriptive statistics: churned vs retained
# Numeric: Mann-Whitney U + rank-biserial effect size.
# Categorical: chi-square + bias-corrected Cramér's V.
# TenureMonths is descriptive only (inconsistent with SubscriptionDate and
# related to the outcome itself), so it is not used in the model.
# =============================================================================
library(effectsize)

# Group label for display
df <- df %>%
  mutate(Group = factor(Churned, levels = c(0, 1), labels = c("بقي", "ألغى")))

# 2.1 Numeric variables: summary by group
num_vars <- c("AvgMonthlySessions", "SupportTickets", "DiscountRate", "TenureMonths")

num_summary <- df %>%
  group_by(Group) %>%
  summarise(across(all_of(num_vars),
                   list(mean = mean, median = median, sd = sd),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(-Group, names_to = c("Variable", "Stat"), names_sep = "_") %>%
  pivot_wider(names_from = c(Group, Stat), values_from = value)

print(num_summary, width = Inf)

# 2.2 Numeric variables: test and effect size
# Positive r_rb = retained group higher; negative = churned group higher
num_tests <- map_dfr(num_vars, function(v) {
  f  <- reformulate("Group", response = v)
  w  <- wilcox.test(f, data = df, exact = FALSE)
  rb <- rank_biserial(f, data = df)
  tibble(Variable = v,
         p_value  = w$p.value,
         r_rb     = rb[[1]],
         CI_low   = rb$CI_low,
         CI_high  = rb$CI_high)
})

print(num_tests, width = Inf)

# 2.3 Categorical variables: distribution within each group (column %)
cat_vars <- c("Plan", "BillingCycle", "PaymentMethod", "AttendedOnboarding")

for (v in cat_vars) {
  cat("\n====", v, "====\n")
  tab <- table(df[[v]], df$Group)
  print(round(100 * prop.table(tab, margin = 2), 1))
}

# 2.4 Categorical variables: test and effect size
# min_expected checks the chi-square assumption (use Fisher if < 5)
cat_tests <- map_dfr(cat_vars, function(v) {
  tab <- table(df[[v]], df$Group)
  ch  <- chisq.test(tab)
  tibble(Variable     = v,
         Chi2         = unname(ch$statistic),
         df           = unname(ch$parameter),
         p_value      = ch$p.value,
         Cramers_V    = cramers_v(tab)[[1]],
         min_expected = min(ch$expected))
})

print(cat_tests, width = Inf)
# Note: 8 tests in Step 2 -> Bonferroni threshold 0.05 / 8 = 0.00625


# =============================================================================
# STEP 3 — Churn rate per category with Wilson confidence intervals
# Difference from the reference (first) level in percentage points,
# with a 95% CI from prop.test. Numeric variables are banded for display only.
# =============================================================================
library(binom)

# 3.1 Display bands for numeric variables
df <- df %>%
  mutate(
    TicketsBand  = cut(SupportTickets, breaks = c(-Inf, 0, 2, 5, Inf),
                       labels = c("0", "1-2", "3-5", "6+")),
    SessionsBand = cut(AvgMonthlySessions, breaks = c(-Inf, 14, 29, 44, Inf),
                       labels = c("2-14", "15-29", "30-44", "45-59")),
    DiscountBand = factor(DiscountRate, labels = c("0%", "5%", "10%", "15%", "25%"))
  )

# 3.2 Function: churn rate + Wilson CI + difference from reference
rate_table <- function(var) {
  t <- df %>%
    group_by(Level = .data[[var]]) %>%
    summarise(n = n(), churned = sum(Churned), .groups = "drop") %>%
    mutate(Variable = var, Level = as.character(Level), rate = churned / n)

  ci <- binom.confint(t$churned, t$n, methods = "wilson")
  t$lower <- ci$lower
  t$upper <- ci$upper

  t$diff_pp <- NA; t$diff_low <- NA; t$diff_high <- NA
  for (i in seq_len(nrow(t))[-1]) {
    pt <- prop.test(c(t$churned[i], t$churned[1]), c(t$n[i], t$n[1]), correct = FALSE)
    t$diff_pp[i]   <- 100 * (t$rate[i] - t$rate[1])
    t$diff_low[i]  <- 100 * pt$conf.int[1]
    t$diff_high[i] <- 100 * pt$conf.int[2]
  }
  t
}

# 3.3 Apply to all factors
vars3 <- c("AttendedOnboarding", "Plan", "BillingCycle", "PaymentMethod",
           "TicketsBand", "SessionsBand", "DiscountBand")

results3 <- map_dfr(vars3, rate_table) %>%
  select(Variable, Level, n, churned, rate, lower, upper, diff_pp, diff_low, diff_high) %>%
  mutate(across(c(rate, lower, upper), ~ round(100 * .x, 1)),
         across(c(diff_pp, diff_low, diff_high), ~ round(.x, 1)))

print(results3, n = Inf, width = Inf)


# =============================================================================
# STEP 4 — Multivariable logistic regression
# Step 3 showed a U-shaped relationship for sessions and a rise at high ticket
# counts, so a linear model is compared with a flexible model where sessions
# and tickets enter as natural splines (3 df). MonthlyPrice is excluded
# (one fixed price per plan -> perfectly collinear with Plan).
# =============================================================================
library(splines); library(broom); library(marginaleffects); library(car)

# Discount in units of 5 percentage points, so its coefficient is meaningful
df <- df %>% mutate(Discount5 = DiscountRate * 20)

# 4.1 Linear model
m_linear <- glm(Churned ~ Plan + BillingCycle + PaymentMethod + AttendedOnboarding +
                  Discount5 + SupportTickets + AvgMonthlySessions,
                data = df, family = binomial)

# 4.2 Flexible model (splines) — 13 parameters, ~17 churn events per parameter
m_spline <- glm(Churned ~ Plan + BillingCycle + PaymentMethod + AttendedOnboarding +
                  Discount5 + ns(SupportTickets, df = 3) + ns(AvgMonthlySessions, df = 3),
                data = df, family = binomial)

# Is the curvature real? (LRT and AIC) -> spline model chosen
anova(m_linear, m_spline, test = "LRT")
AIC(m_linear, m_spline)

# 4.3 Importance of each factor: drop-one likelihood ratio test
# Ranking uses the AIC increase on deletion (fair across terms with different df)
drop1(m_spline, test = "LRT")

# 4.4 Odds ratios with 95% CI (spline terms excluded: not interpretable alone)
tidy(m_spline, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(!str_detect(term, "ns\\(")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(width = Inf)

# 4.5 Average marginal effects (AME) in percentage points
# Pre-specified contrasts for the curved variables:
#   sessions 30 vs 10 (near overall mean vs low use); tickets 8 vs 2 (2 = median)
ame_cat <- avg_comparisons(m_spline,
             variables = c("AttendedOnboarding", "Plan", "BillingCycle",
                           "PaymentMethod", "Discount5"))

ame_num <- avg_comparisons(m_spline,
             variables = list(AvgMonthlySessions = c(10, 30),
                              SupportTickets     = c(2, 8)))

bind_rows(as.data.frame(ame_cat), as.data.frame(ame_num)) %>%
  transmute(term, contrast,
            AME_pp = round(100 * estimate, 1),
            low    = round(100 * conf.low, 1),
            high   = round(100 * conf.high, 1),
            p.value = signif(p.value, 3)) %>%
  print()

# 4.6 Multicollinearity (GVIF^(1/(2*Df)) near 1 = no problem)
vif(m_spline)

# 4.7 Shape of the relationship after adjustment
# Note: other variables are held at typical values, so read the SHAPE, not the level
plot_predictions(m_spline, condition = "AvgMonthlySessions")
plot_predictions(m_spline, condition = "SupportTickets")

# 4.8 Post-hoc check of the upturn at heavy use (55 vs 28 sessions)
# Exploratory: 28 was chosen after seeing the curve (its minimum)
avg_comparisons(m_spline, variables = list(AvgMonthlySessions = c(28, 55)))


# =============================================================================
# STEP 5 — Model evaluation (stratified 5-fold cross-validation)
# Each subscriber is scored by a model that did not see them.
# Accuracy alone is misleading with 9.3% churn, so PR-AUC, recall, precision,
# Brier score and calibration are reported alongside ROC-AUC.
# =============================================================================
library(pROC); library(PRROC)

# 5.1 Stratified folds (same churn rate in each fold)
set.seed(2026)
k <- 5
df <- df %>%
  group_by(Churned) %>%
  mutate(fold = sample(rep(1:k, length.out = n()))) %>%
  ungroup()

f_spline <- formula(m_spline)
df$pred_cv <- NA_real_

for (i in 1:k) {
  train    <- df %>% filter(fold != i)
  test_idx <- which(df$fold == i)
  m <- glm(f_spline, data = train, family = binomial)
  df$pred_cv[test_idx] <- predict(m, newdata = df[test_idx, ], type = "response")
}

# 5.2 Discrimination
roc_obj <- roc(df$Churned, df$pred_cv, quiet = TRUE)
cat("ROC-AUC:", round(auc(roc_obj), 3), "\n")
print(ci.auc(roc_obj))

pr <- pr.curve(scores.class0 = df$pred_cv[df$Churned == 1],
               scores.class1 = df$pred_cv[df$Churned == 0], curve = TRUE)
cat("PR-AUC:", round(pr$auc.integral, 3),
    " | Baseline (churn rate):", round(mean(df$Churned), 3), "\n")

# 5.3 Calibration
brier      <- mean((df$pred_cv - df$Churned)^2)
brier_null <- mean((mean(df$Churned) - df$Churned)^2)
cat("Brier:", round(brier, 4), " | Brier of no-variable model:", round(brier_null, 4), "\n")

df %>%
  mutate(decile = ntile(pred_cv, 10)) %>%
  group_by(decile) %>%
  summarise(n = n(),
            predicted = round(100 * mean(pred_cv), 1),
            observed  = round(100 * mean(Churned), 1)) %>%
  print(n = Inf)

# 5.4 Accuracy vs recall and precision at different thresholds
thr_metrics <- function(thr, label) {
  flag <- as.integer(df$pred_cv >= thr)
  tp <- sum(flag == 1 & df$Churned == 1)
  fp <- sum(flag == 1 & df$Churned == 0)
  fn <- sum(flag == 0 & df$Churned == 1)
  tibble(rule      = label,
         threshold = round(thr, 3),
         flagged   = sum(flag),
         recall    = round(tp / (tp + fn), 3),
         precision = round(tp / (tp + fp), 3),
         accuracy  = round(mean(flag == df$Churned), 3))
}

bind_rows(
  tibble(rule = "No one churns (baseline)", threshold = NA, flagged = 0,
         recall = 0, precision = NA, accuracy = round(1 - mean(df$Churned), 3)),
  thr_metrics(0.5, "Default threshold 0.5"),
  thr_metrics(mean(df$Churned), "Threshold = churn rate"),
  thr_metrics(quantile(df$pred_cv, 0.8), "Top 20% risk")
) %>% print(width = Inf)

# 5.5 Precision-Recall curve (dashed line = baseline)
plot(pr, main = "Precision-Recall (CV)")
abline(h = mean(df$Churned), lty = 2)


# =============================================================================
# STEP 6 — Testing a proposed mechanism
# Hypothesis: not attending onboarding leads to more support tickets.
# Result: not supported (means 2.78 vs 2.80, p = 0.93) -> the two factors
# are associated with churn independently.
# =============================================================================
df %>%
  group_by(AttendedOnboarding) %>%
  summarise(n = n(),
            mean_tickets   = round(mean(SupportTickets), 2),
            median_tickets = median(SupportTickets),
            pct_6plus      = round(100 * mean(SupportTickets >= 6), 1))

wilcox.test(SupportTickets ~ AttendedOnboarding, data = df, exact = FALSE)


# =============================================================================
# Session info (for reproducibility)
# =============================================================================
sessionInfo()
