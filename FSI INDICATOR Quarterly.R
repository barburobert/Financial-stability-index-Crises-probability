# PROJECT: Financial Stability Indicator (FSI)
# SOURCE: EIB Working Paper (2019)
# PURPOSE: Reconstruction of an early-warning Financial Stability Indicator

### 1. ENVIRONMENT SETUP ----

# 1.1 Working directory
setwd("D:/PROIECTE/FSI INDICATOR")

# 1.2 Install required packages
# install.packages("dplyr")
# install.packages("pROC")
# install.packages("ggplot2")

# 1.3 Load libraries
library(dplyr)
library(pROC)
library(ggplot2)

### 2. DATA IMPORT----

# Import the quarterly panel dataset used for FSI construction
X <- read.table(
  file = "FSI INDICATOR.txt",
  sep = "\t",
  header = TRUE,
  dec = ".",
  check.names = FALSE
)

### 3. INITIAL DATA CHECKS----

# 3.1 Convert quarterly date field and extract calendar year
X$QuarterlyDate <- as.Date(X$QuarterlyDate)
X$Year <- as.numeric(format(X$QuarterlyDate, "%Y"))

# 3.2 Structure checks
str(X)
summary(X)

# 3.3 Verify ordering
X <- X[order(X$Country, X$QuarterlyDate), ]

### 4. CRISIS EVENT DEFINITION----

# This section reconstructs crisis start events using:
# - systemic banking crises
# - equity price crashes
# A crisis is flagged if either event type occurs.

# 4.1 Banking crisis events (Laeven & Valencia + EIB appendix)
banking_events <- data.frame(
  Country = c("Austria","Belgia","Columbia","Switzerland","Germany","Denmark","Spain",
              "Finland","France","UnitedKingdom","Greece","Hungary","Hungary","Ireland",
              "Italy","Japan","Korea","Netherlands","Norway","Portugal","Sweden","Sweden",
              "United States","United States"),
  start_year = c(2008,2008,1998,2008,2008,2008,2008,1991,2008,2007,2008,1991,2008,2008,
                 2008,1997,1997,2008,1991,2008,1991,2008,1988,2007)
)

X$banking_crisis_start <- 0

for (i in 1:nrow(banking_events)) {
  X$banking_crisis_start[
    X$Country == banking_events$Country[i] &
      X$Year == banking_events$start_year[i]
  ] <- 1
}

# Number of quarterly observations flagged as banking crisis start
cat("Quarterly observations flagged as banking crisis start:",
    sum(X$banking_crisis_start, na.rm = TRUE), "\n")

# 4.2 Equity price shocks
# The study uses quarterly equity declines greater than 15%.
if ("SharePricesQoQ" %in% names(X)) {
  X$equity_crash_start <- ifelse(
    !is.na(X$SharePricesQoQ) & X$SharePricesQoQ <= -0.15,
    1, 0
  )
} else {
  X$equity_crash_start <- 0
}

# Number of quarterly observations flagged as equity crashes
cat("Quarterly observations flagged as equity crashes:",
    sum(X$equity_crash_start, na.rm = TRUE), "\n")

# 4.3 Combined crisis indicator
X$crisis_start <- ifelse(
  X$banking_crisis_start == 1 | X$equity_crash_start == 1,
  1, 0
)

cat("Quarterly observations flagged as crisis start:",
    sum(X$crisis_start, na.rm = TRUE), "\n")

### 5. QUARTERLY TARGET CONSTRUCTION AND EXCLUSION RULES ----

# In the quarterly version:
# - exclusion removes the crisis quarter and the next 8 quarters
# - target is set to 1 if a crisis starts within the next 4 to 12 quarters
# This approximates a 1–3 year early-warning horizon in quarterly data.

X$crisis_dummy <- 0
X$exclude <- 0

for (ctry in unique(X$Country)) {
  
  idx <- which(X$Country == ctry)
  idx <- idx[order(X$QuarterlyDate[idx])]
  n <- length(idx)
  
  # 5.1 Exclusion window: crisis quarter + next 8 quarters
  event_pos <- which(X$crisis_start[idx] == 1)
  
  for (p in event_pos) {
    end_excl <- min(p + 8, n)
    X$exclude[idx[p:end_excl]] <- 1
  }
  
  # 5.2 Early-warning target: crisis in t+4 to t+12 quarters
  for (k in 1:n) {
    future_pos <- (k + 4):(k + 12)
    future_pos <- future_pos[future_pos <= n]
    
    if (length(future_pos) > 0 &&
        any(X$crisis_start[idx[future_pos]] == 1)) {
      X$crisis_dummy[idx[k]] <- 1
    }
  }
}
### 6. TRAINING SAMPLE DEFINITION----

vars <- c("CreditGDP", "CreditGDPgrowth5Y", "HPIgrowth5Y", "SharePricesgrowth2Y")

X_train <- X[X$QuarterlyDate <= as.Date("2025-06-30") & X$exclude == 0,
             c("Country", "QuarterlyDate", "crisis_dummy", vars)]

X_train <- X_train[complete.cases(X_train[, c("crisis_dummy", vars)]), ]

### 7. FEATURE STANDARDIZATION ----

# Standardization is performed by country using historical means and standard deviations
# computed on the quarterly training set.

stats_list <- list()

for (v in vars) {
  stats <- X_train %>%
    group_by(Country) %>%
    summarise(
      Mean = mean(.data[[v]], na.rm = TRUE),
      Sd   = sd(.data[[v]], na.rm = TRUE),
      .groups = "drop"
    )
  stats_list[[v]] <- stats
}

standardize <- function(df, v, stats) {
  if ("Mean" %in% names(df)) df$Mean <- NULL
  if ("Sd" %in% names(df)) df$Sd <- NULL
  
  df <- left_join(df, stats, by = "Country")
  df[[paste0(v, "_z")]] <- (df[[v]] - df$Mean) / df$Sd
  
  return(df)
}

X_train_std <- X_train
for (v in vars) {
  X_train_std <- standardize(X_train_std, v, stats_list[[v]])
}
### 8. MODEL ESTIMATION ----

# Logistic regression is used to estimate the probability of crisis
# within a 4–12 quarter horizon.

model_fsi_q <- glm(
  crisis_dummy ~ CreditGDP_z + CreditGDPgrowth5Y_z + HPIgrowth5Y_z + SharePricesgrowth2Y_z,
  data = X_train_std,
  family = binomial(link = "logit")
)

summary(model_fsi_q)

#The estimated quarterly logistic specification suggests that financial vulnerability is most strongly associated with equity-market dynamics, followed by credit conditions. In particular, the coefficient on SharePricesgrowth2Y_z is positive and highly statistically significant, indicating that stronger two-year equity price growth is associated with a higher probability of a crisis event within the subsequent 4–12 quarters. This is consistent with the idea that sustained asset-price booms may signal the build-up of financial imbalances.
#Both CreditGDP_z and CreditGDPgrowth5Y_z also enter the model with positive and statistically significant coefficients. This implies that higher credit-to-GDP levels and stronger medium-term credit expansion are associated with greater crisis vulnerability, which is economically consistent with standard early-warning theory. By contrast, HPIgrowth5Y_z is not statistically significant in this specification, suggesting that five-year house-price growth does not add meaningful explanatory power once the other predictors are included.
#At the model level, the decline in deviance from 3696.6 to 3506.6 indicates that the explanatory variables improve fit relative to the intercept-only benchmark. Overall, the results suggest that the quarterly adaptation of the FSI framework captures relevant early-warning information, with equity growth emerging as the dominant predictor and credit-related variables also contributing meaningfully, while housing-price growth appears less informative in this specification.
#Because the predictors are standardized, each coefficient measures the effect of a one-standard-deviation increase in the corresponding variable. 
#A one-standard-deviation increase in CreditGDP_z is associated with an increase of about 18.3% in the odds of a crisis occurring within the next 4–12 quarters, holding the other variables constant.
#A one-standard-deviation increase in CreditGDPgrowth5Y_z is associated with an increase of about 20.2% in the odds of a future crisis, ceteris paribus.
#A one-standard-deviation increase in HPIgrowth5Y_z is associated with a decrease of about 4.5% in the odds of crisis, although this effect is not statistically significant in this specification.
#A one-standard-deviation increase in SharePricesgrowth2Y_z is associated with an increase of about 73.2% in the odds of a future crisis, holding the remaining predictors constant. This makes two-year equity growth the strongest predictor in the model.


### 9. IN-SAMPLE MODEL EVALUATION ----

X_train_std$probs <- predict(model_fsi_q, type = "response")

roc_obj <- roc(X_train_std$crisis_dummy, X_train_std$probs, quiet = TRUE)
auc_val <- auc(roc_obj)

cat("In-Sample AUROC:", round(auc_val, 4), "\n")

# Interpretation:
# The in-sample AUROC of 0.6569 indicates moderate but limited discriminatory power.
# The model performs better than random classification, but its ability to distinguish
# crisis-related observations from tranquil periods remains relatively weak.
# In practical terms, this suggests that the quarterly specification contains useful
# early-warning information, although the signal is not strong enough to support
# high-confidence classification on its own.

# ROC curve
plot(
  roc_obj,
  col = "blue",
  lwd = 3,
  main = "ROC Curve - Quarterly FSI Model",
  legacy.axes = TRUE
)
abline(a = 0, b = 1, lty = 2, col = "gray")
text(
  0.65, 0.2,
  labels = paste("AUC =", round(auc_val, 4)),
  cex = 1.1
)

coords_opt <- coords(roc_obj, "best", ret = "threshold")
threshold <- as.numeric(coords_opt[1])

cat("Optimal Threshold:", round(threshold, 4), "\n")

# Interpretation:
# The optimal classification threshold is 0.3347, which is notably below the
# conventional 0.50 cutoff. This indicates that the model needs a relatively
# permissive alarm threshold in order to balance sensitivity and specificity.
# In other words, crisis-related observations do not typically receive extremely
# high fitted probabilities, so a lower cutoff is required to identify a
# reasonable share of vulnerable periods.

### 10. FULL-SAMPLE QUARTERLY SCORING ----

X_pred <- X[, c("Country", "QuarterlyDate", "Year", "crisis_start", vars)]

for (v in vars) {
  X_pred <- standardize(X_pred, v, stats_list[[v]])
}

vars_z <- paste0(vars, "_z")
X_pred$FSI_raw_q <- NA

valid_idx <- complete.cases(X_pred[, vars_z]) & X_pred$QuarterlyDate <= as.Date("2025-06-30")

X_pred$FSI_raw_q[valid_idx] <- predict(
  model_fsi_q,
  newdata = X_pred[valid_idx, ],
  type = "response"
)
### 11. IN-SAMPLE VISUALIZATION ----

# This chart shows:
# - the quarterly FSI series
# - gray bands for crisis quarters
# - red bands only for warning quarters that were truly followed by a crisis
#   within the next 4 to 12 quarters
# - the optimal threshold as a dashed line

ctry_plot <- "United States"

# 1. Build plotting dataset
df_plot <- subset(
  X_pred,
  Country == ctry_plot &
    QuarterlyDate >= as.Date("1979-01-01") &
    QuarterlyDate <= as.Date("2025-06-30")
)

df_plot <- df_plot[order(df_plot$QuarterlyDate), ]

# 2. Define predicted alarms
df_plot$alarm <- ifelse(!is.na(df_plot$FSI_raw_q) & df_plot$FSI_raw_q > threshold, 1, 0)

# 3. Define "true warning" quarters:
#    an alarm at time t is a true warning only if a crisis starts in t+4 ... t+12
df_plot$true_warning <- 0

n <- nrow(df_plot)

for (i in 1:n) {
  future_idx <- (i + 4):(i + 12)
  future_idx <- future_idx[future_idx <= n]
  
  if (df_plot$alarm[i] == 1 &&
      length(future_idx) > 0 &&
      any(df_plot$crisis_start[future_idx] == 1, na.rm = TRUE)) {
    df_plot$true_warning[i] <- 1
  }
}

# 4. Function to convert consecutive 1s into shaded date intervals
make_intervals <- function(dates, flag_vec, band_days = 90) {
  idx <- which(flag_vec == 1)
  
  if (length(idx) == 0) {
    return(data.frame(xmin = as.Date(character()), xmax = as.Date(character())))
  }
  
  groups <- cumsum(c(1, diff(idx) != 1))
  
  out <- data.frame(
    xmin = tapply(dates[idx], groups, min),
    xmax = tapply(dates[idx], groups, max)
  )
  
  out$xmin <- as.Date(out$xmin, origin = "1970-01-01")
  out$xmax <- as.Date(out$xmax, origin = "1970-01-01") + band_days
  
  rownames(out) <- NULL
  return(out)
}

# 5. Build shaded bands
crisis_bands  <- make_intervals(df_plot$QuarterlyDate, df_plot$crisis_start, band_days = 90)
warning_bands <- make_intervals(df_plot$QuarterlyDate, df_plot$true_warning, band_days = 90)

# 6. Static chart
ggplot(df_plot, aes(x = QuarterlyDate, y = FSI_raw_q)) +
  
  # Gray crisis periods
  geom_rect(
    data = crisis_bands,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "gray60",
    alpha = 0.35
  ) +
  
  # Red true-warning periods
  geom_rect(
    data = warning_bands,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "red",
    alpha = 0.18
  ) +
  
  # FSI line
  geom_line(color = "#005a9e", linewidth = 0.8, na.rm = TRUE) +
  
  # Threshold
  geom_hline(
    yintercept = threshold,
    color = "red",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    name = "Probability of Crisis"
  ) +
  
  labs(
    title = paste("Quarterly FSI -", ctry_plot),
    subtitle = "Gray = crisis periods | Red = valid warning periods (true 1–3 year lead signals)",
    x = "Time"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
### 12. OUT-OF-SAMPLE RECURSIVE ESTIMATION ----

# This block implements a recursive real-time quarterly framework:
# - re-estimate the quarterly model each year using all information available up to the previous year-end
# - compute a dynamic threshold from the current training sample
# - generate quarterly out-of-sample FSI values for the test year

start_pred_year <- 1985
end_pred_year   <- 2025

X_oos_results <- data.frame()

cat("\n--- STARTING RECURSIVE OOS ESTIMATION (QUARTERLY):", start_pred_year, "-", end_pred_year, "---\n")

for (yr in start_pred_year:end_pred_year) {
  
  cat(paste("Processing year:", yr, "... "))
  flush.console()
  
  # 12.1 Define recursive quarterly training set
  X_train_iter <- X[
    X$QuarterlyDate < as.Date(paste0(yr, "-01-01")) & X$exclude == 0,
    c("Country", "QuarterlyDate", "crisis_dummy", vars)
  ]
  
  X_train_iter <- X_train_iter[complete.cases(X_train_iter[, c("crisis_dummy", vars)]), ]
  
  if (sum(X_train_iter$crisis_dummy, na.rm = TRUE) < 5) {
    cat("SKIPPED (insufficient crisis events)\n")
    next
  }
  
  # 12.2 Country-level standardization based on available quarterly history
  stats_iter <- list()
  X_train_std_iter <- X_train_iter
  
  for (v in vars) {
    stats <- X_train_iter %>%
      group_by(Country) %>%
      summarise(
        Mean = mean(.data[[v]], na.rm = TRUE),
        Sd   = sd(.data[[v]], na.rm = TRUE),
        .groups = "drop"
      )
    
    stats_iter[[v]] <- stats
    
    X_train_std_iter <- left_join(X_train_std_iter, stats, by = "Country")
    X_train_std_iter[[paste0(v, "_z")]] <- (X_train_std_iter[[v]] - X_train_std_iter$Mean) / X_train_std_iter$Sd
    X_train_std_iter$Mean <- NULL
    X_train_std_iter$Sd <- NULL
  }
  
  vars_z <- paste0(vars, "_z")
  X_train_std_iter <- X_train_std_iter[complete.cases(X_train_std_iter[, c("crisis_dummy", vars_z)]), ]
  
  # 12.3 Refit quarterly model
  model_oos <- tryCatch({
    glm(
      crisis_dummy ~ CreditGDP_z + CreditGDPgrowth5Y_z + HPIgrowth5Y_z + SharePricesgrowth2Y_z,
      data = X_train_std_iter,
      family = binomial(link = "logit")
    )
  }, error = function(e) return(NULL))
  
  if (is.null(model_oos)) {
    cat("MODEL ERROR\n")
    next
  }
  
  # 12.4 Dynamic threshold from current training sample
  probs_train <- predict(model_oos, type = "response")
  roc_train <- roc(X_train_std_iter$crisis_dummy, probs_train, quiet = TRUE)
  coords_opt <- coords(roc_train, "best", ret = "threshold", transpose = TRUE)
  
  current_threshold <- as.numeric(coords_opt[1])
  
  if (is.infinite(current_threshold) || is.na(current_threshold)) {
    current_threshold <- 0.20
  }
  
  # 12.5 Test-year quarterly prediction
  X_test_quarterly <- X[
    X$Year == yr,
    c("Country", "QuarterlyDate", "Year", "crisis_start", "crisis_dummy", vars)
  ]
  
  for (v in vars) {
    stats_to_use <- stats_iter[[v]]
    
    X_test_quarterly <- left_join(X_test_quarterly, stats_to_use, by = "Country")
    X_test_quarterly[[paste0(v, "_z")]] <- (X_test_quarterly[[v]] - X_test_quarterly$Mean) / X_test_quarterly$Sd
    X_test_quarterly$Mean <- NULL
    X_test_quarterly$Sd <- NULL
  }
  
  X_test_quarterly <- X_test_quarterly[complete.cases(X_test_quarterly[, vars_z]), ]
  
  if (nrow(X_test_quarterly) > 0) {
    X_test_quarterly$FSI_OOS <- predict(model_oos, newdata = X_test_quarterly, type = "response")
    X_test_quarterly$Dynamic_Threshold <- current_threshold
    
    X_oos_results <- bind_rows(
      X_oos_results,
      X_test_quarterly[, c("Country", "QuarterlyDate", "Year", "crisis_start", "crisis_dummy", "FSI_OOS", "Dynamic_Threshold")]
    )
    
    cat("OK (Threshold:", round(current_threshold, 3), ")\n")
  } else {
    cat("SKIPPED\n")
  }
}

cat("--- RECURSIVE OOS ESTIMATION COMPLETED ---\n")

### 13. OOS VISUALIZATION ----

ctry_plot <- "United States"

# 1. Series to plot
df_plot <- subset(
  X_pred,
  Country == ctry_plot &
    QuarterlyDate <= as.Date("2025-06-30")
)

df_plot <- df_plot[order(df_plot$QuarterlyDate), ]

# 2. Crisis-start quarters
df_crisis <- subset(
  X,
  Country == ctry_plot &
    crisis_start == 1 &
    QuarterlyDate <= as.Date("2025-06-30")
)

df_crisis <- df_crisis[order(df_crisis$QuarterlyDate), ]

# 3. Gray crisis windows (one quarter)
crisis_windows <- data.frame(
  xmin = df_crisis$QuarterlyDate,
  xmax = df_crisis$QuarterlyDate + 90
)

# 4. Red pre-crisis windows: from t-12 quarters to t-4 quarters
pre_crisis_windows <- data.frame(
  xmin = df_crisis$QuarterlyDate - 365 * 3,
  xmax = df_crisis$QuarterlyDate - 365
)

if (nrow(pre_crisis_windows) > 0) {
  pre_crisis_windows$xmin <- pmax(
    pre_crisis_windows$xmin,
    min(df_plot$QuarterlyDate, na.rm = TRUE)
  )
}

# 5. Plot
ggplot(df_plot, aes(x = QuarterlyDate, y = FSI_raw_q)) +
  
  # Red pre-crisis windows
  geom_rect(
    data = pre_crisis_windows,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "red",
    alpha = 0.12
  ) +
  
  # Gray crisis quarters
  geom_rect(
    data = crisis_windows,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "gray50",
    alpha = 0.30
  ) +
  
  # FSI line
  geom_line(color = "#005a9e", linewidth = 0.8, na.rm = TRUE) +
  
  # Threshold
  geom_hline(
    yintercept = threshold,
    color = "red",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    name = "Probability of Crisis"
  ) +
  
  labs(
    title = paste("Quarterly FSI with Crisis and Pre-Crisis Windows -", ctry_plot),
    subtitle = "Gray = crisis-start quarters | Red = 1–3 year pre-crisis windows",
    x = "Time"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

### 14. EWMA SMOOTHING OF THE QUARTERLY FSI ----

# This step smooths the raw quarterly FSI series using an exponentially weighted
# moving average over the latest 6 quarters. This is consistent with the paper's
# out-of-sample logic, where recent observations receive the highest weight.

# EWMA weights for 6 observations (oldest -> newest), approximately as in the paper
w6 <- c(0.05, 0.09, 0.14, 0.19, 0.24, 0.29)
w6 <- w6 / sum(w6)

# Helper function: rolling EWMA over the last 6 valid observations
ewma_6 <- function(x, w = w6) {
  n <- length(x)
  out <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    if (i >= 6) {
      window <- x[(i - 5):i]
      if (all(!is.na(window))) {
        out[i] <- sum(window * w)
      }
    }
  }
  
  out
}

# Apply by country
X_pred <- X_pred[order(X_pred$Country, X_pred$QuarterlyDate), ]

X_pred$FSI_ewma6_q <- NA_real_

for (ctry in unique(X_pred$Country)) {
  idx <- which(X_pred$Country == ctry)
  idx <- idx[order(X_pred$QuarterlyDate[idx])]
  
  X_pred$FSI_ewma6_q[idx] <- ewma_6(X_pred$FSI_raw_q[idx])
}

summary(X_pred$FSI_raw_q)
summary(X_pred$FSI_ewma6_q)

sum(!is.na(X_pred$FSI_raw_q))
sum(!is.na(X_pred$FSI_ewma6_q))

### 15. GRAPHIC EWMA OOS ----

ctry_plot <- "United States"

# 1. Plotting dataset
df_plot <- subset(
  X_pred,
  Country == ctry_plot &
    QuarterlyDate <= as.Date("2025-06-30")
)

df_plot <- df_plot[order(df_plot$QuarterlyDate), ]

# 2. Crisis-start quarters for the plotted country
df_crisis <- subset(
  X,
  Country == ctry_plot &
    crisis_start == 1 &
    QuarterlyDate <= as.Date("2025-06-30")
)

df_crisis <- df_crisis[order(df_crisis$QuarterlyDate), ]

# 3. Gray crisis windows: one quarter
crisis_windows <- data.frame(
  xmin = df_crisis$QuarterlyDate,
  xmax = df_crisis$QuarterlyDate + 90
)

# 4. Red pre-crisis windows: from t-12 quarters to t-4 quarters
pre_crisis_windows <- data.frame(
  xmin = df_crisis$QuarterlyDate - 365 * 3,
  xmax = df_crisis$QuarterlyDate - 365
)

if (nrow(pre_crisis_windows) > 0) {
  pre_crisis_windows$xmin <- pmax(
    pre_crisis_windows$xmin,
    min(df_plot$QuarterlyDate, na.rm = TRUE)
  )
}

# 5. Static chart with legend
ggplot(df_plot, aes(x = QuarterlyDate)) +
  
  # Red pre-crisis windows
  geom_rect(
    data = pre_crisis_windows,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "red",
    alpha = 0.10
  ) +
  
  # Gray crisis quarters
  geom_rect(
    data = crisis_windows,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "gray50",
    alpha = 0.28
  ) +
  
  # Raw FSI
  geom_line(
    aes(y = FSI_raw_q, color = "Raw FSI", linetype = "Raw FSI"),
    linewidth = 0.45,
    alpha = 0.55,
    na.rm = TRUE
  ) +
  
  # Smoothed FSI
  geom_line(
    aes(y = FSI_ewma6_q, color = "EWMA(6) FSI", linetype = "EWMA(6) FSI"),
    linewidth = 0.95,
    na.rm = TRUE
  ) +
  
  # Threshold
  geom_hline(
    aes(yintercept = threshold, color = "Threshold", linetype = "Threshold"),
    linewidth = 0.7
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c(
      "Raw FSI" = "#7f7f7f",
      "EWMA(6) FSI" = "#005a9e",
      "Threshold" = "red"
    )
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Raw FSI" = "solid",
      "EWMA(6) FSI" = "solid",
      "Threshold" = "dashed"
    )
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    name = "Probability of Crisis"
  ) +
  
  labs(
    title = paste("Quarterly FSI with EWMA(6), Crisis and Pre-Crisis Windows -", ctry_plot),
    subtitle = "Gray = crisis-start quarters | Red = 1–3 year pre-crisis windows",
    x = "Time"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )