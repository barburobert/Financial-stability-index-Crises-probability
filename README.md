# Financial Stability Index (FSI) – Quarterly Early-Warning Model
**Author:** Robert-Florian Barbu 


## Main Findings

- **Moderate in-sample discriminatory power:** The pooled logistic regression shows moderate in-sample performance in distinguishing periods of elevated financial vulnerability. Out-of-sample predictive performance has not been established.
- **Key predictors:** Equity-price growth is the strongest predictor. The credit-to-GDP level and credit growth also contribute positively, while house-price growth is weaker in this specification.
- **Early-warning horizon:** The model estimates whether a crisis event will occur within the next **4 to 12 quarters (1–3 years)**, rather than identifying crises only when they begin.
- **Quarterly monitoring:** The fitted model generates a raw quarterly crisis-probability series (`FSI_raw_q`), which is smoothed using **EWMA(6)** (`FSI_ewma6_q`) to produce a more stable signal.
- **Interpretation:** The indicator is more appropriate as a financial vulnerability monitoring tool than as a high-precision crisis prediction system.

## Overview

This project implements a quarterly early-warning **Financial Stability Indicator (FSI)** inspired by the **European Investment Bank (EIB) 2019 Working Paper framework**. The model is estimated as a **pooled logistic regression** on a multi-country quarterly panel and is designed to identify periods of elevated financial vulnerability.

The specification predicts whether a crisis event will occur within the next **4 to 12 quarters**, approximating a **1–3 year early-warning horizon**.

## Objective

The purpose of the project is to:

- Reconstruct a crisis-risk indicator using macro-financial variables.
- Estimate crisis probabilities through logistic regression.
- Evaluate in-sample classification performance.
- Generate a quarterly FSI series for visualization and interpretation.
- Smooth the indicator using an **EWMA(6)** procedure for a more stable signal.

## Data

The model uses quarterly panel data across multiple countries.

### Data Coverage and Country Selection

The original intention was to match the country coverage used in the EIB Working Paper framework as closely as possible. However, due to data-availability constraints across the required variables, the final sample includes only the countries for which a sufficiently consistent quarterly dataset could be constructed.

The final working sample covers **30 countries**:

- Australia
- Austria
- Belgium
- Canada
- Chile
- Colombia
- Czechia
- Denmark
- Finland
- France
- Germany
- Greece
- Hungary
- Ireland
- Israel
- Italy
- Japan
- Korea
- Mexico
- Netherlands
- New Zealand
- Norway
- Poland
- Portugal
- Spain
- Sweden
- Switzerland
- Turkiye
- United Kingdom
- United States

The following economies were not included in the final analytical sample due to insufficient data coverage across the required variables:

- Croatia
- Hong Kong
- Luxembourg
- Singapore

The country selection follows the BIS classification of advanced economies, which provides the conceptual basis for the sample design.

### Main Variables

| Variable | Description |
|---|---|
| `CreditGDP` | Credit-to-GDP ratio |
| `CreditGDPgrowth5Y` | Five-year change in the credit-to-GDP ratio |
| `HPIgrowth5Y` | Five-year log change in residential property prices |
| `SharePricesgrowth2Y` | Two-year log change in equity prices |
| `SharePricesQoQ` | Quarterly equity price return, used to identify equity crash events |

### Variable Construction (Excel)

- `CreditGDP` is used in level form.
- `CreditGDPgrowth5Y` is constructed as a **simple difference** between the current quarter and the observation from **20 quarters earlier**.
- `HPIgrowth5Y` is constructed as the **log difference** between the current quarter and the observation from **20 quarters earlier**, corresponding to a five-year horizon.
- `SharePricesgrowth2Y` is constructed as the **log difference** between the current quarter and the observation from **8 quarters earlier**, corresponding to a two-year horizon.
- `SharePricesQoQ` is constructed as a simple quarter-over-quarter return:

  `P_t / P_{t-1} - 1`

### Data Sources

The dataset combines information from two main public sources:

- **FRED:** equity price series.
- **BIS:** credit-to-GDP and residential property price data (HPI).

Equity price data were collected at monthly frequency and converted to quarterly frequency by retaining the **last available monthly observation within each quarter**.

Panel identifiers:

- `Country`
- `QuarterlyDate`
- `Year`

## Crisis Definition

A **crisis-start** observation is defined as either:

- A banking crisis start year taken from the **Laeven & Valencia / EIB appendix** event list.
- An equity crash quarter, defined as a **quarterly equity price decline of at least 15%**.

These two event types are combined into a single binary variable:

- `crisis_start`

## Target Construction

The dependent variable is defined as:

- `crisis_dummy = 1` if a crisis starts within the **next 4 to 12 quarters**.
- `crisis_dummy = 0` otherwise.

An exclusion rule is also applied:

- **The crisis quarter and the following 8 quarters are excluded from model estimation.**

This setup is intended to capture **pre-crisis vulnerability rather than crisis-period dynamics**.

## Methodology

### Model

The model is estimated as a **pooled logistic regression**:

`crisis_dummy ~ CreditGDP_z + CreditGDPgrowth5Y_z + HPIgrowth5Y_z + SharePricesgrowth2Y_z`

### Standardization

Predictors are standardized **by country** using **training-sample means and standard deviations**. This makes the variables comparable across countries and expresses each signal relative to its own national history.

### Evaluation

The project evaluates the model through:

- In-sample **AUROC**.
- **ROC curve** visualization.
- **Optimal classification threshold**.

### Indicator Construction

The fitted model is applied to the full quarterly panel to generate:

- `FSI_raw_q` — raw quarterly fitted crisis probability.

The raw series is then smoothed using:

- `FSI_ewma6_q` — exponentially weighted moving average over the last **6 quarters**.

## Main Results and Interpretation

In the current specification:

- The model shows **moderate in-sample discriminatory power**.
- **Equity-price growth** is the strongest predictor.
- **Credit level and credit growth** also contribute positively.
- **House-price growth** is weaker in this specification.

The indicator is more appropriate as a **monitoring tool** than as a high-precision crisis prediction system. No specific AUROC value or classification threshold is reported in this README; these can be inspected in the script output.

## Visual Outputs

The project includes charts showing:

- The **raw quarterly FSI** series.
- The **EWMA-smoothed FSI** series.
- **Crisis-start** periods.
- **1–3 year pre-crisis windows**.
- The **ROC curve**.

## Files

- `fsi_indicator.R` — main script.
- `FSI INDICATOR.txt` — input dataset.
- Output charts generated directly from the script.

## How to Run

1. Set the working directory to the project folder.
2. Load the required R packages:
   - `dplyr`
   - `pROC`
   - `ggplot2`
3. Run `fsi_indicator.R` from top to bottom.
4. Inspect:
   - Model estimation results.
   - AUROC and classification threshold.
   - Quarterly FSI plots.
   - EWMA-smoothed plots.

## Limitations

- The model is estimated as a **pooled logit**, not a full panel logit with fixed effects or clustered errors.
- **Banking crisis events** are mapped at the year level, while **equity crashes** are identified at the quarter level.
- In-sample performance is only moderate, so the indicator should not be interpreted as a highly accurate forecasting tool.
- The framework is a **quarterly adaptation of the EIB methodology**, not an exact replication of the original annual specification.
