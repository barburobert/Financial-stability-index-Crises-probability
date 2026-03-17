# Financial Stability Indicator (FSI) – Quarterly Early-Warning Model

## Overview
This project implements a quarterly early-warning Financial Stability Indicator (FSI) inspired by the European Investment Bank 2019 working paper framework. The model is estimated as a pooled logistic regression on a multi-country quarterly panel and is designed to identify periods of elevated financial vulnerability.

The specification predicts whether a crisis event will occur within the next 4 to 12 quarters, which approximates a 1–3 year early-warning horizon.

## Objective
The purpose of the project is to:
- reconstruct a crisis-risk indicator using macro-financial variables
- estimate crisis probabilities through logistic regression
- evaluate in-sample classification performance
- generate a quarterly FSI series for visualization and interpretation
- smooth the indicator using an EWMA(6) procedure for a more stable signal

## Data
The model uses quarterly panel data across multiple countries.
### Data Coverage and Country Selection

The original intention was to match the country coverage used in the EIB Working Paper framework as closely as possible. However, due to data-availability constraints across the required variables, the final sample includes only the countries for which a sufficiently consistent quarterly dataset could be constructed.

The final working sample covers 30 countries:

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
Main variables:
- `CreditGDP`: credit-to-GDP ratio
- `CreditGDPgrowth5Y`: five-year change in the credit-to-GDP ratio
- `HPIgrowth5Y`: five-year log change in residential property prices
- `SharePricesgrowth2Y`: two-year log change in equity prices
- `SharePricesQoQ`: quarterly equity price return, used to identify equity crash events

Panel identifiers:
- `Country`
- `QuarterlyDate`
- `Year`

## Crisis Definition
A crisis-start observation is defined as either:
- a banking crisis start year taken from the Laeven & Valencia / EIB appendix event list
- an equity crash quarter, defined as a quarterly equity price decline of at least 15%

These two event types are combined into a single binary variable:
- `crisis_start`

## Target Construction
The dependent variable is:
- `crisis_dummy = 1` if a crisis starts within the next 4 to 12 quarters
- `crisis_dummy = 0` otherwise

An exclusion rule is also applied:
- the crisis quarter and the following 8 quarters are excluded from model estimation

This setup is intended to capture pre-crisis vulnerability rather than crisis-period dynamics.

## Methodology
### Model
The model is estimated as a logistic regression:

`crisis_dummy ~ CreditGDP_z + CreditGDPgrowth5Y_z + HPIgrowth5Y_z + SharePricesgrowth2Y_z`

### Standardization
Predictors are standardized by country using training-sample means and standard deviations. This makes the variables comparable across countries and expresses each signal relative to its own national history.

### Evaluation
The project evaluates the model through:
- in-sample AUROC
- ROC curve visualization
- optimal classification threshold

### Indicator Construction
The fitted model is applied to the full quarterly panel to generate:
- `FSI_raw_q`: raw quarterly fitted crisis probability

The raw series is then smoothed using:
- `FSI_ewma6_q`: exponentially weighted moving average over the last 6 quarters

## Main Results
In the current specification:
- the model shows moderate in-sample discriminatory power
- equity-price growth is the strongest predictor
- credit level and credit growth also contribute positively
- house-price growth is weaker in this specification

The indicator is more appropriate as a monitoring tool than as a high-precision crisis prediction system.

## Visual Outputs
The project includes charts showing:
- the raw quarterly FSI series
- the EWMA-smoothed FSI series
- crisis-start periods
- 1–3 year pre-crisis windows
- ROC curve

## Files
- `fsi_indicator.R` — main script
- `FSI INDICATOR.txt` — input dataset
- output charts generated directly from the script

## How to Run
1. Set the working directory to the project folder
2. Load the required packages:
   - `dplyr`
   - `pROC`
   - `ggplot2`
3. Run the script from top to bottom
4. Inspect:
   - model estimation results
   - AUROC and threshold
   - quarterly FSI plots
   - EWMA-smoothed plots

## Limitations
- The model is estimated as a pooled logit, not a full panel logit with fixed effects or clustered errors.
- Banking crisis events are mapped at the year level, while equity crashes are identified at the quarter level.
- In-sample performance is only moderate, so the indicator should not be interpreted as a highly accurate forecasting tool.
- The framework is a quarterly adaptation of the EIB methodology, not an exact replication of the original annual specification.

## Author
Robert-Florian Barbu
