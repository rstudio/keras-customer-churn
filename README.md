# keras-customer-churn

This repo contains:

1. Shiny Application - Mitigate Customer Churn

2. RMD and supporting files for the [Customer Churn Article](http://www.business-science.io/business/2017/11/28/customer_churn_analysis_keras.html) from the Business Science blog

# How to use the shiny app

The shiny app has two tabs:

1. Customer Scorecard

2. Churn Analysis

## Customer Scorecard

The customer scorecard anlyzes a single customer at a time. The `keras` model is used to return the probability of customer churn. The app then recommends three strategies to mitigate churn risk:

1. Main Strategy - Incorporates tenure, contract type, key services, monthly charges to recommend offerings that reduce churn risk
2. Commercial Strategy - Incorporates specific services that the customer may be interested in
3. Financial Strategy - Incoporates payment method recommendations to reduce churn

## Churn Analysis

The churn analysis aggregates the data to expose general trends. It has two tabs:

1. Customer and revenue - The customer and revenue tab facets churn by various features including type of contract, revenue, tenure and internet service. Drop-box filters are available to subset the data and drill into important customer segments. 

2. Correlation analysis - The correlation analysis shows the features that correlate to churn, which is important for a global perspective of understanding what affects churn.

