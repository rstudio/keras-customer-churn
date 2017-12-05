library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(billboarder)

churn_data_raw <- read_csv("WA_Fn-UseC_-Telco-Customer-Churn.csv") %>% 
    mutate(
        tenure_range = case_when(
            tenure < 12 ~ '< 1 Yr',
            tenure < 24 ~ '1-2 Yrs',
            tenure < 36 ~ '2-3 Yrs',
            tenure >= 36 ~ 'Over 3 Yrs',
            TRUE ~ 'NA'
        ),
        monthly_charge_range = case_when(
            MonthlyCharges < 20 ~ '< 20 per Month',
            MonthlyCharges < 50 ~ '20-50 per Month',
            MonthlyCharges < 100 ~ '50-100 per Month',
            MonthlyCharges >= 100 ~ 'Over 100 per Month',
            TRUE ~ 'NA'
        )
    )