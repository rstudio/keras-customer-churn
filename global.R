library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)
library(glue)
library(scales)
library(rsample)
library(tidyquant)
library(billboarder)
library(shinyjs)
library(shinyBS)
library(lime)
library(recipes)
library(keras)
library(corrr)
library(yardstick)

setwd('~/Documents/midnightBarber/bizsci/keras-customer-churn/')

rm(list = ls())
# Setup lime::model_type() function for keras
model_type.keras.models.Sequential <- function(x, ...) {
    return("classification")
}

# Setup lime::predict_model() function for keras
predict_model.keras.models.Sequential <- function(x, newdata, type, ...) {
    pred <- predict_proba(object = x, x = as.matrix(newdata))
    return(data.frame(Yes = pred, No = 1 - pred))
}

keras_activations <- c('softmax', 'elu', 'selu', 'softplus', 'softsign', 'relu', 'tanh', 'sigmoid', 'hard_sigmoid', 'linear')
keras_initializers <- c('zeros', 'ones', 'constant', 'random_normal', 'random_uniform', 'truncated_normal', 'uniform', 'orthogonal', 'identity', 'lecun_normal', 'glorot_normal', 'glorot_uniform', 'he_normal', 'he_uniform', 'lecun_uniform')
keras_optimizers <- c('sgd', 'rmsprop', 'adagrad', 'adadelta', 'adam', 'adamax', 'nadam')
keras_losses <- c('mean_squared_error', 'mean_absolute_error', 'mean_absolute_percentage_error', 'mean_squared_logarithmic_error', 'categorical_hinge', 'logcosh', 'categorical_crossentropy', 'sparse_categorical_crossentropy', 'binary_crossentropy', 'kullback_leibler_divergence', 'poisson', 'cosine_proximity')

strategy_colors <- c(main = '#f39c12', commercial = '#3498db', financial = '#18bc9c')

main_vars <- c('tenure', 'Contract', 'InternetService', 'MonthlyCharges', 'OnlineBackup', 'OnlineSecurity', 'DeviceProtection', 'TechSupport', 'StreamingMovies', 'PhoneService')
commercial_vars <- c('InternetService', 'OnlineBackup', 'OnlineSecurity', 'DeviceProtection', 'TechSupport', 'StreamingMovies', 'PhoneService')
financial_vars <- c('PaymentMethod')

customer_feature_vars <- c(main_vars, commercial_vars, financial_vars) %>% unique

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

churn_data_tbl <- churn_data_raw %>%
    drop_na() %>%
    select(Churn, everything())

navbarPageWithText <- function(..., text) {
    navbar <- navbarPage(...)
    textEl <- tags$p(class = "navbar-text", text)
    navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
        navbar[[3]][[1]]$children[[1]], textEl)
    navbar
}

########## loading button
withBusyIndicatorUI <- function(button) {
    id <- button[['attribs']][['id']]
    div(
        `data-for-btn` = id,
        button,
        span(
            class = 'btn-loading-container',
            hidden(
                img(src = 'ajax-loader-bar.gif', class = 'btn-loading-indicator'),
                icon('check', class = 'btn-done-indicator')
            )
        ),
        hidden(
            div(class = 'btn-err',
                div(icon('exclamation-circle'),
                    tags$b('Error: '),
                    span(class = 'btn-err-msg')
                )
            )
        )
    )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
    # UX stuff: show the "busy" message, hide the other messages, disable the button
    loadingEl <- sprintf('[data-for-btn=%s] .btn-loading-indicator', buttonId)
    doneEl <- sprintf('[data-for-btn=%s] .btn-done-indicator', buttonId)
    errEl <- sprintf('[data-for-btn=%s] .btn-err', buttonId)
    shinyjs::disable(buttonId)
    shinyjs::show(selector = loadingEl)
    shinyjs::hide(selector = doneEl)
    shinyjs::hide(selector = errEl)
    on.exit({
        shinyjs::enable(buttonId)
        shinyjs::hide(selector = loadingEl)
    })
    
    # Try to run the code when the button is clicked and show an error message if
    # an error occurs or a success message if it completes
    tryCatch({
        value <- expr
        shinyjs::show(selector = doneEl)
        shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = 'fade',
                                           time = 0.5))
        value
    }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
    errEl <- sprintf('[data-for-btn=%s] .btn-err', buttonId)
    errElMsg <- sprintf('[data-for-btn=%s] .btn-err-msg', buttonId)
    errMessage <- gsub('^ddpcr: (.*)', '\\1', err$message)
    shinyjs::html(html = errMessage, selector = errElMsg)
    shinyjs::show(selector = errEl, anim = TRUE, animType = 'fade')
}