library(keras)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)
library(readr)
library(ggplot2)
library(forcats)

churn_data_raw <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Remove unnecessary data
churn_data_tbl <- churn_data_raw %>%
    drop_na() %>%
    select(Churn, everything())

# Split test/training sets
set.seed(100)
train_test_split <- initial_split(churn_data_tbl, prop = 0.8)
train_test_split

# Retrieve train and test sets
train_tbl_with_ids <- training(train_test_split)
test_tbl_with_ids  <- testing(train_test_split)

train_tbl <- select(train_tbl_with_ids, -customerID)
test_tbl <- select(test_tbl_with_ids, -customerID)

# Determine if log transformation improves correlation 
# between TotalCharges and Churn
train_tbl %>%
    select(Churn, TotalCharges) %>%
    mutate(
        Churn = Churn %>% as.factor() %>% as.numeric(),
        LogTotalCharges = log(TotalCharges)
    ) %>%
    correlate() %>%
    focus(Churn) %>%
    fashion()

# Create recipe
rec_obj <- recipe(Churn ~ ., data = train_tbl) %>%
    step_discretize(tenure, options = list(cuts = 6)) %>%
    step_log(TotalCharges) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) %>%
    prep(data = train_tbl)

x_train_tbl <- bake(rec_obj, new_data = train_tbl) %>% select(-Churn)
x_test_tbl  <- bake(rec_obj, new_data = test_tbl) %>% select(-Churn)

y_train_vec <- ifelse(pull(train_tbl, Churn) == 'Yes', 1, 0)
y_test_vec  <- ifelse(pull(test_tbl, Churn) == 'Yes', 1, 0)

# Building our Artificial Neural Network
model_keras <- keras_model_sequential()

model_keras %>% 
    # First hidden layer
    layer_dense(
        units              = 16, 
        kernel_initializer = "uniform", 
        activation         = "relu", 
        input_shape        = ncol(x_train_tbl)) %>% 
    # Dropout to prevent overfitting
    layer_dropout(rate = 0.1) %>%
    # Second hidden layer
    layer_dense(
        units              = 16, 
        kernel_initializer = "uniform", 
        activation         = "relu") %>% 
    # Dropout to prevent overfitting
    layer_dropout(rate = 0.1) %>%
    # Output layer
    layer_dense(
        units              = 1, 
        kernel_initializer = "uniform", 
        activation         = "sigmoid") %>% 
    # Compile ANN
    compile(
        optimizer = 'adam',
        loss      = 'binary_crossentropy',
        metrics   = 'accuracy'
    )

# Fit the keras model to the training data
history <- fit(
    object           = model_keras, 
    x                = as.matrix(x_train_tbl), 
    y                = y_train_vec,
    batch_size       = 50, 
    epochs           = 35,
    validation_split = 0.30,
    verbose = 0
)

# save the model
save_model_hdf5(model_keras, 'model/customer_churn.hdf5')

plot(history) +
    theme_tq() +
    scale_color_tq() +
    scale_fill_tq() +
    labs(title = "Deep Learning Training Results")

# Predicted Class
yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test_tbl)) %>%
    as.vector()

# Predicted Class Probability
yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test_tbl)) %>%
    as.vector()

# Format test data and predictions for yardstick metrics
estimates_keras_tbl <- tibble(
    truth      = as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0"),
    estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(yes = "1", no = "0"),
    class_prob = yhat_keras_prob_vec
)

estimates_keras_tbl

estimates_keras_tbl %>% conf_mat(truth, estimate)

estimates_keras_tbl %>% metrics(truth, estimate)

estimates_keras_tbl %>% roc_auc(truth, class_prob)

options(yardstick.event_first = FALSE)
# Precision
tibble(
    precision = estimates_keras_tbl %>% precision(truth, estimate),
    recall    = estimates_keras_tbl %>% recall(truth, estimate)
)

# F1-Statistic
estimates_keras_tbl %>% f_meas(truth, estimate, beta = 1)

# Setup lime::model_type() function for keras
model_type.keras.engine.sequential.Sequential <- function(x, ...) {
    "classification"
}

# Setup lime::predict_model() function for keras
predict_model.keras.engine.sequential.Sequential <- function(x, newdata, type, ...) {
    pred <- predict_proba(object = x, x = as.matrix(newdata))
    data.frame(Yes = pred, No = 1 - pred)
}

# Test our predict_model() function
predictions <- predict_model(x = model_keras, newdata = x_test_tbl, type = 'raw') %>%
    tibble::as_tibble()

test_tbl_with_ids$churn_prob <- predictions$Yes

# Run lime() on training set
explainer <- lime::lime(
    x              = x_train_tbl, 
    model          = model_keras, 
    bin_continuous = FALSE)

# Run explain() on explainer
explanation <- lime::explain(
    x_test_tbl[1,], 
    explainer    = explainer, 
    n_labels     = 1, 
    n_features   = 4,
    kernel_width = 0.5)

plot_features(explanation) +
    labs(title = "LIME Feature Importance Visualization",
         subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

plot_explanations(explanation) +
    labs(title = "LIME Feature Importance Heatmap",
         subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

save(list = ls(), file = 'data/customer_churn.RData')
