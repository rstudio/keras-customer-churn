shinyServer(function(input, output, session) {
    
    load('keras_model_init.RData')
    model_keras <- load_model_hdf5('model_keras_init.hdf5', custom_objects = NULL, compile = TRUE)
    
    #### Customer Scorecard ####
    observeEvent(input$strategy_box_hover, {
        
        strategy_hover <<- input$strategy_box_hover
        
        if (strategy_hover == 'none') {
            
            row_indices <- 0
            
            output$datatable_rowcolor <- renderUI({
                tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: lightblue !important;}'))
            })
            
        } else {
            
            strategy_features <- get(paste0(strategy_hover, '_vars'))
            row_indices <- match(strategy_features, customer_feature_vars)
            
            output$datatable_rowcolor <- renderUI({
                tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: ', strategy_colors[strategy_hover], '!important;}'))
            })
        
        }
        
        DT::dataTableProxy('customer_info_tbl') %>% 
            DT::selectRows(row_indices)
            
    })
    
    # Update when model is rerun, but initilize with preloaded model on startup
    observeEvent(input$run_keras, {
        
        output$customer_id_selector <- renderUI({
            selectInput('customer_id', 'Customer ID', unique(test_tbl_with_ids$customerID))
        })
        
        output$customer_info_tbl <- DT::renderDataTable({
            
            req(input$customer_id)
            
            selected_customer_id <- test_tbl_with_ids$customerID[1]
            selected_customer_id <- input$customer_id
            
            customer_info <- test_tbl_with_ids %>% 
                filter(customerID == selected_customer_id) %>% 
                mutate(tenure = paste0(tenure, ifelse(tenure == 1, ' Month', ' Months'))) %>% 
                select(customer_feature_vars) %>% 
                gather(metric, value)
            
            DT::datatable(customer_info, rownames = NULL, caption = 'Customer Details', 
                          options = list(
                              dom = 't', 
                              bSort = FALSE, 
                              paging = FALSE,
                              initComplete = DT::JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'display':'none'});",
                                  "}")
                          )
            )
        })
        
        output$churn_risk <- renderText({
            
            req(input$customer_id)
            
            selected_customer_id <- test_tbl_with_ids$customerID[1]
            selected_customer_id <- input$customer_id
            
            # Test our predict_model() function
            predictions <- predict_model(x = model_keras, newdata = x_test_tbl, type = 'raw') %>%
                tibble::as_tibble()
            
            test_tbl_with_ids_predictions <- test_tbl_with_ids %>% 
                mutate(churn_prob = predictions$Yes,
                       churn_risk = case_when(
                           churn_prob >= .66 ~ 'High',
                           churn_prob >= .33 ~ 'Medium',
                           churn_prob >= 0 ~ 'Low',
                           TRUE ~ ''
                       ))
            
            customer_tbl <- test_tbl_with_ids_predictions %>% 
                filter(customerID == selected_customer_id)
            
            glue('<h1 style="text-align:center; font-weight:bold;">Customer Churn Risk: {customer_tbl$churn_risk} ({percent(customer_tbl$churn_prob)}) </h1>') %>% HTML
        })
        
        output$customer_explanation <- renderBillboarder({
            
            req(input$customer_id)
            
            selected_customer_id <- test_tbl_with_ids$customerID[1]
            selected_customer_id <- input$customer_id
            
            # Run lime() on training set
            explainer <- lime(
                x              = x_train_tbl, 
                model        = model_keras, 
                bin_continuous = FALSE)
            
            customer_index <- test_tbl_with_ids %>% 
                mutate(rownum = row_number()) %>% 
                filter(customerID == selected_customer_id) %>%
                select(rownum)
            
            # Run explain() on explainer
            set.seed(42)
            explanation <- explain(
                x_test_tbl[customer_index$rownum, ], 
                explainer    = explainer, 
                n_labels     = 1, 
                n_features   = length(x_test_tbl),
                kernel_width = 0.5)
            
            type_pal <- c("Supports", "Contradicts")
            explanation$type <- factor(ifelse(sign(explanation$feature_weight) == 
                                                  1, type_pal[1], type_pal[2]), levels = type_pal)
            description <- paste0(explanation$case, "_", explanation$label)
            desc_width <- max(nchar(description)) + 1
            description <- paste0(format(description, width = desc_width), 
                                  explanation$feature_desc)
            explanation$description <- factor(description, levels = description[order(abs(explanation$feature_weight))])
            explanation$case <- factor(explanation$case, unique(explanation$case))
            
            explanation_plot_df <- explanation %>%
                mutate(churn_predictor = case_when(
                    (label == 'Yes' & type == 'Supports') | (label == 'No' & type == 'Contradicts') ~ 'More likely to churn',
                    (label == 'Yes' & type == 'Contradicts') | (label == 'No' & type == 'Supports') ~ 'Less likely to churn'
                )) %>%
                arrange(-abs(feature_weight)) %>% 
                head(20)
            
            billboarder() %>%
                bb_barchart(
                    data = explanation_plot_df,
                    mapping = bbaes(x = feature_desc, y = feature_weight, group = churn_predictor),
                    rotated = TRUE,
                    stacked = TRUE
                ) %>%
                bb_colors_manual('Less likely to churn' = 'green', 'More likely to churn' = 'red') %>%
                bb_title(text = glue('Feature contributions to churn, calculated with LIME'))
            
        })
        
        output$main_strategy <- renderText({
            
            req(input$customer_id)
            
            selected_customer_id <- test_tbl_with_ids$customerID[1]
            selected_customer_id <- input$customer_id
            
            customer_tbl <- test_tbl_with_ids %>% 
                filter(customerID == selected_customer_id)
            
            if (customer_tbl$tenure <= 9) {
                main_strategy <- 'Retain until one year'
            } else if (customer_tbl$tenure > 9 | customer_tbl$Contract == 'Month-to-month') {
                main_strategy <- 'Upsell to annual contract'
            } else if (customer_tbl$tenure > 12 & customer_tbl$InternetService == 'No') {
                main_strategy <- 'Offer internet service'
            } else if (customer_tbl$tenure > 18 & customer_tbl$MonthlyCharges > 50) {
                main_strategy <- 'Offer discount in monthly rate'
            } else if (customer_tbl$tenure > 12 & customer_tbl$Contract != 'Month-to-month' & ((customer_tbl$OnlineBackup == 'No' & customer_tbl$OnlineSecurity == 'No' & customer_tbl$DeviceProtection == 'No' & customer_tbl$TechSupport == 'No' & customer_tbl$StreamingMovies == 'No') | customer_tbl$PhoneService == 'No')) {
                main_strategy <- 'Offer additional services'
            } else {
                main_strategy <- 'Retain and maintain'
            }
            paste0('<h2 style = "font-weight:bold;" id = "mainer">', main_strategy, '</h2>') %>% HTML
            
        })
        
        output$commercial_strategy <- renderText({
            
            req(input$customer_id)
            
            selected_customer_id <- test_tbl_with_ids$customerID[1]
            selected_customer_id <- input$customer_id
            
            customer_tbl <- test_tbl_with_ids %>% 
                filter(customerID == selected_customer_id)
            
            if ((customer_tbl$InternetService == 'DSL' & customer_tbl$OnlineBackup == 'No' & customer_tbl$OnlineSecurity == 'No' & customer_tbl$DeviceProtection == 'No' & customer_tbl$TechSupport == 'No' & customer_tbl$StreamingMovies == 'No') | customer_tbl$PhoneService == 'No') {
                commercial_strategy <- 'Offer additional services'
            } else if (customer_tbl$InternetService == 'Fiber optic') {
                commercial_strategy <- 'Offer tech support and services'
            } else if (customer_tbl$InternetService == 'No') {
                commercial_strategy <- 'Upsell to internet service'
            } else {
                commercial_strategy <- 'Retain and maintain'
            }
            paste0('<h2 style = "font-weight:bold;">', commercial_strategy, '</h2>') %>% HTML
        })
        
        output$financial_strategy <- renderText({
            
            req(input$customer_id)
            
            selected_customer_id <- test_tbl_with_ids$customerID[1]
            selected_customer_id <- input$customer_id
            
            customer_tbl <- test_tbl_with_ids %>% 
                filter(customerID == selected_customer_id)
            
            if (customer_tbl$PaymentMethod %in% c('Mailed Check', 'Electronic Check')) {
                financial_strategy <- 'Move to credit card or bank transfer'
            } else {
                financial_strategy <- 'Retain and maintain'
            }
            paste0('<h2 style = "font-weight:bold;">', financial_strategy, '</h2>') %>% HTML
            
        })
        
        output$model_results_table <- renderTable({
            
            # Predicted Class
            yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test_tbl)) %>%
                as.vector()
            
            # Predicted Class Probability
            yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test_tbl)) %>%
                as.vector()
            
            # Format test data and predictions for yardstick metrics
            estimates_keras_tbl <- tibble(
                truth      = as.factor(y_test_vec) %>% fct_recode(yes = '1', no = '0'),
                estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(yes = '1', no = '0'),
                class_prob = yhat_keras_prob_vec
            )
            
            tibble(
                Accuracy = estimates_keras_tbl %>% metrics(truth, estimate) %>% .[[1]],
                AUC = estimates_keras_tbl %>% roc_auc(truth, class_prob) %>% .[[1]],
                Precision = estimates_keras_tbl %>% precision(truth, estimate) %>% .[[1]],
                Recall = estimates_keras_tbl %>% recall(truth, estimate) %>% .[[1]],
                `F1 Statistic` = estimates_keras_tbl %>% f_meas(truth, estimate, beta = 1) %>% .[[1]]
            )
            
        }, caption = 'Model Results', caption.placement = getOption('xtable.caption.placement', 'top'))
        
    }, ignoreNULL = F)
    
    #### Keras Model ####
    observe({
        
        if (nrow(churn_analysis_data()) > 0) {
            plot_df <- churn_analysis_data() %>%
                group_by(Churn) %>% 
                summarise(monthly_revenue = sum(MonthlyCharges)) %>% 
                ungroup %>% 
                mutate(pct = round(monthly_revenue / sum(monthly_revenue), 2)) %>% 
                select(-monthly_revenue) %>% 
                mutate(x = 'Churn') %>% 
                spread(Churn, pct)
        } else {
            plot_df <- data.frame(x = '', Yes = '', No = '')
        }
        
        billboarderProxy('pct_monthly_revenue') %>% 
            bb_barchart(
                data = plot_df,
                stacked = TRUE,
                rotated = TRUE
            ) %>% 
            bb_colors_manual('Yes' = '#2c3e50', 'No' = '#18BC9C') %>% 
            bb_title(text = '% Monthly Revenue')
        
        if (nrow(churn_analysis_data()) > 0) {
            plot_df <- churn_analysis_data() %>% 
                group_by(Churn, Contract) %>% 
                summarise(monthly_revenue = sum(MonthlyCharges))
        } else {
            plot_df <- data.frame(x = '', y = '', Churn = '')
        }
        
        billboarderProxy('monthly_revenue') %>% 
            bb_barchart(
                data = plot_df,
                mapping = bbaes(x = Contract, y = monthly_revenue, group = Churn),
                stacked = TRUE,
                rotated = TRUE
            ) %>% 
            bb_colors_manual('Yes' = '#2c3e50', 'No' = '#18BC9C') %>% 
            bb_title(text = 'Monthly Revenue by Type of Contract')
        
    })
    
    # Initialize keras plots with preoloaded model
    observe({
        plot_df <- fit_keras$metrics %>% 
            as.data.frame() %>% 
            gather(metric, value) %>% 
            mutate(set = ifelse(grepl('val_', metric), 'validation', 'training'),
                   metric = gsub('val_', '', metric)) %>% 
            group_by(set, metric) %>% 
            mutate(epoch = row_number())
        
        output$keras_acc_plot <- renderBillboarder({
            billboarder() %>% 
                bb_linechart(
                    data = filter(plot_df, metric == 'acc'),
                    mapping = bbaes(x = epoch, y = round(value, 4), group = set)
                ) %>% 
                bb_colors_manual('training' = '#2c3e50', 'validation' = '#18BC9C') %>% 
                bb_y_axis(max = 1) %>% 
                bb_title(text = 'Accuracy')
        })
        
        output$keras_loss_plot <- renderBillboarder({
            billboarder() %>% 
                bb_linechart(
                    data = filter(plot_df, metric == 'loss'),
                    mapping = bbaes(x = epoch, y = round(value, 4), group = set)
                ) %>% 
                bb_colors_manual('training' = '#2c3e50', 'validation' = '#18BC9C') %>% 
                bb_title(text = 'Loss')
        })
    })
    
    observeEvent(input$run_keras, {
        hide('keras_acc_plot')
        hide('keras_loss_plot')
        hide('model_results_table')
        
        withBusyIndicatorServer('run_keras', {
            
            train_test_split <- initial_split(churn_data_tbl, prop = input$train_prop)
            
            # save off version of split tables with customer ids for customer analysis
            train_tbl_with_ids <- training(train_test_split)
            test_tbl_with_ids  <- testing(train_test_split)
            
            train_tbl <- select(train_tbl_with_ids, -customerID)
            test_tbl <- select(test_tbl_with_ids, -customerID)
            
            rec_obj <- recipe(Churn ~ ., data = train_tbl) %>%
                step_discretize(tenure, options = list(cuts = 6)) %>%
                step_log(TotalCharges) %>%
                step_dummy(all_nominal(), -all_outcomes()) %>%
                step_center(all_predictors(), -all_outcomes()) %>%
                step_scale(all_predictors(), -all_outcomes()) %>%
                prep(data = train_tbl)
            
            x_train_tbl <<- bake(rec_obj, newdata = train_tbl) %>% select(-Churn)
            x_test_tbl  <<- bake(rec_obj, newdata = test_tbl) %>% select(-Churn)
            
            y_train_vec <<- ifelse(pull(train_tbl, Churn) == 'Yes', 1, 0)
            y_test_vec  <<- ifelse(pull(test_tbl, Churn) == 'Yes', 1, 0)
            
            # Building our Artificial Neural Network
            model_keras <<- keras_model_sequential()
            
            model_keras %>%
                # First hidden layer
                layer_dense(
                    units              = 16,
                    kernel_initializer = input$first_hidden_layer_kernel_initializer,
                    activation         = input$first_hidden_layer_activation,
                    input_shape        = ncol(x_train_tbl)) %>%
                # Dropout to prevent overfitting
                layer_dropout(rate = input$first_hidden_layer_dropout) %>%
                # Second hidden layer
                layer_dense(
                    units              = 16,
                    kernel_initializer = input$second_hidden_layer_kernel_initializer,
                    activation         = input$second_hidden_layer_activation) %>%
                # Dropout to prevent overfitting
                layer_dropout(rate = input$second_hidden_layer_dropout) %>%
                # Output layer
                layer_dense(
                    units              = 1,
                    kernel_initializer = input$output_layer_kernel_initializer,
                    activation         = input$output_layer_activation) %>%
                # Compile ANN
                compile(
                    optimizer = input$optimizer,
                    loss      = input$loss,
                    metrics   = c('accuracy')
                )
            
            fit_keras <<- fit(
                object           = model_keras,
                x                = as.matrix(x_train_tbl),
                y                = y_train_vec,
                batch_size       = 50,
                epochs           = input$epochs,
                validation_split = 0.30,
                verbose = 0
            )
            
            plot_df <- fit_keras$metrics %>% 
                as.data.frame() %>% 
                gather(metric, value) %>% 
                mutate(set = ifelse(grepl('val_', metric), 'validation', 'training'),
                       metric = gsub('val_', '', metric)) %>% 
                group_by(set, metric) %>% 
                mutate(epoch = row_number())
            
            # Setup lime::model_type() function for keras
            model_type.keras.models.Sequential <- function(x, ...) {
                return('classification')
            }
            
            # Setup lime::predict_model() function for keras
            predict_model.keras.models.Sequential <- function(x, newdata, type, ...) {
                pred <- predict_proba(object = x, x = as.matrix(newdata))
                return(data.frame(Yes = pred, No = 1 - pred))
            }
            
            show('keras_acc_plot')
            show('keras_loss_plot')
            
            for (plot_epoch in 2:input$epochs) {
                billboarderProxy('keras_acc_plot') %>%
                    bb_linechart(
                        data = filter(plot_df, metric == 'acc', epoch <= plot_epoch),
                        mapping = bbaes(x = epoch, y = round(value, 4), group = set)
                    ) 
                billboarderProxy('keras_loss_plot') %>%
                    bb_linechart(
                        data = filter(plot_df, metric == 'loss', epoch <= plot_epoch),
                        mapping = bbaes(x = epoch, y = round(value, 4), group = set)
                    )
                Sys.sleep(.05)
            }
            show('model_results_table')
        })
    })
    
    #### Churn Analysis ####
    churn_analysis_data <- reactive({
        
        churn_data_filtered <- churn_data_raw
        
        if (input$payment_methods != 'All') {
            churn_data_filtered <- filter(churn_data_filtered, PaymentMethod == input$payment_methods)
        }
        
        if (input$tech_support != 'All') {
            churn_data_filtered <- filter(churn_data_filtered, TechSupport == input$tech_support)
        }
        
        if (input$monthly_charge_range != 'All') {
            churn_data_filtered <- filter(churn_data_filtered, monthly_charge_range == input$monthly_charge_range)
        }
        
        if (input$tenure_range != 'All') {
            churn_data_filtered <- filter(churn_data_filtered, tenure_range == input$tenure_range)
        }
        
        return(churn_data_filtered)
    })
    
    output$monthly_revenue <- renderBillboarder({
        
        plot_df <- churn_analysis_data() %>% 
            group_by(Churn, Contract) %>% 
            summarise(monthly_revenue = sum(MonthlyCharges))
        
        billboarder() %>% 
            bb_barchart(
                data = plot_df,
                mapping = bbaes(x = Contract, y = monthly_revenue, group = Churn),
                stacked = TRUE,
                rotated = TRUE
            ) %>% 
            bb_colors_manual('Yes' = '#2c3e50', 'No' = '#18BC9C') %>% 
            bb_title(text = 'Monthly Revenue by Type of Contract')
    })
    
    output$number_of_customers <- renderBillboarder({
        plot_df <- churn_analysis_data() %>% 
            group_by(Churn, Contract) %>% 
            summarise(number_of_customers = n())
        
        billboarder() %>% 
            bb_barchart(
                data = plot_df,
                mapping = bbaes(x = Contract, y = number_of_customers, group = Churn),
                stacked = TRUE,
                rotated = TRUE
            ) %>% 
            bb_colors_manual('Yes' = '#2c3e50', 'No' = '#18BC9C') %>% 
            bb_title(text = 'Number of Customers by Type of Contract')
    })
    
    output$pct_monthly_revenue <- renderBillboarder({
        
        plot_df <- isolate(churn_analysis_data()) %>% 
            group_by(Churn) %>% 
            summarise(monthly_revenue = sum(MonthlyCharges)) %>% 
            ungroup %>% 
            mutate(pct = round(monthly_revenue / sum(monthly_revenue), 2)) %>% 
            select(-monthly_revenue) %>% 
            mutate(x = 'Churn') %>% 
            spread(Churn, pct)
        
        billboarder() %>% 
            bb_barchart(
                data = plot_df,
                stacked = TRUE,
                rotated = TRUE
            ) %>% 
            bb_colors_manual('Yes' = '#2c3e50', 'No' = '#18BC9C') %>% 
            bb_title(text = '% Monthly Revenue')
    })
    
    output$pct_customers <- renderBillboarder({
        
        plot_df <- churn_analysis_data() %>% 
            group_by(Churn) %>% 
            summarise(num_customers = n()) %>% 
            ungroup %>% 
            mutate(pct = round(num_customers / sum(num_customers), 2)) %>% 
            select(-num_customers) %>% 
            mutate(x = 'Churn') %>% 
            spread(Churn, pct)
        
        billboarder() %>% 
            bb_barchart(
                data = plot_df,
                stacked = TRUE,
                rotated = TRUE
            ) %>% 
            bb_colors_manual('Yes' = '#2c3e50', 'No' = '#18BC9C') %>% 
            bb_title(text = '% Customers')
    })
    
    output$churn_rate_tenure <- renderBillboarder({
        
        plot_df <- churn_analysis_data() %>% 
            count(tenure_range, Churn) %>% 
            group_by(tenure_range) %>% 
            mutate(pct = round(n / sum(n), 2)) %>% 
            ungroup
        
        plot <- billboarder() %>% 
            bb_y_grid(
                lines = list(
                    list(value = mean(churn_analysis_data()$Churn == 'Yes'), text = "Average Churn Rate")
                )
            ) %>% 
            bb_colors_manual('Yes' = '#2c3e50', 'No' = '#18BC9C') %>% 
            bb_title(text = 'Churn Rate by Tenure Range') 
        
        if (nrow(plot_df) == 2) {
            plot_df <- plot_df %>% 
                select(-n) %>% 
                spread(Churn, pct)
            
            plot <- plot %>% 
                bb_barchart(
                    data = plot_df,
                    stacked = TRUE,
                    rotated = TRUE
                )
        } else {
            plot <- plot %>% 
                bb_barchart(
                    data = plot_df,
                    mapping = bbaes(x = tenure_range, y = pct, group = Churn),
                    stacked = TRUE,
                    rotated = TRUE
                )
        }
        
        return(plot)
        
        
    })
    
    output$churn_rate_internet_service <- renderBillboarder({
        plot_df <- churn_analysis_data() %>% 
            count(InternetService, Churn) %>% 
            group_by(InternetService) %>% 
            mutate(pct = round(n / sum(n), 2))
        
        billboarder() %>% 
            bb_barchart(
                data = plot_df,
                mapping = bbaes(x = InternetService, y = pct, group = Churn),
                stacked = TRUE,
                rotated = TRUE
            ) %>% 
            bb_y_grid(
                lines = list(
                    list(value = mean(churn_analysis_data()$Churn == 'Yes'), text = "Average Churn Rate")
                )
            ) %>% 
            bb_y_axis(list(max = 1)) %>% 
            bb_colors_manual('Yes' = '#2c3e50', 'No' = '#18BC9C') %>% 
            bb_title(text = 'Churn Rate by Internet Service')
    })
    
    output$corr_analysis <- renderPlot({
        train_test_split <- initial_split(select(churn_data_tbl, -customerID), prop = input$train_prop)
        train_tbl <- training(train_test_split)
        test_tbl  <- testing(train_test_split)
        
        rec_obj <- recipe(Churn ~ ., data = train_tbl) %>%
            step_discretize(tenure, options = list(cuts = 6)) %>%
            step_log(TotalCharges) %>%
            step_dummy(all_nominal(), -all_outcomes()) %>%
            step_center(all_predictors(), -all_outcomes()) %>%
            step_scale(all_predictors(), -all_outcomes()) %>%
            prep(data = train_tbl)
        
        x_train_tbl <- bake(rec_obj, newdata = train_tbl) %>% select(-Churn)
        x_test_tbl  <- bake(rec_obj, newdata = test_tbl) %>% select(-Churn)
        
        y_train_vec <- ifelse(pull(train_tbl, Churn) == 'Yes', 1, 0)
        y_test_vec  <- ifelse(pull(test_tbl, Churn) == 'Yes', 1, 0)
        
        # Feature correlations to Churn
        corrr_analysis <- x_train_tbl %>%
            mutate(Churn = y_train_vec) %>%
            correlate() %>%
            focus(Churn) %>%
            rename(feature = rowname) %>%
            arrange(abs(Churn)) %>%
            mutate(feature = as_factor(feature))
        
        # Correlation visualization
        corrr_analysis %>%
            ggplot(aes(x = Churn, y = fct_reorder(feature, desc(Churn)))) +
            geom_point() +
            # Positive Correlations - Contribute to churn
            geom_segment(aes(xend = 0, yend = feature),
                         color = palette_light()[[2]],
                         data = corrr_analysis %>% filter(Churn > 0)) +
            geom_point(color = palette_light()[[2]],
                       data = corrr_analysis %>% filter(Churn > 0)) +
            # Negative Correlations - Prevent churn
            geom_segment(aes(xend = 0, yend = feature),
                         color = palette_light()[[1]],
                         data = corrr_analysis %>% filter(Churn < 0)) +
            geom_point(color = palette_light()[[1]],
                       data = corrr_analysis %>% filter(Churn < 0)) +
            # Vertical lines
            geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
            geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
            geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
            # Aesthetics
            theme_tq() +
            labs(title = "Churn Correlation Analysis",
                 subtitle = "Positive Correlations (contribute to churn), Negative Correlations (prevent churn)",
                 y = "Feature Importance")
    })
})