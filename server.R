shinyServer(function(input, output, session) {
    
    #### Customer Scorecard ####
    observeEvent(input$strategy_box_hover, {
        
        strategy_hover <- input$strategy_box_hover
        
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
        
        glue('<h1 style="text-align:center; font-weight:bold;">Customer Churn Risk: {customer_tbl$churn_risk} ({percent(customer_tbl$churn_prob)})</h1>') %>% HTML
    })
    
    output$customer_explanation <- renderBillboarder({
        
        req(input$customer_id)
        
        selected_customer_id <- test_tbl_with_ids$customerID[1]
        selected_customer_id <- input$customer_id
        
        # Run lime() on training set
        explainer <- lime(
            x = x_train_tbl, 
            model = model_keras, 
            bin_continuous = FALSE)
        
        customer_index <- test_tbl_with_ids %>% 
            mutate(rownum = row_number()) %>% 
            filter(customerID == selected_customer_id) %>%
            select(rownum)
        
        # Run explain() on explainer
        set.seed(42)
        explanation <- explain(
            x_test_tbl[customer_index$rownum,], 
            explainer = explainer, 
            n_labels = 1, 
            n_features = length(x_test_tbl),
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
    
    
    #### Churn Analysis ####
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