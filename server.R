shinyServer(function(input, output, session) {
    
    ##### customer_scorecard
    
    customer_churn_data <- reactive({
        filter(churn_data_raw, customerID == input$customer_id)
    })
    
    output$tenure_box <- renderValueBox({
        tenure_months <- customer_churn_data()$tenure
        valueBox(tenure_months, 'Tenure (months)')
    })
    
    output$contract_box <- renderValueBox({
        contract_type <- customer_churn_data()$Contract
        valueBox(contract_type, 'Contract')
    })
    
    output$products_and_services <- renderTable({
        customer_churn_data() %>% 
            select(PhoneService, InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, StreamingTV, StreamingMovies)
    })
    
    output$payment_method <- renderValueBox({
        payment_method <- customer_churn_data()$PaymentMethod
        valueBox(payment_method, 'Payment Method')
    })
    
    output$monthly_charge <- renderValueBox({
        monthly_charge <- customer_churn_data()$MonthlyCharges
        valueBox(monthly_charge, 'Monthly Charge')
    })
    
    output$main_strategy <- renderText({
        main_strategies <- c('Retain and Maintain', 'Upsell to 1 Yr Contract', 'Offer Addtional Services', 'Retain till 1 Yr', 'Offer discount in Monthly Rate')
        main_strategy <- sample(main_strategies, 1)
        paste0('<h2 style = "font-weight:bold;">', main_strategy, '</h2>') %>% HTML
    })
    
    output$commercial_strategy <- renderText({
        commercial_strategies <- c('Offer Support and Servies', 'Retain and Maintain', 'Upsell to Internet Service', 'Offer Tech Support')
        commercial_strategy <- sample(commercial_strategies, 1)
        paste0('<h2 style = "font-weight:bold;">', commercial_strategy, '</h2>') %>% HTML
    })
    
    output$financial_strategy <- renderText({
        financial_strategies <- c('Retain and Maintain', 'Move to Credit Card or Bank Transfer payment')
        financial_strategy <- sample(financial_strategies, 1)
        paste0('<h2 style = "font-weight:bold;">', financial_strategy, '</h2>') %>% HTML
    })
    
    output$churn_risk <- renderText({
        churn_risks <- c('Low', 'Medium', 'High')
        churn_risk <- sample(churn_risks, 1)
        paste0('<h1 style="text-align:center; font-weight:bold;">Customer Churn Risk: ', churn_risk, '</h1>') %>% HTML
    })
    
    ##### churn analysis
    
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
            bb_title(text = 'Number of Customers by Type of Contract')
    })
    
    output$pct_monthly_revenue <- renderBillboarder({
        plot_df <- churn_analysis_data() %>% 
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
            bb_title(text = '% Customers')
    })
    
    output$churn_rate_tenure <- renderBillboarder({
        plot_df <- churn_analysis_data() %>% 
            count(tenure_range, Churn) %>% 
            group_by(tenure_range) %>% 
            mutate(pct = round(n / sum(n), 2))
        
        billboarder() %>% 
            bb_barchart(
                data = plot_df,
                mapping = bbaes(x = tenure_range, y = pct, group = Churn),
                stacked = TRUE,
                rotated = TRUE
            ) %>% 
            bb_title(text = 'Churn Rate by Tenure Range')
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
            bb_y_axis(list(max = 1)) %>% 
            bb_title(text = 'Churn Rate by Internet Service')
    })
})