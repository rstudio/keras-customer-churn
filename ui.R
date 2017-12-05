dashboardPage(
    dashboardHeader(
        title = 'Customer Analytics'
    ),
    dashboardSidebar(
        sidebarMenu(id = 'menu',
                    menuItem('Customer Scorecard', tabName = 'customer_scorecard', icon = icon('list-alt')),
                    menuItem('Churn Analysis', tabName = 'general_overview', icon = icon('bar-chart'))
        ),
        conditionalPanel("input.menu=='customer_scorecard'", selectInput('customer_id', 'Customer ID', unique(churn_data_raw$customerID))),
        conditionalPanel("input.menu=='general_overview'", selectInput('payment_methods', 'Payment Method', c('All', unique(churn_data_raw$PaymentMethod)))),
        conditionalPanel("input.menu=='general_overview'", selectInput('tech_support', 'Tech Support', c('All', unique(churn_data_raw$TechSupport)))),
        conditionalPanel("input.menu=='general_overview'", selectInput('monthly_charge_range', 'Monthly Charge Range', c('All', unique(churn_data_raw$monthly_charge_range)))),
        conditionalPanel("input.menu=='general_overview'", selectInput('tenure_range', 'Tenure Range', c('All', unique(churn_data_raw$tenure_range))))
    ),
    dashboardBody(
        tabItems(
            tabItem('customer_scorecard',
                    htmlOutput('churn_risk'),
                    box(title = 'Customer Tenure',
                        valueBoxOutput('tenure_box', width = 6),
                        valueBoxOutput('contract_box', width = 6)
                    ),
                    box(title = 'Customer financials', 
                        valueBoxOutput('payment_method', width = 6),
                        valueBoxOutput('monthly_charge', width = 6)
                    ),
                    column(12, offset = 2,
                           box(title = 'Products and Services',
                               tableOutput('products_and_services'),
                               width = 8
                           )
                    ),
                    box(title = 'Main Strategy',
                        htmlOutput('main_strategy'),
                        width = 4),
                    box(title = 'Commerical Strategy',
                        htmlOutput('commercial_strategy'),
                        width = 4),
                    box(title = 'Financial Strategy',
                        htmlOutput('financial_strategy'),
                        width = 4)
            ),
            tabItem('general_overview',
                    column(12, 
                        column(6, billboarderOutput('monthly_revenue', height = '200px')),
                        column(6, billboarderOutput('number_of_customers', height = '200px'))
                    ),
                    column(12,
                        column(6, billboarderOutput('pct_monthly_revenue', height = '200px')),
                        column(6, billboarderOutput('pct_customers', height = '200px'))
                    ),
                    column(12,
                        column(6, billboarderOutput('churn_rate_tenure', height = '200px')),
                        column(6, billboarderOutput('churn_rate_internet_service', height = '200px'))
                    )
            )
        )
    )
)