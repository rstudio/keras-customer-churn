shinyUI(fluidPage(
    tags$head(tags$link(rel = 'icon', type = 'image/png', href = 'business-science-logo.png'),
              tags$title('Customer Analytics')),
    useShinyjs(),
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')),
    includeScript("www/message.js"),
    uiOutput('datatable_rowcolor'),
    
    navbarPageWithText('Customer Analytics',
                       tabPanel('Customer Scorecard',
                                fluidRow(
                                    column(3, uiOutput('customer_id_selector')),
                                    column(6, htmlOutput('churn_risk'))
                                ),
                                fluidRow(
                                    div(id = 'strategies',
                                        map(names(strategy_colors), function(this_strategy) {
                                            column(4,
                                                   div(id = this_strategy,
                                                       class = 'strategy_box',
                                                       h3(glue('{Hmisc::capitalize(this_strategy)} Strategy'),
                                                          htmlOutput(glue('{this_strategy}_strategy')))))
                                        })
                                    )
                                ),
                                fluidRow(
                                    column(6, 
                                           div(id = 'customer_details_table', style = 'thead {display:none;}', DT::dataTableOutput('customer_info_tbl'))
                                    ),
                                    column(6,
                                           withSpinner(billboarderOutput('customer_explanation', height = '600px'))
                                    )
                                )
                       ),
                       tabPanel('Churn Analysis',
                                
                                sidebarLayout(
                                    sidebarPanel(
                                        selectInput('payment_methods', 'Payment Method', c('All', unique(churn_data_raw$PaymentMethod))),
                                        selectInput('tech_support', 'Tech Support', c('All', unique(churn_data_raw$TechSupport))),
                                        selectInput('monthly_charge_range', 'Monthly Charge Range', c('All', unique(churn_data_raw$monthly_charge_range))),
                                        selectInput('tenure_range', 'Tenure Range', c('All', unique(churn_data_raw$tenure_range))),
                                        width = 2
                                    ),
                                    mainPanel(
                                        tabsetPanel(
                                            tabPanel('Customer and revenue', 
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
                                            ),
                                            tabPanel('Correlation analysis', 
                                                     withSpinner(plotOutput('corr_analysis', height = '800px'))
                                            )
                                        )
                                    )
                                )
                       )
                       , text = HTML(
                           '
                            <span style = "font-size: 20px;line-height:34px">
                                Powered by  
                                <a href = "https://www.rstudio.com"><img src="https://rstudio.com/wp-content/uploads/2014/05/logo-white@2x.png" height=40px></a>
                                &
                                <a href = "http://www.business-science.io/"><img src="business-science-logo.png" height=43px><span style = "font-size:20px;line-height:37px;color:white;"> Business Science</span></a>
                            </span>
                           '
                       )
    )
    
))