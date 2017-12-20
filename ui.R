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
                                        column(4, 
                                               div(id = 'main', 
                                                   class = 'strategy_box',
                                                   h3('Main Strategy'),
                                                   htmlOutput('main_strategy')
                                               )
                                        )
                                        ,
                                        column(4,
                                               div(id = 'commercial', 
                                                   class = 'strategy_box',
                                                   h3('Commercial Strategy'),
                                                   htmlOutput('commercial_strategy')
                                               )
                                        ),
                                        column(4,
                                               div(id = 'financial', 
                                                   class = 'strategy_box',
                                                   h3('Financial Strategy'),
                                                   htmlOutput('financial_strategy')
                                               )
                                        )
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
                       tabPanel('Keras Training',
                                sidebarLayout(
                                    sidebarPanel(
                                        h3('ANN Model Parameters'),
                                        div(style = 'display:inline-block',
                                            div(style = 'display:inline-block;vertical-align:top;', numericInput('train_prop', 'Training %', .8, .51, .99, .01, width = '100px')),       
                                            bsTooltip('train_prop', 'Proportion of population to split as training set', 'top', options = list(container = 'body')),
                                            div(style = 'display:inline-block;vertical-align:top;', selectInput('optimizer', 'Optimizer', keras_optimizers, 'adam', width = '100px')),
                                            bsTooltip('optimizer', 'Method used to value the weights of the neural network to minimize the objective function', 'top', options = list(container = 'body')),
                                            div(style = 'display:inline-block;vertical-align:top;', selectInput('loss', 'Loss', keras_losses, 'binary_crossentropy', width = '250px')),
                                            bsTooltip('loss', 'Objective function', 'top', options = list(container = 'body'))
                                            
                                        ),
                                        div(style = 'display:inline-block',
                                            div(style = 'display:inline-block;vertical-align:top;', selectInput('batch_size', 'Batch size', seq(25, 100, by = 25), 50, width = '100px')),
                                            bsTooltip('batch_size', 'Number of samples to be propagated through the network', 'top', options = list(container = 'body')),
                                            div(style = 'display:inline-block;vertical-align:top;', selectInput('epochs', 'Epochs', seq(10, 50, by = 5), 35, width = '100px')),
                                            bsTooltip('epochs', 'Number of iterations to train the model', 'top', options = list(container = 'body'))
                                        ),
                                        h4('First Hidden Layer'),
                                        div(style = 'display:inline-block',
                                            div(style = 'display:inline-block;vertical-align:top;', selectInput('first_hidden_layer_activation', 'Activation', keras_activations, 'relu', width = '120px')),
                                            div(style = 'display:inline-block;vertical-align:top;', selectInput('first_hidden_layer_kernel_initializer', 'Kernel Initializer', keras_initializers, 'uniform', width = '150px')),
                                            div(style = 'display:inline-block;vertical-align:top;', numericInput('first_hidden_layer_dropout', 'Dropout Rate', .1, 0, 1, .01))
                                        ),
                                        h4('Second Hidden Layer'),
                                        div(style = 'display:inline-block',
                                            div(style = 'display:inline-block;vertical-align:top;', selectInput('second_hidden_layer_activation', 'Activation', keras_activations, 'relu', width = '120px')),
                                            div(style = 'display:inline-block;vertical-align:top;', selectInput('second_hidden_layer_kernel_initializer', 'Kernel Initializer', keras_initializers, 'uniform', width = '150px')),
                                            div(style = 'display:inline-block;vertical-align:top;', numericInput('second_hidden_layer_dropout', 'Dropout Rate', .1, 0, 1, .01))
                                        ),
                                        h4('Output Layer'),
                                        div(style = 'display:inline-block',
                                            div(style = 'display:inline-block', selectInput('output_layer_activation', 'Activation', keras_activations, 'sigmoid', width = '120px')),
                                            div(style = 'display:inline-block', selectInput('output_layer_kernel_initializer', 'Kernel Initializer', keras_initializers, 'uniform', width = '150px'))
                                        ),
                                        withBusyIndicatorUI(actionButton('run_keras', 'Run Keras', class = 'btn-primary'))
                                    ),
                                    mainPanel(
                                        billboarderOutput('keras_acc_plot', height = '300px'),
                                        billboarderOutput('keras_loss_plot', height = '300px'),
                                        tableOutput('model_results_table')
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