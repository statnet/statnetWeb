#ergm-undir
#ui.R
library(shiny)
library(statnet)

shinyUI(fluidPage(
  titlePanel('ERGM App for undirected networks'),
  p("This app is based on the ergm-mesa app, but will have more networks to choose from and more features."),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabName == 'Plot Network'",
                       h5('Choose a dataset'),
                       selectInput('dataset',
                                   label = 'Sample datasets',
                                   c(Choose = '','faux.mesa.high'),
                                   selectize = FALSE),
                       br(),
                       actionButton('goButton', 'Run')),
      
      conditionalPanel(condition = "input.tabName != 'Plot Network'",
                       h5('Choose a dataset'),
                       p('You are currently using the network', verbatimTextOutput('currentdataset'),
                         'Return to the first tab if you would like to switch networks.')),
      
      conditionalPanel(condition = "input.tabName == 'Plot Network' | input.tabName == 'Simulations'",
                       tags$hr(),
                       h5('Display Options'),
                       p('Pay attention to whether these options make sense for your network.'),
                       checkboxInput('iso',
                                     label = 'Display isolates?', 
                                     value = TRUE),
                       checkboxInput('vnames',
                                     label = 'Display vertex names?',
                                     value = FALSE),
                       uiOutput('dynamiccolor'),
                       uiOutput('dynamicsize'))
                       ),
    
    mainPanel(
      tabsetPanel(type = 'tabs', id = 'tabName',
                  tabPanel('Plot Network',
                           plotOutput('nwplot'),
                           h4('Network Summary'),
                           verbatimTextOutput('attr')
                  ),
                  
                  tabPanel('Fit Model',
                           fluidRow(
                             column(5,
                                    selectInput('terms',label = 'Choose term(s):',
                                                c('edges','degree','gwesp','nodematch','triangle'),
                                                selected='edges',
                                                multiple=TRUE, 
                                                selectize = FALSE)),
                             column(4,
                                    conditionalPanel(condition = 'input.terms.indexOf("gwesp") > -1', 
                                                     numericInput('choosegwesp', label = 'pick alpha for gwesp',
                                                                  value = 0, min = 0),
                                                     checkboxInput('fixgwesp', label = 'fixed?', value = TRUE)),
                                    conditionalPanel(condition = 'input.terms.indexOf("degree") > -1',
                                                     uiOutput('dynamicdegree')),
                                    conditionalPanel(condition = 'input.terms.indexOf("nodematch") > -1',
                                                     uiOutput('dynamicnodematch'))
                             )
                           ),
                           actionButton('fitButton', 'Fit Model'),
                           tags$hr(),
                           fluidRow(
                             verbatimTextOutput('check1'),
                             p('The output of the model fitting process and the summary of the model
                               fit is below. Pay attention to the coefficient estimates and 
                               significance for each term.'),
                             br(),
                             p('Be sure to continue all the way to the "Goodness of Fit" and "Diagnostics"
                               tabs to check for model degeneracy.'),
                             br(),
                             verbatimTextOutput('modelfit'))
                             ),
                  
                  tabPanel('Goodness of Fit',
                           p('Test how well your model fits the original data by choosing 
                             a network statistic that is not in the model, and comparing 
                             the value of this statistic observed in the original network 
                             to the distribution of values you get in simulated networks from 
                             your model.'),
                           p('If you do not specify a term the default formula for undirected 
                             networks is ', code('~ degree + espartners + distance'), 'and for 
                             directed networks is ', code('~ idegree + odegree + espartners + 
                                                          distance'), '.'),
                           fluidRow(
                             column(6, selectInput('gofterm', 'Goodness of Fit Term:',
                                                   c(Default='', 'degree', 'distance', 'espartners', 
                                                     'dspartners', 'triadcensus', 'model'),
                                                   selectize = FALSE)),
                             column(4, actionButton('gofButton', 'Run'))),
                           verbatimTextOutput('gof.summary'),  
                           plotOutput('gofplot')
                           ),
                  
                  tabPanel('Diagnostics',
                           verbatimTextOutput('diagnostics')),
                  
                  tabPanel('Simulations',
                           fluidRow(
                             column(6,
                                    numericInput('nsims',
                                                 label = 'Number of simulations:',
                                                 min = 1,
                                                 value = 1)),
                             column(4,
                                    actionButton('simButton', 'Simulate'))),
                           verbatimTextOutput('sim.summary'),
                           numericInput('this.sim',
                                        label = 'Choose a simulation to plot',
                                        min = 1,
                                        value = 1),
                           plotOutput('simplot'))
                  )
    )
    )
  )
    )