#ergm-common
#ui.R
library(shiny)
library(statnet)

shinyUI(navbarPage('ergm app',
  tabPanel('Plot Network',
   fluidRow(
     column(1, img(src = 'csdelogo_crop.png', height = 50, width = 50)),
     column(3, h6('Center for Studies in Demography and Ecology')),
     column(8)),
   
    fluidRow(
     column(4,
      wellPanel(
         h5('Choose a dataset'),
         selectInput('dataset',
                     label = 'Sample datasets',
                     c(Choose = '', 'ecoli1', 'ecoli2', 'faux.mesa.high','fauxhigh', 'flobusiness','flomarriage',
                       'kapferer','kapferer2','samplike'),
                     selectize = FALSE),
                       br(),
                       actionButton('goButton', 'Run'),
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
         uiOutput('dynamicsize'))),
                  
      column(8, 
             plotOutput('nwplot'),
             h4('Network Summary'),
             verbatimTextOutput('attr')))
    ),
                  
      tabPanel('Fit Model',
          fluidRow(
            column(3,
              wellPanel(
                p('Current network:', verbatimTextOutput('currentdataset')),
                p('Current ergm formula:',
                  verbatimTextOutput('check1'))
                )),
          column(9,     
               fluidRow(
                 column(5,
                        conditionalPanel(condition = 'input.dataset.gal.directed == TRUE',
                                         uiOutput('dirlistofterms')),
                        conditionalPanel(condition = 'input.dataset.gal.directed == FALSE',
                                         uiOutput('undirlistofterms'))),
                 column(4,
                        conditionalPanel(condition = 'input.terms.indexOf("gwesp") > -1', 
                                         numericInput('choosegwesp', label = 'pick alpha for gwesp',
                                                      value = 0, min = 0),
                                         checkboxInput('fixgwesp', label = 'fixed?', value = TRUE)),
                        conditionalPanel(condition = 'input.terms.indexOf("degree") > -1',
                                         uiOutput('dynamicdegree')),
                        conditionalPanel(condition = 'input.terms.indexOf("nodematch") > -1',
                                         uiOutput('dynamicnodematch')))
               ),
               fluidRow(
                 column(3, p('Right hand side of ergm fit formula:')),
                 column(5, verbatimTextOutput('check1'))
                 ),
               actionButton('fitButton', 'Fit Model')
          )),
         
         br(),
         tags$hr(),
         p('The output of the model fitting process and the summary of the model
           fit is below. Pay attention to the coefficient estimates and 
           significance for each term.'),
         br(),
         p('Check for model degeneracy in the "Diagnostics" tab.'),
         br(),
         verbatimTextOutput('modelfit')     
          
          ),
  
          navbarMenu('Diagnostics',
              tabPanel('Goodness of Fit',
                       
                       mainPanel(
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
                       plotOutput('gofplot'))
                       ),
              
              tabPanel('MCMC Diagnostics',
                       verbatimTextOutput('diagnostics'))
              ),
  
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
    