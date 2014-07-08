#ergm-mesa
#ui.R
library(shiny)
library(statnet)

shinyUI(fluidPage(
  titlePanel('ERGM App Specific to faux.mesa.high'),
  p("This app displays some of the ergm package's power, but has limited flexibility.
    The current version is based on the ", tags$a(href='http://statnet.csde.washington.edu/EpiModel/nme/2014/d2-tut1.html','ergm tutorial'),
    "from the 2014 Network Modeling for Epidemics Workshop. Stay tuned for updates!"),
  sidebarLayout(
    sidebarPanel(
      h5('Choose a dataset'),
      p(em('Note:'), 'This app is only set up to use the faux.mesa.high network.'),
      selectInput('dataset',
                  label = 'Sample datasets',
                  c(Choose = '','faux.mesa.high'),
                  selectize = FALSE),
      br(),
      actionButton('goButton', 'Run'),
      
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
                       selectInput('colorby',
                                   label = 'Color nodes according to:',
                                   c('None', 'Grade','Race','Sex'),
                                   selectize = FALSE),
                       selectInput('sizeby',
                                   label = 'Size nodes according to:',
                                   c('None' = 1, 'Grade','Race','Sex'),
                                   selectize = FALSE))),
    
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
                                                multiple=TRUE, 
                                                selectize = FALSE)),
                             column(4,
                                    conditionalPanel(condition = 'input.terms.indexOf("gwesp") > -1', 
                                      selectInput('gwespterms', label = 'Gwesp terms:',
                                                c('Grade','Race','Sex'),
                                                multiple = TRUE,
                                                selectize = FALSE)),
                                    conditionalPanel(condition = 'input.terms.indexOf("degree") > -1',
                                      selectInput('degreeterms', label = 'Degree terms:',
                                                paste(0:3),
                                                multiple = TRUE,
                                                selectize = FALSE)))
                           ),
                           actionButton('fitButton', 'Fit Model'),
                           tags$hr(),
                           fluidRow(
                             verbatimTextOutput('checkform'),
                             p('The output of the model fitting process and the summary of the model
                               fit is below. Pay attention to the coefficient estimates and 
                               significance for each term.'),
                             br(),
                             p('Be sure to continue all the way to the "Goodness of Fit" and "Diagnostics"
                               tabs to check for model degeneracy.'),
                             br(),
                             verbatimTextOutput('modelfit'))
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
                           plotOutput('simplot')
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
                           verbatimTextOutput('diagnostics'))
                           )
      )
    )
)
)