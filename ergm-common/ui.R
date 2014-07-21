#ergm-common
#ui.R
library(shiny)
library(shinyIncubator)
library(statnet)

customTextInput<-function (inputId, label, value="",...) {
  tagList(tags$label(label, `for` = inputId), tags$input(id = inputId,
                                                         type="text",
                                                         value=value,...))
}

shinyUI(navbarPage(title=p(span('statnet', style='font-family:Courier'),'ergm app'),
#   progressInit(), #need this for progress bar

 

  tabPanel('Plot Network',
    fluidRow(
     column(3,
          wellPanel(
            h5('Choose a dataset'),
            selectInput('dataset',
                         label = 'Sample datasets',
                         c(Choose = '', 'ecoli1', 'ecoli2', 'faux.mesa.high',
                           'fauxhigh', 'flobusiness','flomarriage',
                           'kapferer','kapferer2','samplike'),
                         selectize = FALSE),
                           br(),
                           actionButton('goButton', 'Run')),
      h5('Network Summary'),
      verbatimTextOutput('attr'),
      fluidRow(
        column(3, img(src = 'csdelogo_crop.png', height = 50, width = 50)),
        column(7, h6('Center for Studies in Demography and Ecology'))
        )
      ),
                  
     column(8, 
            plotOutput('nwplot'),

             
             wellPanel(
               fluidRow(h5('Display Options')),
               fluidRow(column(3,
                               checkboxInput('iso',
                                             label = 'Display isolates?', 
                                             value = TRUE),
                               checkboxInput('vnames',
                                             label = 'Display vertex names?',
                                             value = FALSE)),
                        column(3,
                               uiOutput('dynamiccolor')),
                        column(3,
                               uiOutput('dynamicsize')))))
      )
    ),
                  
      tabPanel('Fit Model',
          #code to include progress bar when this tab is loading
               tagList(
                 tags$head(
                   tags$link(rel="stylesheet", type="text/css",href="style.css"),
                   tags$script(type="text/javascript", src = "busy.js")
                 )
               ),
               div(class = "busy",  
                   p("Calculation in progress.."), 
                   img(src="ajax-loader.gif")
               ),      
               
          fluidRow(
            column(2,
               p('Current network:', verbatimTextOutput('currentdataset1'))),
            column(10,
               p('Current ergm formula:',
                  verbatimTextOutput('checkterms1')))
          ),     
         fluidRow(
           column(3,
                  uiOutput('listofterms')),
           conditionalPanel(condition = 'input.terms.indexOf("absdiff") > -1',
                            column(3,
                                   uiOutput('dynamicabsdiff'),
                                   customTextInput('absdiff.choosepow',
                                                   label = 'pow = ',
                                                   value = '1', class='input-small'),
                                   checkboxInput('absdiffform', label='form = "sum"',
                                                 value = FALSE))),
           conditionalPanel(condition = 'input.terms.indexOf("degree") > -1',
                            column(3,
                                   uiOutput('dynamicdegree'),
                                   customTextInput('choosedegree2',
                                                   label = 'OR, input your own',
                                                   value = NULL, class='input-small',
                                                   helpText("If entered, custom user",
                                                            "input will be prioritized"))
                                   )),
           conditionalPanel(condition = 'input.terms.indexOf("b1degree") > -1',
                            column(3,
                                   uiOutput('dynamicb1degree'),
                                   customTextInput('chooseb1degree2',
                                                   label = 'OR, input your own',
                                                   value = NULL, class='input-small',
                                                   helpText("If entered, custom user",
                                                            "input will be prioritized"))
                            )),
           conditionalPanel(condition = 'input.terms.indexOf("b2degree") > -1',
                            column(3,
                                   uiOutput('dynamicb2degree'),
                                   customTextInput('chooseb2degree2',
                                                   label = 'OR, input your own',
                                                   value = NULL, class='input-small',
                                                   helpText("If entered, custom user",
                                                            "input will be prioritized"))
                            )),
           conditionalPanel(condition = 'input.terms.indexOf("idegree") > -1',
                            column(3,
                                   uiOutput('dynamicidegree'),
                                   customTextInput('chooseidegree2',
                                                   label = 'OR, input your own',
                                                   value = NULL, class='input-small',
                                                   helpText("If entered, custom user",
                                                            "input will be prioritized")))),
           conditionalPanel(condition = 'input.terms.indexOf("odegree") > -1',
                            column(3,
                                   uiOutput('dynamicodegree'),
                                   customTextInput('chooseodegree2',
                                                   label = 'OR, input your own',
                                                   value = NULL, class='input-small',
                                                   helpText("If entered, custom user",
                                                            "input will be prioritized")))),
           conditionalPanel(condition = 'input.terms.indexOf("gwesp") > -1', 
                            column(2,
                                   customTextInput('choosegwesp', 
                                                   label = 'Input alpha for gwesp',
                                                   value = 0, class='input-small'),
                                   checkboxInput('fixgwesp', label = 'fixed?', 
                                                 value = TRUE))),
           conditionalPanel(condition = 'input.terms.indexOf("nodecov") > -1',
                            column(3,
                                   uiOutput('dynamicnodecov'),
                                   checkboxInput('nodecovform', label='form = "sum"',
                                                 value = FALSE))),
           conditionalPanel(condition = 'input.terms.indexOf("nodefactor") > -1',
                            column(3,
                                   uiOutput('dynamicnodefactor'),
                                   customTextInput('nodefactor.choosebase',
                                                   label = 'base = ',
                                                   value = '1', class='input-small'),
                                   checkboxInput('nodefactorform', label='form = "sum"',
                                                 value = FALSE))),
           conditionalPanel(condition = 'input.terms.indexOf("nodematch") > -1',
                            column(3,
                                   uiOutput('dynamicnodematch'),
                                   checkboxInput('nodematchform', label='form = "sum"',
                                                 value = FALSE))),
           conditionalPanel(condition = 'input.terms.indexOf("nodemix") > -1',
                            column(3,
                                   uiOutput('dynamicnodemix'),
                                   customTextInput('nodemix.choosebase',
                                                   label = 'base = ',
                                                   value = 'NULL', class='input-small'),
                                   checkboxInput('nodemixform',label='form = "sum"',
                                                 value = FALSE)))
           
         ),
         actionButton('fitButton', 'Fit Model'),
         
         br(),
         tags$hr(),
         h4('Model Summary'),
         p('Check for model degeneracy in the "Diagnostics" tab.'),
         br(),
         tabsetPanel(
           tabPanel('Fitting',
                    verbatimTextOutput('modelfit')),
           tabPanel('Summary',
                    verbatimTextOutput('modelfitsum'))
          )
          ),
   
         navbarMenu('Diagnostics',
            tabPanel('Goodness of Fit',
                     
                     #code to include progress bar when this tab is loading
                     tagList(
                       tags$head(
                         tags$link(rel="stylesheet", type="text/css",href="style.css"),
                         tags$script(type="text/javascript", src = "busy.js")
                       )
                     ),
                     div(class = "busy",  
                         p("Calculation in progress.."), 
                         img(src="ajax-loader.gif")
                     ),  
                     
                     fluidRow(
                       column(2,
                              p('Current network:', verbatimTextOutput('currentdataset2'))),
                       column(10,
                              p('Current ergm formula:',
                                verbatimTextOutput('checkterms2')))
                      ),     
                     fluidRow(
                       column(3, selectInput('gofterm', 'Goodness of Fit Term:',
                                             c(Default='', 'degree', 'distance', 'espartners', 
                                               'dspartners', 'triadcensus', 'model'),
                                             selectize = FALSE))),
                     fluidRow(
                        column(3, actionButton('gofButton', 'Run'))),
                     br(),
                     tags$hr(),
                     p('Test how well your model fits the original data by choosing 
                       a network statistic that is not in the model, and comparing 
                       the value of this statistic observed in the original network 
                       to the distribution of values you get in simulated networks from 
                       your model.'),
                     p('If you do not specify a term the default formula for undirected 
                       networks is ', code('~ degree + espartners + distance'), 'and for 
                       directed networks is ', code('~ idegree + odegree + espartners + 
                                                    distance'), '.'),
                     column(5,
                            verbatimTextOutput('gof.summary')),  
                     column(7,
                            uiOutput('gofplotspace'))
                     ),
            
            tabPanel('MCMC Diagnostics',
                     
                     #code to include progress bar when this tab is loading
                     tagList(
                       tags$head(
                         tags$link(rel="stylesheet", type="text/css",href="style.css"),
                         tags$script(type="text/javascript", src = "busy.js")
                       )
                     ),
                     div(class = "busy",  
                         p("Calculation in progress.."), 
                         img(src="ajax-loader.gif")
                     ),  
                     
                     fluidRow(
                       column(2,
                              p('Current network:', verbatimTextOutput('currentdataset3'))),
                       column(10,
                              p('Current ergm formula:',
                                verbatimTextOutput('checkterms3')))
                     ),     
                     br(),
                     tags$hr(),
                     tabsetPanel(
                       tabPanel('Plot',
                        plotOutput('diagnosticsplot', height = 600)),
                       tabPanel('Summary',
                        verbatimTextOutput('diagnostics'))
                     )
            )
            ),
  
          tabPanel('Simulations',
                   fluidRow(
                     column(2,
                            p('Current network:', verbatimTextOutput('currentdataset4'))),
                     column(10,
                            p('Current ergm formula:',
                              verbatimTextOutput('checkterms4')))
                   ),
                   br(),
                   tags$hr(),
                   
                   fluidRow(
                     column(3,
                            numericInput('nsims',
                                         label = 'Number of simulations:',
                                         min = 1,
                                         value = 1)      
                     ),
                     column(8, 
                            numericInput('this.sim',
                                        label = 'Choose a simulation to plot',
                                        min = 1,
                                        value = 1)
                            )
                   ),
                   fluidRow( 
                     column(3,
                            actionButton('simButton', 'Simulate'),
                            br(),
                            br(),
                            verbatimTextOutput('sim.summary')
                            ),  
                     column(8,
                            plotOutput('simplot'),
                            
                            
                            wellPanel(
                              fluidRow(h5('Display Options')),
                              fluidRow(column(3,
                                checkboxInput('iso2',
                                              label = 'Display isolates?', 
                                              value = TRUE),
                                checkboxInput('vnames2',
                                              label = 'Display vertex names?',
                                              value = FALSE)),
                              column(3,
                                uiOutput('dynamiccolor2')),
                              column(3,
                                uiOutput('dynamicsize2')))
                              )
                            )
                     )
                   ),
  tabPanel('Help')
                  
    
    )
  )
    