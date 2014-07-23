#' ---
#' title: "ergm-common, ui.R"
#' author: "Emily Beylerian"
#' ---
#' ergm-common
#' ============
#' ui.R
#' =========

#' **Before reading this document:** The Shiny app "ergm-common" is not contained in a
#' single R Script. Within the folder "ergm-common" the script `ui.R` controls the 
#' layout and appearance of the app, the script `server.R` controls the content that
#' gets displayed in the app, and the folder "www" contains auxiliary files (javascript,
#' css, and image files). If you are unfamiliar with Shiny apps, it may be more 
#' natural and helpful to start with the documentation for `ui.R` and then move on to
#' `server.R`.
#' 
#' The R functions inside `ui.R` output HTML code, which Shiny turns into a webapp. 
#' Widgets are elements of the UI that the user can interact with to influence the 
#' content that the app produces (see examples in the 
#' [gallery](http://shiny.rstudio.com/gallery/) ). Most
#' of the time, the built-in functions and widgets in Shiny will be powerful enough to
#' do what we want, but sometimes we will more directly access HTML tags with `tags$*`.
#' It is also possible to write the entire UI directly in HTML 
#' (http://shiny.rstudio.com/articles/html-ui.html). 
#' 
#' 
#' The function `customTextInput` is a manipulation of the `textInput` widget
#' that allows for smaller input boxes. In addition to all the normal arguments passed
#' to `textInput`, `class = "input-small"` or `class = "input-mini"` can be specified.
#' 
#+ setup, eval=FALSE
#load necessary packages
library(shiny)
library(statnet)

customTextInput<-function (inputId, label, value="",...) {
  tagList(tags$label(label, `for` = inputId), tags$input(id = inputId,
                                                         type="text",
                                                         value=value,...))
}

#' Everything that gets displayed inside the app is enclosed in a call to `shinyUI`.
#' The first thing to be specified is the type of page to display. The `navbarPage` 
#' includes a navigation bar at the top of the page and each tab leads to different 
#' pages of content. Find out more about layout options 
#' [here](http://shiny.rstudio.com/articles/layout-guide.html)
#' 
#'
#+ eval=FALSE 
shinyUI(
  navbarPage(title=p(a(span('statnet  ', style='font-family:Courier'),
                             href = 'https://statnet.csde.washington.edu/trac'),
                           'ergm app'),

  tabPanel('Plot Network',
    fluidRow(
     column(3,
          wellPanel(
            h5('Choose a dataset'),
            selectInput('dataset',
                         label = 'Sample datasets',
                         c(Choose = '', 'ecoli1', 'ecoli2', 'faux.magnolia.high',
                           'faux.mesa.high', 'flobusiness','flomarriage',
                           'kapferer','kapferer2','samplike'),
                         selectize = FALSE),
                           br(),
                           actionButton('goButton', 'Run')),
      h5('Network Summary'),
      verbatimTextOutput('attr'),
      fluidRow(
        
        column(10, img(src= 'UW.Wordmark_ctr_K.jpg'))
      ),
      fluidRow(
        column(3, a(img(src = 'csdelogo_crop.png', height = 50, width = 50),
                    href = 'https://csde.washington.edu/')),
        column(7, a(img(src = 'csde_goudy.fw.png'), href = 'https://csde.washington.edu/'))
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
              fluidRow(
               column(2, p('Current ergm formula:')),
               column(4, actionButton('fitButton', 'Fit Model'))),
               verbatimTextOutput('checkterms1'))
          ),
         fluidRow(
           column(10, offset=2,
                  p('Pre-fit Summary'),
                  verbatimTextOutput('checkfitsum'))
           ),
         fluidRow(
           column(3,
                  uiOutput('listofterms')),
           conditionalPanel(condition = 'input.terms.indexOf("absdiff") > -1',
                            column(2,
                                   uiOutput('dynamicabsdiff'),
                                   customTextInput('absdiff.choosepow',
                                                   label = 'pow = ',
                                                   value = '1', class='input-small'),
                                   customTextInput('absdiffform', label='form = ',
                                                 value = '', class='input-small',
                                                 helpText("For valued ergms only.",
                                                          "Options are 'sum' or 'nonzero'"))
                                   )),
           conditionalPanel(condition = 'input.terms.indexOf("degree") > -1',
                            column(2,
                                   uiOutput('dynamicdegree'),
                                   customTextInput('choosedegree2',
                                                   label = 'OR, input your own',
                                                   value = NULL, class='input-small',
                                                   helpText("If entered, custom user",
                                                            "input will be prioritized"))
                                   )),
           conditionalPanel(condition = 'input.terms.indexOf("b1degree") > -1',
                            column(2,
                                   uiOutput('dynamicb1degree'),
                                   customTextInput('chooseb1degree2',
                                                   label = 'OR, input your own',
                                                   value = NULL, class='input-small',
                                                   helpText("If entered, custom user",
                                                            "input will be prioritized"))
                            )),
           conditionalPanel(condition = 'input.terms.indexOf("b2degree") > -1',
                            column(2,
                                   uiOutput('dynamicb2degree'),
                                   customTextInput('chooseb2degree2',
                                                   label = 'OR, input your own',
                                                   value = NULL, class='input-small',
                                                   helpText("If entered, custom user",
                                                            "input will be prioritized"))
                            )),
           conditionalPanel(condition = 'input.terms.indexOf("idegree") > -1',
                            column(2,
                                   uiOutput('dynamicidegree'),
                                   customTextInput('chooseidegree2',
                                                   label = 'OR, input your own',
                                                   value = NULL, class='input-small',
                                                   helpText("If entered, custom user",
                                                            "input will be prioritized")))),
           conditionalPanel(condition = 'input.terms.indexOf("odegree") > -1',
                            column(2,
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
                            column(2,
                                   uiOutput('dynamicnodecov')
#                                    checkboxInput('nodecovform', label='form = "sum"',
#                                                  value = FALSE)
                                   )),
           conditionalPanel(condition = 'input.terms.indexOf("nodefactor") > -1',
                            column(2,
                                   uiOutput('dynamicnodefactor'),
                                   customTextInput('nodefactor.choosebase',
                                                   label = 'base = ',
                                                   value = '1', class='input-small')
#                                    checkboxInput('nodefactorform', label='form = "sum"',
#                                                  value = FALSE)
                                   )),
           conditionalPanel(condition = 'input.terms.indexOf("nodematch") > -1',
                            column(2,
                                   uiOutput('dynamicnodematch'),
                                   customTextInput('nodematchkeep', label = 'keep = ',
                                                   value = '', class='input-small'),
                                   checkboxInput('nodematchdiff', label='diff',
                                                 value = FALSE))),
           conditionalPanel(condition = 'input.terms.indexOf("nodemix") > -1',
                            column(2,
                                   uiOutput('dynamicnodemix'),
                                   customTextInput('nodemix.choosebase',
                                                   label = 'base = ',
                                                   value = '', class='input-small')
#                                    checkboxInput('nodemixform',label='form = "sum"',
#                                                  value = FALSE)
                                   ))
           
         ),
         
         
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
                     fluidRow(
                     column(5,
                            verbatimTextOutput('gof.summary')),  
                     column(7,
                            uiOutput('gofplotspace')))
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
  tabPanel('Help',
           h4('Resources'),
           a("statnet Wiki",
             href = "https://statnet.csde.washington.edu/trac"),
           br(),
           a("ergm: Journal of Statistical Software",
             href = "http://www.jstatsoft.org/v24/i03/"),
           br(),
           a("Using ergm: Journal of Statistical Software",
             href = "http://www.jstatsoft.org/v24/i04/"),
           br(),
           a("ergm documentation on CRAN", 
             href = "http://cran.r-project.org/web/packages/ergm/ergm.pdf"),
           br(),
           hr(),
           p("The best way to contact us with questions, comments or suggestions",
             "is through the", strong("statnet users group"), "listserv."),
           p("To post and receive messages from this listserv, you need to join.",
             "Instructions are at:", 
             a("https://mailman.u.washington.edu/mailman/listinfo/statnet_help",
               href = "https://mailman.u.washington.edu/mailman/listinfo/statnet_help")),
           p("You can use the listserv to:"),
           tags$ul(
             tags$li("get help from the statnet development team (and other users)"),
             tags$li("post questions, comments and ideas to other users"),
             tags$li("be informed about statnet updates"),
             tags$li("learn about bugs (and bug fixes)")
             ),
           p("Once you have joined the list, you can post your questions and comments to",
             strong("statnet_help@u.washington.edu")),
           p("A full list of all messages posted to this list is available at",
             a("https://mailman.u.washington.edu/mailman/private/statnet_help",
               href = "https://mailman.u.washington.edu/mailman/private/statnet_help"))
           )
                  
    
    )
  )
    