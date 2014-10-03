#' ---
#' title: "ergm-app, ui.R"
#' author: "Emily Beylerian"
#' ---
#' ergm-app
#' ============
#' ui.R
#' =========

#' **Before reading this document:** The Shiny app "ergm-app" is not contained in a
#' single R Script. Within the folder "ergm-app" the script `ui.R` controls the 
#' layout and appearance of the app, the script `server.R` controls the content that
#' gets displayed in the app, and the folder "www" contains auxiliary files (javascript,
#' css, and image files). If you are unfamiliar with Shiny apps, it may be more 
#' natural and helpful to start with the documentation for `ui.R` and then move on to
#' `server.R`.
#' 
#' **Basics**
#' 
#' The R functions inside `ui.R` output HTML code, which Shiny turns into a webapp. 
#' Widgets specific functions in Shiny that correspond to elements of the UI that the
#' user can interact with to influence the content that the app produces (see widget
#' examples in the [gallery](http://shiny.rstudio.com/gallery/) ). Some common HTML tags 
#' (e.g. `h1`,`p` and `a` below) have built-in functions in Shiny, many others are 
#' included in the `tags` object (see all the `tags` 
#' [here](http://shiny.rstudio.com/articles/tag-glossary.html)). 
#' It is also possible to write the entire UI 
#' [directly in HTML](http://shiny.rstudio.com/articles/html-ui.html).
#' 
#' Since the `server.R` script generates all the dynamic content of the app,
#' if the script only contains an empty function in the call to the Shiny server, 
#' e.g.
#' ```
#' shinyServer(
#'  function(input,output){})
#' ```
#' then all the UI elements will still be displayed statically without any content.
#' 
#' In a functioning app, `server.R` takes input objects and reactively (because the user
#' might change an input object) creates output objects. In order to display the output
#' object in the interface so the user can see it, it must be called with an appropriate
#' output function (`plotOutput`, `textOuput`, `verbatimTextOutput`, etc.) back in `ui.R`. 
#' 
#' **Code**
#' 
#' The function `customTextInput` is a manipulation of the `textInput` widget
#' that allows for smaller input boxes. In addition to all the normal arguments passed
#' to `textInput`, `class = "input-small"` or `class = "input-mini"` can be specified.
#' 
#' Similarly, the `helperButton` function creates a small question mark button. The file
#' `alert.js` (which is sourced later in this script) specifies that when the user clicks
#' on a helperButton, a window with tips on how to use the page will appear. The window
#' disappears the next time the user clicks on the button. 
#+ setup, eval=FALSE
#load necessary packages
library(shiny)
library(statnet)
source("chooser.R")

#' Functions to create new widgets
#+ eval=FALSE
customTextInput<-function (inputId, label, value="",...) {
  tagList(
    tags$label(label, `for` = inputId), 
    tags$input(id = inputId, type="text", value=value,...))
}

#version of selectInput...shorter box and label inline
#lapply allows us to add each element of choices as an option in the select menu
inlineSelectInput<-function (inputId, label, choices,...) {
  tagList(
    tags$label(label, `for` = inputId, style="display:inline"), 
    tags$select(id = inputId, choices=choices,..., 
                style="width:100px; line-height:20px; font-size:12px",
                class="shiny-bound-input",
                lapply(choices, tags$option)))
}


# This function generates the client-side HTML for a helper button
helperButton <- function(id) {
  tagList(
    tags$button(id=id, type="button", class="helper-btn",
                tags$img(src= "200px-Icon-round-Question_mark.svg.png",
                         height = 20, width = 20))
  )
}




#' Everything that gets displayed inside the app is enclosed in a call to `shinyUI`.
#' The first thing to be specified is the type of page to display. The `navbarPage` 
#' includes a navigation bar at the top of the page and each tab leads to different 
#' pages of content. Find out more about layout options 
#' [here](http://shiny.rstudio.com/articles/layout-guide.html).
#' 
#' The only reason we wrap the `navbarPage` in a `bootstrapPage` is to correctly display 
#' the app title in browser tab (rather than displaying the html from the styling of the 
#' title in the app).
#'
#+ eval=FALSE 
shinyUI(
  bootstrapPage(title='statnet - ergm app',
  navbarPage(title=div(a(span('statnet  ', style='font-family:Courier'),
                            href = 'https://statnet.csde.washington.edu/trac',
                            target = '_blank'),HTML('&nbsp;&nbsp;'), 'ergm app'),
             
#' Within each panel of the navbar, the content can be arranged by nesting rows and
#' columns. The first argument to `column` is the desired width, where the whole
#' browser window has a width of 12. Within any column, nested columns set their 
#' width relative to the parent column. Rows are specified by enclosing elements
#' in `fluidRow()`. It is often necessary to specify rows even when elements seem like
#' they should naturally be aligned horizontally, or when a `wellPanel` that is supposed
#' to hold some content doesn't quite enclose everything correctly.
#' 
#' **Data Upload**
#'
#' 
#' 
#+ eval=FALSE
  tabPanel('Data Upload',
           #busy.js is for calculation in progress boxes
           #alert.js is for helper boxes
           #jquery libraries are loaded from google cdn
           #this tagList command has to go inside a tabPanel
           tagList(
             tags$head(
               tags$link(rel="stylesheet", type="text/css",href="style.css"),
               tags$link(rel="stylesheet", type="text/css",href="autocomplete.css"),
               tags$link(rel="stylesheet", type="text/css",
                         href="//ajax.googleapis.com/ajax/libs/jqueryui/1.11.1/themes/smoothness/jquery-ui.css"),
               tags$script(type="text/javascript", src="//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"),
               tags$script(type="text/javascript", src="autocomplete.js"),
               tags$script(type="text/javascript", src="//ajax.googleapis.com/ajax/libs/jqueryui/1.11.1/jquery-ui.min.js"),
               tags$script(type="text/javascript", src="busy.js"),
               tags$script(type="text/javascript", src="alert.js")
             )
           ),
    br(),     
    fluidRow(
      column(7,
        tabsetPanel(id='datatabs',
          tabPanel('Upload Network', value=1,
                   wellPanel(
                     fluidRow(
                       column(5,
                              radioButtons('filetype',label=h5('File type'),
                                           choices=c('pre-loaded sample network'=5, 'statnet network object'=1,'Pajek network (*.net)'=2,'Pajek project (*.paj)'=3,
                                                     'matrix of relational data'=4))),
                       conditionalPanel(condition = 'input.filetype < 5',
                         column(7,
                              br(),
                              fileInput(inputId='rawdatafile', label=NULL, accept='text'),
                              verbatimTextOutput('rawdatafile'))
                          ),
                       conditionalPanel(condition = 'input.filetype == 5',
                              br(),
                              selectInput('samplenet', label='Choose a network',
                                          choices=c('faux.mesa.high','flobusiness',
                                                    'flomarriage','samplike'),
                                          selectize=FALSE) 
                                        
                                        )
                       
                       ),
                     fluidRow(
                       conditionalPanel(condition='input.filetype == 4',
                           h5('Specify'),
                           column(4,
                                  br(),
                                  radioButtons('matrixtype', label='Matrix Type',
                                               choices=c('Adjacency matrix'='adjacency', 
                                                         'Bipartite adjacency matrix'='bipartite',
                                                         'Incidence matrix' = 'incidence', 'Edge list' = 'edgelist'))),
                           column(6,
                                  br(),
                                  span('Network Attributes'),
                                  div(checkboxInput('dir', 'directed?', value=TRUE),
                                       style='padding-top:5px;'),
                                  checkboxInput('loops', 'loops?', value=FALSE),
                                  checkboxInput('multiple', 'multiple?', value=FALSE),
                                  checkboxInput('bipartite', 'bipartite?', value=FALSE))
                       ),
                       conditionalPanel(condition='input.filetype == 3',
                           h5('Specify'),
                           column(6,
                                  uiOutput('pajchooser')))
                     )
                     )
                   ),
          tabPanel('Edit Network', value=2,
                   wellPanel(
                     fluidRow(
                      
                       column(5,strong('Symmetrize'),
                              radioButtons('symmetrize', label=NULL, 
                                           choices=c('Do not symmetrize',
                                                     'upper: Copy upper triangle over lower triangle'='upper',
                                                     'lower: Copy lower triangle over upper triangle'='lower',
                                                     'strong: Intersection of ties'='strong',
                                                     'weak: Union of ties'='weak')),
                              radioButtons('aftersymm', label='After symmetrizing, network should be:',
                                           choices=c('directed', 'undirected'))
                              ),
                       
                       column(5,strong('Set New Attribute'),
                              conditionalPanel(condition="output.nwsum != 'NA'",
                                 radioButtons('newattrtype', label='Choose attribute type',
                                           choices=c('vertex attribute',
                                                   'edge attribute',
                                                   'edge value')),
                                 fileInput(inputId='newattrvalue', label=span('Upload a vector or list of attribute',
                                                                              'values, or a matrix of edge values.')),
                                 textInput('newattrname', label="New attribute name"),
                                 actionButton('newattrButton', label='Set Attribute')              
                                 )
                              
                              )
                       
                     ),
                     br(),
                     strong('Modify Attributes'),
                     uiOutput('modifyattrchooser')
                     )
                   ))

      
      ),
          
   column(4,br(),br(),
           verbatimTextOutput('nwsum')
          )            
    ),
    fluidRow(column(4,            
             column(10, img(src= 'UW.Wordmark_ctr_K.jpg', width=240)),
             fluidRow(
               column(3, a(img(src = 'csdelogo_crop.png', height = 50, width = 50),
                           href = 'https://csde.washington.edu/', target = '_blank')),
               column(7, a(img(src = 'csde_goudy.fw.png', width=180), href = 'https://csde.washington.edu/',
                           target = '_blank'))))),

     helperButton(id = 'tab2help'),
     div(class="helper-box", style="display:none",
         p('Upload a file of observed network data (must be of a supported type).', 
           'Modify or add attributes on the "Edit Network" tab.',
           'Sequentially move through the', 
           'tabs at the top of the page to fit an ergm to the', 
           'observed network.'))
      ),

#' **Network Plots**
#' 
#' Notice that 
#' there are no calls to `selectInput` for the options to color code or size the nodes,
#' even though they appear in the app. Most widget functions are called in `ui.R`, but
#' this means that all the options passed to them must be static. If the options depend
#' on user input (the coloring and sizing menus depend on which network the user
#' selects), the widget must be rendered in `server.R` and output in `ui.R` with 
#' `iuOutput`.
#' 
#+ eval=FALSE

  tabPanel('Network Plots',
    br(), 
    fluidRow(      
     column(7,
        tabsetPanel(
          tabPanel('Network Plot',
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
                               uiOutput('dynamicsize')),
                        br(),
                        column(3,
                               downloadButton('nwplotdownload', label = "Download Plot"))))),
          tabPanel('Degree Distribution',
                   plotOutput('degreedist'),
                   wellPanel(
                     fluidRow(h5('Display Options')),
                     fluidRow(column(3,
                                     selectInput('cmode', 
                                                 label = 'Type of degree centrality',
                                                 choices= c('indegree','outdegree',
                                                            'freeman (total)'='freeman'),
                                                 selected='freeman',
                                                 selectize=FALSE)),
                              column(3,
                                     uiOutput('dynamiccolor_dd')),
                              column(6,
                                     p('Display overlay of expected values?'),
                                     checkboxInput('uniformoverlay', 
                                                   label='uniform graphs conditional on edge count', 
                                                   value=TRUE),
                                     checkboxInput('bernoullioverlay',
                                                   label='Draws from bernoulli graphs with specified tie prob',
                                                   value=TRUE))
                     ),
                     fluidRow(downloadButton('degreedistdownload', label = "Download Plot"))
                     )),
          tabPanel('Geodesic Distribution',
                   plotOutput('geodistplot'),
                   wellPanel(
                     fluidRow(
                          column(9, p('A bar with zero height (empty space on the graph)',
                          'represents a vertex pair that is not connected by',
                          'any path.')),
                          column(3,
                                downloadButton('geodistdownload', label= 'Download Plot'))))
                   ),
          tabPanel('More',
                   h5('Mixing Matrix', icon('angle-double-down'), 
                      class="mixmxtitle"),
                   wellPanel(
                     uiOutput('mixmxchooser'),
                     verbatimTextOutput('mixingmatrix'),
                     class="mixmxbox"
                   ),
                   h5('Graph-level descriptive indices',
                      icon('angle-double-down'), class="graphleveltitle"),
                   wellPanel(fluidRow(

                     column(4, offset=7,tags$u('Measure')),
                     fluidRow(
                     column(4, p('Reciprocity:', class='stitle')),
                     column(3, p(textOutput('grecip'), class='snum')),
                     column(4, inlineSelectInput('grecipmeas',label=NULL,
                                 choices=c('dyadic','dyadic.nonnull','edgewise',
                                           'edgewise.lrr','correlation'),
                                 selectize=FALSE, style='margin-top:5px;'))),
                     fluidRow(
                     column(4, p('Transitivity:'), class='stitle'),
                     column(3, p(textOutput('gtrans'), class='snum')),
                     column(4, inlineSelectInput('gtransmeas',label=NULL,
                                    choices=c('weak','strong','weakcensus',
                                              'strongcensus','rank','correlation'),
                                    selectize=FALSE, style='margin-top:0px;'))),
                     fluidRow(
                     column(4, p('Density:', class='stitle')),
                     column(3, p(textOutput('gden'), class='snum')))
                   ), class="graphlevelbox"),
                   
                   h5('Node-level descriptive indices',
                      icon('angle-double-down'), class="nodeleveltitle"),
                   wellPanel(
                     numericInput('nodeind', label='Input node index',value=1,
                                min=1),
                     tags$hr(),
                     fluidRow(
                       column(4, offset=7, tags$u('Centrality mode')),
                       fluidRow(
                         column(4, p('Degree:', class='stitle')),
                         column(3, p(textOutput('ndeg'), class='snum')),
                         column(4, inlineSelectInput('ndegcmode', label=NULL,
                                                     choices=c('indegree', 'outdegree', 'freeman'),
                                                     style='margin-top:5px;'))
                         ),
                       fluidRow(
                         column(4, p('Betweenness:', class='stitle')),
                         column(3, p(textOutput('nbetw'), class='snum')),
                         column(4, inlineSelectInput('nbetwcmode', label=NULL,
                                                     choices=c('directed','undirected',
                                                               'endpoints','proximalsrc',
                                                               'proximaltar','proximalsum',
                                                               'lengthscaled', 'linearscaled'),
                                                     style='margin-top:0px;'))
                         ),
                       fluidRow(
                         column(4, p('Closeness:', class='stitle')),
                         column(3, p(textOutput('nclose'), class='snum')),
                         column(4, inlineSelectInput('nclosecmode', label=NULL,
                                                     choices=c('directed','undirected',
                                                               'suminvdir','suminvundir',
                                                               'gil-schmidt'),
                                                     style='margin-top:0px;'))
                         ),
                       fluidRow(
                         column(4, p('Stress Centrality:', class='stitle')),
                         column(3, p(textOutput('nstress'), class='snum')),
                         column(4, inlineSelectInput('nstresscmode', label=NULL,
                                                     choices=c('directed','undirected'),
                                                     style='margin-top:0px;'))
                         ),
                       fluidRow(
                         column(4, p('(Harary) Graph Centrality:', class='stitle')),
                         column(3, p(textOutput('ngraphcent'), class='snum')),
                         column(4, inlineSelectInput('ngraphcentcmode', label=NULL,
                                                     choices=c('directed', 'undirected'),
                                                     style='margin-top:0px;'))
                         ),
                       fluidRow(
                         column(4, p('Eigenvector Centrality:', class='stitle')),
                         column(3, p(textOutput('nevcent'), class='snum')),
                         column(4, br())
                         ),
                       fluidRow(
                         column(4, p('Information Centrality:', class='stitle')),
                         column(3, p(textOutput('ninfocent'), class='snum')),
                         column(4, inlineSelectInput('ninfocentcmode',label=NULL,
                                                     choices=c('weak', 'strong', 'upper',
                                                               'lower'), style='margin-top:0px;'))
                         )
                     ), class="nodelevelbox")
                   
                   
                   )
          )),
     column(4,
            br(),br(),
            verbatimTextOutput('attr2'))
     ),
     helperButton(id = 'tab2help'),
     div(class="helper-box", style="display:none",
         p('Use the network plots to gain insight to the observed network.', 
           'Edit the display options below and download a PDF of any of the plots.')),
    
    #include progress box when this tab is loading
    div(class = "busy", 
        p("Calculation in progress..."),
        img(src="ajax-loader.gif")
        ) 
    ),

    

#' **Fit Model**
#' 
#' Since model fitting does not happen instantly, a loading icon will help to assure 
#' users that the app is still working on producing output. The following code chunk
#' uses the files `busy.js`, `style.css` and `ajax-loader.gif` located in the directory
#' `ergm-common/www` to create a loading message. To display the loading message on
#' subsequent tabs, we only need to include the `div` statement on those tabs.
#' 
#+ eval=FALSE                  
      tabPanel('Fit Model',

          #include progress bar when this tab is loading
           div(class = "busy", 
               p("Calculation in progress..."),
               img(src="ajax-loader.gif")
           ),  
          
#' Conditional panels only exist if the javascript expression passed to the condition
#' argument is true. If the expression is false, nothing inside `conditionalPanel()` 
#' will appear in the app, nor will it take up space in the interface. In this tab, each
#' conditional panel contains a menu of options for one of the ergm terms and should only
#' show up if a dataset has been uploaded AND the corresponding term has been chosen from
#' the terms menu. To ensure that a dataset has been uploaded we will check that
#' `output.nwsum` does not contain the string "NA". We check this output object to avoid checking 
#' both `input.rawdatafile` and `input.rawdatamx` and because the reactive expression `nwreac()`
#' is not in the DOM, so we can't check its value using javascript.
#' [This page](http://bonsaiden.github.io/JavaScript-Garden/#types) describes some 
#' important considerations involving equalities, types and classes in javascript. Next, there is no
#' javascript function analagous to `is.element` in R, but the JS `indexOf` will return
#' -1 if an element is not within the specified list, so we use this to check if 
#' the term has been selected. 
#' 
#+ eval=FALSE                                          
          fluidRow(
            column(2,
               p('Current network:', verbatimTextOutput('currentdataset1'))),
            column(10,
               p('Current ergm formula:'),
               verbatimTextOutput('checkterms1'))
          ),
         fluidRow(
           column(10, offset=2,
                  p('Summary Statistics:'),
                  verbatimTextOutput('prefitsum'))
           ),
         fluidRow(
           column(10,
                  textInput(inputId="terms", label="Type in term(s) and their arguments. For multiple terms, separate with '+'.",
                            value="edges"),
                  actionButton('addtermButton', 'Add'),
                  actionButton('resetformulaButton', 'Reset Formula'),
                  actionButton('fitButton', 'Fit Model')
                  ),
           column(2,
                  actionButton('termdocButton', 'Term Documentation'))),
         wellPanel(class= 'docpopup', 
          fluidRow(
              column(3,
                     h5('Term Documentation'),
                     radioButtons('matchingorall', label='Choose from:',
                                  choices=c('Terms compatible with current network',
                                            'All terms')),
                     uiOutput('listofterms')),
              column(9, 
                    verbatimTextOutput('termdoc'))
                  )),
         
         
         br(),
         tags$hr(),
         h4('Model Summary'),
         p('Check for model degeneracy in the "Diagnostics" tab.'),
         br(),
         tabsetPanel(id = 'fittingTabs',
           tabPanel('Fitting',
                    verbatimTextOutput('modelfit')),
           tabPanel('Summary',
                    verbatimTextOutput('modelfitsum'))
          ),
    helperButton(id = 'tab3help'),
    div(class="helper-box", style="display:none",
      p('Consult the "Term Documentation" box to learn about each term and its arguments',
        '(the box can be collapsed or expanded with the button on the right', 
        'of the page). The "Fitting" tab shows MCMC iterations (if any) and MLE coefficients,',
        'while the "Summary" tab shows a comprehensive summary of the model fit.'))
          ),
#' **MCMC Diagnostics**
#' 
#+ eval=FALSE   
#          navbarMenu('Diagnostics',         
                    tabPanel('MCMC Diagnostics',
                             #include progress bar when this tab is loading
                             div(class = "busy", 
                                 p("Calculation in progress..."),
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
                                        uiOutput('diagnosticsplotspace'),
                                        downloadButton('mcmcplotdownload',label = 'Download Plots')),
                               tabPanel('Summary', 
                                        verbatimTextOutput('diagnostics'))
                             ),
                             helperButton(id = 'tab5help'),
                             div(class="helper-box", style="display:none",
                                 p('Check for model degeneracy. When a model converges properly',
                                   'the MCMC sample statistics should vary randomly around the',
                                   'observed values at each step, and the difference between the',
                                   'observed and simulated values of the sample statistics should',
                                   'have a roughly bell shaped distribution, centered at 0.'))
                             
                    ),
#' **Goodness of Fit**
#' 
#+ eval=FALSE  
            tabPanel('Goodness of Fit',
                     
                     #include progress bar when this tab is loading
                     div(class = "busy", 
                         p("Calculation in progress..."),
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
                            verbatimTextOutput('gofsummary')),  
                     column(7,
                            uiOutput('gofplotspace'),
                            downloadButton('gofplotdownload', label = 'Download Plots'))),
                     helperButton(id = 'tab4help'),
                     div(class="helper-box", style="display:none",
                         p('Test how well your model fits the original data by choosing a network',
                            'statistic that is not in the model, and comparing the value of this',
                            'statistic observed in the original network to the distribution of values',
                            'you get in simulated networks from your model.'))
                     ),

#             ),
#' **Simulations**
#' 
#+ eval=FALSE  
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
                                         value = 1),
                            actionButton('simButton', 'Simulate')
                     ),
                     column(8, 
                            numericInput('thissim',
                                        label = 'Choose a simulation to plot',
                                        min = 1,
                                        value = 1)
                            )
                   ),
                   fluidRow( 
                     column(3,
                            br(),
                            wellPanel(
                              h5('Display Options'),
                              checkboxInput('iso2',
                                            label = 'Display isolates?', 
                                            value = TRUE),
                              checkboxInput('vnames2',
                                            label = 'Display vertex names?',
                                            value = FALSE),
                              uiOutput('dynamiccolor2'),
                              uiOutput('dynamicsize2'),
                              downloadButton('simplotdownload',
                                             label = 'Download Plot')
                              )
                            ),  
                     column(8,
                            plotOutput('simplot'),
                            verbatimTextOutput('sim.summary')
                            )
                     ),
                   helperButton(id = 'tab6help'),
                   div(class="helper-box", style="display:none",
                       p('Choose how many simulations to run and click "Simulate".',
                         'Plot any of the simulations, edit the display',
                         'options and download a PDF of the plot.'))
                   ),
#' **Help**
#' 
#+ eval=FALSE  
  tabPanel('Help',
           h4('Resources'),
           a("statnet Wiki",
             href = "https://statnet.csde.washington.edu/trac", target = "_blank"),
           br(),
           a("ergm: Journal of Statistical Software",
             href = "http://www.jstatsoft.org/v24/i03/", target = "_blank"),
           br(),
           a("Using ergm: Journal of Statistical Software",
             href = "http://www.jstatsoft.org/v24/i04/", target = "_blank"),
           br(),
           a("ergm documentation on CRAN", 
             href = "http://cran.r-project.org/web/packages/ergm/ergm.pdf",
             target = "_blank"),
           br(),
           hr(),
           p("The best way to contact us with questions, comments or suggestions",
             "is through the", strong("statnet users group"), "listserv."),
           p("To post and receive messages from this listserv, you need to join.",
             "Instructions are at:", 
             a("https://mailman.u.washington.edu/mailman/listinfo/statnet_help",
               href = "https://mailman.u.washington.edu/mailman/listinfo/statnet_help",
               target = "_blank")),
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
               href = "https://mailman.u.washington.edu/mailman/private/statnet_help",
               target = "_blank")),
           br(),
           hr(),
           p("This web app is built with", a("Shiny",href="http://shiny.rstudio.com/",
                                             target = "_blank")),
           p("Author of app: Emily Beylerian, University of Washington")
           )
                  
    
    ))
  )
    