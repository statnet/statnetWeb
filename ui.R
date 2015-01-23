#' ---
#' title: "statnetWeb, ui.R"
#' author: "Emily Beylerian"
#' ---
#' statnetWeb
#' ============
#' ui.R
#' ============

#' **Before reading this document:** The Shiny app "statnetWeb" is not contained in a
#' single R Script. Within the folder "statnetWeb" the script `ui.R` controls the 
#' layout and appearance of the app, the script `server.R` controls the content that
#' gets displayed in the app, and the folder "www" contains auxiliary files (javascript,
#' css, and image files). If you are unfamiliar with Shiny apps, it may be more 
#' natural and helpful to start with the documentation for `ui.R` and then move on to
#' `server.R`.
#' 
#' **Basics**
#' 
#' The R functions inside `ui.R` output HTML code, which Shiny turns into a webapp. 
#' Widgets are specific functions in Shiny that correspond to elements of the UI that the
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
#' Before the call to `shinyUI`, make sure that necessary packages are loaded
#' and create any custom widgets that we will use in the app.(Custom widgets are
#' in global.R now)
#+ setup, eval=FALSE
# load necessary packages
library(shiny)
library(shinyBS)
library(shinyAce)
#source("chooser.R") #need this for Kirk's widget that moves items left/right


#' Everything that gets displayed inside the app is enclosed in a call to `shinyUI`.
#' The first thing to be specified is the type of page to display. The `navbarPage` 
#' includes a navigation bar at the top of the page and each tab leads to different 
#' pages of content. Find out more about layout options 
#' [here](http://shiny.rstudio.com/articles/layout-guide.html).
#'
#+ eval=FALSE 
shinyUI(
  navbarPage(title=NULL, 
             id= 'navbar', windowTitle = 'statnetWeb', collapsable=TRUE,
             
#' Within each panel of the navbar, the content can be arranged by nesting rows and
#' columns. The first argument to `column` is the desired width, where the whole
#' browser window has a width of 12. Within any column, nested columns set their 
#' width relative to the parent column. Rows are specified by enclosing elements
#' in `fluidRow()`. It is often necessary to specify rows even when elements seem like
#' they should naturally be aligned horizontally, or when a `wellPanel` that is supposed
#' to hold some content doesn't quite enclose everything correctly.
#' 
#' **Front Page (About)**
#' 
#' This page might move to the last tab to be combined with the Help Page.
#' 
#+ eval=FALSE
  tabPanel(title=span('statnetWeb', style='font-family:Courier; font-size:14pt;'),
           value='tab1',
           fluidRow(
                    column(8,
                           h5("About statnetWeb v0.3.0"),
                           p("Welcome to our prototype web interactive interface for the", strong("ergm"),
                             "package.", strong("ergm"), "is part of the statnet network analysis software --",
                             "a suite of packages written in R -- and this app also includes some of the functionality",
                             "from the associated packages", strong("network"), " and ", strong("sna"), ".  This web app", 
                             "is written in R-Shiny, and development is via Github.  More information on the statnet software,",
                             "the ergm package, R-Shiny and our Github repository can be found in the resource links on the right."),
                           p("This app is intended to serve as an introduction to the ergm package for those who",
                             "are just getting started using statnet, or for those who are not familiar with programming",
                             "in R. If you are new to ergm, you may find it helpful to work through the ergm tutorial using",
                             "this app (link is on the right). Advanced users will still want to interact via the command line",
                             "in order to access the full functionality of ergm."),
                           p("A typical network analysis will move sequentially through the tabs at the top of the page.",
                             "Click on the help icon at the top of any page for guidance."),
                           p("Do you have comments/suggestions/complaints on this prototype app? Please share them with us.",
                             "They are best submitted through our", a('Github site,', 
                                                                      href='https://github.com/statnet/statnetWeb',
                                                                      target='_blank'),
                             "or by email to the statnet_help listserv (see Help tab)."),
                           actionButton('startButton', label='Get Started', class="candisable"),
                           br(),
                           
                           h5('Citing statnetWeb'),
                           p('If you use statnet or statnetWeb, please cite them. BibTeX entries are below.'),
                           
                           span(id='swciteButton', 'statnetWeb'),
                           span(id='sciteButton', 'statnet'), br(),br(),
                                  

tags$pre(id='swcitation','@Unpublished{beylerian:statnetWeb,
title = {statnetWeb: An R-Shiny interface for statnet network analysis software},
author = {Emily Beylerian and Martina Morris},
year = {2014},
address = {Seattle, WA},
url = {https://github.com/statnet/statnetWeb}
}'),

tags$pre(id='scitation','@Manual{handcock:statnet,
title = {statnet: Software tools for the Statistical Modeling of Network Data},
author = {Mark S. Handcock and David R. Hunter and Carter T. Butts and Steven M. Goodreau and Martina Morris},
year = {2003},
address = {Seattle, WA},
url = {http://statnetproject.org}
}'),
                           
                           p('Additional citation information for statnet',
                             'and the component packages can be found here:'),
                           tags$ul(
                             tags$li(a('Citing statnet', 
                                       href='https://statnet.csde.washington.edu/trac/wiki/citation%20information',
                                       target='_blank')),
                             tags$li(a('License and source code attribution requirements',
                                       href = 'http://statnet.csde.washington.edu/attribution.shtml',
                                       target = '_blank')),
                             tags$li(a('statnet Development Team',
                                     href = 'http://statnet.csde.washington.edu/about_us.shtml',
                                     target = '_blank'))
                    )),       
             column(4, wellPanel( 
                          h5('Resources'),
                          a("statnet Wiki",
                            href = "https://statnet.csde.washington.edu/trac", target = "_blank"),
                          column(11, offset = 1, p('The homepage of the statnet project. Find tutorials,',
                                'publications and recent news here.'),
                                span('Key background papers'),br(),
                                a("ergm: Journal of Statistical Software",
                                  href = "http://www.jstatsoft.org/v24/i03/", target = "_blank"),
                                br(),
                                a("Using ergm: Journal of Statistical Software",
                                  href = "http://www.jstatsoft.org/v24/i04/", target = "_blank"),
                                br(),br(),
                                span('Tutorials and documentation'),br(),
                                a("ergm tutorial from NME 2014 Workshop",
                                  href = "http://statnet.csde.washington.edu/EpiModel/nme/2014/d2-tut1.html",
                                  target= "_blank"),
                                br(),
                                a("ergm documentation on CRAN", 
                                  href = "http://cran.r-project.org/web/packages/ergm/ergm.pdf",
                                  target = "_blank"), style="margin-bottom:10px;"),
                          br(),
                          p(a("statnetWeb Github repository", href="https://github.com/statnet/statnetWeb",
                            target="_blank")),
                          a("Shiny: a web application framework for R", href="http://shiny.rstudio.com/",
                            target="_blank")
             ),
             fluidRow(img(src= 'UW.Wordmark_ctr_K.jpg', width=200)),
             fluidRow(a(img(src = 'csdelogo_crop.png', height = 40, width = 40),
                       href = 'https://csde.washington.edu/', target = '_blank'),
                      a(img(src = 'csde_goudy.fw.png', width=150), href = 'https://csde.washington.edu/',
                       target = '_blank'))
             )           
             )
           ),
#' 
#' **Data Upload**
#' 
#' Before the code for what is displayed on the Data Upload page,
#' various javaScript and CSS files that will be useful later in the
#' script are linked. For example, since network plotting and model 
#' fitting do not happen instantly (especially for large networks),
#' a loading icon will help to assure users that the app is still working 
#' on producing output. The file `busy.js` controls the behavior of the 
#' loading message and `style.css` controls the appearance. To display 
#' the loading message on subsequent tabs, we only need to include the 
#' `div` statement within those tabs.
#+ eval=FALSE

  tabPanel(title='Data', value='tab2',
           #busy.js is for calculation in progress boxes
           #alert.js is for popup boxes, 
           #jquery libraries are loaded from google cdn, needed for autocomplete
           #this tagList command has to go inside a tabPanel
           tagList(
             tags$head(
               tags$link(rel="stylesheet", type="text/css",href="style.css"),
               #tags$link(rel="stylesheet", type="text/css",href="autocomplete.css"),
               tags$link(rel="stylesheet", type="text/css",
                         href="//ajax.googleapis.com/ajax/libs/jqueryui/1.11.1/themes/smoothness/jquery-ui.css"),
               tags$script(type="text/javascript", src="//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"),
               #tags$script(type="text/javascript", src="autocomplete.js"),
               tags$script(type="text/javascript", src="//ajax.googleapis.com/ajax/libs/jqueryui/1.11.1/jquery-ui.min.js"),
               tags$script(type="text/javascript", src="busy.js"),
               tags$script(type="text/javascript", src="alert.js"),
               tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",
                                function(message) {
                                console.log(message)
                                eval(message.code);
                                });'))
             )
           ),
#' Conditional panels are only displayed when a specified condition is true.
#' The condition is a javascript expression that can refer to the current
#' values of input or output objects. When the condition is false, the panel
#' does not take up any space in the UI.  
           
     
    fluidRow(
      column(7,
        tabsetPanel(id='datatabs',
          tabPanel('Upload Network',
                   wellPanel(
                     fluidRow(
                       column(6,
                              radioButtons('filetype',label=h5('File type'),
                                           choices=c('pre-loaded sample network'=5, 'statnet network object (R-object)'=1,
                                                     'Pajek network (*.net)'=2,'Pajek project (*.paj)'=3,
                                                     'matrix of relational data (*.csv or R-object)'=4)),
                              p(class="helper", id="Robjhelp", icon("question-circle"), span("What is an R-object?", style="font-size:0.85em;")),
                              div(class="mischelperbox", id="Robjbox", 'When working in R, an object in your environment',
                                  'can be saved to a file from the command line in the following way:', 
                                  code('save(objectname, file="newfilename")'),br(),'By default the file will be saved',
                                  'into the current working directory. The full path to a new location can be',
                                  'specified in the ', code('file='), 'argument, or set', code('file=file.choose(new=TRUE)'),
                                  'to use a save dialog box.')),
                       conditionalPanel(condition = 'input.filetype < 5',
                         column(6,
                              br(),
                              fileInput(inputId='rawdatafile', label=NULL, accept='text'),
                              verbatimTextOutput('rawdatafile'))
                          ),
                       conditionalPanel(condition = 'input.filetype == 5',
                              br(),
                              selectInput('samplenet', label='Choose a network',
                                          choices=c('None', 'ecoli1', 'ecoli2',
                                                    'faux.mesa.high','flobusiness',
                                                    'flomarriage', 'kapferer', 'kapferer2',
                                                    'molecule', 'samplike'),
                                          selectize=FALSE) 
                                        )
                       ),
                     fluidRow(
                       conditionalPanel(condition='input.filetype == 4',
                           h5('Specify'),
                           column(1, align="right", 
                                  style="margin-top:5px;margin-left:0px;",
                                  br(),br(),
                                  span(style="line-height:25px;", class="helper",
                                      span(id="filetypehelper1",
                                           icon('question-circle'),
                                           div(id="filetypebox1", class="smallhelperbox",
                                               "For adjacency matrices,",
                                               "the first row and column of .csv files",
                                               "should hold vertex labels.")),
                                      br(),
                                      span(id="filetypehelper2",
                                           icon('question-circle'),
                                           div(id="filetypebox2", class="smallhelperbox",
                                               "For adjacency matrices,",
                                               "the first row and column of .csv files",
                                               "should hold vertex labels.")),
                                      br(),
                                      span(id="filetypehelper3",
                                           icon('question-circle'),
                                           div(id="filetypebox3", class="smallhelperbox",
                                               "For incidence matrices,",
                                               "the first row of .csv files should hold",
                                               "edge labels, the first column should hold vertex labels")),
                                      br(),
                                      span(id="filetypehelper4",
                                           icon('question-circle'),
                                           div(id="filetypebox4", class="smallhelperbox",
                                               "For edge lists, .csv files should not have row or column labels.")),

                                      br())),
                           column(4,
                                  br(),
                                  radioButtons('matrixtype', label='Matrix Type',
                                               choices=c('Adjacency matrix'='adjacency', 
                                                         'Bipartite adjacency matrix'='bipartite',
                                                         'Incidence matrix' = 'incidence', 
                                                         'Edge list' = 'edgelist'))),
                           
                           column(5,
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
                     ))
                   ),
          tabPanel('Edit Network',
                   wellPanel(
                     fluidRow(
                      
                       column(6,strong('Symmetrize'),
                          conditionalPanel(condition="output.nwsum != 'NA'",
                              radioButtons('symmetrize', label=NULL, 
                                           choices=c('Do not symmetrize',
                                                     'upper: Copy upper triangle over lower triangle'='upper',
                                                     'lower: Copy lower triangle over upper triangle'='lower',
                                                     'strong: Intersection of ties'='strong',
                                                     'weak: Union of ties'='weak')),
                              bsButtonGroup(inputId='aftersymm',
                                            label='After symmetrizing, network should be',
                                            toggle='radio', value='directed',
                                            bsButton('symmdir', label='directed', value='directed'),
                                            bsButton('symmundir', label='undirected', value='undirected'))
                              )),
                       
                       column(5,strong('Import new attribute information'),
                              conditionalPanel(condition="output.nwsum != 'NA'",
                                 radioButtons('newattrtype', label='Choose attribute type',
                                           choices=c('vertex attribute',
                                                     'vertex names',
                                                   'edge attribute',
                                                   'edge value')),
                                 span('Upload a file of one of the following types:',br(),
                                      tags$ul(
                                        tags$li('R list object',
                                      span(class="helper",id="filetypehelper5",
                                           icon("question-circle"),
                                           div(id="filetypebox5", class="mischelperbox",
                                               strong("R lists"), "should be named, for example:",
                                               br(),
                                               code("mylist <- list()"),br(),
                                               code("mylist$age <- c(18,27,20)"),br(),
                                               code("mylist$sex <- c('M','F','F')"), br(),
                                               "The names of the elements in the list (e.g. 'age'",
                                               "and 'sex' in the list above)",
                                               "will become the attribute names.",
                                               br(),br(),tags$u("Note:"),'Attributes uploaded as vertex names',
                                               "will automatically be saved into the", code("vertex.names"),
                                               "attribute and the uploaded names will be ignored."))),
                                      tags$li('.csv file',
                                      span(class="helper", id="filetypehelper6",
                                           icon("question-circle"),
                                           div(id="filetypebox6", class="mischelperbox", 
                                               strong(".csv files"), "should include a header in the first row.",
                                               "The header of each column will become an attribute name.",
                                               br(),br(),tags$u("Note:"),'Attributes uploaded as vertex names',
                                               "will automatically be saved into the", code("vertex.names"),
                                               "attribute and the uploaded names will be ignored."))))),
                                 fileInput(inputId='newattrvalue', label=NULL),
                                 p('New attribute name(s):'),
                                 verbatimTextOutput('newattrname'),
                                 actionButton('newattrButton', label='Set Attribute')              
                                 )
                              
                              )
                       
                     )
                     )
                   ),
          tabPanel('Modify Attributes',
                   wellPanel(
                     p('In the future we will build in functions that will ',
                       'allow you to modify the attributes of your network.',
                       'This will include options like:'),
                     tags$ul(
                       tags$li('Applying a function (e.g.', code('sqrt()'), ') to an attribute'),
                       tags$li('Recoding (mapping a set of attributes onto a new set)'),
                       tags$li('Conditional transformations (', code('do...if...'),')'))
                     #uiOutput('modifyattrchooser')
                     )
                   )
          )
      ),
          
   column(4,
          tabsetPanel(
            tabPanel('Network Summary',
                     verbatimTextOutput('nwsum')
                     ))
          )            
    ),
  
#   aceEditor("dataAce", mode="r", theme="textmate", readOnly=TRUE,
#             value="", height="200px"),

   icon('question-circle', class='fa-2x helper-btn'),
   div(class="helper-box", style="display:none",
       p('Upload a file of observed network data (must be of a supported type).', 
         'Add custom attributes or symmetrize on the "Edit Network" tab.')),
  actionLink('dataleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
  actionLink('dataright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
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

  tabPanel(title='Network Descriptives', value='tab3',
     #include progress box when this tab is loading
     div(class = "busy", 
         p("Calculation in progress..."),
         img(src="ajax-loader.gif")
     ),
 
    fluidRow(      
     column(7,
        tabsetPanel(id='plottabs',
          tabPanel('Network Plot',
            plotOutput('nwplot')
            ),
          tabPanel('Degree Distribution',
                   p(class='helper', id='ddhelper', icon('question-circle')),
                   div(class='mischelperbox', id='ddhelperbox',
                       "Degrees are a node level measure for the number of edges incident on each node.",
                       "The degree distribution shows how the edges in a network are distributed among the",
                       "nodes. The amount (or proportion) of nodes with low, medium",
                       "or high degrees contribute to the overall structure of the",
                       "network. The degree distributions of directed graphs can",
                       "be subset by in-degree or out-degree."),
                   plotOutput('degreedist')
                   ),
          tabPanel('Geodesic Distribution',
                   p(class='helper', id='gdhelper', icon('question-circle')),
                   div(class='mischelperbox', id='gdhelperbox',
                       "Geodesics are a dyad level measure for the shortest possible path between a pair of nodes.",
                       'If there is no path between a pair of nodes, the geodesic distance is "inf".', 
                       "The geodesic distribution among all possible dyads contributes to the",
                       "structure of network connectivity."),
                   plotOutput('geodistplot')
                   ),
          tabPanel('More', value='More',
                   h5('Mixing Matrix', icon('angle-double-left'), 
                      id="mixmxtitle"),
                   wellPanel(id="mixmxbox",
                     uiOutput('mixmxchooser'),
                     verbatimTextOutput('mixingmatrix')
                   ),
                   h5('Graph-level descriptive indices',
                      icon('angle-double-left'), id="graphleveltitle"),
                   wellPanel(id="graphlevelbox",
                     fluidRow(
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
                      column(3, p(textOutput('gden'), class='snum'))), 
                     
                     fluidRow(
                      column(4, p('Degree:', class='stitle')),
                      column(3, p(textOutput('gdeg'), class='snum')),
                      column(4, inlineSelectInput('gdegcmode', label=NULL,
                                                 choices=c('indegree', 'outdegree', 'total'),
                                                 style='margin-top:0px;'))),
                     fluidRow(
                      column(4, p('Betweenness:', class='stitle')),
                      column(3, p(textOutput('gbetw'), class='snum')),
                      column(4, inlineSelectInput('gbetwcmode', label=NULL,
                                                 choices=c('directed','undirected',
                                                           'endpoints','proximalsrc',
                                                           'proximaltar','proximalsum',
                                                           'lengthscaled', 'linearscaled'),
                                                 style='margin-top:0px;'))),
                     fluidRow(
                      column(4, p('Closeness:', class='stitle')),
                      column(3, p(textOutput('gclose'), class='snum')),
                      column(4, inlineSelectInput('gclosecmode', label=NULL,
                                                 choices=c('directed','undirected',
                                                           'suminvdir','suminvundir'),
                                                 style='margin-top:0px;'))),
                     fluidRow(
                       column(4, p('Stress Centrality:', class='stitle')),
                       column(3, p(textOutput('gstress'), class='snum')),
                       column(4, inlineSelectInput('gstresscmode', label=NULL,
                                                   choices=c('directed','undirected'),
                                                   style='margin-top:0px;'))
                     ),
                     fluidRow(
                       column(4, p('(Harary) Graph Centrality:', class='stitle')),
                       column(3, p(textOutput('ggraphcent'), class='snum')),
                       column(4, inlineSelectInput('ggraphcentcmode', label=NULL,
                                                   choices=c('directed', 'undirected'),
                                                   style='margin-top:0px;'))
                     ),
                     fluidRow(
                       column(4, p('Eigenvector Centrality:', class='stitle')),
                       column(3, p(textOutput('gevcent'), class='snum')),
                       column(4, br())
                     ),
                     fluidRow(
                       column(4, p('Information Centrality:', class='stitle')),
                       column(3, p(textOutput('ginfocent'), class='snum')),
                       column(4, inlineSelectInput('ginfocentcmode',label=NULL,
                                                   choices=c('weak', 'strong', 'upper',
                                                             'lower'), style='margin-top:0px;'))
                     )
                     
                     
                   )),
                   
                   h5('Node-level descriptive indices',
                      icon('angle-double-left'), id="nodeleveltitle"),
                   wellPanel(id="nodelevelbox",
                     span('Input node index:'),
                     numericInput('nodeind', label=NULL, value=1,
                                min=1),
                     tags$hr(),
                     fluidRow(
                       column(2, offset=3, tags$u('Current node')),
                       column(3, tags$u('Centrality mode')),
                       column(2, tags$u('Min')),
                       column(2, tags$u('Max'))),
                       fluidRow(
                         column(3, p('Degree:', class='stitle')),
                         column(2, p(textOutput('ndeg'), class='snum')),
                         column(3, inlineSelectInput('ndegcmode', label=NULL,
                                                     choices=c('indegree', 'outdegree', 'total'),
                                                     style='margin-top:5px;')),
                         column(2, p(textOutput('ndegmin'), class='snum', align='center')),
                         column(2, p(textOutput('ndegmax'), class='snum', align='center'))
                         ),
                       fluidRow(
                         column(3, p('Betweenness:', class='stitle')),
                         column(2, p(textOutput('nbetw'), class='snum')),
                         column(3, inlineSelectInput('nbetwcmode', label=NULL,
                                                     choices=c('directed','undirected',
                                                               'endpoints','proximalsrc',
                                                               'proximaltar','proximalsum',
                                                               'lengthscaled', 'linearscaled'),
                                                     style='margin-top:0px;')),
                         column(2, p(textOutput('nbetwmin'), class='snum')),
                         column(2, p(textOutput('nbetwmax'), class='snum'))
                         ),
                       fluidRow(
                         column(3, p('Closeness:', class='stitle')),
                         column(2, p(textOutput('nclose'), class='snum')),
                         column(3, inlineSelectInput('nclosecmode', label=NULL,
                                                     choices=c('directed','undirected',
                                                               'suminvdir','suminvundir'),
                                                     style='margin-top:0px;')), 
                         column(2, p(textOutput('nclosemin'))),
                         column(2, p(textOutput('nclosemax')))
                         ),
                       fluidRow(
                         column(3, p('Stress Centrality:', class='stitle')),
                         column(2, p(textOutput('nstress'), class='snum')),
                         column(3, inlineSelectInput('nstresscmode', label=NULL,
                                                     choices=c('directed','undirected'),
                                                     style='margin-top:0px;')),
                         column(2, p(textOutput('nstressmin'))),
                         column(2, p(textOutput('nstressmax')))
                         ),
                       fluidRow(
                         column(3, p('(Harary) Graph Centrality:', class='stitle')),
                         column(2, p(textOutput('ngraphcent'), class='snum')),
                         column(3, inlineSelectInput('ngraphcentcmode', label=NULL,
                                                     choices=c('directed', 'undirected'),
                                                     style='margin-top:0px;')),
                         column(2, p(textOutput('ngraphcentmin'))),
                         column(2, p(textOutput('ngraphcentmax')))
                         ),
                       fluidRow(
                         column(3, p('Eigenvector Centrality:', class='stitle')),
                         column(2, p(textOutput('nevcent'), class='snum')),
                         column(3, br()),
                         column(2, p(textOutput('nevcentmin'))),
                         column(2, p(textOutput('nevcentmax')))
                         ),
                       fluidRow(
                         column(3, p('Information Centrality:', class='stitle')),
                         column(2, p(textOutput('ninfocent'), class='snum')),
                         column(3, inlineSelectInput('ninfocentcmode',label=NULL,
                                                     choices=c('weak', 'strong', 'upper',
                                                               'lower'), style='margin-top:0px;')),
                         column(2, p(textOutput('ninfocentmin'))),
                         column(2, p(textOutput('ninfocentmax')))
                         )
                     ))
                   
                   
                   
          )),
     column(4,
         tabsetPanel(id='displaytabs',
           tabPanel(title='Display Options',
              wellPanel(
                    conditionalPanel(condition='input.plottabs == "Network Plot"',
                                     checkboxInput('iso',
                                                   label = 'Display isolates', 
                                                   value = TRUE),
                                     checkboxInput('vnames',
                                                   label = 'Display vertex names',
                                                   value = FALSE),
                                     br(),
                                     sliderInput('transp',
                                                 label = 'Vertex opacity',
                                                 min = 0, max = 1, value = 1),
                                     br(),
                                     uiOutput('dynamiccolor'),
                                     span(bsAlert(inputId = 'colorwarning'), style='font-size: 0.82em;'),
                                     uiOutput('dynamicsize'),
                                     br(),
                                     downloadButton('nwplotdownload', label = "Download Plot")),
                    conditionalPanel(condition='input.plottabs == "Degree Distribution"',
                                     selectInput('cmode', 
                                                 label = 'Type of degree (for directed graphs):',
                                                 choices= c('total' = 'freeman',
                                                            'indegree',
                                                            'outdegree'),
                                                 selected = 'freeman',
                                                 selectize=FALSE),
                                     uiOutput('dynamiccolor_dd'),
                                     span(bsAlert(inputId = 'colorwarning_dd'), style='font-size: 0.82em;'),
                                     bsButtonGroup(inputId='densplotgroup',
                                                   label='Y-axis units:',
                                                   toggle='radio', value='count',
                                                   bsButton('freqplot', label = 'Count of nodes', value='count'),
                                                   bsButton('densplot', label = 'Percent of nodes', value='percent')),
                                     br(), br(),
                                     p('Expected values of null models:'),
                                     fluidRow(
                                       column(10,
                                              checkboxInput('uniformoverlay_dd', 
                                                     label='Conditional uniform graphs (CUG)', 
                                                     value=FALSE)
                                              ),
                                       
                                        span(icon('question-circle'), id="cughelper_dd", class="helper",
                                             div(id="cughelperbox_dd", class="mischelperbox",
                                                 "Draws from the distribution of simple random graphs with the same",
                                                 "fixed density as the observed network. The mean and 95% confidence",
                                                 "intervals for each degree are plotted."))),
                                     fluidRow(
                                       column(10,
                                              checkboxInput('bernoullioverlay_dd',
                                                     label='Bernoulli random graphs (BRG)',
                                                     value=FALSE)
                                              ),
                                       span(icon('question-circle'), id="brghelper_dd", class="helper",
                                            div(id="brghelperbox_dd", class="mischelperbox",
                                                "Draws from the distribution of simple random graphs with the same", 
                                                "stochastic tie probability as the observed network.",
                                                "The mean and 95% confidence intervals for each degree are plotted."))),
                                     br(),
                                     downloadButton('degreedistdownload', label = "Download Plot")
                      ),
                    conditionalPanel(condition='input.plottabs == "Geodesic Distribution"',
                                     bsButtonGroup(inputId='densplotgroup_gd',
                                                   label='Y-axis units:',
                                                   toggle='radio', value='count',
                                                   bsButton('freqplot_gd', label = 'Count of vertex pairs', value='count'),
                                                   bsButton('densplot_gd', label = 'Percent of vertex pairs', value='percent')),
                                     br(), br(),
                                     p('Expected values of null models:'),
                                     fluidRow(
                                       column(10,
                                         checkboxInput('uniformoverlay_gd', 
                                                     label='Conditional uniform graphs (CUG)', 
                                                     value=FALSE)     
                                              ),
                                       span(icon('question-circle'), id="cughelper_gd", class="helper",
                                            div(id="cughelperbox_gd", class="mischelperbox",
                                                "Draws from the distribution of simple random graphs with the same",
                                                "fixed density as the observed network. The mean and 95% confidence",
                                                "intervals for each degree are plotted."))),
                                     fluidRow(
                                       column(10,
                                         checkboxInput('bernoullioverlay_gd',
                                                   label='Bernoulli random graphs (BRG)',
                                                   value=FALSE)     
                                              ),
                                       span(icon('question-circle'), id="brghelper_gd", class="helper",
                                            div(id="brghelperbox_gd", class="mischelperbox",
                                                "Draws from the distribution of simple random graphs with the same", 
                                                "stochastic tie probability as the observed network.",
                                                "The mean and 95% confidence intervals for each degree are plotted."))),
                                     br(),
                                     verbatimTextOutput('infsummary'),
                                     fluidRow(
                                       column(10,
                                              checkboxInput('excludeInfs', 
                                                            label=span('Exclude "inf"s from plot'), 
                                                            value=FALSE)),
                                       span(icon('question-circle'), id="infhelper_gd", class="helper",
                                            div(id="infhelperbox_gd", class="mischelperbox",
                                                "A pair of nodes without any path connecting",
                                                'it has a geodesic distance of "inf".'))),
                                     br(),
                                     downloadButton('geodistdownload', label= 'Download Plot')
                      ),
                    conditionalPanel(condition='input.plottabs == "More"',
                                     p("No display options at this time,",
                                       "stay tuned for updates!")
                                     )
                    )),
           tabPanel(title='Network Summary',
            verbatimTextOutput('attr2'))
            )
         )
     ),
    div(id='plottabhelp', class='helper-btn', icon('question-circle', 'fa-2x')),
     div(class="helper-box", style="display:none",
         p('Use the network plots to gain insight to the observed network.', 
           'Edit the display options in the panel on the right and download a PDF of any of the plots.')),
    actionLink('plotleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
    actionLink('plotright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
    ),

    

#' **Fit Model**
#' 
#' The output objects for the current dataset and current formula have default values 
#' specified in server.R to prevent errors from NULL values and so that there are 
#' helpful messages for the user before they begin entering data.
#+ eval=FALSE                  
      tabPanel(title='Fit Model',value='tab4',

          #include progress bar when this tab is loading
           div(class = "busy", 
               p("Calculation in progress..."),
               img(src="ajax-loader.gif")
           ),  
                                                  
          fluidRow(
            column(2,
               p('Network:', class="nwlabel"), 
               verbatimTextOutput('currentdataset1')
              ),
            
            column(5,
                   p("Type in term(s) and their arguments. For multiple terms, separate with '+'. "),
                   fluidRow(
                      fluidRow(
                       textInput(inputId="terms", label=NULL,
                                 value="edges"),
                       actionButton('fitButton', 'Fit Model')
                       ),
                       fluidRow(actionButton('addtermButton', 'Add Term(s)'),
                                actionButton('resetformulaButton', 'Reset Formula'),
                                div(class="helper chromewarning", icon('warning'),"Note for Chrome users", 
                                    style="font-size:.80em; display:inline;",
                                    div(class="chromewarningbox",
                                        p("When running statnetWeb on the Google Chrome browser",
                                          "we have noticed an occasional delay between clicking the 'Add Term(s)' button and",
                                          "the terms populating the formula. You don't need to type the terms again,",
                                          "they will be added on your next click of the button. Thank you",
                                          "for your patience as we look for a solution!"),
                                        p(strong('Update: '), 'We believe this problem has been fixed. If you',
                                          'still experience a delay please let us know by filing an issue on Github.')))
                                )
                   )
            ),
            column(5,
               tabsetPanel(
                 tabPanel("Term Documentation",
                  div(class="placeholder",
                  fluidRow(
                           bsButtonGroup("matchingorall", label=NULL, toggle="radio", 
                                 value="matching",
                                 bsButton("matchingButton",label="Compatible Terms", value="matching"),
                                 bsButton("allButton",label="All Terms", value="all")),
                           uiOutput("listofterms")
                           ),
                  div(id="termdocbox",
                    verbatimTextOutput("termdoc")
                    ),
                  div(id="termexpand",
                          icon(name="expand"))
                  )
                 ),
                 tabPanel("Control Options",
                    div(class="placeholder",
                    fluidRow(
                      column(3,
                        inlineSelectInput('controltype',label=NULL, 
                                          choices=c("MCMC",
                                                    "MCMLE",
                                                    "Other"))),
                      column(4,
                        checkboxInput('controldefault','Use default options', value=TRUE))
                    ),
                        conditionalPanel(condition="input.controltype == 'MCMC'",
                          fluidRow(
                            div(class="tool", span(class="tip","Number of proposals between sampled statistics.", 
                                                   img(src="callout2.png",class="callout")),
                                span("Interval:"),
                                customNumericInput('MCMCinterval',label=NULL, value=1024, class="mcmcopt input-mini")),
                            
                            div(class="tool",span(class="tip","Number of proposals before any MCMC sampling is done. Defaults to 16 times the MCMC interval, unless burn-in is specified after the interval.", 
                                                  img(src="callout2.png",class="callout")),  
                              span("Burn-in:"),
                              customNumericInput('MCMCburnin', label=NULL, value=16384, class="mcmcopt input-mini")),
                            
                            div(class="tool",span(class="tip","Number of network statistics, randomly drawn from a given distribution on the set of all networks, returned by the Metropolis-Hastings algorithm.", 
                                                  img(src="callout2.png",class="callout")),  
                              span("Sample size:"),
                              customNumericInput('MCMCsamplesize', label=NULL, value=1024, class="input-mini mcmcopt"))
                          ),
                             
                          fluidRow(
                              div(class="tool",span(class ="tip", id="controltip",
                                                    "Type in other arguments to be passed to", span("control.ergm", style="font-family:Courier;"),
                                                    ", e.g.", span("MCMC.burnin.retries=1", style="font-family:Courier;"), 
                                                    img(src="callout2.png",class="callout")),
                                  span("Other controls:"),
                                  textInput("customMCMCcontrol",label=NULL,value=""))
                            )),
                        conditionalPanel(condition="input.controltype == 'MCMLE'",
                                         p("Coming soon")),
                        conditionalPanel(condition="input.controltype == 'Other'",
                                         p("Coming soon"))
                        )))
                     )
            ),
         fluidRow(
           column(2,
                  p('Current ergm formula:')),
           column(10,
                  verbatimTextOutput('checkterms_fit'))),
        fluidRow(
           column(2,
                  p('Summary statistics:')),
           column(10,
                  verbatimTextOutput('prefitsum'))),
         tags$hr(),
         fluidRow(column(12,
                         uiOutput('savemodel'),
                         bsActionButton('clearmodelButton',
                                        label='Clear All Models', block=FALSE)
         )),
         br(),
         tabsetPanel(id = 'fittingTabs',
           tabPanel('Current Model Summary',
                    verbatimTextOutput('modelfitsum'),
                    downloadButton("modelfitdownload", "Download Summary (.txt)")),
           tabPanel('Current Model Fit Report',
                    verbatimTextOutput('modelfit')),
           tabPanel('Model Comparison',
                    verbatimTextOutput('modelcomparison'),
                    downloadButton("modelcompdownload", "Download Comparison (.txt)"))
          ), br(),br(),
  div(id='fittabhelp', class='helper-btn', icon('question-circle', 'fa-2x')),
    div(class="helper-box", style="display:none",
      p('Create an ergm formula by typing terms into the text box.',
        'Notice the summary statistics populate for each term added to the formula. ',
        'After fitting the model, the "Fitting" tab will show MCMC iterations (if any) and MLE coefficients,',
        'while the "Summary" tab shows a comprehensive summary of the model fit.',br(),
        'Find more help in the', a('ergm tutorial.', 
                        href='http://statnet.csde.washington.edu/workshops/SUNBELT/EUSN/ergm/ergm_tutorial.html',
                        target="_blank"))),
actionLink('fitleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
actionLink('fitright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
          ),
#' **MCMC Diagnostics**
#' 
#+ eval=FALSE          
        tabPanel(title='MCMC Diagnostics', value='tab5',
                 #include progress bar when this tab is loading
                 div(class = "busy", 
                     p("Calculation in progress..."),
                     img(src="ajax-loader.gif")
                 ),
                 
                 fluidRow(
                   column(2,
                          p('Network:', class="nwlabel"), 
                          verbatimTextOutput('currentdataset_mcmc')),
                   column(10,
                          p('ergm formula:',style="display:inline;"),
                            uiOutput('uichoosemodel_mcmc'),
                            verbatimTextOutput('checkterms_mcmc'))
                 ),     
                 br(),
                 tags$hr(),
                 tabsetPanel(id='mcmctabs',
                   tabPanel('Plot',
                            #intercept error and give friendly message when MCMC doesn't run
                            conditionalPanel(condition="output.diagnostics == 'MCMC was not run or MCMC sample was not stored.'",
                                        column(1,span(class='helper', id='mcmchelper', icon('question-circle')),
                                               style='width:20px; margin-left:0px'),
                                        column(11,pre('MCMC was not run or MCMC sample was not stored.'),
                                               style='margin-left:0px;'),
                                        column(3, 
                                               div(class='mischelperbox', id='mcmchelpbox',
                                                "MCMC is only run when at least one of the terms in the model represents",
                                                "dyad dependence (e.g., degree terms, or triad related terms).  For",
                                                "models with only dyadic independent terms, estimation relies on",
                                                "traditional maximum likelihood algorithms used for generalized linear",
                                                "models."))
                                             ),
                            uiOutput('diagnosticsplotspace'),
                            downloadButton('mcmcplotdownload',label = 'Download Plots')),
                   tabPanel('Summary', 
                            verbatimTextOutput('diagnostics'))
                 ),
                 div(id='mcmctabhelp', class='helper-btn', icon('question-circle', 'fa-2x')),
                 div(class="helper-box", style="display:none",
                     p('Check for model degeneracy. When a model converges properly',
                       'the MCMC sample statistics should vary randomly around the',
                       'observed values at each step, and the difference between the',
                       'observed and simulated values of the sample statistics should',
                       'have a roughly bell shaped distribution, centered at 0.')),
                 actionLink('mcmcleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                 actionLink('mcmcright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                 
        ),
#' **Goodness of Fit**
#' 
#+ eval=FALSE  
    tabPanel(title='Goodness of Fit',value='tab6',
             
             #include progress bar when this tab is loading
             div(class = "busy", 
                 p("Calculation in progress..."),
                 img(src="ajax-loader.gif")
             ),
             
             fluidRow(
               column(2,
                      p('Network:', class="nwlabel"), 
                      verbatimTextOutput('currentdataset_gof')),
               column(10,
                      p('ergm formula:',style="display:inline;"),
                      uiOutput('uichoosemodel_gof'),
                      verbatimTextOutput('checkterms_gof'))
              ),
             p('If you do not specify a term the default formula for undirected 
               networks is ', code('~ degree + espartners + distance'), 'and for 
               directed networks is ', code('~ idegree + odegree + espartners + 
                                            distance'), '.'),
             fluidRow(
               column(3, selectInput('gofterm', 'Goodness of Fit Term:',
                                     c(Default='', 'degree', 'distance', 'espartners', 
                                       'dspartners', 'triadcensus', 'model'),
                                     selectize = FALSE))),
             fluidRow(
                column(3, actionButton('gofButton', 'Run'))),
             br(),
         tabsetPanel(
           tabPanel("Current Model",
                    fluidRow(
                      column(5,
                             verbatimTextOutput('gofsummary')),  
                      column(7,
                             uiOutput('gofplotspace'),
                             downloadButton('gofplotdownload', label = 'Download Plots')))
                    ),
           tabPanel("Compare Models",align="center",
                    uiOutput('gofplotcompspace'),
                    fluidRow(align="left",
                             downloadButton('gofplotcompdownload', 
                                            label='Download Plots'),
                             br())
                    )
           ),
         
             div(id='goftabhelp', class='helper-btn', icon('question-circle', 'fa-2x')),
             div(class="helper-box", style="display:none",
                 p('Test how well your model fits the original data by choosing a network',
                    'statistic that is not in the model, and comparing the value of this',
                    'statistic observed in the original network to the distribution of values',
                    'you get in simulated networks from your model.')),
             actionLink('gofleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
             actionLink('gofright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
             ),

#' **Simulations**
#' 
#+ eval=FALSE  
          tabPanel(title='Simulations', value='tab7',
                   fluidRow(
                     column(7,
                        fluidRow(
                          column(4,
                              p('Network:', verbatimTextOutput('currentdataset_sim'))),
                          column(5,
                                 customNumericInput('nsims', class="input-small",
                                                    labelstyle="display:block; padding-bottom:5px;",
                                                    label = 'Number of simulations:',
                                                    min = 1,
                                                    value = 1),
                                 actionButton('simButton', 'Simulate'))
                          ),
                        p('ergm formula:',style="display:inline;"),
                        uiOutput('uichoosemodel_sim'),
                        verbatimTextOutput('checkterms_sim')   
                        ),
                     column(5,
                          tabsetPanel(
                            tabPanel("Control Options",
                                     fluidRow(
                                       column(3,
                                              inlineSelectInput('simcontroltype',label=NULL, 
                                                                choices=c("MCMC",
                                                                          "Parallel"))),
                                       column(4,
                                              checkboxInput('simcontroldefault','Use default options', value=TRUE))
                                     ),
                                     conditionalPanel(condition="input.simcontroltype == 'MCMC'",
                                       fluidRow(
                                              div(class="tool", span(class="tip","Number of proposals between sampled statistics.", 
                                                                     img(src="callout2.png",class="callout")),
                                                  span("Interval:"),
                                                  customNumericInput('simMCMCinterval',label=NULL, value=1024, class="mcmcopt input-mini")),
                                              div(class="tool",span(class="tip","Number of proposals before any MCMC sampling is done. Defaults to 16 times the MCMC interval, unless burn-in is specified after the interval.", 
                                                                    img(src="callout2.png",class="callout")),  
                                                  span("Burn-in:"),
                                                  customNumericInput('simMCMCburnin', label=NULL, value=16384, class="mcmcopt input-mini"))
                                              
                                       ),
                                       fluidRow(
                                              div(class="tool", span(class="tip", id="controltip2",
                                                                     "Type in other arguments to be passed to", span("control.simulate", style="font-family:Courier;"),
                                                                     ", e.g.", span("MCMC.init.maxedges=200", style="font-family:Courier;"), 
                                                                     img(src="callout2.png",class="callout")),
                                                  span("Other controls:"),
                                                  textInput("simcustomMCMCcontrol",label=NULL,value=""))
                                       )),
                                     conditionalPanel(condition="input.simcontroltype == 'Parallel'",
                                                     p("Coming soon"))
                                     )
                            )
                     )
                   ),
                   
                   tags$hr(),
                   
                   fluidRow(
                     column(7,
                       tabsetPanel(id="simplotpanel",
                       tabPanel("Network Plots",
                           customNumericInput('thissim', class="input-small",
                                              labelstyle="display:block;",
                                              label = 'Choose a simulation to plot:',
                                              min = 1, value = 1),
                           plotOutput('simplot')
                                ),
                       tabPanel("Simulation Statistics",
                                conditionalPanel("output.simnum >1",
                                    div(plotOutput('simstatsplot'),
                                        title=paste("Statistics from each simulation,",
                                                    "plotted over horizontal \n",
                                                    "lines of the corresponding target statistics."))
                                 )
                         )    
                        )
                       ),
                     
                      
                   column(4,
                            tabsetPanel(
                              tabPanel('Display Options',
                                  conditionalPanel("input.simplotpanel == 'Network Plots'",
                                       wellPanel(
                                         checkboxInput('iso2',
                                                       label = 'Display isolates?', 
                                                       value = TRUE),
                                         checkboxInput('vnames2',
                                                       label = 'Display vertex names?',
                                                       value = FALSE),
                                         br(),
                                         sliderInput('transp2',
                                                     label = 'Vertex opacity',
                                                     min = 0, max = 1, value = 1),
                                         br(),
                                         uiOutput('dynamiccolor2'),
                                         span(bsAlert(inputId = 'colorwarning2'), style='font-size: 0.82em;'),
                                         uiOutput('dynamicsize2'),
                                         downloadButton('simplotdownload',
                                                        label = 'Download Plot'))
                                  ),
                                  conditionalPanel("input.simplotpanel == 'Simulation Statistics'",
                                         conditionalPanel("output.simnum > 1",
                                                   plotOutput('simstatslegend'),
                                                   downloadButton('simstatsplotdownload',
                                                                  label='Download Plot')
                                                   ))
                                ),
                              tabPanel('Simulation Summary',
                                   wellPanel(    
                                   conditionalPanel(condition="output.simnum != 1",
                                          verbatimTextOutput('simsummary'),
                                          verbatimTextOutput('simcoef'),
                                          verbatimTextOutput('simstatslabel'),
                                          conditionalPanel("output.simnum < 10",
                                            verbatimTextOutput('simstats')),
                                          conditionalPanel("output.simnum >= 10",
                                            verbatimTextOutput('simstats2'))
                                                      ),
                                   conditionalPanel(condition="output.simnum == 1",
                                     verbatimTextOutput('simsummary2')
                                     ),
                                   br(),
                                   fluidRow(
                                     column(7, 
                                          downloadButton('simstatsdownload', 
                                                  label = 'Download Statistics')),
                                     column(4,
                                        div(class="tool", span(class="tip", id="statstip",".txt: Summary of simulations",
                                                               "plus full list of statistics.",br(), 
                                                               ".csv: Full list of statistics only.",
                                                               img(src="callout2.png",class="callout")),
                                          radioButtons('simstatsfiletype', label=NULL,
                                                       choices=c('.txt','.csv'))
                                          )
                                        )
                                     )
                                   
                                   
                                     )
                                  )
                              )
                     )),
                   div(id='simtabhelp', class='helper-btn', icon('question-circle', 'fa-2x')),
                   div(class="helper-box", style="display:none",
                       p('Choose how many simulations to run and click "Simulate".',
                         'Plot any individual simulation, or compare',
                         'simulation statistics with target statistics.',
                         'Download any of the plots or a .csv file of the',
                         'simulation statistics.')),
                   actionLink('simleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                   actionLink('simright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                   ),
#' **Help**
#' 
#+ eval=FALSE  
  tabPanel(title='Help', value='tab8',
           sidebarLayout(position = 'right',
                         sidebarPanel(
                           h5('Resources'),
                           a("statnet Wiki",
                             href = "https://statnet.csde.washington.edu/trac", target = "_blank"),
                           column(11, offset = 1, p('The homepage of the statnet project. Find tutorials,',
                                                    'publications and recent news here.'),
                                  span('Key background papers'),br(),
                                  a("ergm: Journal of Statistical Software",
                                    href = "http://www.jstatsoft.org/v24/i03/", target = "_blank"),
                                  br(),
                                  a("Using ergm: Journal of Statistical Software",
                                    href = "http://www.jstatsoft.org/v24/i04/", target = "_blank"),
                                  br(),br(),
                                  span('Tutorials and documentation'),br(),
                                  a("ergm tutorial from NME 2014 Workshop",
                                    href = "http://statnet.csde.washington.edu/EpiModel/nme/2014/d2-tut1.html",
                                    target= "_blank"),
                                  br(),
                                  a("ergm documentation on CRAN", 
                                    href = "http://cran.r-project.org/web/packages/ergm/ergm.pdf",
                                    target = "_blank"), style="margin-bottom:10px;"),
                           br(),
                           p(a("statnetWeb Github repository", href="https://github.com/statnet/statnetWeb",
                               target="_blank")),
                           a("Shiny: a web application framework for R", href="http://shiny.rstudio.com/",
                             target="_blank")
                         ),
                         mainPanel(
                           h5('Help with the app'),
                           p("This app is maintained on Github. To request new features or report a bug,",
                             "please interact with the", 
                             a("repository", href='https://github.com/statnet/statnetWeb',
                               target="_blank"), 
                             "or email the statnet_help listserv (below)."),
                           h5('Help with statnet software'),
                           p("The best way to contact us with questions, comments or suggestions",
                             "is through the statnet users group listserv."),
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
                               target = "_blank"))
                           ))
           )
                  
    
    )
  )
    