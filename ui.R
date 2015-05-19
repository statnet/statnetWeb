#' ---
#' title: "statnetWeb, ui.R"
#' author: "Emily Beylerian"
#' ---
#' statnetWeb
#' ============
#' ui.R, v0.3.3
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


#' Everything that gets displayed inside the app is enclosed in a call to `shinyUI`.
#' The first thing to be specified is the type of page to display. The `navbarPage`
#' includes a navigation bar at the top of the page and each tab leads to different
#' pages of content. Find out more about layout options
#' [here](http://shiny.rstudio.com/articles/layout-guide.html).
#'
#+ eval=FALSE
shinyUI(
  navbarPage(
    #theme="mycosmo.css",
    title=NULL,
    id= 'navbar', windowTitle = 'statnetWeb', collapsible=TRUE,

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
tabPanel(title=span('statnetWeb', id="sWtitle"),
         value='tab1',
         fluidRow(
                  column(8,
                         h5(tags$u("About statnetWeb v0.3.3")),
                         p("Welcome to our prototype web interactive interface for the", strong("ergm"),
                           "package.", strong("ergm"), "is part of the statnet network analysis software --",
                           "a suite of packages written in R -- and this app also includes some of the functionality",
                           "from the associated packages", strong("network"), " and ", strong("sna"), ".  This web app",
                           "is written in R-Shiny, and development is via Github.  More information on the statnet software,",
                           "the ergm package, R-Shiny and our Github repository can be found in the resource links on the right."),

                         div(p("This app is intended to serve as an introduction to the ergm package for those who",
                           "are just getting started using statnet, or for those who are not familiar with programming",
                           "in R. If you are new to ergm, you may find it helpful to work through the", a("ergm tutorial",
                          href="http://statnet.csde.washington.edu/workshops/SUNBELT/EUSN/ergm/ergm_tutorial.html",
                          target="_blank"), "using this app. Advanced users will still want to interact",
                          "via the command line in order to access the full functionality of ergm."),
                         p("A typical network analysis will move sequentially through the tabs at the top of the page.",
                           "Click on the help icon at the top of any page for guidance."),
                         p("Do you have comments/suggestions/complaints on this prototype app? Please share them with us.",
                           "They are best submitted through our", a('Github site,',
                                                                    href='https://github.com/statnet/statnetWeb',
                                                                    target='_blank'),
                           "or by email to the statnet_help listserv (see", actionLink("helpLink", "Help"), "tab).")
                         ),
                         actionButton('startButton', label='Get Started', class="btn btn-primary btn-sm"),
                         br(),

                         h5(tags$u('Citing statnetWeb')),
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
   column(4,
          wellPanel(
              h5(tags$u('Resources')),
              div(title=paste("The homepage of the statnet project. Find tutorials,",
                       "publications and recent news here."),
                  a("statnet Wiki",
                    href = "https://statnet.csde.washington.edu/trac", target = "_blank")
              ),

              column(11, offset = 1,
                    span(id="linktitle1",'Key background papers',icon('angle-double-left')),br(),
                    div(id="linkbox1",
                      a("ergm: Journal of Statistical Software",
                        href = "http://www.jstatsoft.org/v24/i03/", target = "_blank"),
                      br(),
                      a("Using ergm: Journal of Statistical Software",
                        href = "http://www.jstatsoft.org/v24/i04/", target = "_blank")),

                    span(id="linktitle2",'Tutorials and documentation',icon('angle-double-left')),br(),
                    div(id="linkbox2",
                        a("ergm tutorial from Sunbelt EUSN 2014 Workshop",
                        href = "http://statnet.csde.washington.edu/workshops/SUNBELT/EUSN/ergm/ergm_tutorial.html",
                        target= "_blank"),
                      br(),
                      a("ergm documentation on CRAN",
                        href = "http://cran.r-project.org/web/packages/ergm/ergm.pdf",
                        target = "_blank")),
                    style="margin-bottom:10px;"),
              br(),
              p(a("statnetWeb Github repository", href="https://github.com/statnet/statnetWeb",
                target="_blank")),
              a("Shiny: a web application framework for R", href="http://shiny.rstudio.com/",
                target="_blank")
   ),
   fluidRow(img(src= 'UW.Wordmark_ctr_K.jpg', width=200), style="margin-left:15px;"),
   fluidRow(a(img(src = 'csdelogo_crop.png', height = 40, width = 40),
             href = 'https://csde.washington.edu/', target = '_blank'),
            a(img(src = 'csde_goudy.fw.png', width=150), href = 'https://csde.washington.edu/',
             target = '_blank'), style="margin-left:15px;")
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
      tabPanel('Upload Network', br(),
         wellPanel(
           fluidRow(
             column(6,
                    selectInput('filetype',label='File type',
                                 choices=c('built-in network'= 5,
                                           'statnet network object (*.rds)' = 1,
                                           'matrix of relational data (*.csv or *.rds)' = 4,
                                           'Pajek network (*.net)' = 2,
                                           'Pajek project (*.paj)' = 3))
                    ),
             conditionalPanel(condition = 'input.filetype < 5',
               column(6,
                    br(),
                    fileInput(inputId='rawdatafile', label=NULL, accept='text'),
                    verbatimTextOutput('rawdatafile'))
                ),
             conditionalPanel(condition = 'input.filetype == 5',
                column(6,
                    br(style="line-height:26px;"),
                    selectInput('samplenet', label=NULL,
                                choices=c('Choose a network', 'ecoli1', 'ecoli2',
                                          'faux.mesa.high','flobusiness',
                                          'flomarriage', 'kapferer', 'kapferer2',
                                          'molecule', 'samplike', 'samplk1',
                                          'samplk2', 'samplk3'))
                )
               )
             ),
           fluidRow(
             conditionalPanel(condition='input.filetype == 4',
                 column(1, align="right",
                        style="margin-top:5px; margin-left:0px;",
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
                        span(strong('Network Attributes')),
                        div(checkboxInput('dir', 'directed?', value=TRUE),
                             style='padding-top:5px;'),
                        checkboxInput('loops', 'loops?', value=FALSE),
                        checkboxInput('multiple', 'multiple?', value=FALSE),
                        checkboxInput('bipartite', 'bipartite?', value=FALSE))
             ),
             conditionalPanel(condition='input.filetype == 3',
                 column(6,
                        uiOutput('pajchooser'))),
             conditionalPanel(condition='input.filetype == 1',
                              p(class="helper", id="Robjhelp", icon("question-circle"), span("What is an .rds file?", style="font-size:0.85em;")),
                              div(class="mischelperbox", id="Robjbox", 'When working in R, an object in your environment',
                                  'can be saved to a .rds file from the command line in the following way:',
                                  code('saveRDS(objectname, file="newfilename.rds")'),br(),'By default the file will be saved',
                                  'into the current working directory. The full path to a new location can be',
                                  'specified in the ', code('file='), 'argument, or set', code('file=file.choose(new=TRUE)'),
                                  'to use a save dialog box.')
                              )
           )),
         conditionalPanel(
           condition="input.filetype == 5 & input.samplenet != 'Choose a network'",
           wellPanel(uiOutput("datadesc"))
           )
         ),
    tabPanel('Edit Network', br(),
         wellPanel(
           fluidRow(

             column(6,strong('Symmetrize'),
                conditionalPanel(condition="output.nwsum != 'NA'",
                    br(),
                    selectizeInput('symmetrize', label=NULL,
                                 choices=c('Do not symmetrize',
                                           'upper: Copy upper triangle over lower'='upper',
                                           'lower: Copy lower triangle over upper'='lower',
                                           'strong: Intersection of ties'='strong',
                                           'weak: Union of ties'='weak')),
                    conditionalPanel(condition="input.symmetrize != 'Do not symmetrize'",
                                     p("After symmetrizing, network should be:"),
                                     actionButton("symmdir", "directed", class="btn-sm"),
                                     actionButton("symmundir", "undirected", class="btn-sm active")
                                     )

                    )),

             column(5,strong('Import new attribute information'),
                    conditionalPanel(condition="output.nwsum != 'NA'",
                       br(),
                       selectizeInput("newattrtype", label=NULL,
                                 choices=c("Vertex attributes" = "vertexattr",
                                           "Vertex names" = "vertexnames",
                                           "Edge attributes (for edge list)" = "edgeattr",
                                           "Edge values (as matrix)" = "edgevalue"),
                                 options=list(placeholder="Select attribute type",
                                              onInitialize = I('function() { this.setValue(""); }')
                                              )),
                       conditionalPanel(condition="input.newattrtype == 'edgevalue'",
                            span('Upload a file of one of the following types:',br(),
                            tags$ul(
                              tags$li('.rds file',
                                      span(class="helper",id="filetypehelper5",
                                           icon("question-circle"),
                                           div(id="filetypebox5", class="mischelperbox",
                                               strong("Edge values"), "should be in adjacency matrix form",
                                               "and saved into a list object in R. For example,",br(),
                                               code("mylist <- list()"),br(),
                                               code("mylist$eval1 <- matrix(...)"), br(),
                                               code("mylist$eval2 <- matrix(...)"), br(),
                                               "The named elements of the list ('eval1' and 'eval2') will",
                                               "become the names of the edge values in statnetWeb.",br(),br(),
                                               strong(".rds files"), "can be saved with",
                                               br(),
                                               code('saveRDS(objectname, file="newfilename.rds")'),br(),br(),
                                               "Multiple edge value matrices can be uploaded from one list object."
                                               ))),
                              tags$li('.csv file',
                                      span(class="helper", id="filetypehelper6",
                                           icon("question-circle"),
                                           div(id="filetypebox6", class="mischelperbox",
                                               strong(".csv files"), "should include a single header in the",
                                               "first row, which will become the name of the set of edge values.",
                                               "The values should be in matrix form.",br(),br(),
                                               "Only one edge value matrix can be uploaded at a time.")))))
                                        ),
                       conditionalPanel(condition="input.newattrtype != '' & input.newattrtype != 'edgevalue'",
                            span('Upload a file of one of the following types:',br(),
                                 tags$ul(
                                   tags$li('R list object',
                                           span(class="helper",id="filetypehelper7",
                                                icon("question-circle"),
                                                div(id="filetypebox7", class="mischelperbox",
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
                                                    "attribute and the name of the list or .csv header will be ignored."))),
                                   tags$li('.csv file',
                                           span(class="helper", id="filetypehelper8",
                                                icon("question-circle"),
                                                div(id="filetypebox8", class="mischelperbox",
                                                    strong(".csv files"), "should include a header in the first row.",
                                                    "The header of each column will become an attribute name.",
                                                    br(),br(),tags$u("Note:"),"Attributes uploaded as vertex names",
                                                    "will automatically be saved into the", code("vertex.names"),
                                                    "attribute and the uploaded names will be ignored.")))))
                                        ),
                    conditionalPanel(condition="input.newattrtype != ''",
                          fileInput(inputId='newattrvalue', label=NULL),
                          p('New attribute name(s):'),
                          verbatimTextOutput('newattrname'),
                          actionButton('newattrButton', label='Set Attribute', class="btn-sm")
                                        )

                       )

                    )

           )
           )
         ),
tabPanel('Modify Attributes', br(),
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
  tabPanel('Network Summary', br(),
           verbatimTextOutput('nwsum')
           ))
  )
),

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
 conditionalPanel("output.errstate != 'FALSE'",
                  div(class = "error", uiOutput("errbox"))
 ),

fluidRow(
 column(7,
    tabsetPanel(id='plottabs',
      tabPanel('Network Plot', br(),
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
      tabPanel('More', value='More', br(),
               h5('Conditional uniform graph tests', icon('angle-double-down'),
                  id="cugtitle"),
               wellPanel(id="cugbox",
                 column(4, uiOutput("dynamiccugterm")),
                 column(4, selectInput("ncugsims",
                                       label = "Number of simulations",
                                       choices = c(100, 200, 500))),
                 column(3, actionButton("cugButton", label = "Run",
                                        style="margin-top: 25px;")),
                 br(),
                 plotOutput("cugtest")
#                  downloadButton('cugtestdownload', label = "Download Plot", class="btn-sm")
               ),
               h5('Mixing matrix', icon('angle-double-left'),
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
                  column(4, selectInput('grecipmeas',label=NULL,
                             choices=c('dyadic','dyadic.nonnull','edgewise',
                                       'edgewise.lrr','correlation')))),
                 fluidRow(
                  column(4, p('Transitivity:'), class='stitle'),
                  column(3, p(textOutput('gtrans'), class='snum')),
                  column(4, selectInput('gtransmeas',label=NULL,
                                choices=c('weak','strong','weakcensus',
                                          'strongcensus','rank','correlation'))
                         )),
                 fluidRow(
                  column(4, p('Density:', class='stitle')),
                  column(3, p(textOutput('gden'), class='snum'))),

                 fluidRow(
                  column(4, p('Degree:', class='stitle')),
                  column(3, p(textOutput('gdeg'), class='snum')),
                  column(4, selectInput('gdegcmode', label=NULL,
                                         choices=c('indegree', 'outdegree', 'total'))
                         )),
                 fluidRow(
                  column(4, p('Betweenness:', class='stitle')),
                  column(3, p(textOutput('gbetw'), class='snum')),
                  column(4, selectInput('gbetwcmode', label=NULL,
                                         choices=c('directed','undirected',
                                                   'endpoints','proximalsrc',
                                                   'proximaltar','proximalsum',
                                                   'lengthscaled', 'linearscaled'))
                         )),
                 fluidRow(
                  column(4, p('Closeness:', class='stitle')),
                  column(3, p(textOutput('gclose'), class='snum')),
                  column(4, selectInput('gclosecmode', label=NULL,
                                        choices=c('directed','undirected',
                                                  'suminvdir','suminvundir')))),
                 fluidRow(
                   column(4, p('Stress Centrality:', class='stitle')),
                   column(3, p(textOutput('gstress'), class='snum')),
                   column(4, selectInput('gstresscmode', label=NULL,
                                         choices=c('directed','undirected')))
                 ),
                 fluidRow(
                   column(4, p('(Harary) Graph Centrality:', class='stitle')),
                   column(3, p(textOutput('ggraphcent'), class='snum')),
                   column(4, selectInput('ggraphcentcmode', label=NULL,
                                         choices=c('directed', 'undirected')))
                 ),
                 fluidRow(
                   column(4, p('Eigenvector Centrality:', class='stitle')),
                   column(3, p(textOutput('gevcent'), class='snum')),
                   column(4, br())
                 ),
                 fluidRow(
                   column(4, p('Information Centrality:', class='stitle')),
                   column(3, p(textOutput('ginfocent'), class='snum')),
                   column(4, selectInput('ginfocentcmode',label=NULL,
                                         choices=c('weak', 'strong', 'upper',
                                                   'lower')))
                 )


               )),

               h5('Node-level descriptive indices',
                  icon('angle-double-left'), id="nodeleveltitle"),
               wellPanel(id="nodelevelbox",
                 fluidRow(
                     column(2,span("Node index:")),
                     column(5, numericInput('nodeind', label=NULL, value=1,
                                            min=1))
                     ),
                 tags$hr(),
                 fluidRow(
                   column(2, offset=3, tags$u('Current node')),
                   column(3, tags$u('Centrality mode')),
                   column(2, tags$u('Min')),
                   column(2, tags$u('Max'))),
                   fluidRow(
                     column(3, p('Degree:', class='stitle')),
                     column(2, p(textOutput('ndeg'), class='snum')),
                     column(3, selectInput('ndegcmode', label=NULL,
                                           choices=c('indegree', 'outdegree', 'total')),
                            class = "smallselect"),
                     column(2, p(textOutput('ndegmin'), class='snum', align='center')),
                     column(2, p(textOutput('ndegmax'), class='snum', align='center'))
                     ),
                   fluidRow(
                     column(3, p('Betweenness:', class='stitle')),
                     column(2, p(textOutput('nbetw'), class='snum')),
                     column(3, selectInput('nbetwcmode', label=NULL,
                                           choices=c('directed','undirected',
                                                     'endpoints','proximalsrc',
                                                     'proximaltar','proximalsum',
                                                     'lengthscaled', 'linearscaled')),
                            class = "smallselect"),
                     column(2, p(textOutput('nbetwmin'), class='snum')),
                     column(2, p(textOutput('nbetwmax'), class='snum'))
                     ),
                   fluidRow(
                     column(3, p('Closeness:', class='stitle')),
                     column(2, p(textOutput('nclose'), class='snum')),
                     column(3, selectInput('nclosecmode', label=NULL,
                                           choices=c('directed','undirected',
                                                     'suminvdir','suminvundir')),
                            class = "smallselect"),
                     column(2, p(textOutput('nclosemin'))),
                     column(2, p(textOutput('nclosemax')))
                     ),
                   fluidRow(
                     column(3, p('Stress Centrality:', class='stitle')),
                     column(2, p(textOutput('nstress'), class='snum')),
                     column(3, selectInput('nstresscmode', label=NULL,
                                           choices=c('directed','undirected')),
                            class = "smallselect"),
                     column(2, p(textOutput('nstressmin'))),
                     column(2, p(textOutput('nstressmax')))
                     ),
                   fluidRow(
                     column(3, p('(Harary) Graph Centrality:', class='stitle')),
                     column(2, p(textOutput('ngraphcent'), class='snum')),
                     column(3, selectInput('ngraphcentcmode', label=NULL,
                                           choices=c('directed', 'undirected')),
                            class = "smallselect"),
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
                     column(3, selectInput('ninfocentcmode',label=NULL,
                                           choices=c('weak', 'strong', 'upper',
                                                     'lower')),
                            class = "smallselect"),
                     column(2, p(textOutput('ninfocentmin'))),
                     column(2, p(textOutput('ninfocentmax')))
                     )
                 ))



      )),
 column(4,
     tabsetPanel(id='displaytabs',
       tabPanel(title='Display Options', br(),
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
                                 uiOutput("dynamiccolor"),
                                 conditionalPanel(condition="Number(output.attrlevels) > 9",
                                   column(10,
                                          p(id = "closewarning1", icon(name = "remove"), class = "warning"),
                                          div(class = "warning", id = "colorwarning1",
                                              span(tags$u("Note:"),
                                                   "Color palette becomes a gradient for attributes with more than nine levels.")
                                          )
                                   )),

#                                      span(bsAlert(inputId = 'colorwarning'), style='font-size: 0.82em;'),
                                 uiOutput('dynamicsize'),
                                 br(),
                                 downloadButton('nwplotdownload', label = "Download Plot", class="btn-sm")),

                conditionalPanel(condition='input.plottabs == "Degree Distribution"',
                                 uiOutput("dynamiccmode_dd"),
                                 uiOutput("dynamiccolor_dd"),
                                 tags$label("Y-axis units:"), br(),
                                 actionButton("countButton_dd", label="Count of nodes", class="btn-sm active"),
                                 actionButton("percButton_dd", label="Percent of nodes", class="btn-sm"),
                                 br(), br(),
                                 tags$label('Expected values of null models:'), br(),
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
                                 downloadButton('degreedistdownload', label = "Download Plot", class="btn-sm")
                  ),
                conditionalPanel(condition='input.plottabs == "Geodesic Distribution"',
                                 tags$label("Y-axis units:"), br(),
                                 actionButton("countButton_gd", "Count of vertex pairs", class="btn-sm active"),
                                 actionButton("percButton_gd", "Percent of vertex pairs", class="btn-sm"),
                                 br(), br(),
                                 tags$label('Expected values of null models:'), br(),
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
                                 downloadButton('geodistdownload', label= 'Download Plot', class="btn-sm")
                  ),
                conditionalPanel(condition='input.plottabs == "More"',
                                 p("No display options at this time,",
                                   "stay tuned for updates!")
                                 )
                )),
       tabPanel(title='Network Summary', br(),
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

            column(4,
                   p("ERGM terms:"),
                   div(textInput(inputId="terms", label=NULL, value="edges"),
                       title=paste("Type in term(s) and their arguments.",
                                   "For multiple terms, separate with '+'. ")
                   ),
                   actionButton('addtermButton', 'Add Term(s)', class="btn-primary btn-sm"),
                   actionButton('resetformulaButton', 'Reset Formula', class="btn-sm")


            ),
            column(5,
               tabsetPanel(
                 tabPanel("Term Documentation",
                  br(),
                  div(class="placeholder",
                      fluidRow(
                        column(6,
                               actionButton("matchingButton", "Compatible terms", class="btn-sm active"),
                               actionButton("allButton", "All terms", class="btn-sm")
                               ),
                        column(4, uiOutput("listofterms"))
                      ),
                  div(id="termdocbox",
                      verbatimTextOutput("termdoc")
                    ),
                  div(id = "termexpand",
                      icon(name = "expand"))
                  )
                 ),
                 tabPanel("Control Options",
                    div(class = "placeholder",
                    fluidRow(
                      column(3,
                        inlineSelectInput('controltype',label = NULL,
                                          choices = c("MCMC","MCMLE"),
                                          style = "margin-top:10px;")),
                      column(5,
                        checkboxInput('controldefault', 'Use default options', value = TRUE))
                    ),
                        conditionalPanel(condition = "input.controltype == 'MCMC'", class = "shiftright",
                          fluidRow(
                            column(4,
                                   span("Interval:"),
                                   customNumericInput('MCMCinterval', label = NULL, value = 1024, class = "mcmcopt input-mini"),
                                   title = paste("Number of proposals between sampled statistics.")
                                   ),

                            column(4,
                                   span("Burn-in:"),
                                   customNumericInput('MCMCburnin', label = NULL, value = 16384, class = "mcmcopt input-mini"),
                                   title = paste("Number of proposals before any MCMC sampling is done.",
                                                 "Defaults to 16 times the MCMC interval, unless burn-in is specified after the interval.")
                                   ),

                            column(4,
                                   span("Sample size:"),
                                   customNumericInput('MCMCsamplesize', label = NULL, value = 1024, class = "mcmcopt input-mini"),
                                   title = paste("Number of network statistics, randomly drawn from a given distribution",
                                                 "on the set of all networks, returned by the Metropolis-Hastings algorithm.")
                                   )
                          ),

                          fluidRow(
                              div(span("Other controls:", class = "shiftright"),
                                  customTextInput("customMCMCcontrol", label = NULL, value = "", class = "input-small"),
                                  title = paste("Other arguments to be passed to",
                                       "control.ergm, e.g. MCMC.burnin.retries = 1")
                                  )
                            )),
                        conditionalPanel(condition = "input.controltype == 'MCMLE'",
                                         p("Coming soon"))
                        )))
                     )
            ),
          br(),
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
         fluidRow(column(12,
                         actionButton("fitButton", "Fit Model", class="btn-primary btn-sm"),
                         uiOutput("savemodel"),
                         actionButton("clearmodelButton", label="Clear All Models", class="btn-sm")
         )),
         br(),
         tabsetPanel(id = 'fittingTabs',
           tabPanel('Current Model Summary', br(),
                    verbatimTextOutput('modelfitsum'),
                    downloadButton("modelfitdownload", "Download Summary (.txt)", class="btn-sm")),
           tabPanel('Current Model Fit Report', br(),
                    verbatimTextOutput('modelfit')),
           tabPanel('Model Comparison', br(),
                    verbatimTextOutput('modelcomparison'),
                    downloadButton("modelcompdownload", "Download Comparison (.txt)", class="btn-sm"))
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
                  div(p('ergm formula:',style="display:inline;"),
                    uiOutput('uichoosemodel_mcmc'), class="nwlabel"),
                  verbatimTextOutput('checkterms_mcmc'))
         ),
         br(),
         tags$hr(),
         tabsetPanel(id='mcmctabs',
           tabPanel('Plot', br(),
                    wellPanel(
                      class = "mcmcwarning",
                      p("Recent changes in the ergm estimation algorithm mean that these plots",
                        "can no longer be used to ensure that the mean statistics from the model match the",
                        "observed network statistics. For that functionality, please use the GOF page.")
                      #p(icon("close"), class = "topright")
                       ),
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
                    downloadButton('mcmcplotdownload',label = 'Download Plots', class="btn-sm")),
           tabPanel('Summary', br(),
                    wellPanel(
                      class = "mcmcwarning",
                      p("Recent changes in the ergm estimation algorithm mean that these plots",
                        "can no longer be used to ensure that the mean statistics from the model match the",
                        "observed network statistics. For that functionality, please use the GOF page.")
                      #p(icon("close"), class = "topright")
                    ),
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
                  div(p('ergm formula:',style="display:inline;"),
                  uiOutput('uichoosemodel_gof'), class="nwlabel"),
                  verbatimTextOutput('checkterms_gof'))
          ),
         p('If you do not specify a term the default formula for undirected
           networks is ', code('~ degree + espartners + distance'), 'and for
           directed networks is ', code('~ idegree + odegree + espartners +
                                        distance'), '.'),
         fluidRow(
           column(3, selectInput('gofterm', 'Goodness of Fit Term:',
                                 c('Default', 'degree','idegree','odegree',
                                   'distance', 'espartners','dspartners', 'triadcensus',
                                   'model'),
                                 ))),
         fluidRow(
            column(3, actionButton('gofButton', 'Run', class="btn-sm"))),
         br(),
     tabsetPanel(
       tabPanel("Current Model", br(),
                fluidRow(
                  column(5,
                         verbatimTextOutput('gofsummary')),
                  column(7,
                         uiOutput('gofplotspace'),
                         downloadButton('gofplotdownload', label = 'Download Plots', class="btn-sm")))
                ),
       tabPanel("Compare Saved Models",align="center", br(),
                uiOutput('gofplotcompspace'),
                fluidRow(align="left",
                         downloadButton('gofplotcompdownload',
                                        label='Download Plots', class="btn-sm"),
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
                       actionButton('simButton', 'Simulate',class="btn-sm"))
                ),
              div(p('ergm formula:',style="display:inline;"),
              uiOutput('uichoosemodel_sim'), class="nwlabel"),
              verbatimTextOutput('checkterms_sim')
              ),
           column(5,
                tabsetPanel(
                  tabPanel("Control Options",
                           fluidRow(
                             column(3,
                                    inlineSelectInput('simcontroltype',label=NULL,
                                                      choices=c("MCMC","Parallel"),
                                                      style="margin:10px 0px;")),
                             column(4,
                                    checkboxInput('simcontroldefault','Use default options', value=TRUE))
                           ),
                           conditionalPanel(condition="input.simcontroltype == 'MCMC'", class="shiftright",
                             fluidRow(
                                    column(5,
                                        span("Interval:"),
                                        customNumericInput('simMCMCinterval',label=NULL, value=1024, class="mcmcopt input-mini"),
                                        title=paste("Number of proposals between sampled statistics.")
                                        ),
                                    column(5,
                                        span("Burn-in:"),
                                        customNumericInput('simMCMCburnin', label=NULL, value=16384, class="mcmcopt input-mini"),
                                        title=paste("Number of proposals before any MCMC sampling is done.",
                                                    "Defaults to 16 times the MCMC interval, unless burn-in is specified after the interval.")
                                        )

                             ),
                             fluidRow(
                                    div(
                                        span("Other controls:"),
                                        customTextInput("simcustomMCMCcontrol",label=NULL,value=""),
                                        title=paste("Type in other arguments to be passed to control.simulate,",
                                                    "e.g. MCMC.init.maxedges=200")
                                        )
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
             tabPanel("Network Plots", br(),
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
                    tabPanel('Display Options', br(),
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
#                                          span(bsAlert(inputId = 'colorwarning2'), style='font-size: 0.82em;'),
                               uiOutput('dynamicsize2'),
                               downloadButton('simplotdownload',
                                              label = 'Download Plot', class="btn-sm"))
                        ),
                        conditionalPanel("input.simplotpanel == 'Simulation Statistics'",
                               conditionalPanel("output.simnum > 1",
                                         plotOutput('simstatslegend'),
                                         downloadButton('simstatsplotdownload',
                                                        label='Download Plot', class="btn-sm")
                                         ))
                      ),
                    tabPanel('Simulation Summary', br(),
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
                                        label = 'Download Statistics', class="btn-sm")),
                           column(4,
                              div(title=paste0(".txt: Summary of simulations",
                                              " plus full list of statistics. \n",
                                              ".csv: Full list of statistics only."),
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
                         h5(tags$u('Resources')),
                         div(title=paste("The homepage of the statnet project. Find tutorials,",
                                         "publications and recent news here."),
                             a("statnet Wiki",
                               href = "https://statnet.csde.washington.edu/trac", target = "_blank")
                         ),
                         column(11, offset = 1,
                                span(id="linktitle3",'Key background papers',icon('angle-double-left')),br(),
                                div(id="linkbox3",
                                    a("ergm: Journal of Statistical Software",
                                      href = "http://www.jstatsoft.org/v24/i03/", target = "_blank"),
                                    br(),
                                    a("Using ergm: Journal of Statistical Software",
                                      href = "http://www.jstatsoft.org/v24/i04/", target = "_blank")),

                                span(id="linktitle4",'Tutorials and documentation',icon('angle-double-left')),br(),
                                div(id="linkbox4",
                                    a("ergm tutorial from Sunbelt EUSN 2014 Workshop",
                                      href = "http://statnet.csde.washington.edu/workshops/SUNBELT/EUSN/ergm/ergm_tutorial.html",
                                      target= "_blank"),
                                    br(),
                                    a("ergm documentation on CRAN",
                                      href = "http://cran.r-project.org/web/packages/ergm/ergm.pdf",
                                      target = "_blank")),
                                style="margin-bottom:10px;"),
                         br(),
                         p(a("statnetWeb Github repository", href="https://github.com/statnet/statnetWeb",
                             target="_blank")),
                         a("Shiny: a web application framework for R", href="http://shiny.rstudio.com/",
                           target="_blank")
                       ),
                       mainPanel(
                         h5(tags$u('Help with statnetWeb')),
                         p("This app is maintained on Github. To request new features or report a bug,",
                           "please interact with the",
                           a("repository", href='https://github.com/statnet/statnetWeb',
                             target="_blank"),
                           "or email the statnet_help listserv (below)."),
                         h5(tags$u('Help with statnet software')),
                         p("The best way to contact us with questions, comments or suggestions",
                           "is through the statnet users group listserv."),
                         p("To post and receive messages from this listserv, you need to join.",
                           "See the",
                           a("statnet_help info page",
                             href = "https://mailman.u.washington.edu/mailman/listinfo/statnet_help",
                             target = "_blank"),
                           "for instructions on how to join."),
                         p("You can use the listserv to:"),
                         tags$ul(
                           tags$li("get help from the statnet development team (and other users)"),
                           tags$li("post questions, comments and ideas to other users"),
                           tags$li("be informed about statnet updates"),
                           tags$li("learn about bugs (and bug fixes)")
                         ),
                         p("Once you have joined the list, you can post your questions and comments to",
                           strong("statnet_help@u.washington.edu")),
                         p("A full list of all messages posted to this list is available",
                           a("here.",
                             href = "https://mailman.u.washington.edu/mailman/private/statnet_help",
                             target = "_blank"))
                         ))
         )


  )
)
