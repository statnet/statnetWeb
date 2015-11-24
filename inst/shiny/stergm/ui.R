
## ui.R for stergm app


library(statnetWeb)
library(tergm)

# Everything that gets displayed inside the app is enclosed in a call to `shinyUI`.
# The first thing to be specified is the type of page to display. The `navbarPage`
# includes a navigation bar at the top of the page and each tab leads to different
# pages of content.

shinyUI(
  navbarPage(
    title=NULL,
    id= "navbar", windowTitle = "STERGM", collapsible = TRUE,

    # Front Page (About) ------------------------------------------------------

    tabPanel(title = span(span("statnetWeb:", id = "sttitle"), "STERGM"),
             value = "tab1",

             tags$head(
               tags$link(rel = "stylesheet", type = "text/css",
                         href = "style.css"),
               tags$script(type = "text/javascript", src = "alert.js")),

             fluidRow(
               column(2,
                      actionButton("aboutButton", label = "About stergm",
                                   class = "btn active"),
                      actionButton("citeButton", label = "Citing stergm",
                                   class = "btn"),
                      actionButton("startButton", label = "Get Started",
                                   class="btn btn-primary")
               ),
               column(6, style="padding: 0 30px 0 0;",
                      div(id="aboutbox",
                          p("about statnetWeb and stergm")
                      ),
                      div(id="citebox",
                          tabsetPanel(
                            tabPanel("BibTeX",
p(strong("statnet")),
tags$pre(id = "scitation", "@Manual{handcock:statnet,
title = {statnet: Software tools for the Statistical Modeling of Network Data},
author = {Mark S. Handcock and David R. Hunter and Carter T. Butts and Steven M. Goodreau and Martina Morris},
year = {2003},
address = {Seattle, WA},
url = {http://statnetproject.org}
}"),

p(strong("tergm package")),
tags$pre(id = "swcitation", "@Manual{krivitsky:tergm,
author = {Pavel N. Krivitsky and Mark S. Handcock},
title = {tergm: Fit, Simulate and Diagnose Models for Network Evolution based on
Exponential-Family Random Graph Models},
organization = {The Statnet Project (http://www.statnet.org)},
year = {2014},
note = {R package version 3.2.4},
url = {CRAN.R-project.org/package=tergm}
}")
                       ),
              tabPanel("Other",
p(strong("statnet")),
tags$pre("Mark S. Handcock, David R. Hunter, Carter T. Butts, Steven M. Goodreau, and
 Martina Morris (2003). statnet: Software tools for the Statistical Modeling
of Network Data. URL: http://statnetproject.org"),

p(strong("tergm")),
tags$pre("Krivitsky P and Handcock M (2014). tergm: Fit, Simulate and Diagnose
Models for Network Evolution based on Exponential-Family Random Graph Models. The
Statnet Project (URL: http://www.statnet.org). R package version 3.2.4, URL:
CRAN.R-project.org/package=tergm.")
                       )
            ),

            p('If you use statnet packages, please cite them.',
              'Additional citation information for statnet',
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
            )
            )
                      ),
               column(4,
                      wellPanel(
                        h5(tags$u('Resources')),
                        div(title=paste("Homepage of the statnet project with tutorials,",
                                        "publications and recent news."),
                            a("About statnet",
                              href = "https://statnet.csde.washington.edu/trac",
                              target = "_blank")
                        ),
                        div(
                          a("About stergm")
                        ),

                        column(11, offset = 1,
                               span(id="linktitle1",'Key background papers',
                                    icon('angle-double-left')),
                               br(),
                               div(id="linkbox1",
                                   a("Krivitsky, Handcock 2013.",
                                     href = "http://onlinelibrary.wiley.com/doi/10.1111/rssb.12014/abstract",
                                     target = "_blank")
                                    ),

                               span(id="linktitle2",'Tutorials and documentation',
                                    icon('angle-double-left')),
                               br(),
                               div(id="linkbox2",
                                   a("tergm tutorial from Sunbelt 2015",
                                     href = "http://statnet.csde.washington.edu/workshops/SUNBELT/current/tergm/tergm_tutorial.pdf",
                                     target= "_blank"),
                                   br(),
                                   a("tergm documentation on CRAN",
                                     href = "https://cran.r-project.org/web/packages/tergm/tergm.pdf",
                                     target = "_blank")),
                               style="margin-bottom:10px;"),
                        br(),
                        div(a("statnetWeb on GitHub",
                              href="https://github.com/statnet/statnetWeb",
                              target="_blank")),
                        div(a("Shiny: a web application framework for R",
                              href="http://shiny.rstudio.com/",
                              target="_blank"))
                      ),
                      fluidRow(img(src= 'UW.Wordmark_ctr_K.jpg', width = 200),
                               style="margin-left:15px;"),
                      fluidRow(a(img(src = 'csdelogo_crop.png', height = 40,
                                     width = 40),
                                 href = 'https://csde.washington.edu/',
                                 target = '_blank'),
                               a(img(src = 'csde_goudy.fw.png', width = 150),
                                 href = 'https://csde.washington.edu/',
                                 target = '_blank'),
                               style="margin-left: 15px;")
               )
    )
             ),

# Data --------------------------------------------------------------------


tabPanel("Data", value = "tab2",
column(7,
tabsetPanel(
  tabPanel("Upload Network",
     br(),
     wellPanel(
       fluidRow(
         column(3,
           actionButton("nwnum1", label = "Single network panel",
                        class = "active")
         ),
         column(4,
           actionButton("nwnum2", label = "Multiple network panels")
         ),
         column(3, style = "float: right;",
           actionButton("uploadnet", label = "Refresh network",
                        class = "btn-primary"))
       ),
       br(),
       fluidRow(
         column(6,
          selectInput("filetype", label = "File type",
              choices=c("built-in network" = "builtin",
                        "statnet network object(s) (*.rds)" = "statnet",
                        "matrix of relational data (*.rds)" = "rmat",
                        "Excel sheet(s) of relational data (*.xlsx)" = "excel",
                        "Pajek network (*.net)" = "pajeknet",
                        "Pajek project (*.paj)" = "pajekpaj")),
          uiOutput("npanelsui"),
          conditionalPanel(condition='input.filetype == "statnet"',
                           p(class="helper", id="Robjhelp", icon("question-circle"),
                             span("What is an .rds file?", style="font-size:0.85em;")),
                           div(class="mischelperbox", id="Robjbox",
                               'When working in R, an object in your environment',
                               'can be saved to a .rds file from the command line',
                               'in the following way:',
                               code('saveRDS(robject, file="newfilename.rds")'),
                               br(), 'By default the file will be saved',
                               'into the current working directory. The full path',
                               'to a new location can be specified in the ',
                               code('file ='), 'argument, or set',
                               code('file = file.choose(new = TRUE)'),
                               'to use a save dialog box.', br(), br(),
                               'To upload multiple network panels, data should be',
                               'saved in a list:',
                               code('robject <- list(net1, net2, ...)'))
          ),
          conditionalPanel(condition='input.filetype == "rmat"',
                           p(class="helper", id="Robjhelp2", icon("question-circle"),
                             span("What is an .rds file?", style="font-size:0.85em;")),
                           div(class="mischelperbox", id="Robjbox2",
                               'When working in R, an object in your environment',
                               'can be saved to a .rds file from the command line',
                               'in the following way:',
                               code('saveRDS(robject, file="newfilename.rds")'),
                               br(), 'By default the file will be saved',
                               'into the current working directory. The full path',
                               'to a new location can be specified in the ',
                               code('file ='), 'argument, or set',
                               code('file = file.choose(new = TRUE)'),
                               'to use a save dialog box.', br(), br(),
                               'To upload multiple network panels, data should be',
                               'saved in a list:',
                               code('robject <- list(matrix1, matrix2, ...)'))
          )
         ),
         conditionalPanel(condition = "input.filetype == 'builtin'",
          column(6,
                 br(style="line-height:26px;"),
                 uiOutput("samplenetUI")
                 )
         ),
         conditionalPanel(condition = 'input.filetype != "builtin"',
          column(6,
                 br(),
                 fileInput(inputId = 'rawdatafile',
                           label = NULL, accept = 'text'),
                 verbatimTextOutput('rawdatafile'))
         )
       ),
       fluidRow(
         conditionalPanel(condition = 'input.filetype == "excel"',
            column(1, align = "right",
                   style = "margin-top:5px; margin-left:0px;",
                   br(), br(),
                   span(style = "line-height:25px;",
                        class = "helper",
                        span(id = "filetypehelper1",
                             icon('question-circle'),
                             div(id = "filetypebox1",
                                 class = "smallhelperbox",
                                 "For adjacency matrices,",
                                 "the first row and column of Excel sheets",
                                 "should hold vertex labels.")),
                        br(),
                        span(id = "filetypehelper2",
                             icon('question-circle'),
                             div(id = "filetypebox2",
                                 class = "smallhelperbox",
                                 "For adjacency matrices,",
                                 "the first row and column of Excel sheets",
                                 "should hold vertex labels.")),
                        br(),
                        span(id = "filetypehelper3",
                             icon('question-circle'),
                             div(id = "filetypebox3",
                                 class = "smallhelperbox",
                                 "For incidence matrices,",
                                 "the first row of Excel sheets should hold",
                                 "edge labels, the first column should hold vertex labels")),
                        br(),
                        span(id = "filetypehelper4",
                             icon('question-circle'),
                             div(id = "filetypebox4",
                                 class = "smallhelperbox",
                                 "For edge lists, Excel sheets should not have row or column labels.")),

                        br()))
            ),
         conditionalPanel(condition = 'input.filetype == "rmat" | input.filetype == "excel"',
            column(4,
                   br(),
                   radioButtons('matrixtype', label = 'Matrix Type',
                                choices = c('Adjacency matrix' = 'adjacency',
                                          'Bipartite adjacency matrix' = 'bipartite',
                                          'Incidence matrix' = 'incidence',
                                          'Edge list' = 'edgelist'))),

            column(5,
                   br(),
                   span(strong('Network Attributes')),
                   checkboxInput('dir', 'directed?', value = TRUE),
                   checkboxInput('loops', 'loops?', value = FALSE),
                   checkboxInput('multiple', 'multiple?',
                                 value = FALSE),
                   checkboxInput('bipartite', 'bipartite?',
                                 value = FALSE))
         ),
         conditionalPanel(condition = 'input.filetype == "pajekpaj"',
                          column(6,
                                 uiOutput('pajchooser')))
       )
     )

  )
#   tabPanel("Edit Network",
#      br(),
#      wellPanel(
#        fluidRow(
#          column(6,strong('Symmetrize'),
#             conditionalPanel("output.nwsum != 'NA'",
#                  br(),
#                  selectizeInput('symmetrize', label = NULL,
#                                 choices = c('Do not symmetrize',
#                                             'upper: Copy upper triangle over lower' = 'upper',
#                                             'lower: Copy lower triangle over upper' = 'lower',
#                                             'strong: Intersection of ties' = 'strong',
#                                             'weak: Union of ties' = 'weak')),
#                  conditionalPanel("input.symmetrize != 'Do not symmetrize'",
#                     p("After symmetrizing, network should be:"),
#                     actionButton("symmdir", "directed", class = "btn-sm"),
#                     actionButton("symmundir", "undirected",
#                                  class = "btn-sm active")
#                  )
#              )),
#
#          column(5,strong('Import new attribute information'),
#             conditionalPanel("output.nwsum != 'NA'",
#              br(),
#              selectizeInput("newattrtype", label = NULL,
#                             choices = c("Vertex attributes" = "vertexattr",
#                                       "Vertex names" = "vertexnames",
#                                       "Edge attributes" = "edgeattr"),
#                             options = list(placeholder = "Select attribute type",
#                                          onInitialize = I('function() { this.setValue(""); }')
#                             )),
#              conditionalPanel(condition = "input.newattrtype == 'edgeattr'",
#                               radioButtons("edgeform", label = NULL,
#                                            choices = c("Attributes are in adjacency matrix form" = "matrix",
#                                                        "Attributes are in vector form" = "vector")
#                                            ),
#
#                               conditionalPanel("input.edgeform == 'matrix'",
#                                  span('Upload a file of one of the following types:',br(),
#                                       tags$ul(
#                                         tags$li('.rds file',
#                                           span(class="helper",id="filetypehelper5",
#                                                icon("question-circle"),
#                                                div(id="filetypebox5", class="mischelperbox",
#                                                    strong("Edge values"), "should be in adjacency matrix form",
#                                                    "and saved into a list object in R. For example,",br(),
#                                                    code("mylist <- list()"),br(),
#                                                    code("mylist$eval1 <- matrix(...)"), br(),
#                                                    code("mylist$eval2 <- matrix(...)"), br(),
#                                                    "The named elements of the list ('eval1' and 'eval2') will",
#                                                    "become the names of the edge attributes in statnetWeb.",br(),br(),
#                                                    strong(".rds files"), "can be saved with",
#                                                    br(),
#                                                    code('saveRDS(objectname, file = "newfilename.rds")'),br(),br(),
#                                                    "Multiple edge value matrices can be uploaded from one list object."
#                                                      ))),
#                                         tags$li('.csv file',
#                                           span(class = "helper", id = "filetypehelper6",
#                                                icon("question-circle"),
#                                                div(id = "filetypebox6", class = "mischelperbox",
#                                                    strong(".csv files"), "should include vertex labels in the first",
#                                                    "row and column of the matrix. The edge attribute name will be taken",
#                                                    "from the filename.", br(), br(),
#                                                    "Only one edge value matrix can be uploaded at a time.")))
#                                         )) #end list
#                               ),
#                               conditionalPanel("input.edgeform == 'vector'",
#                                  span('Upload a file of one of the following types:',
#                                       br(),
#                                       tags$ul(
#                                         tags$li('.rds file',
#                                           span(class = "helper",
#                                                id = "filetypehelper9",
#                                                icon("question-circle"),
#                                                div(id = "filetypebox9",
#                                                    class = "mischelperbox",
#                                                    strong("Edge attributes"),
#                                                    "should be in vector form",
#                                                    "and saved into a list object",
#                                                    "in R. For example,", br(),
#                                                    code("mylist <- list()"), br(),
#                                                    code("mylist$eval1 <- c()"), br(),
#                                                    code("mylist$eval2 <- matrix(...)"), br(),
#                                                    "The named elements of the list ('eval1' and 'eval2') will",
#                                                    "become the names of the edge attributes in statnetWeb.",
#                                                    "The values in each vector should be in the same order as the",
#                                                    "edge IDs they will be applied to.",
#                                                    br(), br(),
#                                                    strong(".rds files"),
#                                                    "can be saved with",
#                                                    br(),
#                                                    code('saveRDS(objectname, file = "newfilename.rds")'),
#                                                    br(), br(),
#                                                    "Multiple edge attributes can be uploaded from one list object."
#                                                      ))),
#                                         tags$li('.csv file',
#                                           span(class="helper", id="filetypehelper10",
#                                                icon("question-circle"),
#                                                div(id="filetypebox10", class="mischelperbox",
#                                                    strong(".csv files"), "should include headers in the first row.",
#                                                    "The header of each column will become an edge attribute name.",
#                                                    "The values in each column should be in the same order as the",
#                                                    "edge IDs they will be applied to.")))
#                                         )) #end list
#                               )
#              ),
#              conditionalPanel("input.newattrtype != '' & input.newattrtype != 'edgeattr'",
#                 span('Upload a file of one of the following types:', br(),
#                      tags$ul(
#                        tags$li('.rds file',
#                                span(class = "helper",id = "filetypehelper7",
#                                     icon("question-circle"),
#                                     div(id = "filetypebox7",
#                                         class = "mischelperbox",
#                                         "Each attribute should be a vector",
#                                         "element of an R list object.",
#                                         strong("R lists"),
#                                         "should be named, for example:",
#                                         br(),
#                                         code("mylist <- list()"), br(),
#                                         code("mylist$age <- c(18,27,20)"), br(),
#                                         code("mylist$sex <- c('M','F','F')"), br(),
#                                         "The names of the elements in the list",
#                                         "(e.g. 'age' and 'sex' in the list above)",
#                                         "will become the attribute names.",
#                                         "The values in each vector should be",
#                                         "in the same order as the",
#                                         "vertex IDs they will be applied to.",
#                                         br(), br(),
#                                         tags$u("Note:"),
#                                         'Attributes uploaded as vertex names',
#                                         "will automatically be saved into the",
#                                         code("vertex.names"),
#                                         "attribute and the names of the list",
#                                         "will be ignored.", br(), br(),
#                                         strong(".rds files"), "can be saved with",
#                                         br(),
#                                         code('saveRDS(objectname, file = "newfilename.rds")')
#                                     ))),
#                        tags$li('.csv file',
#                                span(class = "helper", id = "filetypehelper8",
#                                     icon("question-circle"),
#                                     div(id = "filetypebox8",
#                                         class = "mischelperbox",
#                                         strong(".csv files"),
#                                         "should include headers in the first row.",
#                                         "The header of each column will become",
#                                         "an attribute name. The values in each",
#                                         "column should be in the same order as the",
#                                         "vertex IDs they will be applied to.",
#                                         br(), br(), tags$u("Note:"),
#                                         "Attributes uploaded as vertex names",
#                                         "will automatically be saved into the",
#                                         code("vertex.names"),
#                                         "attribute and the names in the .csv",
#                                         "file will be ignored.")))))
#              ),
#              conditionalPanel("input.newattrtype != ''",
#                 fileInput(inputId = 'newattrvalue', label = NULL),
#                 p('New attribute name(s):'),
#                 verbatimTextOutput('newattrname'),
#                 actionButton('newattrButton', label = 'Set Attribute',
#                              class = "btn-sm")
#              )
#
#                 )
#
#          )
#
#        )
#      ) # end wellPanel
#   ) # end tabPanel
)

),
column(4,
       tabsetPanel(
         tabPanel('Network Summary', br(),
                  verbatimTextOutput('nwsum')
        ))
),
icon("question-circle", class = "fa-2x helper-btn"),
div(class = "helper-box",
    p("Upload a file of observed network data (must be of a supported type).",
      'Add custom attributes or symmetrize on the "Edit Network" tab.')),
actionLink("dataleft", icon = icon("arrow-left", class = "fa-2x"),
           label = NULL),
actionLink("dataright", icon = icon("arrow-right", class = "fa-2x"),
           label = NULL)
)

# Network Descriptives ---------------------------------------------------

#
# tabPanel("Network Descriptives", value = "tab3",
#    fluidRow(
#    column(7,
#     tabsetPanel(id = 'plottabs',
#         tabPanel('Network Plot', br(),
#            plotOutput('nwplot', click = "plot_click",
#                       dblclick = dblclickOpts(id = "plot_dblclick"),
#                       hover = hoverOpts(id = "plot_hover", delay = 100,
#                                         delayType = "throttle"),
#                       brush = brushOpts(id = "plot_brush")
#            )
#         ),
#         tabPanel('Attributes', br(),
#            conditionalPanel('input.attrview == "Large table"',
#                             dataTableOutput("attrtbl_lg")
#            ),
#            conditionalPanel('input.attrview == "Small tables"',
#                             verbatimTextOutput("attrtbl_sm")
#            ),
#            conditionalPanel('input.attrview == "Plot summaries"',
#                             tags$label("Type of plots"),
#                             helpText("Density plots will only be created for",
#                                      "numeric attributes with more than nine",
#                                      "levels."),
#                             selectInput("attrhistaxis",
#                                         label = NULL,
#                                         choices = c("Barplot: counts" = "count",
#                                                     "Barplot: percents" = "percent",
#                                                     "Density plot" = "density")),
#                             uiOutput("attrhistplotspace"))
#
#         ),
#         tabPanel('Degree Distribution',
#            p(class = 'helper', id = 'ddhelper', icon('question-circle')),
#            div(class = 'mischelperbox', id = 'ddhelperbox',
#                "Degrees are a node level measure for the number of edges incident on each node.",
#                "The degree distribution shows how the edges in a network are distributed among the",
#                "nodes. The amount (or proportion) of nodes with low, medium",
#                "or high degrees contribute to the overall structure of the",
#                "network. The degree distributions of directed graphs can",
#                "be subset by in-degree or out-degree."),
#            plotOutput('degreedist')
#         ),
#         tabPanel('Geodesic Distribution',
#            p(class = 'helper', id = 'gdhelper', icon('question-circle')),
#            div(class = 'mischelperbox', id = 'gdhelperbox',
#                "Geodesics are a dyad level measure for the shortest possible path between a pair of nodes.",
#                'If there is no path between a pair of nodes, the geodesic distance is "inf".',
#                "The geodesic distribution among all possible dyads contributes to the",
#                "structure of network connectivity."),
#            plotOutput('geodistplot')
#         ),
#         tabPanel('More', value = 'More', br(),
#            h5('Conditional uniform graph tests', icon('angle-double-left'),
#               id = "cugtitle"),
#            wellPanel(id = "cugbox",
#                    column(4, uiOutput("dynamiccugterm")),
#                    column(4, selectInput("ncugsims",
#                                          label = "Number of simulations",
#                                          choices = c(100, 200, 500))),
#                    column(3, actionButton("cugButton", label = "Run",
#                                           style = "margin-top: 25px;")),
#                    br(),
#                    plotOutput("cugtest"),
#                    br(),
#                    downloadButton('cugtestdownload', label = "Download Plot",
#                                   class = "btn-sm")
#           ),
#            h5('Mixing matrix', icon('angle-double-left'),
#               id = "mixmxtitle"),
#            wellPanel(id = "mixmxbox",
#                    fluidRow(
#                      column(6, uiOutput('mixmxchooser')),
#                      column(6, downloadButton("mixmxdownload",
#                                               class = "shiftdown25"))
#                    ),
#                    fluidRow(
#                      verbatimTextOutput('mixingmatrix')
#                    )
#            ),
#            h5('Graph-level descriptive indices',
#               icon('angle-double-left'), id="graphleveltitle"),
#            wellPanel(id="graphlevelbox",
#                    fluidRow(
#                      column(4, offset = 7, tags$u('Measure')),
#                      fluidRow(
#                        column(4, p('Density:', class = 'stitle')),
#                        column(3, p(textOutput('gden'), class = 'snum'))),
#
#                      fluidRow(
#                        column(4, p('Degree:', class = 'stitle')),
#                        column(3, p(textOutput('gdeg'), class = 'snum')),
#                        column(4, selectInput('gdegcmode', label = NULL,
#                                              choices = c('indegree', 'outdegree', 'total')
#                        ))),
#                      fluidRow(
#                        column(4, p('Reciprocity:', class = 'stitle')),
#                        column(3, p(textOutput('grecip'), class = 'snum')),
#                        column(4, selectInput('grecipmeas',label = NULL,
#                                              choices = c('dyadic','dyadic.nonnull','edgewise',
#                                                          'edgewise.lrr','correlation')))),
#                      fluidRow(
#                        column(4, p('Transitivity:'), class = 'stitle'),
#                        column(3, p(textOutput('gtrans'), class = 'snum')),
#                        column(4, selectInput('gtransmeas',label = NULL,
#                                              choices = c('weak','strong','weakcensus',
#                                                          'strongcensus','rank','correlation'))
#                        )),
#                      fluidRow(
#                        column(4, p('Betweenness:', class = 'stitle')),
#                        column(3, p(textOutput('gbetw'), class = 'snum')),
#                        column(4, selectInput('gbetwcmode', label = NULL,
#                                              choices = c('directed','undirected',
#                                                          'endpoints','proximalsrc',
#                                                          'proximaltar','proximalsum',
#                                                          'lengthscaled', 'linearscaled'))
#                        )),
#                      fluidRow(
#                        column(4, p('Closeness:', class = 'stitle')),
#                        column(3, p(textOutput('gclose'), class = 'snum')),
#                        column(4, selectInput('gclosecmode', label = NULL,
#                                              choices = c('directed','undirected',
#                                                          'suminvdir','suminvundir')))),
#                      fluidRow(
#                        column(4, p('Stress Centrality:', class = 'stitle')),
#                        column(3, p(textOutput('gstress'), class = 'snum')),
#                        column(4, selectInput('gstresscmode', label = NULL,
#                                              choices = c('directed','undirected')))
#                      ),
#                      fluidRow(
#                        column(4, p('(Harary) Graph Centrality:', class = 'stitle')),
#                        column(3, p(textOutput('ggraphcent'), class = 'snum')),
#                        column(4, selectInput('ggraphcentcmode', label = NULL,
#                                              choices = c('directed', 'undirected')))
#                      ),
#                      fluidRow(
#                        column(4, p('Eigenvector Centrality:', class = 'stitle')),
#                        column(3, p(textOutput('gevcent'), class = 'snum')),
#                        column(4, br())
#                      ),
#                      fluidRow(
#                        column(4, p('Information Centrality:', class = 'stitle')),
#                        column(3, p(textOutput('ginfocent'), class = 'snum')),
#                        column(4, selectInput('ginfocentcmode',label = NULL,
#                                              choices = c('weak', 'strong', 'upper',
#                                                          'lower')))
#                      )
#                      )),
#
#            h5('Vertex-level descriptive indices',
#               icon('angle-double-left'), id = "nodeleveltitle"),
#            wellPanel(id = "nodelevelbox",
#                    fluidRow(
#                      column(2, span("Vertex index:")),
#                      column(5, numericInput('nodeind',
#                                             label = NULL,
#                                             value = 1,
#                                             min = 1))
#                    ),
#                    tags$hr(),
#                    fluidRow(
#                      column(2, offset = 3, tags$u('Current vertex')),
#                      column(3, tags$u('Centrality mode')),
#                      column(2, tags$u('Min')),
#                      column(2, tags$u('Max'))),
#                    fluidRow(
#                      column(3, p('Degree:', class = 'stitle')),
#                      column(2, p(textOutput('ndeg'), class = 'snum')),
#                      column(3, selectInput('ndegcmode',
#                                            label = NULL,
#                                            choices = c('indegree', 'outdegree', 'total')),
#                             class = "smallselect"),
#                      column(2, p(textOutput('ndegmin'), class = 'snum', align = 'center')),
#                      column(2, p(textOutput('ndegmax'), class = 'snum', align = 'center'))
#                    ),
#                    fluidRow(
#                      column(3, p('Betweenness:', class = 'stitle')),
#                      column(2, p(textOutput('nbetw'), class = 'snum')),
#                      column(3, selectInput('nbetwcmode', label = NULL,
#                                            choices=c('directed','undirected',
#                                                      'endpoints','proximalsrc',
#                                                      'proximaltar','proximalsum',
#                                                      'lengthscaled', 'linearscaled')),
#                             class = "smallselect"),
#                      column(2, p(textOutput('nbetwmin'), class = 'snum')),
#                      column(2, p(textOutput('nbetwmax'), class = 'snum'))
#                    ),
#                    fluidRow(
#                      column(3, p('Closeness:', class = 'stitle')),
#                      column(2, p(textOutput('nclose'), class = 'snum')),
#                      column(3, selectInput('nclosecmode', label = NULL,
#                                            choices=c('directed','undirected',
#                                                      'suminvdir','suminvundir')),
#                             class = "smallselect"),
#                      column(2, p(textOutput('nclosemin'))),
#                      column(2, p(textOutput('nclosemax')))
#                    ),
#                    fluidRow(
#                      column(3, p('Stress Centrality:', class = 'stitle')),
#                      column(2, p(textOutput('nstress'), class = 'snum')),
#                      column(3, selectInput('nstresscmode', label = NULL,
#                                            choices = c('directed','undirected')),
#                             class = "smallselect"),
#                      column(2, p(textOutput('nstressmin'))),
#                      column(2, p(textOutput('nstressmax')))
#                    ),
#                    fluidRow(
#                      column(3, p('(Harary) Graph Centrality:', class = 'stitle')),
#                      column(2, p(textOutput('ngraphcent'), class = 'snum')),
#                      column(3, selectInput('ngraphcentcmode', label = NULL,
#                                            choices = c('directed', 'undirected')),
#                             class = "smallselect"),
#                      column(2, p(textOutput('ngraphcentmin'))),
#                      column(2, p(textOutput('ngraphcentmax')))
#                    ),
#                    fluidRow(
#                      column(3, p('Eigenvector Centrality:', class = 'stitle')),
#                      column(2, p(textOutput('nevcent'), class = 'snum')),
#                      column(3, br()),
#                      column(2, p(textOutput('nevcentmin'))),
#                      column(2, p(textOutput('nevcentmax')))
#                    ),
#                    fluidRow(
#                      column(3, p('Information Centrality:', class = 'stitle')),
#                      column(2, p(textOutput('ninfocent'), class = 'snum')),
#                      column(3, selectInput('ninfocentcmode',label = NULL,
#                                            choices = c('weak', 'strong', 'upper',
#                                                      'lower')),
#                             class = "smallselect"),
#                      column(2, p(textOutput('ninfocentmin'))),
#                      column(2, p(textOutput('ninfocentmax')))
#                    )
#            )
#
#         )
#       ), #end tabsetPanel
#      br(),
#      br()
#      ), #end 7 column
#      column(4,
#             tabsetPanel(id = 'displaytabs',
#               tabPanel(title = 'Display Options', br(),
#                  wellPanel(
#                    conditionalPanel('input.plottabs == "Network Plot"',
#                       checkboxInput('iso',
#                                     label = 'Display isolates',
#                                     value = TRUE),
#                       checkboxInput('vnames',
#                                     label = 'Display vertex names',
#                                     value = FALSE),
#                       br(),
#                       sliderInput('transp',
#                                   label = 'Vertex opacity',
#                                   min = 0, max = 1, value = 1),
#                       br(),
#                       uiOutput("dynamiccolor"),
#                       conditionalPanel("Number(output.attrlevels) > 9",
#                          column(10,
#                                 p(id = "closewarning1", icon(name = "remove"),
#                                   class = "warning"),
#                                 div(class = "warning", id = "colorwarning1",
#                                     span(tags$u("Note:"),
#                                          "Color palette becomes a gradient for
#                                          attributes with more than nine levels.")
#                                 )
#                          )),
#                       uiOutput('dynamicsize'),
#                       br(),
#                       actionButton("refreshplot", icon = icon("refresh"),
#                                    label = "Refresh Plot", class = "btn-sm"),
#                       downloadButton('nwplotdownload',
#                                      label = "Download Plot", class = "btn-sm")),
#                    conditionalPanel(condition='input.plottabs == "Attributes"',
#                       selectInput("attrview", label = "View attributes in:",
#                                   choices = c("Large table",
#                                               "Small tables",
#                                               "Plot summaries")),
#                       br(),
#                       uiOutput("attrcheck")
#                    ),
#                    conditionalPanel('input.plottabs == "Degree Distribution"',
#                       uiOutput("dynamiccmode_dd"),
#                       uiOutput("dynamiccolor_dd"),
#                       tags$label("Y-axis units:"), br(),
#                       actionButton("countButton_dd", label = "Count of vertices",
#                                    class = "btn-sm active"),
#                       actionButton("percButton_dd", label = "Percent of vertices",
#                                    class = "btn-sm"),
#                       br(), br(),
#                       tags$label('Expected values of null models:'), br(),
#                       fluidRow(
#                         column(10,
#                                checkboxInput('uniformoverlay_dd',
#                                              label = 'Conditional uniform graphs (CUG)',
#                                              value = FALSE)
#                         ),
#
#                         span(icon('question-circle'), id = "cughelper_dd",
#                              class = "helper",
#                              div(id = "cughelperbox_dd", class = "mischelperbox",
#                                  "Draws from the distribution of simple random graphs with the same",
#                                  "fixed density as the observed network. The mean and 95% confidence",
#                                  "intervals for each degree are plotted."))),
#                       fluidRow(
#                         column(10,
#                                checkboxInput('bernoullioverlay_dd',
#                                              label = 'Bernoulli random graphs (BRG)',
#                                              value = FALSE)
#                         ),
#                         span(icon('question-circle'), id = "brghelper_dd",
#                              class = "helper",
#                              div(id = "brghelperbox_dd", class = "mischelperbox",
#                                  "Draws from the distribution of simple random graphs with the same",
#                                  "stochastic tie probability as the observed network.",
#                                  "The mean and 95% confidence intervals for each degree are plotted."))),
#                       br(),
#                       downloadButton('degreedistdownload',
#                                      label = "Download Plot",
#                                      class = "btn-sm")
#                    ),
#                    conditionalPanel('input.plottabs == "Geodesic Distribution"',
#                       tags$label("Y-axis units:"), br(),
#                       actionButton("countButton_gd", "Count of vertex pairs",
#                                    class = "btn-sm active"),
#                       actionButton("percButton_gd", "Percent of vertex pairs",
#                                    class = "btn-sm"),
#                       br(), br(),
#                       tags$label('Expected values of null models:'), br(),
#                       fluidRow(
#                         column(10,
#                                checkboxInput('uniformoverlay_gd',
#                                              label = 'Conditional uniform graphs (CUG)',
#                                              value = FALSE)
#                         ),
#                         span(icon('question-circle'), id = "cughelper_gd", class = "helper",
#                              div(id = "cughelperbox_gd", class = "mischelperbox",
#                                  "Draws from the distribution of simple random graphs with the same",
#                                  "fixed density as the observed network. The mean and 95% confidence",
#                                  "intervals for each degree are plotted."))),
#                       fluidRow(
#                         column(10,
#                                checkboxInput('bernoullioverlay_gd',
#                                              label = 'Bernoulli random graphs (BRG)',
#                                              value = FALSE)
#                         ),
#                         span(icon('question-circle'), id = "brghelper_gd",
#                              class = "helper",
#                              div(id = "brghelperbox_gd", class = "mischelperbox",
#                                  "Draws from the distribution of simple random graphs with the same",
#                                  "stochastic tie probability as the observed network.",
#                                  "The mean and 95% confidence intervals for each degree are plotted."))),
#                       br(),
#                       verbatimTextOutput('infsummary'),
#                       fluidRow(
#                         column(10,
#                                checkboxInput('excludeInfs',
#                                              label = span('Exclude "inf"s from plot'),
#                                              value = FALSE)),
#                         span(icon('question-circle'), id = "infhelper_gd",
#                              class = "helper",
#                              div(id="infhelperbox_gd", class = "mischelperbox",
#                                  "A pair of nodes without any path connecting",
#                                  'it has a geodesic distance of "inf".'))),
#                       br(),
#                       downloadButton('geodistdownload', label = 'Download Plot',
#                                      class = "btn-sm")
#                    ),
#                    conditionalPanel(condition = 'input.plottabs == "More"',
#                       p("No display options at this time,",
#                         "stay tuned for updates!")
#                    )
#                  )),
#               tabPanel(title = 'Network Summary', br(),
#                        verbatimTextOutput('attr2'))
#             )
#      )
#    ),
#    icon("question-circle", class = "fa-2x helper-btn"),
#    div(class = "helper-box",
#        p("help help help")),
#    actionLink("plotleft", icon = icon("arrow-left", class = "fa-2x"),
#               label = NULL),
#    actionLink("plotright", icon = icon("arrow-right", class = "fa-2x"),
#               label = NULL)
#    ),
#
# # Fit Model ---------------------------------------------------------------
#
#
# tabPanel("Fit Model", value = "tab4",
#
#    fluidRow(
#      column(3,
#         div(class = "xscroll",
#             strong("Network:"),
#             verbatimTextOutput("currentnw1"),
#             strong("Formation:"),
#             verbatimTextOutput("form"),
#             strong("Dissolution:"),
#             verbatimTextOutput("diss")
#         ),
#         actionButton("fitButton", "Fit Model",
#                      class = "btn-primary")
#      ),
#      column(7,
#             tabsetPanel(
#               tabPanel("Edit Formation",
#               fluidRow(
#                 column(6,
#                    strong("Formula:"),
#                    helpText("Type in term(s) and their arguments. For
#                             multiple terms, separate with '+'."),
#                    div(textInput(inputId = "formation", label = NULL,
#                                  value = "edges"),
#                        title = paste("Type in term(s) and their arguments.",
#                                      "For multiple terms, separate with '+'.")
#                    ),
#                   actionButton('updateformulaButton', 'Update Formula',
#                                class = "btn-primary btn-sm"),
#                   actionButton('resetformulaButton', 'Reset Formula',
#                                class = "btn-sm")
#                       ),
#                 column(6,
#                    strong("Target stats:"),
#                    helpText("If values to be targeted are not sufficient statistics
#                             from cross-sectional network."),
#                    textInput("target.stats", label = NULL, value = "")
#                        )
#                 ),
#               fluidRow(style = "margin-top: 5px;",
#                 column(2,
#                        strong('Summary statistics:')),
#                 column(10,
#                        verbatimTextOutput('prefitsum')))
#               ),
#               tabPanel("Edit Dissolution",
#
#                  helpText("The dissolution formula may only include offsets
#                           of the terms in the formation. The order of the
#                           coefficients must correspond to the order of the terms."),
#
#                  fluidRow(column(12,
#                     strong("Terms:"),
#                     uiOutput("dissterms")
#                  )),
#                  fluidRow(column(12,
#                         strong("Coefficient value(s):"),
#                         div(class = "skinny",
#                           uiOutput("disscoefs")
#                         )
#                         ))
#               ),
#               tabPanel("Control Options",
#                      fluidRow(
#                         column(5,
#                                checkboxInput('controldefault',
#                                              'Use default options',
#                                              value = TRUE))
#                      ),
#                      div(id = "controls",
#                       fluidRow(
#                         conditionalPanel("0",
#                           column(4,
#                                  numericInput('MCMCinterval',
#                                               label = "Interval:",
#                                               value = 1024),
#                                  title = paste("Number of proposals between sampled statistics.")
#                             ),
#
#                           column(4,
#                                  numericInput('MCMCburnin',
#                                               label = "Burn-in:",
#                                               value = 16384),
#                                  title = paste("Number of proposals before any MCMC sampling is done.",
#                                                "Defaults to 16 times the MCMC interval, unless burn-in is specified after the interval.")
#                             )
#                           ),
#                         conditionalPanel("1",
#                           column(4,
#                                  numericInput("EGMME.burnin.min",
#                                               label = "Burn-in min:",
#                                               value = 1000),
#                                  numericInput("EGMME.burnin.max",
#                                               label = "Burn-in max:",
#                                               value = 100000)
#                           ),
#                           column(4,
#                                  numericInput("EGMME.burnin.pval",
#                                               label = "Burn-in p-value:",
#                                               value = 0.5,
#                                               step = 0.05),
#                                  numericInput("EGMME.burnin.add",
#                                               label = "Burn-in add:",
#                                               value = 1)
#                                  )
#                           ),
#                         column(4,
#                                textInput("customMCMCcontrol",
#                                          label = "Other controls:",
#                                          value = ""),
#                                title = paste("Other arguments to be passed to",
#                                              "control.stergm")
#                         )
#                       )
#                       )
#                      ),
#               tabPanel("Term Documentation",
#                        br(),
#                        div(class = "placeholder",
#                            fluidRow(
#                              column(12,
#                                     a("Commonly used ergm terms",
#                                       href = "http://statnet.csde.washington.edu/EpiModel/nme/d2-ergmterms.html",
#                                       target = "_blank"), br(),
#                                     a("Term cross-reference tables",
#                                       href = "http://cran.r-project.org/web/packages/ergm/vignettes/ergm-term-crossRef.html",
#                                       target = "_blank"), br(), br()
#                              ),
#                              column(6,
#                                     actionButton("matchingButton", "Compatible terms",
#                                                  class = "btn-sm active"),
#                                     actionButton("allButton", "All terms",
#                                                  class = "btn-sm")
#                              ),
#                              column(4, uiOutput("listofterms"))
#                            ),
#                            fluidRow(
#                              column(12,
#                                     div(id="termdocbox",
#                                         uiOutput("termdoc")
#                                     ),
#                                     div(id = "termexpand",
#                                         icon(name = "angle-double-up"))
#                              )
#
#                            )
#
#                        )
#               )
#             ) #end tabsetPanel
#         )
#    ),
#    br(),
#    tabsetPanel(id = 'fittingTabs',
#                tabPanel('Model Summary', br(),
#                         verbatimTextOutput('modelfitsum'),
#                         downloadButton("modelfitdownload",
#                                        "Download Summary (.txt)",
#                                        class="btn-sm")),
#                tabPanel('Model Fit Report', br(),
#                         verbatimTextOutput('modelfit'))
#    ), br(), br(),
#
#    icon("question-circle", class = "fa-2x helper-btn"),
#    div(class = "helper-box",
#        p("help help help")),
#    actionLink("fitleft", icon = icon("arrow-left", class = "fa-2x"),
#               label = NULL),
#    actionLink("fitright", icon = icon("arrow-right", class = "fa-2x"),
#               label = NULL)
#    )
  ) #end navbarPage
) #end shinyUI