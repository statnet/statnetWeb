
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
         column(6,
          selectInput("filetype", label = "File type",
              choices=c("built-in network" = 1,
                        "statnet network object (*.rds)" = 2,
                        "matrix of relational data (*.csv or *.rds)" = 3,
                        "Pajek network (*.net)" = 4,
                        "Pajek project (*.paj)" = 5))
         ),
         conditionalPanel(condition = "input.filetype == 1",
          column(6,
                 br(style="line-height:26px;"),
                 selectizeInput('samplenet', label=NULL,
                                choices=c("Choose a network" = "",
                                          "ecoli1", "ecoli2",
                                          "faux.mesa.high", "flobusiness",
                                          "flomarriage", "kapferer",
                                          "kapferer2", "molecule",
                                          "samplike", "samplk1",
                                          "samplk2", "samplk3"))
                          )
         )
       ),
       conditionalPanel(condition='input.filetype == 2 | input.filetype == 3',
                        p(class="helper", id="Robjhelp", icon("question-circle"),
                          span("What is an .rds file?", style="font-size:0.85em;")),
                        div(class="mischelperbox", id="Robjbox",
                            'When working in R, an object in your environment',
                            'can be saved to a .rds file from the command line',
                            'in the following way:',
                            code('saveRDS(objectname, file="newfilename.rds")'),
                            br(), 'By default the file will be saved',
                            'into the current working directory. The full path',
                            'to a new location can be specified in the ',
                            code('file ='), 'argument, or set',
                            code('file = file.choose(new = TRUE)'),
                            'to use a save dialog box.')
       ),
       fluidRow(
         conditionalPanel(condition='input.filetype == 3',
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
                                 checkboxInput('dir', 'directed?', value=TRUE),
                                 checkboxInput('loops', 'loops?', value=FALSE),
                                 checkboxInput('multiple', 'multiple?', value=FALSE),
                                 checkboxInput('bipartite', 'bipartite?', value=FALSE))
         ),
         conditionalPanel(condition='input.filetype == 5',
                          column(6,
                                 uiOutput('pajchooser')))
       )
     )

  ),
  tabPanel("Edit Network")
)

),
column(4,
       tabsetPanel(
         tabPanel('Network Summary', br(),
                  verbatimTextOutput('nwsum')
        ))
),
icon("question-circle", class = "fa-2x helper-btn"),
div(class = "helper-box", style = "display: none;",
    p("Upload a file of observed network data (must be of a supported type).",
      'Add custom attributes or symmetrize on the "Edit Network" tab.')),
actionLink("dataleft", icon = icon("arrow-left", class = "fa-2x"),
           label = NULL),
actionLink("dataright", icon = icon("arrow-right", class = "fa-2x"),
           label = NULL)
),

# Network Plot ------------------------------------------------------------


    tabPanel("Network Plot", value = "tab3",

             icon("question-circle", class = "fa-2x helper-btn"),
             div(class = "helper-box", style = "display: none;",
                 p("help help help")),
             actionLink("plotleft", icon = icon("arrow-left", class = "fa-2x"),
                        label = NULL),
             actionLink("plotright", icon = icon("arrow-right", class = "fa-2x"),
                        label = NULL)
             ),

# Fit Model ---------------------------------------------------------------


    tabPanel("Fit Model", value = "tab4",

             icon("question-circle", class = "fa-2x helper-btn"),
             div(class = "helper-box", style = "display: none;",
                 p("help help help")),
             actionLink("fitleft", icon = icon("arrow-left", class = "fa-2x"),
                        label = NULL),
             actionLink("fitright", icon = icon("arrow-right", class = "fa-2x"),
                        label = NULL)
             )
  )
)