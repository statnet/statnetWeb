
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
    id= 'navbar', windowTitle = 'stergm', collapsible=TRUE,

    # Front Page (About) ------------------------------------------------------

    tabPanel(title=span('statnetWeb: stergm', id = "sttitle"),
             value='tab1',

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
                      actionButton('startButton', label='Get Started',
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
tags$pre(id='scitation','@Manual{handcock:statnet,
title = {statnet: Software tools for the Statistical Modeling of Network Data},
author = {Mark S. Handcock and David R. Hunter and Carter T. Butts and Steven M. Goodreau and Martina Morris},
year = {2003},
address = {Seattle, WA},
url = {http://statnetproject.org}
}'),

p(strong("tergm package")),
tags$pre(id='swcitation',"@Manual{krivitsky:tergm,
author = {Pavel N. Krivitsky and Mark S. Handcock},
title = {tergm: Fit, Simulate and Diagnose Models for Network Evolution based on
Exponential-Family Random Graph Models},
organization = {The Statnet Project (http://www.statnet.org)},
year = {2014},
note = {R package version 3.2.4},
url = {CRAN.R-project.org/package=tergm},
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
                                   a("Krivitsky, Handcock",
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


    tabPanel("Data"),

# Network Plot ------------------------------------------------------------


    tabPanel("Network Plot"),

# Fit Model ---------------------------------------------------------------


    tabPanel("Fit Model")
  )
)