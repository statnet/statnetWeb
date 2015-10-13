
## server.R for stergm app

library(statnet)
library(tergm)

data(faux.mesa.high)
data(florentine)
data(sampson)
data(samplk)
data(ecoli)
data(molecule)
data(kapferer)

shinyServer(
  function(input, output, session){

#     #move to Help page when user clicks Help link button
#     observe({
#       if(input$helpLink == 0) {return()}
#       isolate({
#         updateTabsetPanel(session, 'navbar', selected = 'tab8')
#       })
#     })

    #move to Data panel when user clicks Get Started button
    observe({
      if(input$startButton == 0) {return()}
      isolate({
        updateTabsetPanel(session, 'navbar', selected = 'tab2')
      })
    })

    #update active tab in navbar when arrows are clicked
    leftarrowclicks <- reactive({
      input$dataleft+input$plotleft+input$fitleft
    })
    rightarrowclicks <- reactive({
      input$dataright+input$plotright+input$fitright
    })
    observe({
      if(leftarrowclicks() == 0) {return()}
      tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4')
      current <- isolate(which(input$navbar==tabOptions))
      updateTabsetPanel(session, 'navbar', selected=tabOptions[current-1])
    })
    observe({
      if(rightarrowclicks() == 0) {return()}
      tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4')
      current <- isolate(which(input$navbar==tabOptions))
      updateTabsetPanel(session, 'navbar', selected=tabOptions[current+1])
    })

})
