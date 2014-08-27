# Author: KirkLi
###############################################################################
inputTextarea <- function(inputId,  label="",value="", nrows=3, ncols=5) {
  tagList(
    singleton(tags$head(tags$script(src = "textarea.js"))),
    tags$label(label, `for` = inputId),
    tags$textarea(id = inputId,
                  class = "inputtextarea",
                  rows = nrows,
                  cols = ncols,
                  as.character(value))
  )
}

chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",tagList(
          tags$h5(leftLabel),
          tags$select(class="left", size=size, multiple=multiple, leftChoices))
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-right", "right-arrow fa-2x"),
            tags$br(),
            icon("arrow-circle-left", "left-arrow fa-2x")
        ),
        div(class="chooser-container chooser-right-container",tagList(
          tags$h5(rightLabel),
          tags$select(class="right", size=size, multiple=multiple, rightChoices))
        )
    )
  )
}



registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)