
library(shiny)
library(visNetwork)

inputSlider <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    column(12,id = id, column(3,
                      actionButton(ns("rm"), "remove")
    ),
    column(9, sliderInput(ns("bins"),
                          "Number of bins:",
                          min = 1,
                          max = 50,
                          value = 30))
    )
  )
}

inputSlider_action <- function(input,
                               output,
                               session,
                               action){
  reactive({
    list(
      rm = input$rm,
      bins = input$bins)
  })
  
}

reactL <- function(moduleName,
                     inputName,
                     k){
  if(k == 0)return(NULL)
  sapply(1:k, function(X){
    toEval = paste0(moduleName, X, '()$', inputName)
    TT = try(eval(parse(text = toEval)))
    if("try-error" %in% class(TT))return(NULL)
    TT
  }, simplify = FALSE)
}

