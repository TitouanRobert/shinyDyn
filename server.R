function(input, output, session) {
  
  observe({
    if(input$k!=0){
      for(i in input$k)
      {
        v <- paste0("out", i)
        assign(v, callModule(module = inputSlider_action,
                             id = v), pos = .GlobalEnv)
      }
    }
  })
  
  observeEvent(input$k, {
    insertUI(
      selector = "#k",
      where = "afterEnd",
      ui = inputSlider(paste0("out", input$k))
    )
  })
  
  DOL <- reactive({
    reactL("out", "bins", input$k)
  })
  
  output$sum <- renderText(sum(unlist(DOL())))
  
  observe(
    {
      print(DOL())
      if(input$k>0)
      {
        D = NULL
        for(i in 1:input$k){
          d = paste0("out", i)
          FF = try(get(d)()$rm)
          if(is.null(FF))FF = 0
          if("try-error" %in% class(FF))FF = 0
          D = c(D, FF)
        }
        print(D)
        if(length(D)>0)
        {
          for(i in 1:length(D))
          {
            if(D[i]>0){
              
              removeUI(
                selector = paste0("#out",i, "-bins")
              )
              removeUI(
                selector = paste0("#out",i)
              )
              toRm = paste0("out",i)
              rm(list = toRm, envir = .GlobalEnv)
            }
          }
        }
      }
    }
  )
}