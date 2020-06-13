output$pageHousekeeping <- renderUI(tagList(
  fluidRow(useShinyjs(),
           includeHTML("External/HTML/homePageGlobal.html"),
           
           includeHTML("External/HTML/footer.html"))

)) # End renderUI
