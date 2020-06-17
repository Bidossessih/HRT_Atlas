output$pageHousekeeping <- renderUI(tagList(
  fluidRow(useShinyjs(),
           includeHTML("External/HTML/homePageGlobal.html"),
           
           includeHTML("External/HTML/footer_home.html"))

)) # End renderUI
