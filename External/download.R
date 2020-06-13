output$pageHousekeeping <- renderUI(tagList(
  fluidRow(#tags$title("Search Reference genes"), tags$meta(name="description", content="Search for the most stable human tissue specific reference genes and primers to normalize qPCR expression."), 
    class="Container-fluid", includeHTML('www/download.html'),useShinyjs(), 
               
               includeHTML("External/HTML/footer.html")
           
           
  )
  
))






output$frame <- renderUI({
  test = "http://amp.pharm.mssm.edu/Harmonizome/gene/EEF2"
  my_test <- tags$iframe(src=test, height=600, width=835)
  print(my_test)
  my_test
})