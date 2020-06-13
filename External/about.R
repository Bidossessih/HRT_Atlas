output$pageHousekeeping <- renderUI(tagList(
fluidRow(#tags$title("Search Reference genes"), tags$meta(name="description", content="Search for the most stable human tissue specific reference genes and primers to normalize qPCR expression."), 
  class="Container-fluid",useShinyjs(), 
  includeHTML("External/HTML/about.html"),
  
  includeHTML("External/HTML/footer.html")
  
)
  
))
 



