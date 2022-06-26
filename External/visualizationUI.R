div(
  class = "Container-fluid panelstyle",
  #includeHTML("External/HTML/searchHsGenes.html"),
  
  selectInput(
    'searchHSGenes',
    'Select a human housekeeping gene to show its transcript expression in different tissues/cells',
    sort(HK_geneAtualizadoRef$choice),
    selectize = FALSE
  ),
  
  #span(class="badge badge-pill badge-danger", style="float: right;", "New feature"),
  br(),
  #verbatimTextOutput("outx"),
  div(style = "width: 100%;", uiOutput("genename")),
  
  
  div(
    class = "col-xs-12",
    plotlyOutput("plotHsSearch", height = "500px", width = "100%")
    
  )
)