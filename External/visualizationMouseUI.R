div(
  class = "Container-fluid panelstyle",
  selectInput(
    'searchMMGenes',
    'Select a mouse housekeeping gene to show its transcript expression level in different tissues and cells',
    sort(unique(Mouse_HK_genes$choice)),
    selectize = FALSE
  ),
  
  
  #span(class="badge badge-pill badge-danger", style="float: right;", "New feature"),
  br(),
  #verbatimTextOutput("outx"),
  div(style = "width: 100%;", uiOutput("plotSearchMM")),
  
  
  div(
    id = "tabpanelViz",
    class = "panelstyle",
    style = "margin-top: 50px;",
    
    plotlyOutput("plotMmSearch", height = "500px")
    
  )
)