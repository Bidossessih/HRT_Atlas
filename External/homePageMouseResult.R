hidden(
  
  div(style = "width:95%; margin:auto; padding-bottom:20px;", id = "tabpanelUI",
      div(class = "container-fluid",
          
          div(class="row",
            
            div(class = "col-12", style = "padding-bottom: 3%;", a(
              img(src = "arrow-alt-circle-left-regular.svg", style = "width:20px; margin-top:-3px"),
              actionLink('previousSearch', ' Previous', icon = NULL)
            ))
            ,
            #verbatimTextOutput("out5"),
            
            
            
            div(class = "col-sm-3 well",
              style = "margin-bottom:0px; padding:15px",
              tags$form(
                style = "padding:0px",
                
                div("Filter Criteria", class = "text-black text-center font-weight-bold", style =
                      "width: 100%; height: 35px;"),
                hr(),
                #span(class="badge badge-pill badge-danger", style="float: right;", "New feature"), br(),
                span(
                  "log transformation of RPKM is the recommended option of MFC metric calculation. However,
                  users have the possibility to enable linear scale.",
                  class = "text-muted text-justify fontmuted"
                ),
                br(),
                
                radioButtons(
                  "mfc",
                  "Scale of RPKM:",
                  c("Logarithmic" = "log",
                    "Linear" = "linear"),
                  selected = "log"
                ),
                br(),
                hr(),
                br(),
                sliderInput(
                  "rpkm",
                  "Mean of RPKM higher or equal than:",
                  min = 1,
                  max = 100,
                  value = 30
                ),
                br(),
                hr(),
                
                
                #verbatimTextOutput("outxId", placeholder = FALSE),
                
                div(id = "sel1", style = "width: 100%;", uiOutput("selectUI")),
                
                div(
                  id = "sel2",
                  style = "width: 100%;",
                  uiOutput('validatedPrimerSelectInput')
                )
                
              )#End form
              ),
            #div sidebar
            
            
            div(class = "col-sm-9 tabpadding",
              
              
              tabsetPanel(
                id = "tabsetID",
                tabPanel(
                  id = "ref",
                  "Reference Transcripts",
                  value = "refTranscr",
                  br(),
                  uiOutput("outCap"),
                  withSpinner(uiOutput("tabHK"))
                ),
                
                tabPanel(
                  id = "refPrimer",
                  "Specific Primers",
                  value = "spec_primer", br(),
                  p(class="text-justify", "We manually designed some transcript specific primers that can be used in your 
                    experiments. The temperature calculations are done assuming 200 nM of annealing 
                    oligo concentration, 50mM of salt concentration (Na+) and 1.5 mM of divalent ion 
                    concentration (Mg++). We recommend using of at least two reference transcripts for
                    normalization purpose."),
                  hidden(div(id = "noPrimer", "No primer")),
                  withSpinner(uiOutput("tabRef"))
                ),
                tabPanel(
                  id = "valPrimer",
                  value = "Validation",
                  "Validation",
                  class = "Container-fluid",
                  
                  div(
                    class = "Container-fluid panelstyle",
                    #span(class="badge badge-pill badge-danger", style="float: right;", "New feature"),
                    br(),
                    
                    
                    #selectIput from server
                    #verbatimTextOutput("outx"),
                    
                    div(style = "width: 100%;", withSpinner(uiOutput("genenameVal"))),
                    withSpinner(uiOutput('imageVal'))
                  )
                  
                )
                
              )#End tabsetPanel
              
            )#mainPanel
            
            
            
          ) #End sidebarlayout
        )
      ))#End hidden), #End wrapper of hideen, #End of hidden
      
