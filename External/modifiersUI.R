div(class="Container-fluid panelstyle",       
             
             selectInput('searchHSGenes', 'Select a gene to show some modifiers of its expression', sort(HK_geneAtualizadoRef$choice), selectize=FALSE),
           
             
            #verbatimTextOutput("outx"),
            div(style="width: 100%;", uiOutput("genename")),
            
            
            div(class="col-xs-12",
                
                #tabsetPanel(
                 # tabPanel(id="exp", "Expression", plotlyOutput("plotHsSearch", height = "500px")),
                  #tabPanel(id="funcAsso", "Funcional associations",
                   #        br(),
                           
                           span(style="text-align:justify", "HT Atlas v1.0 is integrated with", HTML("<a href='http://amp.pharm.mssm.edu/Harmonizome/' target='_blank'>Harmonizome</a>"), "database.", br(),
                                textOutput("genenameChoice")), br(),
                           DT::DTOutput("accordionhk1"),br(),
                           DT::DTOutput("accordionhk2"), br(),
                           DT::DTOutput("accordionhk3"))
          )