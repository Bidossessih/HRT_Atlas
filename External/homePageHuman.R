output$pageHousekeeping <- renderUI(tagList(
fluidRow(#tags$title("Search Reference genes"), tags$meta(name="description", content="Search for the most stable human tissue specific reference genes and primers to normalize qPCR expression."), 
useShinyjs(), 
includeHTML("External/HTML/homePageHuman.html"),


                  hidden(div(id="tabpanelUI", class="container", style="min-height: 300px;",
                             
                             div(class="row",
                                 
                                 
                                 
                                 div(class="col-12", style="margin-top: -6%; padding-bottom: 3%;", a(img(src = "arrow-alt-circle-left-regular.svg", style="width:20px; margin-top:-3px"), actionLink('previousSearch',' Previous', icon = NULL)))
                                 ,
                        #verbatimTextOutput("out5"),
                        
                        
                            
                            div(class="col-sm-3 well", style="min-height:100vh; margin-bottom:0px; padding:15px",
                                        tags$form(style="padding:0px",
                                                  
                                        div("Filter Criteria", class="text-black text-center font-weight-bold", style="width: 100%; height: 35px;"),
                                        hr(),
                                        span("log transformation of RPKM is the recommended option of MFC metric calculation. However, 
                                             users have the possibility to enable linear scale.", class="text-muted text-justify fontmuted"),br(),
                                        
                                        radioButtons("mfc", "Scale of RPKM:",
                                                     c("Logarithmic" = "log",
                                                       "Linear" = "linear")), br(), hr(), br(), 
                                        sliderInput("rpkm", "Minimum of RPKM:",
                                                    min = 5, max = 30, value = 30
                                        ), br(), hr(),
                                        
                                        verbatimTextOutput("outxId", placeholder = FALSE),
                                        
                                        shinyjs::hide(div(id="sel1", style="width: 100%;", uiOutput("selectUI"))),
                                        
                                        div(id="sel2", style="width: 100%;", uiOutput('validatedPrimerSelectInput'))
                                        
                                        )#End form
                                      ), #div sidebar
                                      
                                      
                                      div(class="col-sm-9",
                          
                          
                          tabsetPanel(
                              tabPanel(id="ref", "Reference Transcripts", DT::DTOutput("tabHK")),#)#,
                              tabPanel(id="rmodifiers", "Expression Modifiers",
                                       span(style="text-align:justify", "HT Atlas v1.0 is integrated with", HTML("<a href='http://amp.pharm.mssm.edu/Harmonizome/' target='_blank'>Harmonizome</a>"), "database.", br(),
                                            strong(textOutput("genenameChoice"))), br(),
                                       
                                       withSpinner(DT::DTOutput("Mod1")),br(),
                                       withSpinner(DT::DTOutput("Mod2")), br(),
                                       withSpinner(DT::DTOutput("Mod3"))),
                              
                              #########Epiregio
                              tabPanel(id="epi", "Regulatory Elements",
                                       span(style="text-align:justify", "HT Atlas v1.0 is integrated with", HTML("<a href='http://amp.pharm.mssm.edu/Harmonizome/' target='_blank'>Epiregio server</a>"), "via REST API.", br()
                                           ), br()
                                     
                                       ),
                              
                              tabPanel(id="refPrimer", "Specific Primers", hidden(div(id="noPrimer", "No primer")), DT::DTOutput("tabRef")),
                              tabPanel(id="valPrimer", value="Validation", "Validation", class="Container-fluid",  
                                 onclick= "alert(date())",
                                       div(class="Container-fluid panelstyle",br(),
                                           
                                
                                          #selectIput from server
                                          #verbatimTextOutput("outx"),
                                   
                                         div(style="width: 100%;", uiOutput("genenameVal")),
                                          withSpinner(uiOutput('imageVal')))
                                       
                                       )
                          )#End tabsetPanel
                          
                              )#mainPanel
                          
                        
                        
                         ) #End sidebarlayout
                        )
                        )#End hidden
                        
                  ),# wrapper

includeHTML("External/HTML/footer.html")
                  
          
)# End fluidPage

) # End renderUI






################################## Server code
observeEvent(input$rmodifiers, {
  shinyjs::show("sel1")

})

observeEvent(input$epi, {
  shinyjs::show("sel1")
  
})

observeEvent(input$refPrimer, {
  shinyjs::show("sel2")
  
})

  observeEvent(input$Validation, {
    shinyjs::show("sel2")
    alert(date())
  
})


 shinyjs::click("idown1", asis = FALSE)
 shinyjs::click("idown2", asis = FALSE)
 
 onclick("idown1", alert(date()))
 shinyjs::onclick("valPrimer", alert(date()))

###################################

output$out5 <- renderText(paste0("External/Data/db1/", gsub(" ", "_", input$search), ".RData"))

observeEvent(input$button, {
  shinyjs::hide("homehuman")
  shinyjs::show("tabpanelUI")
  shinyjs::hide("tabpanelUI2")
  shinyjs::show("headersearch")
  shinyjs::show("headersearch2")
  
})

observeEvent(input$previousSearch, {
  shinyjs::show("homehuman")
  shinyjs::hide("tabpanelUI")
  
  
})


observeEvent(input$buttonresult, {
  shinyjs::hide("homehuman")
  shinyjs::show("tabpanelUI2")
  shinyjs::show("tabpanelUI")
  shinyjs::show("headersearch")
  shinyjs::show("headersearch2")
  
})

observeEvent(input$geneinputID, {
  shinyjs::hide("homehuman")
  shinyjs::show("tabpanelUI")
  addClass(id = "pageUIhuman", class = "addedbackground")
})






###################### Render SelectectInput UI


output$selectUI <- renderUI({
  
  table=gsub(" ", "_", input$search)
  
  data <- data.frame(tbl(con, table))# uncomment
  
  #data <- load(paste0("External/Data/db1/", table, "RData"))
  
  
  tab <- tab_echant[table,]
  
  data <- data.frame(data)
  
  data <- na.omit(select(data, c("Rank","Transcript_ID","Gene_name")) %>% arrange(Rank))
  
  #Use interaction and lexicography to preserve the ranking order
  
  choiceList <- interaction(data[,3], " ", "(", data[,2], ")", sep=  "", lex.order = FALSE)
  
  selectInput('selectHSGenes', 'Select a gene to show some modifiers of its expression', choiceList, selectize=FALSE)
  
})



###############################

output$tabHK <- DT::renderDT({ 
  #load(paste0("External/Data/", input$search,".RData"))
  
  #connect to database
  #con <- dbConnect(RSQLite::SQLite(), "/srv/shiny-server/Housekeepingapp/db/Housekeeping.sqlite")
  #con <- src_sqlite("/home/bidossessi/Desktop/Ubuntu/RefTranscriptDb/HK Transcript_sqlite/Housekeeping.sqlite")
  #Query
  table=gsub(" ", "_", input$search)
  
    data <- data.frame(tbl(con, table))
  #data <- load(paste0("External/Data/db1/", table, ".RData"))
  
  
  #disconnect
  #dbDisconnect(con)
  #load(paste0("External/Data/Ref/", table,"nonPseudogene.RData"))
  
  tab <- tab_echant[table,]
  
  data <- data.frame(data)
  
  #data = TissueResultMerged
  
  hk <- select(data, c("Rank","Transcript_ID","Gene_name", "RPKM", "SD_Log2_RPKM", "MFC_mean", "Chromosome","Transcript_start","Transcript_end"))
  
  #hk <- data[, which(colnames(data) %in% c("Rank","Transcript_ID","Gene_name", "RPKM", "SD_Log2_RPKM", "MFC_min", "Chromosome","Transcript_start","Transcript_end"))]
  hk[, c(4)] <- round(hk[, c(4)], digits = 0)
  
  hk[, c(5:6)] <- round(hk[, c(5:6)], digits = 2)
  colnames(hk) <- c("Rank", "Ensembl ID", "Gene", "RPKM" , "Std deviation", "MFC", "Chromosome", "Start position", "End position")
  hk <- mutate(hk, Link1=paste0('<a href=https://www.genecards.org/cgi-bin/carddisp.pl?gene=',hk$Gene, ' ' , 'target="_blank"'))
  hk <- mutate(hk, Link=paste0(Link1,'>',hk$Gene, '</a>'))
  
  hk[,3] <- hk[,11]
  hk <- hk[,1:6]
  
  #Popup of colnames
  
  rpkmPop <-'<a  data-toggle="tooltip" title="Read count are normalized to Reads Per Kilobase Million. RPKM is a normalized transcript/gene expression based on RNA-seq data. Mean RPKM are presented for each tissue or cell type.">Normalized Expression<img src="info.png" style="width: 10px;"></a>'
  
  mfcPop <-'<a  data-toggle="tooltip" title="The Maximum Fold Change (MFC) is the ratio of the maximum RPKM and mean RPKM values observed for each transcript. To be considered stable, a transcript must have a MFC less than 2.">MFC <img src="info.png" style="width: 10px;"></a>'
  
  ensemblPop <-'<a  data-toggle="tooltip" title="Transcript identification according to Ensembl database."> Ensembl ID<img src="info.png" style="width: 10px;"></a>'
  
  sdPop <-'<a  data-toggle="tooltip" title="Variation estimated as standard deviation of log2 of Read Per Kilobase Million."> Std Deviation <img src="info.png" style="width: 10px;"></a>'
  
  #rpkm <- HTML("<a href='https://www.ebi.ac.uk/training/online/glossary/rpkm'>RPKM</a>") #link of rpkm definition
  colnames(hk)[4] <- rpkmPop
  
  colnames(hk)[6] <- mfcPop
  
  colnames(hk)[2] <- ensemblPop
  
  colnames(hk)[5] <- sdPop
  #https://www.w3schools.com/howto/tryit.asp?filename=tryhow_css_tooltip
  
  #colnames(hk)[4] <- rpkmPop#'<div class="tooltip">RPKM<span class="tooltiptext">Reads Per Kilobase of transcript, per Million mapped reads (<a id="inf" style="color:white" href="http://www.arrayserver.com/wiki/index.php?title=RPKM">RPKM</a>) is a normalized unit of transcript expression. It scales by transcript length to compensate for the fact that most RNA-seq protocols will generate more sequencing reads from longer RNA molecules.</span></div>'
  
  #colnames(hk)[4] <- as.character(popify(actionLink(inputId="t_1", label=colnames(hk)[4]), title=paste("message1"), placement = "bottom", trigger = "hover", options = NULL))
  
  DT::datatable(na.omit(arrange(as.data.frame(hk), Rank)), rownames = FALSE, escape = FALSE, class = 'cell-border stripe',
                
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', HTML(paste0(nrow(hk), " ", "Transcripts found from ", " ", tab[,2], " ", "high quality", " ", "<a class='font-weight-bold font-italic'>",input$search, "</a> ", "samples. 
                                                                                                                        Highly expressed transcripts based on", " ", "<a href='https://www.ebi.ac.uk/training/online/glossary/rpkm'", " ", "target='_blank'>RPKM</a>"), "are highlighted in green and moderatly expressed transcripts in blue.")
                  #paste0("abc", "<div><a id='inf'>RPKM</a></div>")
                  ),
                extensions =c ('ColReorder', 'Buttons', 'FixedHeader'),
                
                options = list(
                  fixedHeader = FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),   
                  dom = 'Blfrtip',   
                  buttons = 
                    list( 'copy', 'print', list(
                      extend = 'collection',
                      buttons = c('csv', 'excel','pdf'),
                      text = 'Download'
                    )), language = list(search = 'Filter:'),
                  orderClasses = F,
                  scrollX = TRUE,
                  #pageLength = 5, lengthMenu = c(5, 10, 15, 20, nrow(hk)),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'padding-left' : '0px', 'color': '#fff', 'width' : 'auto !important'});",
                    "$(this.api().table().body()).css({'padding-left' : '0px', 'width' : 'auto'});",
                    "}")
                )) %>% formatStyle(
                  colnames(hk)[4],
                  backgroundColor = styleInterval(50, c('rgba(0, 147, 255, 0.72)', '#4fda4f')))
})

#Display designed primers


output$tabRef <- DT::renderDT({ 
  # Check the availability of primers
  #validate(
   # need(gsub(" ", "_", input$search) %in% sampleTable$Type, paste("No available primer for", input$search, ". Please consider sharing with us any designed primer in your experiment."))
  #)
  
  # load primer table
  #table=gsub(" ", "_", input$search)
  
  #load(paste0("External/Data/DesignedPrimer/", table, ".RData"))
  table=gsub(" ", "_", input$search)
  
  data <- data.frame(tbl(con, table))# uncomment
  
  #data <- load(paste0("External/Data/db1/", table, "RData"))
  
  
  tab <- tab_echant[table,]
  
  data <- data.frame(data)
  
  data <- na.omit(select(data, c("Rank","Transcript_ID","Gene_name")) %>% arrange(Rank))
  
  
  dPrimer = na.omit(unique(merge(data[,1:2], humanPrimer, all.x = T, by = "Transcript_ID")))
  
  dPrimer = arrange(dPrimer, Rank)
  
  #dPrimer$Rank = c(1:nrow(dPrimer))
  
  #Popup of colnames
  
  rankPop <-'<a  data-toggle="tooltip" title="Transcript rank according to Reference Transcripts table.">Rank<img src="info.png" style="width: 10px;"></a>'
  
  
  #rpkm <- HTML("<a href='https://www.ebi.ac.uk/training/online/glossary/rpkm'>RPKM</a>") #link of rpkm definition
  colnames(dPrimer)[2] <- rankPop
  
  #svgico  ====> voir global.R
  
  dPrimer = dPrimer[, c(2,1,3:8)]
  
  #dPrimer = mutate(dPrimer, Validation=paste('<a class=" dt-center" data-toggle="modal" data-target="#exampleModalScrollable">', 
   #                                          svgico1,"id=", Gene,"data-filter=", Gene, svgico2, "</a>"))
  #dPrimer = dPrimer[, c(1,2,9,3:8)]

  
  
  DT::datatable(na.omit(dPrimer), rownames = FALSE, escape = FALSE, class = 'cell-border stripe',
                
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', "We manually designed some transcript specific primers that can be used in your experiments. The temperature calculations are done assuming 200 nM of annealing oligo concentration. We recommend using of at least two reference transcripts for normalization purpose."
                ),
                extensions =c ('ColReorder', 'Buttons', 'FixedHeader'),
                
                options = list(
                  fixedHeader = FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),   
                  dom = 'Blfrtip',   
                  buttons = 
                    list('csv', 'excel','pdf'), language = list(search = 'Filter:'),
                     
                  orderClasses = TRUE,
                  scrollX = TRUE,
                  #pageLength = nrow(dPrimer),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'color': '#fff'});",
                    "}")
                )) %>% formatStyle(
                  colnames(dPrimer)[4],
                  backgroundColor = styleInterval(50, c('#00ffe4', '#4fda4f')))
})


refPop <-HTML('<a  data-toggle="tooltip" title="Suitable reference transcripts for normalization of target gene expression during RT-qPCR analysis. This list present highly stable transcripts filtered stringent std deviation, MFC and expression level"> Reference Transcripts <img src="info.png" style="width: 10px;"></a>')



onevent("mousehover", "essai", shinyjs::alert("Type 'name' or '2' to see the features. "))







################################Display table with disease from harmonizome

output$Mod1 <- DT::renderDT({ 
  
  ensemblPop <-'<a  data-toggle="tooltip" title="Transcript identification according to Ensembl database."> Ensembl ID<img src="info.png" style="width: 10px;"></a>'
  
  sdPop <-'<a  data-toggle="tooltip" title="Variation estimated as standard deviation of log2 of Read Per Kilobase Million."> Std Deviation <img src="info.png" style="width: 10px;"></a>'
  
  
  tab <- as.character(HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$selectHSGenes))))), 2])# Extract gene symbol
  
  #tab = "AAMP"
  load(paste0("External/Harmonizome/", tab, "interest.RData"))
  
  dataHarmonizome = data
  
  rm(data)
  
  dataHarmonizome <- merge(data.frame(dataset="GEO Signatures of Differentially Expressed Genes for Diseases"), dataHarmonizome, by="dataset", all.x = T)[,-1]
  
  dataHarmonizome <- data.frame(Disease=dataHarmonizome[,1])
  
  # Transform NA in string
  dataHarmonizome$Disease <- as.character(dataHarmonizome$Disease)
  dataHarmonizome$Disease[is.na(dataHarmonizome$Disease)] <- "Not available for this gene."
  
  link_a <- "<a href = 'http://amp.pharm.mssm.edu/Harmonizome/gene_set/"
  
  #datalink <- gsub(" ", "+", as.character(dataHarmonizome[3,2]))
  
  link_b <- "/GEO+Signatures+of+Differentially+Expressed+Genes+for+Diseases' target= '_blank'>"
  
  dataHarmonizome <- mutate(dataHarmonizome, Link1=paste0(link_a,gsub(" ", "+", Disease),link_b,Disease,"</a>"))
  
  
  dataHarmonizome$Disease = dataHarmonizome$Link1
  
  #essai <- dataHarmonizome[1:2,]
  
  
  DT::datatable(unique(arrange(data.frame(Disease=dataHarmonizome[,1]), Disease)), colnames = "GEO Signatures of Differentially Expressed Genes for Diseases", rownames = FALSE, escape = FALSE, class = 'cell-border stripe',
                
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', HTML(paste("Disease perturbations changing expression of", tab, "gene from the", "<a href='http://amp.pharm.mssm.edu/Harmonizome/dataset/GEO+Signatures+of+Differentially+Expressed+Genes+for+Diseases'", " ", "target='_blank'>GEO Signatures of Differentially Expressed Genes for Diseases</a>"), "dataset.")
                  #paste0("abc", "<div><a id='inf'>RPKM</a></div>")
                ),
                extensions =c ('ColReorder', 'Buttons', 'FixedHeader'),
                
                options = list(
                  fixedHeader = FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),   
                  dom = 'Blfrtip',   
                  buttons = 
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                  orderClasses = F,
                  scrollX = TRUE,
                  pageLength = 5, lengthMenu = c(5, 10, 15, 20, nrow(dataHarmonizome)),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'padding-left' : '0px', 'color': '#fff', 'width' : 'auto !important'});",
                    "$(this.api().table().body()).css({'padding-left' : '0px', 'width' : 'auto'});",
                    "}")
                ))
})




################################Display table with GEO Signatures of Differentially Expressed Genes for small Molecules from harmonizome

output$Mod2 <- DT::renderDT({ 
  
  ensemblPop <-'<a  data-toggle="tooltip" title="Transcript identification according to Ensembl database."> Ensembl ID<img src="info.png" style="width: 10px;"></a>'
  
  sdPop <-'<a  data-toggle="tooltip" title="Variation estimated as standard deviation of log2 of Read Per Kilobase Million."> Std Deviation <img src="info.png" style="width: 10px;"></a>'
  
  #colnames(hk)[2] <- ensemblPop
  
  #colnames(hk)[5] <- sdPop
  
  #tab <- as.character(which(HK_geneAtualizadoRef$Ensembl %in% "ENST00000248450"))
  
  tab <- as.character(HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$selectHSGenes))))), 2])# Extract gene symbol
  
  #tab = "AAMP"
  load(paste0("External/Harmonizome/", tab, "interest.RData"))
  
  dataHarmonizome2 = data
  #dataHarmonizome2=AAMPinterest
  rm(data)
  
  dataHarmonizome2 <- merge(data.frame(dataset="GEO Signatures of Differentially Expressed Genes for Small Molecules"), dataHarmonizome2, by="dataset", all.x = T)[,-1]
  
  dataHarmonizome2 <- data.frame(Disease=dataHarmonizome2[,1])
  
  dataHarmonizome2$Disease <- as.character(dataHarmonizome2$Disease)
  dataHarmonizome2$Disease[is.na(dataHarmonizome2$Disease)] <- "No records found for this dataset."
  
  link_a <- "<a href = 'http://amp.pharm.mssm.edu/Harmonizome/gene_set/"
  
  #datalink <- gsub(" ", "+", as.character(dataHarmonizome2[3,2]))
  
  link_b <- "/GEO+Signatures+of+Differentially+Expressed+Genes+for+small+Molecules' target= '_blank'>"
  
  dataHarmonizome2 <- mutate(dataHarmonizome2, Link1=paste0(link_a,gsub(" ", "+", Disease),link_b,Disease,"</a>"))
  
  dataHarmonizome2$Disease = dataHarmonizome2$Link1
  
  #essai <- dataHarmonizome2[1:2,]
  
  
  DT::datatable(unique(arrange(data.frame(small_molecules=dataHarmonizome2[,1]), small_molecules)), colnames = "GEO Signatures of Differentially Expressed Genes for small Molecules", rownames = FALSE, escape = FALSE, class = 'cell-border stripe',
                
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', HTML(paste("Small molecule perturbations changing expression of", tab, "gene from the", "<a href='http://amp.pharm.mssm.edu/Harmonizome/dataset/GEO+Signatures+of+Differentially+Expressed+Genes+for+Small+Molecules'", " ", "target='_blank'>GEO Signatures of Differentially Expressed Genes for Small Molecules</a>"), "dataset.")
                  #paste0("abc", "<div><a id='inf'>RPKM</a></div>")
                ),
                
                extensions =c ('ColReorder', 'Buttons', 'FixedHeader'),
                
                options = list(
                  fixedHeader = FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),   
                  dom = 'Blfrtip',   
                  buttons = 
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                  orderClasses = F,
                  scrollX = TRUE,
                  pageLength = 5, lengthMenu = c(5, 10, 15, 20, nrow(dataHarmonizome2)),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'padding-left' : '0px', 'color': '#fff', 'width' : 'auto !important'});",
                    "$(this.api().table().body()).css({'padding-left' : '0px', 'width' : 'auto'});",
                    "}")
                ))
})




################################Display table with CMAP Signatures of Differentially Expressed Genes for small Molecules from harmonizome

output$Mod3 <- DT::renderDT({ 
  
  ensemblPop <-'<a  data-toggle="tooltip" title="Transcript identification according to Ensembl database."> Ensembl ID<img src="info.png" style="width: 10px;"></a>'
  
  sdPop <-'<a  data-toggle="tooltip" title="Variation estimated as standard deviation of log2 of Read Per Kilobase Million."> Std Deviation <img src="info.png" style="width: 10px;"></a>'
  
  #colnames(hk)[2] <- ensemblPop
  
  #colnames(hk)[5] <- sdPop
  
  #tab <- as.character(which(HK_geneAtualizadoRef$Ensembl %in% "ENST00000248450"))
  
  tab <- as.character(HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$selectHSGenes))))), 2])# Extract gene symbol
  
  #tab = "AAMP"
  load(paste0("External/Harmonizome/", tab, "interest.RData"))
  
  dataHarmonizome2 = data
  #dataHarmonizome2=AAMPinterest
  rm(data)
  
  dataHarmonizome2 <- merge(data.frame(dataset="CMAP Signatures of Differentially Expressed Genes for Small Molecules"), dataHarmonizome2, by="dataset", all.x = T)[,-1]
  
  dataHarmonizome2 <- data.frame(Disease=dataHarmonizome2[,1])
  
  dataHarmonizome2$Disease <- as.character(dataHarmonizome2$Disease)
  dataHarmonizome2$Disease[is.na(dataHarmonizome2$Disease)] <- "Not available for this gene."
  
  
  link_a <- "<a href = 'http://amp.pharm.mssm.edu/Harmonizome/gene_set/"
  
  #datalink <- gsub(" ", "+", as.character(dataHarmonizome2[3,2]))
  
  link_b <- "/CMAP+Signatures+of+Differentially+Expressed+Genes+for+small+Molecules' target= '_blank'>"
  
  dataHarmonizome2 <- mutate(dataHarmonizome2, Link1=paste0(link_a,gsub(" ", "+", Disease),link_b,Disease,"</a>"))
  
  dataHarmonizome2$Disease = dataHarmonizome2$Link1
  
  #essai <- dataHarmonizome2[1:2,]
  
  DT::datatable(unique(arrange(data.frame(small_molecules=dataHarmonizome2[,1]), small_molecules)), colnames = "CMAP Signatures of Differentially Expressed Genes for small Molecules", rownames = FALSE, escape = FALSE, class = 'cell-border stripe',
                
                
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', HTML(paste("Small molecule perturbations changing expression of", tab, "gene from the", "<a href='http://amp.pharm.mssm.edu/Harmonizome/dataset/CMAP+Signatures+of+Differentially+Expressed+Genes+for+Small+Molecules'", " ", "target='_blank'>Connectivity Map Signatures of Differentially Expressed Genes for Small Molecules</a>"), "dataset.")
                  #paste0("abc", "<div><a id='inf'>RPKM</a></div>")
                ),
                
                
                extensions =c ('ColReorder', 'Buttons', 'FixedHeader'),
                
                options = list(
                  fixedHeader = FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),   
                  dom = 'Blfrtip',   
                  buttons = 
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                  orderClasses = F,
                  scrollX = TRUE,
                  pageLength = 5, lengthMenu = c(5, 10, 15, 20, nrow(dataHarmonizome2)),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'padding-left' : '0px', 'color': '#fff', 'width' : 'auto !important'});",
                    "$(this.api().table().body()).css({'padding-left' : '0px', 'width' : 'auto'});",
                    "}")
                ))
})



################################Validation results#################
##########
###########################################################
############### Render selectInput from server side ###########
outGeneName <- reactive({
  #select transcript with primers
  table=gsub(" ", "_", input$search)
  
  data <- data.frame(tbl(con, table))
  
  data <- data.frame(data)
  
  data <- na.omit(select(data, c("Transcript_ID","Gene_name")))
  
  data = na.omit(unique(merge(data, humanPrimer, all.x = T, by = "Transcript_ID")))
  
  data = mutate(data, textInp = paste0(Transcript_ID, " (", Gene_name, ")"))
  
  genelist <- as.list(data$textInp)
  #vars <- all.vars(parse(text = input$text))
  #vars <- as.list(vars)
  return(genelist)
})

############### SelectInput in server side ####
output$validatedPrimerSelectInput = renderUI({
  selectInput('searchHSGenes2', 'Select a reference transcript to show its qPCR validation', as.list(outGeneName()), selectize=FALSE)
})


####output$outxId####
output$outxId <- renderPrint(input$rmodifiers.Validation)
#######################################GeneName######################################################
output$genenameVal <- renderUI({
  
  condTest = input$searchHSGenes2
  if ( length(input$searchHSGenes2) != 0 && (input$searchHSGenes2 != "Select a reference transcript...")) {
  gene <- gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , input$searchHSGenes2)))# Extract gene symbol
  
  ind <- which(GeneInfo$hgnc_symbol %in% gene)
  genelower <- gsub("<", "", gsub(">", "", gsub(".*dd\\>(.+)\\<span cl.*", "\\1", GeneInfo[ind,5])))
  geneupper <- paste(toupper(substr(genelower, 1, 1)), substr(genelower, 2, nchar(genelower)), sep="") #Upper of the first letter
  
  genenameI <- gsub("</dd>", "", gsub("<dd>", "", gsub(genelower, geneupper,  GeneInfo[ind, 5]))) #gene official name
  
  synonyme <- GeneInfo[ind, 4]
  
  description <- GeneInfo[ind, 3]
  
  
  if(!is.na(GeneInfo[ind, 5])) {
    genenameHtml <- paste0('<tr >
                      <td style="text-align: justify;" class="col-xs-2">Gene name</td>',
                           '<td style="text-align: justify;" class="col-xs-10">',gsub("</dd>", "", gsub("<dd>", "",genenameI)),'</td>
                      </tr>')
  } else {
    genenameHtml <- paste0('<tr style="display: none">
                      <td class="col-xs-2">Gene name</td>',
                           '<td class="col-xs-10">',gsub("</dd>", "", gsub("<dd>", "",genenameI)),'</td>
                      </tr>')
  }
  
  
  
  if(!is.na(GeneInfo[ind, 4])) {
    
    synonymeHtml <- paste0('<tr>
                              <td style="text-align: justify;" class="col-xs-2">Other names</td>',
                           '<td style="text-align: justify;" class="col-xs-10">',gsub("</dd>", "", gsub("<dd>", "",synonyme)),'</td>
                            </tr>')
  } else {
    synonymeHtml <- paste0('<tr style="display: none">
                              <td class="col-xs-2">Other names</td>',
                           '<td class="col-xs-10">',synonyme,'</td>
                            </tr>')
  }
  
  
  if(!is.na(GeneInfo[ind, 3])) {
    
    descriptionHtml <- paste0('<tr>
                              <td style="text-align: justify;" class="col-xs-2">Description</td>',
                              '<td style="text-align: justify;" class="col-xs-10">',gsub("</dd>", "", gsub("<dd>", "", description)),'</td>
                            </tr>')
  } else {
    descriptionHtml <- paste0("<tr style='display: none'>
                              <td style='text-align: justify;' class='col-xs-2'>Description</td>
                              <td style='text-align: justify;' class='col-xs-10'>",gsub("</dd>", "", gsub("<dd>", "", description)),"</td>
                            </tr>")
  }
  
  ################### Orthologous
  orthologous <- gsub("<dd>NO</dd>", "Not identified as candidate reference transcript in mouse",GeneInfo[ind, 7])
  
  
  orthologousHtml <- paste0('<tr>
                              <td style="text-align: justify;" class="col-xs-3">Mouse orthologous Housekeeping gene</td>',
                            '<td style="text-align: justify;" class="col-xs-9">',orthologous,'</td>
                            </tr>')
  
  ##################Standard curve#############################
  indPrimer <- which(humanPrimer$Gene %in% gene)
  eff= humanPrimer[indPrimer, 8]
  r2 = humanPrimer[indPrimer, 9]
  effic1 = paste0('<a>Efficiency %: ', eff, '</a>')
  effic2 = paste0('<a><var>R<sup>2</sup></var>: ', r2, '</a>')
  standardHtml <- paste0('<tr>
                              <td rowspan="2" style="text-align: justify;" class="col-xs-3">qPCR Efficiency with Sybr Green</td>',
                            '<td style="text-align: justify; font-weight: bold; color: green" class="col-xs-9">',effic1,'</td>
                            </tr><tr>
                            <td style="text-align: justify; font-weight: bold; color: green" class="col-xs-9">',effic2,'</td>
                            </tr>')
 
  ############################################
  return(HTML(paste0("<h4 style='padding-left: 10px; color: rgb(101, 61, 213);'>", tags$strong(input$searchHSGenes2), "</h4><section style='border-bottom:1px #ccc solid; margin-bottom:50px;'><table class='table'><tbody>",genenameHtml, synonymeHtml, orthologousHtml, descriptionHtml,standardHtml, "</tbody></table></section>")))
  
  }
  
})


############### Validation Figure ####

outGeneForImg <- reactive({
  
    gene <- gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , input$searchHSGenes2)))# Extract gene symbol
    return(gene)
})

output$imageVal = renderUI({
  
  if ( length(input$searchHSGenes2) != 0 && (input$searchHSGenes2 != "Select a reference transcript...")) {
  
  HTML(paste0('<div class="container">
          <div class="row">

            <div id="standValId" class="col-11 html-widget-output shiny-bound-output">
              <a data-toggle="tooltip" title="Click to download the standard curve" href="Validation/Human_', outGeneForImg(), '.JPG" target="_blank">
              <svg class="bi bi-download" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
              <path fill-rule="evenodd" d="M.5 8a.5.5 0 01.5.5V12a1 1 0 001 1h12a1 1 0 001-1V8.5a.5.5 0 011 0V12a2 2 0 01-2 2H2a2 2 0 01-2-2V8.5A.5.5 0 01.5 8z" clip-rule="evenodd"/>
              <path fill-rule="evenodd" d="M5 7.5a.5.5 0 01.707 0L8 9.793 10.293 7.5a.5.5 0 11.707.707l-2.646 2.647a.5.5 0 01-.708 0L5 8.207A.5.5 0 015 7.5z" clip-rule="evenodd"/>
              <path fill-rule="evenodd" d="M8 1a.5.5 0 01.5.5v8a.5.5 0 01-1 0v-8A.5.5 0 018 1z" clip-rule="evenodd"/>
              </svg> 
              </a>
            <a id="idown1" style="margin-left:10px">Download</a>

        </div>

            <div id="standValId" class="col-11 html-widget-output shiny-bound-output">
            <img style="width:100%; height:100%;" src="Validation/Human_', outGeneForImg(), '.JPG" style="margin-top: 10px;">
              
</div>
            
            <div id="meltValId" class="col-11 html-widget-output shiny-bound-output">
            
            
            <a data-toggle="tooltip" title="Click to download a representative melting curve" href="Validation/Melt_Human_', outGeneForImg(), '.JPG" target="_blank">
              <svg class="bi bi-download" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
              <path fill-rule="evenodd" d="M.5 8a.5.5 0 01.5.5V12a1 1 0 001 1h12a1 1 0 001-1V8.5a.5.5 0 011 0V12a2 2 0 01-2 2H2a2 2 0 01-2-2V8.5A.5.5 0 01.5 8z" clip-rule="evenodd"/>
              <path fill-rule="evenodd" d="M5 7.5a.5.5 0 01.707 0L8 9.793 10.293 7.5a.5.5 0 11.707.707l-2.646 2.647a.5.5 0 01-.708 0L5 8.207A.5.5 0 015 7.5z" clip-rule="evenodd"/>
              <path fill-rule="evenodd" d="M8 1a.5.5 0 01.5.5v8a.5.5 0 01-1 0v-8A.5.5 0 018 1z" clip-rule="evenodd"/>
              </svg> 
            </a>
            <a id="idown2" style="margin-left:10px">Download</a>
            
           </div> 

<div id="meltValId" class="col-11 html-widget-output shiny-bound-output">
            <img style="width:100%; height:100%;" src="Validation/Melt_Human_', outGeneForImg(), '.JPG">
            
</div>
              
            </div>
          
        </div>
            ')#End of HTML function
       )
  }
})



################ Epiregio #####################

output$epiregio <- DT::renderDT({ 
  
  ensemblPop <-'<a  data-toggle="tooltip" title="Transcript identification according to Ensembl database."> Ensembl ID<img src="info.png" style="width: 10px;"></a>'
  
  sdPop <-'<a  data-toggle="tooltip" title="Variation estimated as standard deviation of log2 of Read Per Kilobase Million."> Std Deviation <img src="info.png" style="width: 10px;"></a>'
  
  #colnames(hk)[2] <- ensemblPop
  
  #colnames(hk)[5] <- sdPop
  
  #tab <- as.character(which(HK_geneAtualizadoRef$Ensembl %in% "ENST00000248450"))
  
  tab <- as.character(HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$selectHSGenes))))), 2])# Extract gene symbol
  
  #tab = "AAMP"
  load(paste0("External/Harmonizome/", tab, "interest.RData"))
  
  dataHarmonizome2 = data
  #dataHarmonizome2=AAMPinterest
  rm(data)
  
  dataHarmonizome2 <- merge(data.frame(dataset="CMAP Signatures of Differentially Expressed Genes for Small Molecules"), dataHarmonizome2, by="dataset", all.x = T)[,-1]
  
  dataHarmonizome2 <- data.frame(Disease=dataHarmonizome2[,1])
  
  dataHarmonizome2$Disease <- as.character(dataHarmonizome2$Disease)
  dataHarmonizome2$Disease[is.na(dataHarmonizome2$Disease)] <- "Not available for this gene."
  
  
  link_a <- "<a href = 'http://amp.pharm.mssm.edu/Harmonizome/gene_set/"
  
  #datalink <- gsub(" ", "+", as.character(dataHarmonizome2[3,2]))
  
  link_b <- "/CMAP+Signatures+of+Differentially+Expressed+Genes+for+small+Molecules' target= '_blank'>"
  
  dataHarmonizome2 <- mutate(dataHarmonizome2, Link1=paste0(link_a,gsub(" ", "+", Disease),link_b,Disease,"</a>"))
  
  dataHarmonizome2$Disease = dataHarmonizome2$Link1
  
  #essai <- dataHarmonizome2[1:2,]
  
  DT::datatable(unique(arrange(data.frame(small_molecules=dataHarmonizome2[,1]), small_molecules)), colnames = "CMAP Signatures of Differentially Expressed Genes for small Molecules", rownames = FALSE, escape = FALSE, class = 'cell-border stripe',
                
                
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', HTML(paste("Small molecule perturbations changing expression of", tab, "gene from the", "<a href='http://amp.pharm.mssm.edu/Harmonizome/dataset/CMAP+Signatures+of+Differentially+Expressed+Genes+for+Small+Molecules'", " ", "target='_blank'>Connectivity Map Signatures of Differentially Expressed Genes for Small Molecules</a>"), "dataset.")
                  #paste0("abc", "<div><a id='inf'>RPKM</a></div>")
                ),
                
                
                extensions =c ('ColReorder', 'Buttons', 'FixedHeader'),
                
                options = list(
                  fixedHeader = FALSE,
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),   
                  dom = 'Blfrtip',   
                  buttons = 
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                  orderClasses = F,
                  scrollX = TRUE,
                  pageLength = 5, lengthMenu = c(5, 10, 15, 20, nrow(dataHarmonizome2)),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'padding-left' : '0px', 'color': '#fff', 'width' : 'auto !important'});",
                    "$(this.api().table().body()).css({'padding-left' : '0px', 'width' : 'auto'});",
                    "}")
                ))
})


