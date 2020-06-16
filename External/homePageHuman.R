output$pageHousekeeping <- renderUI(tagList(
fluidRow(#tags$title("Search Reference genes"), tags$meta(name="description", content="Search for the most stable human tissue specific reference genes and primers to normalize qPCR expression."), 
useShinyjs(), 
includeHTML("External/HTML/homePageHuman.html"),


                hidden(div(style="width:100%", id="tabpanelUI",
                    div( class="container", style="min-height: 300px;",
                             
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
                                                       "Linear" = "linear"), selected = "log"), br(), hr(), br(), 
                                        sliderInput("rpkm", "Mean of RPKM higher or equal than:",
                                                    min = 1, max = 100, value = 30
                                        ), br(), hr(),
                                        
                                        
                                        verbatimTextOutput("outxId", placeholder = FALSE),
                                        
                                        div(id="sel1", style="width: 100%;", uiOutput("selectUI")),
                                        
                                        div(id="sel2", style="width: 100%;", uiOutput('validatedPrimerSelectInput'))
                                        
                                        )#End form
                                      ), #div sidebar
                                      
                                      
                                      div(class="col-sm-9",
                          
                          
                          tabsetPanel(id="tabsetID",
                              tabPanel(id="ref", "Reference Transcripts", value="refTranscr", br(), uiOutput("outCap"), withSpinner(uiOutput("tabHK"))),#)#,
                              tabPanel(id="rmodifiers", "Expression Modifiers", br(),
                                       span(style="text-align:justify", "HRT Atlas v1.0 is integrated with", HTML("<a href='http://amp.pharm.mssm.edu/Harmonizome/' target='_blank'>Harmonizome</a>"), "database.", br(),
                                            strong(textOutput("genenameChoice"))), br(),
                                       
                                       withSpinner(DT::DTOutput("Mod1")),br(),
                                       withSpinner(DT::DTOutput("Mod2")), br(),
                                       withSpinner(DT::DTOutput("Mod3"))),
                              
                              #########Epiregio
                              tabPanel(id="epi", "Regulatory Elements", br(),
                                       span(style="text-align:justify", "HRT Atlas v1.0 is integrated with", HTML("<a href='http://amp.pharm.mssm.edu/Harmonizome/' target='_blank'>Epiregio server</a>"), "via REST API.", br()
                                           ), br(),
                                       withSpinner(DT::DTOutput("epiregio1")),br(),br(),
                                       withSpinner(DT::DTOutput("epiregio2"))
                                     
                                       ),
                              
                              tabPanel(id="refPrimer", "Specific Primers", value="spec_primer", hidden(div(id="noPrimer", "No primer")), withSpinner(uiOutput("tabRef"))),
                              tabPanel(id="valPrimer", value="Validation", "Validation", class="Container-fluid",  
                                 
                                       div(class="Container-fluid panelstyle",br(),
                                           
                                
                                          #selectIput from server
                                          #verbatimTextOutput("outx"),
                                   
                                         div(style="width: 100%;", withSpinner(uiOutput("genenameVal"))),
                                          withSpinner(uiOutput('imageVal')))
                                       
                                       )
                          )#End tabsetPanel
                          
                              )#mainPanel
                          
                        
                        
                         ) #End sidebarlayout
                        )
                          )#End hidden
                )#End wrapper of hideen
                  ),# wrapper

includeHTML("External/HTML/footer.html")
                  
          
)# End fluidPage

) # End renderUI






################################## Server code

# Show selectInput on click
observeEvent(input$tabsetID, {
  if (input$tabsetID=="Validation"){
    disable("mfc")
    disable("rpkm")
    
    shinyjs::show("sel2")
  } else {
  shinyjs::hide("sel2")
}
})

observeEvent(input$tabsetID, {
  if (input$tabsetID=="spec_primer"){
    disable("mfc")
    disable("rpkm")
    
  } 
})

observeEvent(input$tabsetID, {
  if (input$tabsetID=="refTranscr"){
    enable("mfc")
    enable("rpkm")
    
  } 
})

# Show selectInput on click

observeEvent(input$tabsetID, {
  if (input$tabsetID=="Regulatory Elements" || input$tabsetID=="Expression Modifiers"){
    
    shinyjs::show("sel1")
  } else {
    shinyjs::hide("sel1")
  }
  
})


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
  reset("search")
  reset("rpkm")
  reset("mfc")
  
  
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


####################### Reference transcript table in reactive variable ###################


humanRefTable <- reactive({
  
  
  # Compute reference table based on user inputs and filtering criteria
  if(input$mfc=="linear"){
    con <- dbConnect(RSQLite::SQLite(), "~/Área de Trabalho/analise_HKG/tissue_types/connective_tissue/New_Analysis/New_11_06_2020/MCF_Housekeeping_human_mouse.sqlite")
    
  } 
  
  table=gsub(" ", "_", input$search)
  
 
  data <- data.frame(tbl(con, table))
  #data <- load(paste0("External/Data/db1/", table, ".RData"))
  
 
  
  data <- data.frame(data)
  
  if(input$mfc=="log"){
    hk <- select(data, c("Ensembl","Gene.name", "Mean", "SD_of_log", "MCF_Log_Mean", "Max","Min","Log_MFC_min_and_mean"))
    
  } else {
    hk <- select(data, c("Ensembl","Gene.name", "Mean", "SD_of_log", "MCF_mean", "Max","Min","MFC_min_and_mean"))
  }
  
 # cat("dimension of line 241:", dim(hk), "\n")
  #Filter only HK transcripts
  hk <- unique(na.omit(merge(hk, Housekeeping_TranscriptFiltered[,c(1,3:5)], by="Ensembl", all.y=T)))
  
  
  #Apply rpkm criteria selected by the user
  hk = filter(hk, Mean >= input$rpkm)
  
  
  # Ranking
  
  
  if(input$mfc=="log"){
    #hk <- select(data, c("Ensembl","Gene.name", "Mean", "SD_of_log", "MCF_Log_Mean", "Max","Min","Log_MFC_min_and_mean"))
    hk <- hk %>% arrange(Log_MFC_min_and_mean) %>% mutate(RankMFC=1:nrow(hk))
    
    hk <- hk %>% arrange(desc(Mean)) %>% mutate(RankRPKM=1:nrow(hk)) 
    
    hk <- hk %>% mutate(RankProd=RankMFC*RankRPKM) %>%  arrange(RankProd) %>% mutate(Rank=1:nrow(hk))
    
    
    
  } else {
    #hk <- select(data, c("Ensembl","Gene.name", "Mean", "SD_of_log", "MCF_mean", "Max","Min","MFC_min_and_mean"))
    
    hk <- hk %>% arrange(MFC_min_and_mean) 
    
    if(nrow(hk)>0){
      
      hk <- hk %>% mutate(RankMFC=1:nrow(hk))
      
      
      hk <- hk %>% arrange(desc(Mean)) %>% mutate(RankRPKM=1:nrow(hk)) 
      
      hk <- hk %>% mutate(RankProd=RankMFC*RankRPKM) %>%  arrange(RankProd) %>% mutate(Rank=1:nrow(hk))
    } else {
      # This condition has only been used to make error mensage rending when user filter
      hk = vector(mode = "list")
    }
    
    
  }
  
  
  return(hk)
  
})




###################### Render SelectectInput UI


output$selectUI <- renderUI({
  
  
  # Reference table from rective "humanRefTable"
  
  hk = humanRefTable()
  
  if(class(hk)=="data.frame"){
    hk = filter(hk, Mean >= input$rpkm)
    
  data <- na.omit(select(hk, c("Rank","Ensembl","Gene.name")) %>% arrange(Rank))
  
  #Use interaction and lexicography to preserve the ranking order
  
  choiceList <- interaction(data[,3], " ", "(", data[,2], ")", sep=  "", lex.order = FALSE)
  
  selectInput('selectHSGenes', 'Select a gene to show some modifiers of its expression', choiceList, selectize=FALSE)
  }
  
})



##############Reference transcript table#################

output$outCap <- renderUI({
  
  # Reference table from rective "humanRefTable"
  table=gsub(" ", "_", input$search)
  
  
  
  tab <- tab_echant[table,]
  
  hk = humanRefTable()
  
  # Render caption outside of the table
  
  if(class(hk)=="data.frame"){
    hk = filter(hk, Mean >= input$rpkm)
    HTML(paste0(nrow(hk), " ", "Transcripts found from ", " ", tab[,2], " ", "high quality", " ", "<a class='font-weight-bold font-italic'>",input$search, "</a> samples."))
    
  } 
  
 
  
  })





#wrappe DT::renderDT in a renderUI to control error mensage
#This strategy can solve many shiny problem and is able to either table or other Ui element 
#with the same code

output$tabHK <- renderUI({
  
  
  # Reference table from rective "humanRefTable"
  
  hk = humanRefTable()
  

  #########Test whether the filtering used by the user provide any reference transcript#######
  
  
  if(class(hk)=="data.frame") {
    
    #Apply rpkm criteria selected by the user
    hk = filter(hk, Mean >= input$rpkm)
    
    
  hk <- hk[, c(15,1:4,8:11)]
  
  hk[,3] <- paste0(paste0('<a href=https://www.genecards.org/cgi-bin/carddisp.pl?gene=',hk[,3], ' ' , 'target="_blank"'),'>',hk[,3], '</a>')
  
  
  #Popup of colnames
  
  rpkmPop <-'<a  data-toggle="tooltip" title="Read count are normalized to Reads Per Kilobase Million. RPKM is a normalized transcript/gene expression based on RNA-seq data. Mean RPKM are presented for each tissue or cell type.">Normalized Expression<img src="info.png" style="width: 10px;"></a>'
  
  mfcPop <-'<a  data-toggle="tooltip" title="The Maximum Fold Change (MFC) is the ratio of the maximum RPKM and mean RPKM values observed for each transcript. To be considered stable, a transcript must have a MFC less than 2.">MFC <img src="info.png" style="width: 10px;"></a>'
  
  ensemblPop <-'<a  data-toggle="tooltip" title="Transcript identification according to Ensembl database."> Ensembl ID<img src="info.png" style="width: 10px;"></a>'
  
  sdPop <-'<a  data-toggle="tooltip" title="Variation estimated as standard deviation of log2 of Read Per Kilobase Million."> Std Deviation <img src="info.png" style="width: 10px;"></a>'
  hk = data.frame(hk)
  names(hk) <- c("Rank", ensemblPop, "Gene Symbol",rpkmPop,sdPop,mfcPop, "Chromosome", "Start position", "End position")
  
  ##################### End of reference transcript table computation#######################
output$mytabHkwapper <- DT::renderDT({ 
  hkRow = nrow(hk)
  #https://github.com/rstudio/DT/issues/155
  DT::datatable(hk, rownames = FALSE, escape = FALSE, class = 'cell-border stripe', 
                
                #caption = htmltools::tags$caption(
                 # style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', HTML(paste0(hkRow, " ", "Transcripts found from ", " ", tab[,2], " ", "high quality", " ", "<a class='font-weight-bold font-italic'>",input$search, "</a> samples."))
                  #paste0("abc", "<div><a id='inf'>RPKM</a></div>")
                  #),
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
                  pageLength = 5, #lengthMenu = c(5, nrow(hk)),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'padding-left' : '0px', 'color': '#fff', 'width' : 'auto !important'});",
                    "$(this.api().table().body()).css({'padding-left' : '0px', 'width' : 'auto'});",
                    "}")
                )) #%>% formatStyle(
                  #colnames(hk)[4],
                  #backgroundColor = styleInterval(50, c('rgba(0, 147, 255, 0.72)', '#4fda4f')))
})

############## Render reference table if filtering is not very restrictive or display alert#################
DT::DTOutput("mytabHkwapper")

  
  
  } else {
    
    shinyjs::alert(paste(" Gene expression is naturally a skewed data. Please use logarithmic scale. \n",
                         "Alternatively you can select only candidate reference transcripts with low expression. \n",
                        
                         "Warning: Low expressed transcripts may probably display high Ct value in qPCR experiments."))
  }
  
  

})#renderUI End




############# Display designed primers table ####################

output$tabRef <- renderUI({
if(input$tabsetID=="spec_primer"){
  # Reference table from rective "humanRefTable"
  
  hk = humanRefTable()
  
  table=gsub(" ", "_", input$search)
  
  
  tab <- tab_echant[table,]
  #########Test whether the filtering used by the user provide any reference transcript#######
  
  
  if(class(hk)=="data.frame") {
    
output$mytabHkprimer <- DT::renderDT({ 
  
  
  # primer table from rective "humanRefTable"
  
  data <- hk
  
  data <- na.omit(select(data, c("Rank","Ensembl","Gene.name")) %>% arrange(Rank))
  
  names(data) = c("Rank", "Transcript_ID", "Gene_name")
  
  dPrimer = na.omit(unique(merge(data[,1:2], humanPrimer, all.x = T, by = "Transcript_ID")))
  
  dPrimer = arrange(dPrimer, Rank)
  
  
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
                  pageLength = 5, lengthMenu = c(5, nrow(dPrimer)),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'color': '#fff'});",
                    "}")
                )) %>% formatStyle(
                  colnames(dPrimer)[4],
                  backgroundColor = styleInterval(50, c('#00ffe4', '#4fda4f')))
  


})

############## Render reference table if filtering is not very restrictive or display alert#################
DT::DTOutput("mytabHkprimer")



} else {
  
  shinyjs::alert("No primer available with this filtering criteria.")
  
  }
}
  
})# End of reactive primer table





refPop <-HTML('<a  data-toggle="tooltip" title="Suitable reference transcripts for normalization of target gene expression during RT-qPCR analysis. This list present highly stable transcripts filtered stringent std deviation, MFC and expression level"> Reference Transcripts <img src="info.png" style="width: 10px;"></a>')



#onevent("mousehover", "essai", shinyjs::alert("Type 'name' or '2' to see the features. "))







################################Display table with disease from harmonizome#####################

output$Mod1 <- DT::renderDT({ 
  
  ensemblPop <-'<a  data-toggle="tooltip" title="Transcript identification according to Ensembl database."> Ensembl ID<img src="info.png" style="width: 10px;"></a>'
  
  sdPop <-'<a  data-toggle="tooltip" title="Variation estimated as standard deviation of log2 of Read Per Kilobase Million."> Std Deviation <img src="info.png" style="width: 10px;"></a>'
  
  
  tab <- as.character(HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$selectHSGenes))))), 2])# Extract gene symbol
  #cat("tab at line 498 is:", tab, "\n")
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
                  pageLength = 5, lengthMenu = c(5, nrow(dataHarmonizome)),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'padding-left' : '0px', 'color': '#fff', 'width' : 'auto !important'});",
                    "$(this.api().table().body()).css({'padding-left' : '0px', 'width' : 'auto'});",
                    "}")
                ))
})




################################Display table with GEO Signatures of Differentially Expressed ##################
############Genes for small Molecules from harmonizome

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
                  pageLength = 5, lengthMenu = c(5, nrow(dataHarmonizome2)),
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
                  pageLength = 5, lengthMenu = c(5, nrow(dataHarmonizome2)),
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
  
  if(input$tabsetID=="Validation"){
  # Reference table from rective "humanRefTable"
  
  hk = humanRefTable()
  
  if(class(hk)=="data.frame"){
    hk = filter(hk, Mean >= input$rpkm)
    
  hk = filter(hk, Mean >= input$rpkm)
  #select transcript with primers

    
  data <- na.omit(select(hk, c("Ensembl","Gene.name")))
  
  names(data) = c("Transcript_ID", "Gene_name")
  
  data = na.omit(unique(merge(data, humanPrimer, all.x = T, by = "Transcript_ID")))
  
  data = mutate(data, textInp = paste0(Transcript_ID, " (", Gene_name, ")"))
  
  genelist <- as.list(data$textInp)
  #vars <- all.vars(parse(text = input$text))
  #vars <- as.list(vars)
  return(genelist)
  
  } else {
    shinyjs::alert("No primer available with this filtering criteria.")
  }
  
  }
})

############### SelectInput in server side ####
output$validatedPrimerSelectInput = renderUI({
  
  selectInput('searchHSGenes2', 'Select a reference transcript to show its qPCR validation', as.list(outGeneName()), selectize=FALSE)
})


####output$outxId####
output$outxId <- renderPrint(length(outGeneName())==0)
#######################################GeneName######################################################
output$genenameVal <- renderUI({
  
  #if(
  if(input$tabsetID=="Validation"){
  
    #rm(hk)
  
  hk = humanRefTable()
  
  if(class(hk)=="data.frame") {
    
  condTest = input$searchHSGenes2
  
  if(length(outGeneName())==0){
    shinyjs::alert("No primer available with this filtering criteria.")
    shinyjs::hide("sel2")
  }
  
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
  
  
  } else {
    shinyjs::alert("No primer available with this filtering criteria.")
    shinyjs::hide("sel2")
  }
  
  }
  
})


############### Validation Figure ####

outGeneForImg <- reactive({
  
    gene <- gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , input$searchHSGenes2)))# Extract gene symbol
    return(gene)
})

output$imageVal = renderUI({
  
  if(input$tabsetID=="Validation"){
    
  
    
    #rm(hk)
    # Reference table from rective "humanRefTable"
    
    hk = humanRefTable()
    
    if(class(hk)=="data.frame") {
      
      
  if ( length(input$searchHSGenes2) != 0) {
  
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
            
            <div id="meltValId" class="col-11 html-widget-output shiny-bound-output" style="margin-top:50px;">
            
            
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
      
      
    } else {
      shinyjs::alert("No primer available with this filtering criteria.")
    }
    }
})



################ Epiregio #####################

outGeneForEpiregio <- reactive({
  
  gene <- as.character(HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$selectHSGenes))))), 2])# Extract gene symbol
  
  return(gene)
})

##### Data epiregio
dataEpiregio <- reactive({
  
  gene <- as.character(HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$selectHSGenes))))), 2])# Extract gene symbol
  
  idx = which(geneName$geneSymbol %in% gene)
  ################ Regulatory Elements function predicted overall cell ########################
  
  data = data.frame(fromJSON(paste0("https://epiregio.de/REST_API/GeneQuery/",as.character(geneName[idx,2]),"/")))
  
  return(data)
})


###############################
############# Begining #####################
output$epiregio1 <- DT::renderDT({ 
 
  
  gene = outGeneForEpiregio()
  
  ################ Regulatory Elements function predicted overall cell ########################
  
  data=dataEpiregio()
 
  
  dataEpi1 = data[,3:12]
  
  
  
  dataEpi1$'Predicted Function' = ifelse(dataEpi1$regressionCoefficient > 0, "Activator", "Repressor")
  
  for (i in 1:nrow(dataEpi1)) {
   dataEpi1$CREMID[i] = ifelse(dataEpi1$CREMID[i]=="No CREM", "No CREM", paste0("<a href=", paste0('https://epiregio.de/cluster/',dataEpi1$CREMID[i]),
                                                                                " target='_blank'>",dataEpi1$CREMID[i],"</a>"))
  }
  
  dataEpi1 = arrange(dataEpi1, desc(normModelScore))[,c(1,11,3,4,9,10)]
  
   colnames(dataEpi1)= c("REM ID", "Predicted Function", "REM Start", "REM End", "CREM ID", "Model Score")
  
  
  
  DT::datatable(dataEpi1, rownames = FALSE, escape = FALSE, class = 'cell-border stripe',
                
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', HTML(paste("This table shows the Regulatory Elements (REMs) associated to", gene, ", their Predicted function, the Model score and the REM cluster (CREM) it is belonging to."))
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
                  pageLength = 5, lengthMenu = c(5, nrow(dataEpi1)),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'padding-left' : '0px', 'color': '#fff', 'width' : 'auto !important'});",
                    "$(this.api().table().body()).css({'padding-left' : '0px', 'width' : 'auto'});",
                    "}")
                ))
})


########################
##############################
output$epiregio2 <- DT::renderDT({ 

  gene = outGeneForEpiregio()
  
  data=dataEpiregio()
  
  ################ Regulatory Elements cell-specific predicted function ########################
  x1='<a  data-toggle="tooltip" '
  x2='</a>'
  
#  interpretation: positive regression and open chomatin
  
  title1 = "title = 'Interpretation: The REM is an activator (positive regression coefficient) and the chromatin is open, so the REM is likely to enhance the gene’s expression in comparisons to cell types, where the chromatin is more closed.'"
  
  
  
  #  interpretation: positive regression and open chomatin
  
  title2 = "title = 'Interpretation: The REM is a repressor (negative regression coefficient) of the gene, but the chromatin is rather closed, so the REM is most likely not able to regulate the gene’s expression. This leads to a higher gene expression in comparison to cell types where the chromatin is more open.'"
  
  
  
  #  interpretation: positive regression and open chomatin
  title3 = "title = 'Interpretation: The REM is interpreted as an activator (positive regression coefficient) , but the chromatin is closed. Thus, the REM is most likely not able to regulate the expression of the gene. Consequently, the gene expression is decreased in comparsion to a cell type where the chromatin is more open.'"
  
  
  #  interpretation: positive regression and open chomatin
  
  title4 = "title = 'Interpretation: The REM is a repressor (positive regression coefficient) and the chromatin is rather open. This leads to a decreasing gene expression in comparsion to a cell type where the chroamtin is more closed.'"
  
  dataEpi2 = data.frame(data[,c(3,7)], data$cellTypeScore)
 # cat("dataEpi2 dimension", dim(dataEpi2), "\n")
  
  for (i in 1:nrow(dataEpi2)) {
    for (j in 3:ncol(dataEpi2)) {
      
      if(dataEpi2[i,2] > 0 & dataEpi2[i,j] > 0) {
        dataEpi2[i,j] = paste0(x1,title1, ">", dataEpi2[i,j],x2)
      } 
      
      if(dataEpi2[i,2] < 0 & dataEpi2[i,j] < 0) {
        dataEpi2[i,j] = paste0(x1,title2, ">", dataEpi2[i,j],x2)
      } 
      
      if(dataEpi2[i,2] > 0 & dataEpi2[i,j] < 0) {
        dataEpi2[i,j] = paste0(x1,title3, ">", dataEpi2[i,j],x2)
      } 
      
      if(dataEpi2[i,2] < 0 & dataEpi2[i,j] > 0) {
        dataEpi2[i,j] = paste0(x1,title4, ">", dataEpi2[i,j],x2)
      }
      
      
    }
   
  }
  
  
  
  colnames(dataEpi2) = gsub("\\.", " ", gsub("\\.\\.", " and ", colnames(dataEpi2)))
  
  colnames(dataEpi2)[1:2] = c("REM ID", "Regression Coefficient")
  
  DT::datatable(dataEpi2, rownames = FALSE, escape = FALSE, class = 'cell-border stripe',
                
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', HTML(paste("Cell type specific regulatory elements of:", gene, ". Hover the mouse cursor over the each Cell type score to see the interpretation.", "<a href='https://epiregiodb.readthedocs.io/en/latest/UseCases.html#gene-query'", " ", "target='_blank'>More informations</a>"), " from Epiregio documentation.")
                  #paste0("abc", "<div><a id='inf'>RPKM</a></div>")
                ),
                
                extensions =c ('ColReorder', 'Buttons', 'FixedHeader'),
                
                options = list(
                  fixedHeader = FALSE,
                  autoWidth = TRUE,
                  columnDefs = list(list(width = '200px', className = 'dt-center', targets = "_all")),   
                  dom = 'Blfrtip',   
                  buttons = 
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                  orderClasses = F,
                  scrollX = TRUE,
                  pageLength = 5, lengthMenu = c(5, nrow(dataEpi2)),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'padding-left' : '0px', 'color': '#fff', 'width' : 'auto !important'});",
                    "$(this.api().table().body()).css({'padding-left' : '0px', 'width' : 'auto'});",
                    "}")
                ))
})


