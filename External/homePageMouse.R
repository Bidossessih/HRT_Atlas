output$pageHousekeeping <- renderUI(tagList(

fluidRow(#tags$title("Search Reference genes"), tags$meta(name="description", content="Search for the most stable mouse tissue specific reference genes and primers to normalize qPCR expression."), 
          class="Container-fluid", useShinyjs(), 
      includeHTML("External/HTML/homePageMouse.html"),
                  
      #verbatimTextOutput("out6"),
                  hidden(div(id="tabpanelUI", class="panelstyle", style="min-height: 300px;",
                        div(style="margin-top: -6%; padding-bottom: 3%;", a(img(src = "arrow-alt-circle-left-regular.svg", style="width:20px; margin-top:-3px"), actionLink('previousSearch',' Previous', icon = NULL))),
                        tabsetPanel(
                              tabPanel(id="ref", "Reference Transcripts", DT::DTOutput("tabHKMouse")),
                              tabPanel(id="refPrimer", "Specific Primers", hidden(div(id="noPrimer", "No primer")), DT::DTOutput("tabRef")),
                              tabPanel(id="valPrimerMouse", "Validation",
                                       class="Container-fluid",  
                                       
                                       div(class="Container-fluid panelstyle",
                                           
                                           
                                           #selectIput from server
                                           verbatimTextOutput("outxId"),
                                           uiOutput('validatedPrimerSelectInputMouse'),
                                           
                                           
                                           div(style="width: 100%;", uiOutput("genenameValMouse")),
                                           uiOutput('imageValMouse'))
                                       
                              )
                             ))),
      
      includeHTML("External/HTML/footer.html")
                      
           
                  
          
)# End fluidPage

)) # End renderUI






##################################
################################### Server code

output$out5 <- renderText(input$in1)
#output$out6 <- renderPrint(input$search)
#output$out6 <- renderPrint(search())
output$out7 <- renderPrint(input$button)
#output$default <- renderText({input$txt})
observeEvent(input$button, {
  shinyjs::hide("homeMouse")
  shinyjs::show("tabpanelUI")
  shinyjs::hide("tabpanelUI2")
  shinyjs::show("headersearch")
  shinyjs::show("headersearch2")
  
})

observeEvent(input$previousSearch, {
  shinyjs::show("homeMouse")
  shinyjs::hide("tabpanelUI")
  
  
})


observeEvent(input$buttonresult, {
  shinyjs::hide("homeMouse")
  shinyjs::show("tabpanelUI2")
  shinyjs::show("tabpanelUI")
  shinyjs::show("headersearch")
  shinyjs::show("headersearch2")
  
})

observeEvent(input$geneinputID, {
  shinyjs::hide("homeMouse")
  shinyjs::show("tabpanelUI")
  addClass(id = "pageUIMouse", class = "addedbackground")
})





output$tabHKMouse <- DT::renderDT({ 
  #load(paste0("External/Data/", input$search,".RData"))
  
  #connect to database
  #con <- dbConnect(RSQLite::SQLite(), "/srv/shiny-server/Housekeepingapp/db/Housekeeping.sqlite")
  #con <- src_sqlite("/home/bidossessi/Desktop/Ubuntu/RefTranscriptDb/HK Transcript_sqlite/Housekeeping.sqlite")
  #Query
  table=gsub(" ", "_", input$search)
  
    data <- data.frame(tbl(con, paste0(table, "_mouse"))) #uncommente
    #data <- load(paste0("External/Data/db2/", table, ".RData"))
  
  
  #disconnect
  #dbDisconnect(con)
  #load(paste0("External/Data/Ref/", table,"nonPseudogene.RData"))
  
  tab <- tab_echant_mouse[table,]
  
  hk <- data.frame(data)
  
 
  hk[, c(4)] <- round(hk[, c(4)], digits = 0)
  
  hk[, c(5:6)] <- round(hk[, c(5:6)], digits = 2)
  colnames(hk) <- c("Rank", "Ensembl Transcript", "Gene", "RPKM" , "SD(log2(RPKM))", "MFC", "Chromosome")
  hk <- mutate(hk, Link1=paste0('<a href=https://www.genecards.org/cgi-bin/carddisp.pl?gene=',hk$Gene, ' ' , 'target="_blank"'))
  hk <- mutate(hk, Link=paste0(Link1,'>',hk$Gene, '</a>'))
  
  hk[,3] <- hk[,9]
  hk <- hk[,1:6]
  
  #tooltip of colnames

  rpkmPop <-'<a  data-toggle="tooltip" title="Read count are normalized to Reads Per Kilobase Million. RPKM is a normalized transcript/gene expression based on RNA-seq data. Mean RPKM are presented for each tissue or cell type.">Normalized Expression<img src="info.png" style="width: 10px;"></a>'
  
  mfcPop <-'<a  data-toggle="tooltip" title="The Maximum Fold Change (MFC) is the ratio of the maximum RPKM and mean RPKM values observed for each transcript. To be considered stable, a transcript must have a MFC less than 2.">MFC <img src="info.png" style="width: 10px;"></a>'
  
  ensemblPop <-'<a  data-toggle="tooltip" title="Transcript identification according to Ensembl database."> Ensembl ID<img src="info.png" style="width: 10px;"></a>'
  
  sdPop <-'<a  data-toggle="tooltip" title="Variation estimated as standard deviation of log2 of Read Per Kilobase Million."> Std Deviation <img src="info.png" style="width: 10px;"></a>'
  
  #rpkm <- HTML("<a href='https://www.ebi.ac.uk/training/online/glossary/rpkm'>RPKM</a>") #link of rpkm definition
  colnames(hk)[4] <- rpkmPop
  
  colnames(hk)[6] <- mfcPop
  
  colnames(hk)[5] <- sdPop
  hk = hk[, c(2,1,3:6)]
  colnames(hk)[2] <- ensemblPop
  colnames(hk)[1] <- "Rank"
  #https://www.w3schools.com/howto/tryit.asp?filename=tryhow_css_tooltip
  
  #colnames(hk)[4] <- rpkmPop#'<div class="tooltip">RPKM<span class="tooltiptext">Reads Per Kilobase of transcript, per Million mapped reads (<a id="inf" style="color:white" href="http://www.arrayserver.com/wiki/index.php?title=RPKM">RPKM</a>) is a normalized unit of transcript expression. It scales by transcript length to compensate for the fact that most RNA-seq protocols will generate more sequencing reads from longer RNA molecules.</span></div>'
  
  #colnames(hk)[4] <- as.character(popify(actionLink(inputId="t_1", label=colnames(hk)[4]), title=paste("message1"), placement = "bottom", trigger = "hover", options = NULL))
  
  DT::datatable(na.omit(arrange(as.data.frame(hk), Rank)), rownames = FALSE, escape = FALSE, class = 'cell-border stripe',
                
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', HTML(paste0(nrow(hk), " ", "Transcripts found from ", " ", tab[,2], " ", "high quality", " ", input$search, " ", "samples. 
                                                                                                                        Highly expressed transcripts based on", " ", "<a href='https://www.ebi.ac.uk/training/online/glossary/rpkm'", " ", "target='_blank'>RPKM</a>"), "are highlighted in green and moderatly expressed transcripts in blue.", "<br>",
                                                                                                                 "Click ", "<a href='?visualizationMouse'><strong>here</strong></a>", " ", "to visualize housekeeping genes expression in different tissues.")
                  #paste0("abc", "<div><a id='inf'>RPKM</a></div>")
                  ),
                extensions =c ('ColReorder', 'Buttons', 'FixedHeader'),
                
                options = list(
                  fixedHeader = TRUE,
                  columnDefs = list(list(className = 'dt-right', targets = "_all")),   
                  dom = 'Blfrtip',   
                  buttons = 
                    list( 'csv', 'excel','pdf'), language = list(search = 'Filter:'),
                  orderClasses = TRUE,
                  scrollX = TRUE,
                  #pageLength = 5, lengthMenu = c(5, 10, 15, 20, nrow(hk)),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'color': '#fff'});",
                    "}")
                )) %>% formatStyle(
                  colnames(hk)[4],
                  backgroundColor = styleInterval(50, c('#00ffe4', '#4fda4f')))
})

#Display designed primers


output$tabRef <- DT::renderDT({ 
  # Check the availability of primers
  #validate(
    #need(gsub(" ", "_", input$search) %in% sampleTable$Type, paste("No available primer for", input$search, ". Please consider sharing with us (email to: openbiotools@gmail.com) if you test them in your project."))
  #)
  
  # load primer table
  #table=gsub(" ", "_", input$search)
  
  #load(paste0("External/Data/primer/", table, ".RData"))
  
  
  #Popup of colnames
  
  table=gsub(" ", "_", input$search)
  
  data <- data.frame(tbl(con, paste0(table, "_mouse"))) #uncommente
  #data <- load(paste0("External/Data/db2/", table, ".RData"))
  
  
  #disconnect
  #dbDisconnect(con)
  #load(paste0("External/Data/Ref/", table,"nonPseudogene.RData"))
  
  tab <- tab_echant_mouse[table,]
  
  hk <- data.frame(data)
  
  colnames(hk) <- c("Transcript_ID", "Rank", "Gene", "RPKM" , "SD(log2(RPKM))", "MFC", "Chromosome")
  
 
  dPrimer_mouse = unique(merge(hk[,c(2,1)], mousePrimer, all.x = T, by = "Transcript_ID"))
  
 
  dPrimer_mouse = arrange(dPrimer_mouse, Rank)
  dPrimer_mouse = dPrimer_mouse[, c(2,1,3:8)]
  
  rankPop <-'<a  data-toggle="tooltip" title="Transcript rank according to Reference Transcripts table.">Rank<img src="info.png" style="width: 10px;"></a>'
  
  
  #rpkm <- HTML("<a href='https://www.ebi.ac.uk/training/online/glossary/rpkm'>RPKM</a>") #link of rpkm definition
  colnames(dPrimer_mouse)[1] <- rankPop
  
  dPrimer_mouse[,1] = c(1:nrow(dPrimer_mouse))
  
  if (table == "Heart_bulk_Tissue") {
    load("www/Heart_bulk_Tissue.RData")
  }
  
  DT::datatable(na.omit(dPrimer_mouse), rownames = FALSE, escape = FALSE, class = 'cell-border stripe',
                
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', "We manually designed some transcript specific primers that can be used in your experiments. The temperature calculations are done assuming 200 nM of annealing oligo concentration. We recommend using of at least two reference transcripts for normalization purpose."
                ),
                extensions =c ('ColReorder', 'Buttons', 'FixedHeader'),
                
                options = list(
                  fixedHeader = TRUE,
                  columnDefs = list(list(className = 'dt-right', targets = "_all")),   
                  dom = 'Blfrtip',   
                  buttons = 
                    list( 'csv', 'excel','pdf'), language = list(search = 'Filter:'),
                      
                  orderClasses = TRUE,
                  scrollX = TRUE,
                  #pageLength = nrow(dPrimer_mouse),
                  colReorder = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(95, 95, 99)', 'color': '#fff'});",
                    "}")
                )) %>% formatStyle(
                  colnames(dPrimer_mouse)[4],
                  backgroundColor = styleInterval(50, c('rgba(0, 147, 255, 0.72)', '#4fda4f')))
})


refPop <-HTML('<a  data-toggle="tooltip" title="Suitable reference transcripts for normalization of target gene expression during RT-qPCR analysis. This list present highly stable transcripts filtered stringent standard deviation, MFC and expression level"> Reference Transcripts <img src="info.png" style="width: 15px;"></a>')

#output$out6 <- renderPrint(gsub(" ", "_", input$search))

onevent("mousehover", "essai", shinyjs::alert("Type 'name' or '2' to see the features. "))

##################################### Mouse Validation #############################
####################################################################################
####################################################################################

############### Render selectInput from server side ###########
outGeneNameMouse <- reactive({
  #select transcript with primers
  table=gsub(" ", "_", input$search)
  
  data <- data.frame(tbl(con, paste0(table, "_mouse")))
  
  data <- data.frame(data)
  
  data <- na.omit(select(data, c("Transcript_ID","Gene_name")))
  
  colnames(data)[2]="Gene"
  data = na.omit(unique(merge(data, mousePrimer, all.x = T, by = "Transcript_ID")))
  
  data = mutate(data, textInp = paste0(Transcript_ID, " (", Gene_name, ")"))
  
  genelist <- as.list(data$textInp)
  #vars <- all.vars(parse(text = input$text))
  #vars <- as.list(vars)
  return(genelist)
})

############### SelectInput in server side ####
output$validatedPrimerSelectInputMouse = renderUI({
  selectInput('searchMMGenes3', 'Select a reference transcript to show its qPCR validation', c("Select a reference transcript...", as.list(outGeneNameMouse())), selectize=FALSE)
})


####output$outxId####
#output$outxId <- renderPrint(Housekeeping_TranscriptsMouse[which(Housekeeping_TranscriptsMouse$Genes %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchMMGenes3))))), 2])
#######################################GeneName######################################################
output$genenameValMouse <- renderUI({
  
  
  if ( length(input$searchMMGenes3) != 0 && (input$searchMMGenes3 != "Select a reference transcript...")) {
    
    
    gene <- Housekeeping_TranscriptsMouse[which(Housekeeping_TranscriptsMouse$Genes %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchMMGenes3))))), 2]# Extract gene symbol
    
    transcr <- gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchMMGenes3))))
    
    #gene <- "Hist2h2be"
    
    ind <- which(geneMouse$mgi_symbol %in% gene)
    
    ind2 <- which(refInfoMouseHK2$`Ensembl ID` %in% transcr)
    
    
    #output$outx=renderPrint(transcr)
    
    genelower <- gsub("<", "", gsub(">", "", gsub(".*dd\\>(.+)\\<span cl.*", "\\1", geneMouse[ind,5])))
    geneupper <- paste(toupper(substr(genelower, 1, 1)), substr(genelower, 2, nchar(genelower)), sep="") #Upper of the first letter
    
    #plotSearchMMI <- gsub(genelower, geneupper,  gsub('\\\"', "", geneMouse[ind,5])) #gene official name
    if (length(which(geneMouse$mgi_symbol %in% gene)) != 0) { 
      plotSearchMMI <- geneMouse[ind, 5]
      
      synonyme <- geneMouse[ind, 3]
      
      description <- geneMouse[ind, 4]
      
      orthologous <- gsub("<dd>NO</dd>", "Not identified as candidate housekeeping gene", geneMouse[ind, 7])
      ################### Orthologous
      
      
      orthologousHtml <- paste0('<tr>
                              <td style="text-align: justify;" class="col-xs-3">Human orthologous Housekeeping gene</td>',
                                '<td style="text-align: justify;" class="col-xs-9">',orthologous,'</td>
                            </tr>')
      
      ###############################################
      
      
      
      stableTissuem <- gsub("_", " ", gsub("_expr", "", refInfoMouseHK2[ind2, 3]))
      
      if(!is.na(as.character(geneMouse[ind, 5]))) {
        plotSearchMMHtml <- paste0('<tr><td style="text-align: justify;" class="col-xs-3">Gene name</td>',
                                   '<td style="text-align: justify;" class="col-xs-9">', geneMouse[ind, 5],'</td></tr>')
        
      } else {
        plotSearchMMHtml <- paste0('<tr style="display: none">
                      <td class="col-xs-3">Gene name</td>',
                                   '<td class="col-xs-9">',gsub("</dd>", "", gsub("<dd>", "",plotSearchMMI)),'</td>
                     </tr>')
      }
      
      
      
      if(!is.na(geneMouse[ind, 3])) {
        
        synonymeHtml <- paste0('<tr>
                              <td style="text-align: justify;" class="col-xs-3">Other names</td>',
                               '<td style="text-align: justify;" class="col-xs-9">',gsub("</dd>", "", gsub("<dd>", "",synonyme)),'</td>
                            </tr>')
      } else {
        synonymeHtml <- paste0('<tr style="display: none">
                              <td class="col-xs-3">Other names</td>',
                               '<td class="col-xs-9">',synonyme,'</td>
                            </tr>')
      }
      
      
      if(!is.na(geneMouse[ind, 4])) {
        
        descriptionHtml <- paste0('<tr>
                              <td style="text-align: justify;" class="col-xs-3">Description</td>',
                                  '<td style="text-align: justify;" class="col-xs-9">',gsub("</dd>", "", gsub("<dd>", "", description)),'</td>
                            </tr>')
      } else {
        descriptionHtml <- paste0("<tr style='display: none'>
                              <td style='text-align: justify;' class='col-xs-3'>Description</td>
                              <td style='text-align: justify;' class='col-xs-9'>",gsub("</dd>", "", gsub("<dd>", "", description)),"</td>
                            </tr>")
      }
      
      #if(!is.na(refInfoMouseHK2[ind2, 3])) {
        
      #  descriptionHtml2 <- paste0('<tr>
       #                       <td style="text-align: justify;" class="col-xs-3">', paste('<span style="color:red; font-weight: bold;">',transcr,"</span><br>", "<br>", "Reference transcript", "<br>", "<br>", "Proposed for normalization of: "), '</td>',
        #                           '<td style="text-align: justify;" class="col-xs-9">',gsub("</dd>", "", gsub("<dd>", "", stableTissuem)),'</td>
         #                   </tr>')
      #} else {
       # descriptionHtml2 <- "<tr style='display: none'></tr>"
        
      #}
      
      ##################Standard curve#############################
      indPrimer <- which(mousePrimer$Gene %in% gene)
      eff= mousePrimer[indPrimer, 8]
      r2 = mousePrimer[indPrimer, 9]
      effic1 = paste0('<a>Efficiency %: ', eff, '</a>')
      effic2 = paste0('<a><var>R<sup>2</sup></var>: ', r2, '</a>')
      standardHtml <- paste0('<tr>
                              <td rowspan="2" style="text-align: justify;" class="col-xs-3">qPCR Efficiency with Sybr Green</td>',
                             '<td style="text-align: justify; font-weight: bold; color: green" class="col-xs-9">',effic1,'</td>
                            </tr><tr>
                            <td style="text-align: justify; font-weight: bold; color: green" class="col-xs-9">',effic2,'</td>
                            </tr>')
      
      ############################################
      
      HTML(paste0("<h4 style='padding-left: 10px; color: rgb(101, 61, 213);'>", tags$strong(input$searchMMGenes3), "</h4><section style='border-bottom:1px #ccc solid; margin-bottom:50px;'><table class='table'><tbody>",plotSearchMMHtml, synonymeHtml, orthologousHtml, descriptionHtml, standardHtml, "</tbody></table></section>"))
      
    } else {HTML('<br>')}
    
   
   # return(HTML(paste0("<h4 style='padding-left: 10px; color: rgb(101, 61, 213);'>", tags$strong(input$searchHSGenes2), "</h4><section style='border-bottom:1px #ccc solid; margin-bottom:50px;'><table class='table'><tbody>",genenameHtml, synonymeHtml, orthologousHtml, descriptionHtml,standardHtml, "</tbody></table></section>")))
    
  }
  
})


############### Validation Figure ####

outGeneForImg <- reactive({
  
  gene <- gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , input$searchMMGenes3)))# Extract gene symbol
  return(gene)
})

output$imageValMouse = renderUI({
  
  if ( length(input$searchMMGenes3) != 0 && (input$searchMMGenes3 != "Select a reference transcript...")) {
    
    HTML(paste0('<div class="container">
          <div class="row">
            <div id="standValId" class="col-md-11 col-lg-6 html-widget-output shiny-bound-output">
              <a data-toggle="tooltip" title="Click to download the standard curve" href="Validation/Mouse_', outGeneForImg(), '.JPG" target="_blank">
              <svg class="bi bi-download" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
              <path fill-rule="evenodd" d="M.5 8a.5.5 0 01.5.5V12a1 1 0 001 1h12a1 1 0 001-1V8.5a.5.5 0 011 0V12a2 2 0 01-2 2H2a2 2 0 01-2-2V8.5A.5.5 0 01.5 8z" clip-rule="evenodd"/>
              <path fill-rule="evenodd" d="M5 7.5a.5.5 0 01.707 0L8 9.793 10.293 7.5a.5.5 0 11.707.707l-2.646 2.647a.5.5 0 01-.708 0L5 8.207A.5.5 0 015 7.5z" clip-rule="evenodd"/>
              <path fill-rule="evenodd" d="M8 1a.5.5 0 01.5.5v8a.5.5 0 01-1 0v-8A.5.5 0 018 1z" clip-rule="evenodd"/>
</svg> </a>
<img src="Validation/Mouse_', outGeneForImg(), '.JPG" style="margin-top: 10px;">
              
              
              
            </div>
            <div id="meltValId" class="col-md-11 col-lg-6 html-widget-output shiny-bound-output">
            
            <a data-toggle="tooltip" title="Click to download a representative melting curve" href="Validation/Melt_Mouse_', outGeneForImg(), '.JPG" target="_blank">
              <svg class="bi bi-download" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
              <path fill-rule="evenodd" d="M.5 8a.5.5 0 01.5.5V12a1 1 0 001 1h12a1 1 0 001-1V8.5a.5.5 0 011 0V12a2 2 0 01-2 2H2a2 2 0 01-2-2V8.5A.5.5 0 01.5 8z" clip-rule="evenodd"/>
              <path fill-rule="evenodd" d="M5 7.5a.5.5 0 01.707 0L8 9.793 10.293 7.5a.5.5 0 11.707.707l-2.646 2.647a.5.5 0 01-.708 0L5 8.207A.5.5 0 015 7.5z" clip-rule="evenodd"/>
              <path fill-rule="evenodd" d="M8 1a.5.5 0 01.5.5v8a.5.5 0 01-1 0v-8A.5.5 0 018 1z" clip-rule="evenodd"/>
</svg> </a>
<img src="Validation/Melt_Mouse_', outGeneForImg(), '.JPG">
</div>
              
            </div>
          
        </div>
            ')#End of HTML function
    )
  }
})
