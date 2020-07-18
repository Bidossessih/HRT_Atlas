
##################################
# Show selectInput on click
observeEvent(input$tabsetID, {
  if (input$tabsetID == "Validation") {
    disable("mfc")
    disable("rpkm")
    
    shinyjs::show("sel2")
  } else {
    shinyjs::hide("sel2")
  }
})

observeEvent(input$tabsetID, {
  if (input$tabsetID == "spec_primer") {
    disable("mfc")
    disable("rpkm")
    
  }
})

observeEvent(input$tabsetID, {
  if (input$tabsetID == "refTranscr") {
    enable("mfc")
    enable("rpkm")
    
  }
})

# Show selectInput on click

observeEvent(input$tabsetID, {
  if (input$tabsetID == "Regulatory Elements" || input$tabsetID == "Expression Modifiers") {
    shinyjs::show("sel1")
  } else {
    shinyjs::hide("sel1")
  }
  
})


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
  reset("search")
  reset("rpkm")
  reset("mfc")
  
})


observeEvent(input$buttonresult, {
  
  if(input$search==""){
    shinyjs::alert("Please select a tissue or cell type.")
  } else {
    shinyjs::hide("homeMouse")
    shinyjs::show("tabpanelUI2")
    shinyjs::show("tabpanelUI")
    shinyjs::show("headersearch")
    shinyjs::show("headersearch2")
  }
  
})

observeEvent(input$geneinputID, {
  shinyjs::hide("homeMouse")
  shinyjs::show("tabpanelUI")
  addClass(id = "pageUIMouse", class = "addedbackground")
})

##################################### Code from Human ##########################

################### Reference transcript table in reactive variable ###################


mouseRefTable <- reactive({
  
  
  # Compute reference table based on user inputs and filtering criteria
  if(input$mfc=="linear"){
    con <- con1
  } 
  
  table=gsub(" ", "_", input$search)
  
  
  data <- data.frame(tbl(con, paste0(table, "_Mouse")))
  #data <- load(paste0("External/Data/db1/", table, ".RData"))
  
  
  
  data <- data.frame(data)
  
  if(input$mfc=="log"){
    hk <- select(data, c("Ensembl","Gene", "Mean", "SD_of_log", "MCF_Log_Mean", "Max","Min","Log_MFC_min_and_mean"))
    
  } else {
    hk <- select(data, c("Ensembl","Gene", "Mean", "SD_of_log", "MCF_mean", "Max","Min","MFC_min_and_mean"))
  }
  
  # cat("dimension of line 241:", dim(hk), "\n")
  #Filter only HK transcripts
  colnames(Housekeeping_TranscriptsMouse)[1]="Ensembl"
  hk <- unique(na.omit(merge(hk, Housekeeping_TranscriptsMouse[,c(1,3:5)], by="Ensembl", all.y=T)))
  
  
  #Apply rpkm criteria selected by the user
  hk = filter(hk, Mean >= input$rpkm)
  
  
  # Ranking
  
  
  if(input$mfc=="log"){
    #hk <- select(data, c("Ensembl","Gene", "Mean", "SD_of_log", "MCF_Log_Mean", "Max","Min","Log_MFC_min_and_mean"))
    hk <- hk %>% arrange(Log_MFC_min_and_mean) %>% mutate(RankMFC=1:nrow(hk))
    
    hk <- hk %>% arrange(desc(Mean)) %>% mutate(RankRPKM=1:nrow(hk)) 
    
    hk <- hk %>% mutate(RankProd=RankMFC*RankRPKM) %>%  arrange(RankProd) %>% mutate(Rank=1:nrow(hk))
    
    
    
  } else {
    #hk <- select(data, c("Ensembl","Gene", "Mean", "SD_of_log", "MCF_mean", "Max","Min","MFC_min_and_mean"))
    
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
  
  
  # Reference table from rective "mouseRefTable"
  
  hk = mouseRefTable()
  
  if(class(hk)=="data.frame"){
    hk = filter(hk, Mean >= input$rpkm)
    
    data <- na.omit(select(hk, c("Rank","Ensembl","Gene")) %>% arrange(Gene))
    
    #Use interaction and lexicography to preserve the ranking order
    
    choiceList <- interaction(data[,3], " ", "(", data[,2], ")", sep=  "", lex.order = FALSE)
    
    selectInput('selectHSGenes', 'Select a gene to show some modifiers of its expression', choiceList, selectize=FALSE)
  }
  
})



##############Reference transcript table#################

output$outCap <- renderUI({
  
  # Reference table from rective "mouseRefTable"
  table=gsub(" ", "_", input$search)
  
  
  
  tab <- tab_echant_mouse[table,]
  
  hk = mouseRefTable()
  
  # Render caption outside of the table
  
  if(class(hk)=="data.frame"){
    hk = filter(hk, Mean >= input$rpkm)
    HTML(paste0(nrow(hk), " ", "Transcripts found from ", " ", tab[,2], " ", "high quality", " ", "<a class='font-weight-bold font-italic'>",input$search, "</a> samples. <a class='font-weight-bold'>Following the MIQE guidelines we recommend using of at least two reference transcripts.</a>"))
    
  } 
  
  
  
})





#wrappe DT::renderDT in a renderUI to control error mensage
#This strategy can solve many shiny problem and is able to either table or other Ui element 
#with the same code

output$tabHK <- renderUI({
  
  
  # Reference table from rective "mouseRefTable"
  
  hk = mouseRefTable()
  
  
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
    # Reference table from rective "mouseRefTable"
    
    hk = mouseRefTable()
    
    table=gsub(" ", "_", input$search)
    
    
    tab <- tab_echant_mouse[table,]
    #########Test whether the filtering used by the user provide any reference transcript#######
    
    
    if(class(hk)=="data.frame") {
      
      output$mytabHkprimer <- DT::renderDT({ 
        
        
        # primer table from rective "mouseRefTable"
        
        data <- hk
        
        data <- na.omit(select(data, c("Rank","Ensembl","Gene")) %>% arrange(Rank))
        
        names(data) = c("Rank", "Transcript_ID", "Gene_name")
        
        dPrimer = na.omit(unique(merge(data[,1:2], mousePrimer, all.x = T, by = "Transcript_ID")))
        
        dPrimer = arrange(dPrimer, Rank)
        ####################### Reference transcript table in reactive variable ###################
        
        
        mouseRefTable <- reactive({
          
          
          # Compute reference table based on user inputs and filtering criteria
          if(input$mfc=="linear"){
            con <- con1
          } 
          
          table=paste0(gsub(" ", "_", input$search), "_Mouse")
          
          
          data <- data.frame(tbl(con, table))
          #data <- load(paste0("External/Data/db1/", table, ".RData"))
          
          
          
          data <- data.frame(data)
          
          if(input$mfc=="log"){
            hk <- select(data, c("Ensembl","Gene", "Mean", "SD_of_log", "MCF_Log_Mean", "Max","Min","Log_MFC_min_and_mean"))
            
          } else {
            hk <- select(data, c("Ensembl","Gene", "Mean", "SD_of_log", "MCF_mean", "Max","Min","MFC_min_and_mean"))
          }
          
          # cat("dimension of line 241:", dim(hk), "\n")
          #Filter only HK transcripts
          colnames(Housekeeping_TranscriptsMouse)[1]="Ensembl"
          hk <- unique(na.omit(merge(hk, Housekeeping_TranscriptsMouse[,c(1,3:5)], by="Ensembl", all.y=T)))
          
          
          #Apply rpkm criteria selected by the user
          hk = filter(hk, Mean >= input$rpkm)
          
          
          # Ranking
          
          
          if(input$mfc=="log"){
            #hk <- select(data, c("Ensembl","Gene", "Mean", "SD_of_log", "MCF_Log_Mean", "Max","Min","Log_MFC_min_and_mean"))
            hk <- hk %>% arrange(Log_MFC_min_and_mean) %>% mutate(RankMFC=1:nrow(hk))
            
            hk <- hk %>% arrange(desc(Mean)) %>% mutate(RankRPKM=1:nrow(hk)) 
            
            hk <- hk %>% mutate(RankProd=RankMFC*RankRPKM) %>%  arrange(RankProd) %>% mutate(Rank=1:nrow(hk))
            
            
            
          } else {
            #hk <- select(data, c("Ensembl","Gene", "Mean", "SD_of_log", "MCF_mean", "Max","Min","MFC_min_and_mean"))
            
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
          
          
          # Reference table from rective "mouseRefTable"
          
          hk = mouseRefTable()
          
          if(class(hk)=="data.frame"){
            hk = filter(hk, Mean >= input$rpkm)
            
            data <- na.omit(select(hk, c("Rank","Ensembl","Gene")) %>% arrange(Gene))
            
            #Use interaction and lexicography to preserve the ranking order
            
            choiceList <- interaction(data[,3], " ", "(", data[,2], ")", sep=  "", lex.order = FALSE)
            
            selectInput('selectHSGenes', 'Select a gene to show some modifiers of its expression', choiceList, selectize=FALSE)
          }
          
        })
        
        
        
        ##############Reference transcript table#################
        
        output$outCap <- renderUI({
          
          # Reference table from rective "mouseRefTable"
          table=gsub(" ", "_", input$search)
          
         # cat("table value 608:", table)
          
          tab <- tab_echant_mouse[table,]
          
          hk = mouseRefTable()
          
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
          
          
          # Reference table from rective "mouseRefTable"
          
          hk = mouseRefTable()
          
          
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
            # Reference table from rective "mouseRefTable"
            
            hk = mouseRefTable()
            
            table=gsub(" ", "_", input$search)
            
            
            tab <- tab_echant_mouse[table,]
            
            #########Test whether the filtering used by the user provide any reference transcript#######
            
            
            if(class(hk)=="data.frame") {
              
              output$mytabHkprimer <- DT::renderDT({ 
                
                
                # primer table from rective "mouseRefTable"
                
                data <- hk
                
                data <- na.omit(select(data, c("Rank","Ensembl","Gene")) %>% arrange(Rank))
                
                names(data) = c("Rank", "Transcript_ID", "Gene_name")
                
                dPrimer = na.omit(unique(merge(data[,1:2], mousePrimer, all.x = T, by = "Transcript_ID")))
                
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
                              
                              #caption = htmltools::tags$caption(
                                #style = 'caption-side: top; text-align: left; color: black; font-family: "Proxima Nova"', "We manually designed some transcript specific primers that can be used in your experiments. The temperature calculations are done assuming 200 nM of annealing oligo concentration, 50mM of salt concentration (Na+) and 1.5 mM of divalent ion concentration (Mg++). We recommend using of at least two reference transcripts for normalization purpose."
                              #),
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
        
        
        
        #Popup of colnames
        
        rankPop <-'<a  data-toggle="tooltip" title="Transcript rank according to Reference Transcripts table.">Rank<img src="info.png" style="width: 10px;"></a>'
        
        
        #rpkm <- HTML("<a href='https://www.ebi.ac.uk/training/online/glossary/rpkm'>RPKM</a>") #link of rpkm definition
        colnames(dPrimer)[2] <- rankPop
        
        #svgico  ====> voir global.R
        
        dPrimer = dPrimer[, c(2,1,3:8)]
        
        #dPrimer = mutate(dPrimer, Validation=paste('<a class=" dt-center" data-toggle="modal" data-target="#exampleModalScrollable">', 
        #                                          svgico1,"id=", Gene,"data-filter=", Gene, svgico2, "</a>"))
        #dPrimer = dPrimer[, c(1,2,9,3:8)]
        
        colnames(dPrimer)[c(2,8)] = c("Ensembl ID", "Amplicon length (bp)")
        
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

#################****************###############################
########################****################
################################Validation results#################
##########
###########################################################
############### Render selectInput from server side ###########
outGeneName <- reactive({
  
  if(input$tabsetID=="Validation"){
    # Reference table from rective "mouseRefTable"
    
    hk = mouseRefTable()
    
    if(class(hk)=="data.frame"){
      
      hk = filter(hk, Mean >= input$rpkm)
      #select transcript with primers
      
      
      data <- na.omit(select(hk, c("Ensembl","Gene")))
      
      names(data)[1] = "Transcript_ID"
      
      data = na.omit(unique(merge(data, mousePrimer, all.x = T, by = "Transcript_ID")))
      
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
    
    hk = mouseRefTable()
    
    if(class(hk)=="data.frame") {
      
      condTest = input$searchHSGenes2
      
      if(length(outGeneName())==0){
        shinyjs::alert("No primer available with this filtering criteria.")
        shinyjs::hide("sel2")
      }
      
      if ( length(input$searchHSGenes2) != 0 && (input$searchHSGenes2 != "Select a reference transcript...")) {
        gene <- gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , input$searchHSGenes2)))# Extract gene symbol
        #cat("gene is at line 976:", gene, "\n")
        ind <- which(geneMouse$mgi_symbol %in% gene)
        #cat("ind is at line 978:", ind, "\n")
        genelower <- gsub("<", "", gsub(">", "", gsub(".*dd\\>(.+)\\<span cl.*", "\\1", geneMouse[ind,5])))
        geneupper <- paste(toupper(substr(genelower, 1, 1)), substr(genelower, 2, nchar(genelower)), sep="") #Upper of the first letter
        
        genenameI <- gsub("</dd>", "", gsub("<dd>", "", gsub(genelower, geneupper,  geneMouse[ind, 5]))) #gene official name
        
        synonyme <- geneMouse[ind, 4]
        
        description <- geneMouse[ind, 3]
        
        
        if(!is.na(geneMouse[ind, 5])) {
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
        
        
        
        if(!is.na(geneMouse[ind, 4])) {
          
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
        
        
        if(!is.na(geneMouse[ind, 3])) {
          
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
        orthologous <- gsub("<dd>NO</dd>", "Not identified as candidate reference transcript in mouse",geneMouse[ind, 7])
        
        
        orthologousHtml <- paste0('<tr>
                                  <td style="text-align: justify;" class="col-xs-3">Mouse orthologous Housekeeping gene</td>',
                                  '<td style="text-align: justify;" class="col-xs-9">',orthologous,'</td>
                                  </tr>')
        
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
    # Reference table from rective "mouseRefTable"
    
    hk = mouseRefTable()
    
    if(class(hk)=="data.frame") {
      
      
      if ( length(input$searchHSGenes2) != 0) {
        
        HTML(paste0('<div class="container">
                    <div class="row">
                    
                    <div id="standValId" class="col-11 html-widget-output shiny-bound-output">
                    <a data-toggle="tooltip" title="Click to download the standard curve" href="Validation/Mouse_', outGeneForImg(), '.JPG" target="_blank">
                    <svg class="bi bi-download" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
                    <path fill-rule="evenodd" d="M.5 8a.5.5 0 01.5.5V12a1 1 0 001 1h12a1 1 0 001-1V8.5a.5.5 0 011 0V12a2 2 0 01-2 2H2a2 2 0 01-2-2V8.5A.5.5 0 01.5 8z" clip-rule="evenodd"/>
                    <path fill-rule="evenodd" d="M5 7.5a.5.5 0 01.707 0L8 9.793 10.293 7.5a.5.5 0 11.707.707l-2.646 2.647a.5.5 0 01-.708 0L5 8.207A.5.5 0 015 7.5z" clip-rule="evenodd"/>
                    <path fill-rule="evenodd" d="M8 1a.5.5 0 01.5.5v8a.5.5 0 01-1 0v-8A.5.5 0 018 1z" clip-rule="evenodd"/>
                    </svg> 
                    </a>
                    <a id="idown1" style="margin-left:10px">Download</a>
                    
                    </div>
                    
                    <div id="standValId" class="col-11 html-widget-output shiny-bound-output">
                    <img style="width:100%; height:100%;" src="Validation/Mouse_', outGeneForImg(), '.JPG" style="margin-top: 10px;">
                    
                    </div>
                    
                    <div id="meltValId" class="col-11 html-widget-output shiny-bound-output" style="margin-top:50px;">
                    
                    
                    <a data-toggle="tooltip" title="Click to download a representative melting curve" href="Validation/Melt_Mouse_', outGeneForImg(), '.JPG" target="_blank">
                    <svg class="bi bi-download" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
                    <path fill-rule="evenodd" d="M.5 8a.5.5 0 01.5.5V12a1 1 0 001 1h12a1 1 0 001-1V8.5a.5.5 0 011 0V12a2 2 0 01-2 2H2a2 2 0 01-2-2V8.5A.5.5 0 01.5 8z" clip-rule="evenodd"/>
                    <path fill-rule="evenodd" d="M5 7.5a.5.5 0 01.707 0L8 9.793 10.293 7.5a.5.5 0 11.707.707l-2.646 2.647a.5.5 0 01-.708 0L5 8.207A.5.5 0 015 7.5z" clip-rule="evenodd"/>
                    <path fill-rule="evenodd" d="M8 1a.5.5 0 01.5.5v8a.5.5 0 01-1 0v-8A.5.5 0 018 1z" clip-rule="evenodd"/>
                    </svg> 
                    </a>
                    <a id="idown2" style="margin-left:10px">Download</a>
                    
                    </div> 
                    
                    <div id="meltValId" class="col-11 html-widget-output shiny-bound-output">
                    <img style="width:100%; height:100%;" src="Validation/Melt_Mouse_', outGeneForImg(), '.JPG">
                    
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


