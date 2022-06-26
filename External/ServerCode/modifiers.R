# Render gene name in Tabpanel

output$genenameChoice <- renderText({ 
  choice=HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchHSGenes))))), 2]
paste("The following datasets present some small molecules and diseases which may modify", choice, "gene expression. These resources may guide the choice of suitable reference genes for qPCR normalization.")
  
})



# Create column used as choice in SelectInput function
HK_geneAtualizadoRef <- mutate(HK_geneAtualizadoRef, choice=paste0(Gene.name, " ", "(",Ensembl,")"))


#output$outx <- renderPrint(input$searchHSGenes)





################################Display table with disease from harmonizome

output$accordionhk1 <- DT::renderDT({ 
  
  ensemblPop <-'<a  data-toggle="tooltip" title="Transcript identification according to Ensembl database."> Ensembl ID<img src="info.png" style="width: 10px;"></a>'
  
  sdPop <-'<a  data-toggle="tooltip" title="Variation estimated as standard deviation of log2 of Read Per Kilobase Million."> Std Deviation <img src="info.png" style="width: 10px;"></a>'
  
  tab <- as.character(HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchHSGenes))))), 2])# Extract gene symbol
  
  #tab = "AAMP"
  load(paste0("External/Harmonizome/", tab, "interest.RData"))
  
  dataHarmonizome = data
  #dataHarmonizome=AAMPinterest
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



output$genename <- renderUI({
  
  gene <- HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchHSGenes))))), 2]# Extract gene symbol
  
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
  
  
  
  
  
   
HTML(paste0("<h4>", gene, "</h4><section style='border-bottom:1px #ccc solid; margin-bottom:50px;'><table class='table'><tbody>",genenameHtml, synonymeHtml, descriptionHtml, "</tbody></table></section>"))
  
  
  
  })



################################Display table with GEO Signatures of Differentially Expressed Genes for small Molecules from harmonizome

output$accordionhk2 <- DT::renderDT({ 
  
  ensemblPop <-'<a  data-toggle="tooltip" title="Transcript identification according to Ensembl database."> Ensembl ID<img src="info.png" style="width: 10px;"></a>'
  
  sdPop <-'<a  data-toggle="tooltip" title="Variation estimated as standard deviation of log2 of Read Per Kilobase Million."> Std Deviation <img src="info.png" style="width: 10px;"></a>'
  
  #colnames(hk)[2] <- ensemblPop
  
  #colnames(hk)[5] <- sdPop
  
  #tab <- as.character(which(HK_geneAtualizadoRef$Ensembl %in% "ENST00000248450"))
  
  tab <- as.character(HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchHSGenes))))), 2])# Extract gene symbol
  
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

output$accordionhk3 <- DT::renderDT({ 
  
  ensemblPop <-'<a  data-toggle="tooltip" title="Transcript identification according to Ensembl database."> Ensembl ID<img src="info.png" style="width: 10px;"></a>'
  
  sdPop <-'<a  data-toggle="tooltip" title="Variation estimated as standard deviation of log2 of Read Per Kilobase Million."> Std Deviation <img src="info.png" style="width: 10px;"></a>'
  
  #colnames(hk)[2] <- ensemblPop
  
  #colnames(hk)[5] <- sdPop
  
  #tab <- as.character(which(HK_geneAtualizadoRef$Ensembl %in% "ENST00000248450"))
  
  tab <- as.character(HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchHSGenes))))), 2])# Extract gene symbol
  
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