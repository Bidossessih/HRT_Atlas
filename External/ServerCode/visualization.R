
# Render gene name in Tabpanel

output$genenameChoice <- renderText({ 
  choice=HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchHSGenes))))), 2]
paste("The following datasets present some small molecules and diseases which may modify", choice, "gene expression. These resources may guide the choice of suitable reference genes for qPCR normalization.")
  
})



#Plot

x <- list(
  title = "",
  showticklabels = TRUE,
  tickangle = 60
)

y <- list(
  title = "Normalized Expression (log2 RPKM)"
  
)

HK_geneAtualizadoRef <- mutate(HK_geneAtualizadoRef, choice=paste0(Gene.name, " ", "(",Ensembl,")"))

output$plotHsSearch <- renderPlotly({
  load("External/Data/MostStableRPKM.RData")
  GenesHKs <- input$searchHSGenes
  plot_ly(HK_RPKM[1:11281,], y = ~eval(parse(text=gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchHSGenes)))))), x = ~Tissue, type = "box", color = ~Tissue, hoverinfo = "none", 
          textfont = list(color = 'red', size = 12)) %>%  
    layout(title = paste0("Expression level of ", HK_geneAtualizadoRef$Gene.name[which(HK_geneAtualizadoRef$Ensembl %in% GenesHKs)], " ", GenesHKs, " ", "in different tissues")) %>% 
    layout(showlegend = FALSE, xaxis=x, yaxis=y)
})

#output$outx <- renderPrint(input$searchHSGenes)





#Display gene description

output$genename <- renderUI({
  
  gene <- HK_geneAtualizadoRef[which(HK_geneAtualizadoRef$Ensembl %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchHSGenes))))), 2]# Extract gene symbol
  transcr <- gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchHSGenes))))
  ind <- which(GeneInfo$hgnc_symbol %in% gene)
  
  ind2 <- which(refInfoHumanHK2$Ensembl %in% transcr) 
  
  genelower <- gsub("<", "", gsub(">", "", gsub(".*dd\\>(.+)\\<span cl.*", "\\1", GeneInfo[ind,5])))
  geneupper <- paste(toupper(substr(genelower, 1, 1)), substr(genelower, 2, nchar(genelower)), sep="") #Upper of the first letter
  
  if (length(which(HK_geneAtualizadoRef$Gene.name %in% gene)) != 0) {  # description
 genenameI <- gsub("</dd>", "", gsub("<dd>", "", gsub(genelower, geneupper,  GeneInfo[ind, 5]))) #gene official name
  
  synonyme <- GeneInfo[ind, 4]
  
  description <- GeneInfo[ind, 3]
  
  orthologous <- gsub("<dd>NO</dd>", "Not identified as candidate reference gene in mouse",GeneInfo[ind, 7])
  
  stableTissue <- gsub("_", " ", gsub("_expr", "", refInfoHumanHK2[ind2, 3]))
  
  if(!is.na(GeneInfo[ind, 5])) {
    genenameHtml <- paste0('<tr >
                      <td style="text-align: justify;" class="col-xs-3">Gene name</td>',
                      '<td style="text-align: justify;" class="col-xs-9">',gsub("</dd>", "", gsub("<dd>", "",genenameI)),'</td>
                      </tr>')
  } else {
    genenameHtml <- paste0('<tr style="display: none">
                      <td class="col-xs-3">Gene name</td>',
           '<td class="col-xs-9">',gsub("</dd>", "", gsub("<dd>", "",genenameI)),'</td>
                      </tr>')
  }
  
  
  
  if(!is.na(GeneInfo[ind, 4])) {
    
    synonymeHtml <- paste0('<tr>
                              <td style="text-align: justify;" class="col-xs-3">Other names</td>',
                              '<td style="text-align: justify;" class="col-xs-9">',gsub("</dd>", "", gsub("<dd>", "",synonyme)),'</td>
                            </tr>')
  } else {
    synonymeHtml <- paste0('<tr style="display: none">
                              <td class="col-xs-2">Other names</td>',
                              '<td class="col-xs-9">',synonyme,'</td>
                            </tr>')
  }
  
  
  if(!is.na(GeneInfo[ind, 3])) {
    
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
  
  ################### Orthologous
  
  
    orthologousHtml <- paste0('<tr>
                              <td style="text-align: justify;" class="col-xs-3">Mouse orthologous Housekeeping gene</td>',
                              '<td style="text-align: justify;" class="col-xs-9">',orthologous,'</td>
                            </tr>')
 
  ###############################################
  
  
  if(!is.na(refInfoHumanHK2[ind2, 3])) {
    
    descriptionHtml2 <- paste0('<tr>
                              <td style="text-align: justify;" class="col-xs-3">', paste('<span style="color:red; font-weight: bold;">',transcr,"</span><br>", "<br>", "Reference transcript", "<br>", "<br>", "Proposed for normalization of: "), '</td>',
                              '<td style="text-align: justify;" class="col-xs-9">',gsub("</dd>", "", gsub("<dd>", "", stableTissue)),'</td>
                            </tr>')
  } else {
    descriptionHtml2 <- "<tr style='display: none'></tr>"
    
    }
  
  
  
   
HTML(paste0("<h4>", gene, "</h4><section style='border-bottom:1px #ccc solid; margin-bottom:50px;'><table class='table'><tbody>",genenameHtml, synonymeHtml, orthologousHtml, descriptionHtml, descriptionHtml2, "</tbody></table></section>"))
  
  } else {HTML('<br>')} # Description End
  
  
  
  })

