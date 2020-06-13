output$pageHousekeeping <- renderUI(tagList(
  
  fluidRow(useShinyjs(), 
    class="Container-fluid",
    includeHTML("External/HTML/visualizationMouse.html"),
             
    div(class="Container-fluid panelstyle",   
             selectInput('searchMMGenes', 'Select a mouse housekeeping gene to show its transcript expression level in different tissues and cells', sort(unique(Mouse_HK_genes$choice)), selectize=FALSE),
             
             
            #verbatimTextOutput("outx"),
            div(style="width: 100%;", uiOutput("plotSearchMM")),
            
            
            div(id="tabpanelViz", class="panelstyle", style="margin-top: 50px;",
                
                plotlyOutput("plotMmSearch", height = "500px")
                  
                )),
      
      includeHTML("External/HTML/footer.html")
            
            


)


))



load("www/Housekeeping_Genes_Mouse.RData")
colnames(Mouse_HK_genes)[2]="Ensembl"
Mouse_HK_genes = mutate(Mouse_HK_genes, choice=paste0(Gene, " ", "(", Ensembl, ")"))

#Plot

x <- list(
  title = "",
  showticklabels = TRUE,
  tickangle = 60
)

y <- list(
  title = "Normalized Expression ( log2 RPKM)"
  
)

#geneMouse <- mutate(geneMouse, choice=paste0(Gene.name, " ", "(",Ensembl,")"))

output$plotMmSearch <- renderPlotly({
  load("External/Data/HK_RPKM_Mouse.RData")
  
  GenesHKs <- input$searchMMGenes
  plot_ly(HK_RPKM_Mouse, y = ~eval(parse(text=gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchMMGenes)))))), x = ~Tissue, type = "box", color = ~Tissue, hoverinfo = "none",
          textfont = list(color = '#ccc', size = 12)) %>%  
    layout(title = paste("Expression level (RPKM) of", geneMouse$Gene.name[which(geneMouse$Ensembl %in% GenesHKs)], GenesHKs, " in different tissues and cells")) %>% 
    layout(showlegend = FALSE, xaxis=x, yaxis=y)
})


#output$outx <- renderPrint(which(Housekeeping_TranscriptsMouse$`Ensembl ID` %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , input$searchMMGenes)))))





################################Display table with disease from harmonizome


output$plotSearchMM <- renderUI({
  
  load("www/Housekeeping_TranscriptsMouse.RData")
  gene <- Housekeeping_TranscriptsMouse[which(Housekeeping_TranscriptsMouse$`Ensembl ID` %in% gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchMMGenes))))), 2]# Extract gene symbol
  
  transcr <- gsub("^[A-z0-9_]+[:(:]","" , gsub("[:):]$", "", gsub(" ","" , gsub("-", "_", input$searchMMGenes))))
  
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
  
  if(!is.na(refInfoMouseHK2[ind2, 3])) {
    
    descriptionHtml2 <- paste0('<tr>
                              <td style="text-align: justify;" class="col-xs-3">', paste('<span style="color:red; font-weight: bold;">',transcr,"</span><br>", "<br>", "Reference transcript", "<br>", "<br>", "Proposed for normalization of: "), '</td>',
                               '<td style="text-align: justify;" class="col-xs-9">',gsub("</dd>", "", gsub("<dd>", "", stableTissuem)),'</td>
                            </tr>')
  } else {
    descriptionHtml2 <- "<tr style='display: none'></tr>"
    
  }
  
  
  
    HTML(paste0("<h4>", gene, "</h4><section style='border-bottom:1px #ccc solid; margin-bottom:50px;'><table class='table'><tbody>",plotSearchMMHtml, synonymeHtml, orthologousHtml, descriptionHtml, descriptionHtml2, "</tbody></table></section>"))
  
  } else {HTML('<br>')}
  
  })



