# Plot Bulk rnaseq Gtex
#ENST00000248450 ENST00000444053
ens_l=gene_info$ens_transcript_id
ens_data = loadDataTrans(ens_l)
#trans_plot = violin_bulk_tissue(data=ens_data)

ens_data$cell_type = factor(ens_data$cell_type)

output$bulk_plot <- renderPlotly({
  
  plot_ly(ens_data, y = ~log_rpkm, x = ~cell_type, type = "violin",
          hoverinfo = "none", split = ~cell_type, box = list(visible = T),
          meanline = list(visible = T),
          textfont = list(color = 'red', size = 12)) %>%
    layout(title = paste0("Expression level of ", ens_l[1])) %>%
    layout(showlegend = FALSE, xaxis=x, yaxis=y)
  
})
