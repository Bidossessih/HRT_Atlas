#' Display table
#'


table_func = function(col1_info, col2_info) {
  tags$tr(tags$td(col1_info),
          tags$td(col2_info))


}



#' Function to define render the description of each transcript
#'
#'
#' @param specie human or mouse
#' @param gene.symbol the symbol of selected gene. Default is the default name in selectize
#' @param gene.name is the official name of the seclected gene
#' @param gene.other.names Other known names of the gene. It will appear if not Null
#' @param tissue_info tissues for which the transcript has been proposed
#'  to be a reference transcript
#' @param tissue_info_id id of the div where tissue
#'  ortholog the second specie if the same gene has been idenfity as HK in both
#'  @importFrom stringr str_glue


hk_transcript_desc = function(specie, gene.symbol,
                              gene.name,
                              gene.description,
                              tissue_info_id,
                              gene.other.names,
                              ortholog, trans_selectize){

# Define orthologous gene
  if(specie == "Human") {
    ort_specie = "Mouse"
  } else if (specie == "Mouse") {
    ort_specie = "Human"
  }

  #first column
  ortholog_0 = stringr::str_glue("{ort_specie} orthologous Housekeeping gene")
  #second column. When an ortholog HK is not identified
  if (is.na(ortholog)) {
    ortholog = stringr::str_glue("Not identified as candidate reference gene in {ort_specie}")
  }



  return(

    tags$div(class = "Container-fluid panelstyle",
             tags$br(),
             #Display gene symbol
             h4(str_glue("{gene.symbol}")),
             tags$section(style="border-top:1px #ccc solid; margin-bottom:50px;",
             tags$table(class = "table",

                        tags$tbody(
                          # Gene name row
                          table_func(col1_info = "Gene name", col2_info = HTML(gene.name)),
                          # Other Gene name
                          if(!is.na(gene.other.names)) {
                            table_func(col1_info = "Synonyms",
                                       col2_info = HTML(gene.other.names))
                          },
                          # Orthog Gene if it has been identified in the second specie
                          table_func(col1_info = ortholog_0, col2_info = HTML(ortholog)),
                          # Gene description from RefSeq
                          if(!is.na(gene.description)) {
                            table_func(col1_info = "Description",
                                       col2_info = HTML(gene.description))
                          },
                          # Transcript names and tissue in which it can be used for qpcr
                          #normalization
                          table_func(
                              col1_info = trans_selectize,
                              # the list of tissue with be inserted conditionally from the
                              # visualization module server using the id tissue_info_id
                              col2_info = HTML(str_glue("<div id = '{tissue_info_id}'></div>"))
                            )

                        ))
    )#End of table section
  ))
}


#' Violin Plot
#'
#' @import ggplot2
#' @import ggiraph
#'
#'


violin_bulk_tissue = function(data, width_svg, height_svg){

  data$cell_type = factor(data$cell_type)

  data %>%

  ggplot(aes(x=cell_type, y=log_rpkm, fill=cell_type)) +
    geom_violin_interactive(scale="width",adjust = 1, aes(tooltip=after_stat(density), fill=cell_type)) +
    geom_boxplot_interactive(scale="width",adjust = 1, width = 0.2, color = "white", alpha=0.7) +
    theme_linedraw() +
    theme(legend.position='none',
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) -> gg_violin

  girafe(ggobj = gg_violin,
         options = list(
           opts_sizing(rescale = FALSE)),
         width_svg=width_svg, height_svg = height_svg
         ) -> gg_violin


  girafe_options(
    gg_violin,
    #opts_selection(type = input$opt_selected_data),
    #opts_hover(reactive = input$opt_hover_data),
    opts_selection_key(
      #type = input$opt_selected_key,
      css = girafe_css("stroke:red; stroke-width:2px",
                       text = "stroke:none;fill:red;font-size:12px")
    )
  )

  gg_violin

}




violin_plotly = function(data){

  data$cell_type = factor(data$cell_type)

  data %>%

    ggplot(aes(x=cell_type, y=log_rpkm, fill=cell_type)) +
    geom_violin(scale="width",adjust = 1) +
    geom_boxplot(width = 0.1, color = "white", alpha=0.3) +
    theme_minimal() + coord_flip() +
    theme(legend.position='none',
          axis.text.x = element_text(angle = 120)) -> gg_violin

  plotly::ggplotly(gg_violin)

}
