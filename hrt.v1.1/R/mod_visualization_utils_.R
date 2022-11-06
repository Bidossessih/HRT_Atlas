#' Display table
#'

table_func = function(col1_info, col2_info) {
  tags$tbody(tags$tr(tags$td(col1_info),
                     tags$td(col2_info))
             )

}



#' Function to define render the description of each transcript
#'
#'
#' @param specie human or mouse
#' @param gene.symbol the symbol of selected gene. Default is the default name in selectize
#' @param gene.name is the official name of the seclected gene
#' @param gene.other.names Other known names of the gene. It will appear if not Null
#' @param transcript string or vector of string of transcripts. If length is > 1,
#' all of the transcripts will be displayed if the target tissues are different.
#' @param tissue_info tissues for which the transcript has been proposed
#'  to be a reference transcript
#'  ortholog the second specie if the same gene has been idenfity as HK in both


hk_transcript_desc = function(specie, gene.symbol,
                              gene.name,
                              gene.description,
                              transcript,
                              tissue_info,
                              gene.other.names = NULL,
                              ortholog = NULL){

# Define orthologous gene
  if(is.null(ortholog)) {
    if(specie == "Human"){
      ort_specie = "mouse"
    } else if (specie == "Mouse"){
      ort_specie = "human"
    } else {
      ort_specie = NA # to be modified
    }

    #first column
    ortholog_0 = stringr::str_glue("{stringr::str_to_sentence(ort_specie)} orthologous Housekeeping gene")
    #second column
    ortholog = stringr::str_glue("Not identified as candidate reference gene in {ort_specie}")
  }


  return(

    tags$div(class = "Container-fluid panelstyle",
             tags$br(),
             #Display gene symbol
             h1("Housekeeping gene: {gene.symbol}"),
             tags$section(style="border-bottom:1px #ccc solid; margin-bottom:50px;",
             tags$table(class = "table",

                        # Gene name row
                        table_func(col1_info = "Gene name", col2_info = gene.name),
                        # Other Gene name
                        table_func(col1_info = "Other names", col2_info = gene.other.names),
                        # Orthog Gene if it has been identified in the second specie
                        table_func(col1_info = ortholog_0, col2_info = ortholog),
                        # Gene description from RefSeq
                        table_func(col1_info = "Description", col2_info = gene.description),
                        # Transcript names and tissue in which it can be used for qpcr
                        #normalization
                        if(length(transcript == 1)){
                          table_func(col1_info = stringr::str_glue('tags$span(style = "color:red; font-weight: bold;",
                                                           {transcript})'),
                                     col2_info = tissue_info)
                        } else {

                          return(NULL)

                        }



                        )
    )#End of table section
  ))
}
