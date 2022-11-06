#' visualization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visualization_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("viz"))
  )
}

#' visualization Server Functions
#'
#' @noRd
mod_visualization_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    specie = "human"
    gene.symbol="EEF2"
    gene.name="gene.name of EEF2"
    gene.description="gene.description of EEF2"
    transcript="transcript of EEF2"
    tissue_info="tissue_info of EEF2"
    gene.other.names = NULL
    ortholog = NULL



    output$viz = renderUI(

      hk_transcript_desc(
        specie=specie,
        gene.symbol=gene.symbol,
        gene.name=gene.name,
        gene.description=gene.description,
        transcript=transcript,
        tissue_info=tissue_info,
        gene.other.names = NULL,
        ortholog = NULL
      )
    )


  })
}

## To be copied in the UI
# mod_visualization_ui("visualization_1")

## To be copied in the server
# mod_visualization_server("visualization_1")
