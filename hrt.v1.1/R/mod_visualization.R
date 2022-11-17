#' visualization UI Function
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import stringr
#' @importFrom shiny NS tagList
#' @importFrom dplyr %>%
#' @import ggiraph
#' @import plotly
#' @import shinyWidgets
#'
#'
mod_visualization_ui <- function(id){
  ns <- NS(id)
  tags$head(
    tags$script('$(document).on("shiny:connected", function(e) {
                                    var w = window.innerWidth;
                                    var h = window.innerHeight;
                                    var d =  document.getElementById("visualization_1-ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("visualization_1-pltChange", obj);
                                });
                                $(window).resize(function(e) {
                                    var w = $(this).width();
                                    var h = $(this).height();
                                    var d =  document.getElementById("visualization_1-ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("visualization_1-pltChange", obj);
                                })')
  )
  div(id=ns("ppitest"), style="padding:25px",

    uiOutput(ns("sel_viz")),
    uiOutput(ns("viz")),
   # girafeOutput(ns("bulk_plot")),
    plotlyOutput(ns("bulk_violinplot"))
  )
}

#' visualization Server Functions
#'
#' @noRd
mod_visualization_server <- function(id){
  moduleServer( id, function(input, output, session, sel_trans){
    ns <- session$ns
    options(warn = -1)
    query = isolate(getQueryString())

    # get specie from url
    query["page"] %>%
      str_remove("-housekeeping-gene/visualization.*$") %>%
      str_to_sentence() -> specie


# Selectize information

    # OPtions.Fetch only HK genes of a specific specie and gene info
    selectize_opt = get_HK_genes(spec=specie)


    #Title text

    title_text = str_glue(
      "Select a {specie} housekeeping gene to show its transcript
    expression in different tissues/cells"
    )

# Render visualization page

    output$sel_viz = renderUI(
      # Selectize Input. Function from utils
      display_selectize(
        id = ns("viz_selectize"),
        items = selectize_opt,
        selectize.title = title_text
      )
    )



    # Render gene information

    observeEvent(input$viz_selectize, {
      sel_gene = input$viz_selectize

      #sel_gene = "EEF2"

      # gene info and tissue list(top10 detected transcript in each tissue)
      # top can be modified later
      top_ref = 10
      gene_info = get_gene_info(spec = specie, gene = sel_gene, top = top_ref)

      gene.name = gene_info$Name
      gene.description = gene_info$Summary
      # transcript list order by lenght of tissue for which they have been recommended
      # See get_gene_info() definition
      transcript = names(gene_info$default_tissue_norm)

      gene.other.names = gene_info$Synonym
      ortholog = gene_info$Ortholog_symbol


      ### Group button to display transcript information and plot

      trans_button = radioGroupButtons(
          inputId = ns("trans_sel"),
          label = "Ensembl transcript id:",
          choices = transcript,
          direction = "vertical"
        )


      #print(paste("Gene synonym ", str(gene.other.names)))
      output$viz = renderUI(tagList(
        # Gene and transcript information
        hk_transcript_desc(
          specie = specie,
          gene.symbol = input$viz_selectize,
          gene.name = gene.name,
          gene.description = gene.description,
          tissue_info_id = ns("ref_tissue_id"),
          gene.other.names = gene.other.names,
          ortholog = ortholog,
          trans_selectize = trans_button
        )


      ))

    # Create a reactive valor of the selected transcript
    ### This value will be updated when the user click on the transcript id from the
    #visualization table(Gene information). See trans_button value above

    sel_transcript_id = NULL
    makeReactiveBinding("sel_transcript_id")

    #tissue_info = NULL
    #makeReactiveBinding("tissue_info")

    observeEvent(input$trans_sel != "", {
      print("It is working,...")
      sel_transcript_id <<- input$trans_sel

    })



    observe({
      #print("Loading...")
      #Wait for the loading of transcript's Group button

      if(!is.null(sel_transcript_id)){

        tissue_info = gene_info$default_tissue_norm
        print(sel_transcript_id)

        removeUI(
          selector = paste0("#", ns("ref-tissue-id"))
        )


        if(all(is.na(tissue_info[[sel_transcript_id]]))){
          tissue_title = str_glue("{sel_transcript_id} is not among the top{top_ref}
                                  candicate reference transcripts of any tissue.")

          tissue_res = NULL
        }else{
          tissue_title = str_glue("{sel_transcript_id} has been ranked as a top{top_ref}
                                  candicate reference transcripts of the following tissue.")
          # Tissue recommended
          tissue_res = tissue_info[[sel_transcript_id]]

          tis = c()

          for(i in 1:length(tissue_res)){
            tis0 = HTML(str_glue("<a href='/?page={str_to_lower(specie)}-housekeeping-gene&tissue={tissue_res[i]}'>
                                 <span class='badge bg-secondary'>{tissue_res[i]}</span></a>"))

            tis = c(tis, tis0)
          }

          tissue_res = HTML(str_c(tis, collapse = "\t"))

        }




        insertUI(
          selector = paste0("#", ns("ref_tissue_id")),
          where = "beforeEnd",
          ui = div(id = ns("ref-tissue-id"),
                   em(tissue_title),
                   br(),
                   br(),
                   div(tissue_res))
        )

      }

    })




    output$bulk_violinplot <- renderPlotly({
        p = plot_ly(
          loadDataTrans(ens_l=sel_transcript_id),
          y = ~ log_rpkm,
          x = ~ cell_type,
          type = "violin",
          hoverinfo = "none",
          split = ~ cell_type,
          box = list(visible = T),
          meanline = list(visible = T),
          textfont = list(color = 'red', size = 10)
        ) %>%
          layout(title = paste0("Expression level of ", sel_transcript_id)) %>%
          layout(
            showlegend = FALSE,
            xaxis = list(
              title = "",
              showticklabels = TRUE,
              tickangle = 60
            ),
            yaxis = list(title = "Normalized Expression \n (log2 RPKM)")
          ) %>%
          config(
            displayModeBar = TRUE,
            modeBarButtonsToRemove = c(
              'zoom',
              'pan',
              'select',
              'zoomIn',
              'zoomOut',
              'autoScale',
              'hoverClosestCartesian',
              'hoverCompareCartesian'
            )
          )

        suppressWarnings(p)


      })

    }) #End of plotting


  })
}

## To be copied in the UI
# mod_visualization_ui("visualization_1")

## To be copied in the server
# mod_visualization_server("visualization_1")

