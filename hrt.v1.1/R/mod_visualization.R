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
    #uiOutput(ns("viz")),
    #smDeviceBanner(message = "default"),
   # girafeOutput(ns("bulk_plot")),

   uiOutput(ns("geneVizTab"))

  )
}

#' visualization Server Functions
#'
#' @noRd
mod_visualization_server <- function(id){
  moduleServer( id, function(input, output, session, sel_g){
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


    #if(is.na(isolate(img_url))){

     # tags$script(
     #   'document.getElementById("sub_cellular-tab").classList.add("disabled")'
     # )
    #}

# Render visualization page



    # Render gene information

    pageview = NULL#reactive variable to render the page
    makeReactiveBinding("pageview")

    sel_gene = NULL
    makeReactiveBinding("sel_gene")#selected gene


    ## Conditional to render the main visualization page

    if(length(query) == 1) {


      output$sel_viz = renderUI(
        # Selectize Input. Function from utils
        display_selectize(
          id = ns("viz_selectize"),
          items = selectize_opt,
          selectize.title = title_text
        )
      )

      observeEvent(input$viz_selectize, {
        sel_gene = input$viz_selectize
        # gene info and tissue list(top10 detected transcript in each tissue)
        # top can be modified later
        top_ref = 10

        # Visualization page
        pageview = viz.helper(
          sel_gene = input$viz_selectize,
          top_ref = top_ref,
          specie = specie,
          ns = ns,
          output = output,
          input = input
        )



      }) #End of main Visualization page

    } else if (length(query) == 2) {#From here gene specific page will be rendered
      sel_gene = query[["gene"]]

      top_ref = 10

      output$sel_viz = renderUI(
        # Selectize Input. Function from utils
        display_selectize(
          id = ns("viz_selectize"),
          items = selectize_opt,
          seleted.gene = query[["gene"]],
          selectize.title = title_text
        )
      )


      updateSelectizeInput(
        session,
        inputId = "viz_selectize",
        choices = selectize_opt,
        selected = query[["gene"]],
        server = TRUE
      )


      pageview = viz.helper(
        sel_gene = query[["gene"]],
        top_ref = top_ref,
        specie = specie,
        ns = ns,
        output = output,
        input = input
      )

      observeEvent(input$viz_selectize, {
        sel_gene = input$viz_selectize

        updateSelectizeInput(
          session,
          inputId = "viz_selectize",
          choices = selectize_opt,
          selected = input$viz_selectize,
          server = TRUE
        )

        updateQueryString(
          str_glue("?page=human-housekeeping-gene/visualization&gene={input$viz_selectize}"),
          mode = "replace"
        )
#Function to render the visualization page
        pageview = viz.helper(
          sel_gene = sel_gene,
          top_ref = top_ref,
          specie = specie,
          ns = ns,
          output = output,
          input = input
        )

      })


    }

    isolate(pageview)


    return(
      list(gene = reactive({
      input$viz_selectize
    })
    ))


  })


}

## To be copied in the UI
# mod_visualization_ui("visualization_1")

## To be copied in the server
# mod_visualization_server("visualization_1")

