#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # Modules
  # Visualization
  mainviz = mod_visualization_server("visualization_1")

  # Immunocytochemistry (ICC) and Immunofluorescence (IF)

  mod_icc_if_image_viewer_server(id = "icc_if_image_viewer_1", icc_data = mainviz$gene)

  #URL dispached

  source("inst/app/www/url_dispacher.R", local = T)

  output$ui_dispached = renderUI(
    get_url()
  )
}
