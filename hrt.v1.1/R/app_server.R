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
  mod_visualization_server("visualization_1")

  #URL dispached

  source("inst/app/www/url_dispacher.R", local = T)

  output$ui_dispached = renderUI(
    get_url()
  )
}
