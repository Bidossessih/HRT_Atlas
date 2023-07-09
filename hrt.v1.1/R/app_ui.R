#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinyjs
#' @noRd
app_ui <- function(request) {
  #Use bootstrap 5
  fluidPage(useShinyjs(),
            uiOutput("ui_dispached"), theme = bs_theme(version = 5)
  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')

  )



  tags$head(
   # favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Housekeeping and Reference Transcript Atlas',
      all_files = TRUE
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    tags$head(

      HTML('


           ')

  ))
}
