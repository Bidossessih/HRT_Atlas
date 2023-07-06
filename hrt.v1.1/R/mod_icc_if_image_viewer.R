#' icc_if_image_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @import shinyWidgets stringr shinyjs
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_icc_if_image_viewer_ui <- function(id){
  ns <- NS(id)
  div(class = "container",
      div(class = "row",
        div(id = "channel_container", class = "col-2",

            #textOutput(ns("res")),

            checkboxGroupButtons(
              #blue_red_green_yellow
              inputId = ns("Id056"),
              label = "Toggle channels",
              choices = c("Nucleus", "Microtubules", "Target protein", "ER"),
              direction = "vertical",
              selected = c("Nucleus", "Target protein"),
              size = "lg",
              #justified = TRUE,
              #individual = TRUE
            )
            ),
        div(id = ns("icc_img_id"), content = "icc_container", class = "col-10", style = "background: black",

            uiOutput(ns("main_img"))
            )
      ))
}

#' icc_if_image_viewer Server Functions
#'
#' @noRd
mod_icc_if_image_viewer_server <- function(id, icc_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$res <- renderText({ input$Id056 })

    observeEvent(input$Id056, {
      print("Input")

      print(input$Id056)

    })

    output$chosen_icon <- renderPrint(input$Id056)


    img_url = NULL

    makeReactiveBinding("img_url")

    # Define the colors of button...

    col_tp = NULL #target protein col
    col_ncl = NULL # nucleus col
    col_mtb = NULL # microtubule col
    col_er = NULL # ER col

    makeReactiveBinding("col_tp")
    makeReactiveBinding("col_ncl")
    makeReactiveBinding("col_mtb")
    makeReactiveBinding("col_er")


    observe({

      input$Id056

      #print("From module 2")
      #print(icc_data())

      icc_image = gene2iccImg(gene = isolate(icc_data()))


      img_url = icc_image$imageUrl[1]




      output$main_img = renderUI(
      tagList(
        icc_viewer(icc_data = icc_image))
      )


      # check channel color
      tp = NULL #target protein
      ncl = NULL # nucleus
      mtb = NULL # microtubule
      er = NULL # ER

      if ("Target protein" %in% input$Id056) {
        tp = "green_"
        col_tp = "success"
      }

      if ("Nucleus" %in% input$Id056) {
        ncl = "blue_"
        col_ncl = "primary"
      }

      if ("Microtubules" %in% input$Id056) {
        mtb = "red_"
        col_mtb = "danger"
      }

      if ("ER" %in% input$Id056) {
        er = "yellow"
        col_er = "warning"
      }

      col = NULL

      makeReactiveBinding("col")

      #col = str_glue("{ncl}{mtb}{tp}{er}")
      #clean string
      #col = gsub("_{1}$", "", col)

      col = str_c(input$Id056, collapse = "_") %>%
        str_replace(., "Nucleus", "blue") %>%
        str_replace(., "Microtubules", "red") %>%
        str_replace(., "Target protein", "green") %>%
        str_replace(., "ER", "yellow")


      print("Col")
      print(input$Id056)
      print(col)

      # define color based on input$Id056 value
      icc_image$imageUrl = str_replace(icc_image$imageUrl, "blue_red_green", col)


      shinyjs::onclick("Id056", shinyjs::html("time", date()))
      ### Return button color

      observe({
        if ("ER" %in% input$Id056) {
          shinyjs::inlineCSS(
            'input[value="ER"] + label {
              background-color: #ffc107 !important;
            }'
          )
        }
      })


      observeEvent(input$Id056, {
        updateCheckboxGroupButtons(
          session = session,
          inputId = "Id056",
          selected = input$Id056
        )
      }, ignoreNULL = TRUE, ignoreInit = TRUE)


      return(list(
        button_color = list(
          col_tp = col_tp,
          col_ncl = col_ncl,
          col_mtb = col_mtb,
          col_er = col_er
        )
      ))



    })

  })
}

## To be copied in the UI
# mod_icc_if_image_viewer_ui("icc_if_image_viewer_1")

## To be copied in the server
# mod_icc_if_image_viewer_server("icc_if_image_viewer_1")
