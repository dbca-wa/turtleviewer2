#' core UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_core_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList()
}

#' core Server Functions
#'
#' @noRd
mod_core_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_core_ui("core_1")

## To be copied in the server
# mod_core_server("core_1")
