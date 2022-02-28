#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import wastdr
#' @import bs4Dash
#' @noRd
app_server <- function(input, output, session) {

  output$empty_map <- leaflet::renderLeaflet({
    leaflet::leaflet(width = 800, height = 600) %>%
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Basemap") %>%
      leaflet::addProviderTiles(
        "OpenStreetMap.Mapnik",
        group = "Basemap",
        options = leaflet::providerTileOptions(opacity = 0.35)
      ) %>%
      leaflet.extras::addFullscreenControl(pseudoFullscreen = TRUE) %>%
      leaflet::clearBounds()
  })

  output$total_emergences <- renderbs4ValueBox({
    bs4ValueBox(
      value = 1234,
      subtitle = "Total Emergences",
      color = "primary",
      icon = icon("home")
    )
  })

  output$processed <- renderbs4ValueBox({
    bs4ValueBox(
      value = 1234,
      subtitle = "Processed",
      color = "success",
      icon = icon("pencil-alt")
    )
  })

  output$missed <- renderbs4ValueBox({
    bs4ValueBox(
      value = 1234,
      subtitle = "Missed",
      color = "warning",
      icon = icon("question")
    )
  })



}

# TODO https://shiny.rstudio.com/reference/shiny/1.3.2/reactivePoll.html
# TODO https://shiny.rstudio.com/reference/shiny/1.3.2/reactiveFileReader.html
