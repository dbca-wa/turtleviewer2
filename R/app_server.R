#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import wastdr
#' @import bs4Dash
#' @noRd
app_server <- function(input, output, session) {

  # Data ----------------------------------------------------------------------#
  # WAStD areas cross session file reader
  wastd_sites <- reactiveFileReader(
    1000 * 60, # Read data file every 60 seconds
    NULL, # Across sessions
    here::here("inst/app/media/wastd_sites.rds"), # from folder "inst/app/media"
    readRDS # using function readRDS
  )

  # WAStD data cross session file reader
  wastd_data <- reactiveFileReader(
    1000 * 60, # Read data file every 60 seconds
    NULL, # Across sessions
    here::here("inst/app/media/wastd_data.rds"), # from folder "inst/app/media"
    readRDS # using function readRDS
  )

  expire_saved_data <- reactiveTimer(1000 * 10)

  observe({
    expire_saved_data()

    'Downloading fresh data to {here::here("inst/app/media")}' %>%
      glue::glue() %>%
      wastdr::wastdr_msg_info()

    sites <- download_wastd_sites()
    saveRDS(sites, file = here::here("inst/app/media/wastd_sites.rds"))

    # Replace with download_wastd_turtledata() every 2h at most
    e <- new.env()
    wastd_data <- data("wastd_data", package = "wastdr", envir = e)
    saveRDS(e$wastd_data, file = here::here("inst/app/media/wastd_data.rds"))

    'Data saved locally to folder {here::here("inst/app/media")}.' %>%
      glue::glue() %>%
      wastdr::wastdr_msg_success()
  })

  # Outputs -------------------------------------------------------------------#
  #
  output$empty_map <- leaflet::renderLeaflet({
    x <- wastd_data()

    if (is.null(x)) {
      leaflet::leaflet(width = 800, height = 600) %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Basemap") %>%
        leaflet::addProviderTiles(
          "OpenStreetMap.Mapnik",
          group = "Basemap",
          options = leaflet::providerTileOptions(opacity = 0.35)
        ) %>%
        leaflet.extras::addFullscreenControl(pseudoFullscreen = TRUE) %>%
        leaflet::clearBounds()
    } else {
      wastdr::map_wastd(x)
    }
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

  output$wastd_dl_on <- renderbs4ValueBox({
    bs4ValueBox(
      value = wastd_data()$downloaded_on,
      subtitle = "WAStD data downloaded",
      color = "success",
      icon = icon("download")
    )
  })

  # /outputs ------------------------------------------------------------------#
}

# TODO https://shiny.rstudio.com/reference/shiny/1.3.2/reactivePoll.html
# TODO https://shiny.rstudio.com/reference/shiny/1.3.2/reactiveFileReader.html
