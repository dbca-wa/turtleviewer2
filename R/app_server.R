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
    1000 * 10, # Poll data file for changes every 10 seconds
    NULL, # Across sessions
    here::here("inst/app/media/wastd_data.rds"), # from folder "inst/app/media"
    readRDS # using function readRDS
  )

  # WAMTRAM data cross session file reader
  w2_data <- reactiveFileReader(
    1000 * 10, # Poll data file for changes every 10 seconds
    NULL, # Across sessions
    here::here("inst/app/media/w2_data.rds"), # from folder "inst/app/media"
    readRDS # using function readRDS
  )

  # WAStD sites ---------------------------------------------------------------#
  expire_wastd_sites <- reactiveTimer(1000 * 60) # Expire every 60s

  observe({
    expire_wastd_sites()

    'Downloading WAStD Sites to {here::here("inst/app/media")}' %>%
      glue::glue() %>%
      wastdr::wastdr_msg_info()

    sites <- wastdr::download_wastd_sites()
    saveRDS(sites, file = here::here("inst/app/media/wastd_sites.rds"))
  })

  # WAStD data ----------------------------------------------------------------#
  expire_wastd_data <- reactiveTimer(1000 * 60 * 60 * 2) # Expire every 2h

  observe({
    expire_wastd_data()

    'Downloading WAStD Data to {here::here("inst/app/media")}' %>%
      glue::glue() %>%
      wastdr::wastdr_msg_info()

    wastd_data <- wastdr::download_wastd_turtledata(
      max_records = 1000 # TODO: drop limit for prod
    )
    saveRDS(wastd_data, file = here::here("inst/app/media/wastd_data.rds"))


    'WAStD Data saved locally to folder {here::here("inst/app/media")}.' %>%
      glue::glue() %>%
      wastdr::wastdr_msg_success()
  })

  # WAMTRAM data ----------------------------------------------------------------#
  expire_wamtram_data <- reactiveTimer(1000 * 60 * 10) # Expire every 10 min

  observe({
    expire_wamtram_data()

    'Downloading WAMTRAM Data to {here::here("inst/app/media")}' %>%
      glue::glue() %>%
      wastdr::wastdr_msg_info()

    w2_data <- wastdr::download_w2_data(
      save = here::here("inst/app/media/w2_data.rds")
    )


    'WAMTRAM Data saved locally to folder {here::here("inst/app/media")}.' %>%
      glue::glue() %>%
      wastdr::wastdr_msg_success()
  })


  # Outputs -------------------------------------------------------------------#
  #
  output$wastd_map <- leaflet::renderLeaflet({
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

  output$w2_dl_on <- renderbs4ValueBox({
    bs4ValueBox(
      value = w2_data()$downloaded_on,
      subtitle = "WAMTRAM data downloaded",
      color = "success",
      icon = icon("download")
    )
  })

  # W2 Places
  output$w2_places_map <- leaflet::renderLeaflet({
    wastdr::map_wastd_wamtram_sites(wastd_data()$areas, wastd_sites(), w2_data()$sites)
  })

  # /outputs ------------------------------------------------------------------#
}

# TODO https://shiny.rstudio.com/reference/shiny/1.3.2/reactivePoll.html
# TODO https://shiny.rstudio.com/reference/shiny/1.3.2/reactiveFileReader.html
