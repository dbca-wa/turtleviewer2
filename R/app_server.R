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
  # TODO https://stackoverflow.com/a/51108534/2813717
  fn_wastd_sites <- here::here("inst/wastd_sites.rds")
  wastd_sites <- if (!fs::file_exists(fn_wastd_sites)) {
    NULL
  } else {
    reactiveFileReader(
      1000, # Poll data file for changes every 10 seconds
      NULL, # Across sessions
      fn_wastd_sites, # relative to Dockerfile's workdir
      readRDS # using function readRDS
    )
  }

  # WAStD data cross session file reader
  fn_wastd_data <- here::here("inst/wastd_data.rds")
  wastd_data <- if (!fs::file_exists(fn_wastd_data)) {
    NULL
  } else {
    reactiveFileReader(
      1000, # Poll data file for changes every 10 seconds
      NULL, # Across sessions
      fn_wastd_data,
      readRDS # using function readRDS
    )
  }

  # WAMTRAM data cross session file reader
  fn_w2_data <- here::here("inst/w2_data.rds")
  w2_data <- if (!fs::file_exists(fn_w2_data)) {
    NULL
  } else {
    reactiveFileReader(
      1000, # Poll data file for changes every 10 seconds
      NULL, # Across sessions
      fn_w2_data,
      readRDS # using function readRDS
    )
  }

  # Manual data download ------------------------------------------------------#
  observeEvent(input$action_dl_wastd_sites, {
    toast(
      title = "Updating WAStD sites",
      body = "This will take a few seconds...",
      options = list(autohide = TRUE, icon = "fas fa-update", class="info")
    )
    sites <- wastdr::download_wastd_sites()
    saveRDS(sites, file = fn_wastd_sites)
    toast(
      title = "Finished WAStD sites download",
      subtitle = "Reloading ...",
      options = list(autohide = TRUE, icon = "fas fa-tick", class="success")
    )
  })

  observeEvent(input$action_dl_wastd_data, {
    toast(
      title = "Updating WAStD data",
      body = "This will take a few minutes...",
      options = list(autohide = TRUE, icon = "fas fa-update", class="info")
    )
    # TODO remove max_records limit used for DEV
    wastd_data <- wastdr::download_wastd_turtledata(max_records = 1000)
    saveRDS(wastd_data, file = fn_wastd_data)
    toast(
      title = "Finished WAStD data download",
      subtitle = "Reloading ...",
      options = list(autohide = TRUE, icon = "fas fa-tick", class="success")
    )
  })

  observeEvent(input$action_dl_w2_data, {
    if (wastdr::w2_online() == FALSE) {
      toast(
        title = "WAMTRAM not accessible",
        body= "WAMTRAM is only accessible from the DBCA intranet with valid credentials.",
        options = list(autohide = TRUE, icon = "fas fa-exclamation-triangle")
      )
        } else {
          toast(
            title = "Updating WAMTRAM data",
            body = "This will take about a minute...",
            options = list(autohide = TRUE, icon = "fas fa-update", class = "warning")
          )

          w2_data <- wastdr::download_w2_data(save = fn_w2_data)

          toast(
            title = "Finished WAMTRAM data download",
            subtitle = "Reloading ...",
            options = list(autohide = TRUE, icon = "fas fa-tick", class="success")
          )
        }
  })

  # Automatic data download ---------------------------------------------------#
  # WAStD sites ---------------------------------------------------------------#
  # expire_wastd_sites <- reactiveTimer(1000 * 60 * 10) # Expire every 10 min
  #
  # observe({
  #   expire_wastd_sites()
  #
  #   "Downloading WAStD Sites to {fn_wastd_sites}" %>%
  #     glue::glue() %>%
  #     wastdr::wastdr_msg_info()
  #
  #   sites <- wastdr::download_wastd_sites()
  #   saveRDS(sites, file = fn_wastd_sites)
  # })

  # WAStD data ----------------------------------------------------------------#
  # expire_wastd_data <- reactiveTimer(1000 * 60 * 60 * 2) # Expire every 2h
  #
  # observe({
  #   expire_wastd_data()
  #
  #   "Downloading WAStD Data to {fn_wastd_data}" %>%
  #     glue::glue() %>%
  #     wastdr::wastdr_msg_info()
  #
  #   # wastd_data <- wastdr::download_wastd_turtledata(
  #   #   max_records = 1000 # TODO: drop limit for prod
  #   # )
  #   # saveRDS(wastd_data, file = fn_wastd_data)
  #
  #
  #   "WAStD Data saved locally to folder {fn_wastd_data}." %>%
  #     glue::glue() %>%
  #     wastdr::wastdr_msg_success()
  # })

  # WAMTRAM data --------------------------------------------------------------#
  # expire_wamtram_data <- reactiveTimer(1000 * 60 * 10) # Expire every 10 min
  #
  # observe({
  #   expire_wamtram_data()
  #
  #   if (wastdr::w2_online() == FALSE) {
  #     "WAMTRAM not accessible. Need to run in DBCA intranet with credentials in env vars." %>%
  #       glue::glue() %>%
  #       wastdr::wastdr_msg_info()
  #   } else {
  #     "Downloading WAMTRAM Data to {fn_w2_data}" %>%
  #       glue::glue() %>%
  #       wastdr::wastdr_msg_info()
  #
  #     w2_data <- wastdr::download_w2_data(save = fn_w2_data)
  #
  #     "WAMTRAM Data saved locally to folder {fn_w2_data}." %>%
  #       glue::glue() %>%
  #       wastdr::wastdr_msg_success()
  #   }
  # })

  # Processed data ------------------------------------------------------------#
  locations <- reactive({
    req(w2_data())
    c("", sort(unique(w2_data()$enc$location_code)))
  })

  places <- reactive({
    req(w2_data())
      c("", sort(unique(w2_data()$enc$place_code)))

  })

  # sites_by_pc <- reactive({
  #   if (is.null(sites)) {NULL} else {
  #     sites() %>% tidyr::separate_rows(w2_place_code)
  #   }
  # })

  enc_by_sites <- reactive({
    req(w2_data())
      w2_data()$enc %>%
        dplyr::group_by(place_code) %>%
        dplyr::tally(name = "observations") %>%
        dplyr::ungroup()

  })

  homeless_places <- reactive({
    req(w2_data())
    req(enc_by_sites())

      w2_data()$sites %>%
        dplyr::filter(is.na(site_latitude)) %>%
        dplyr::mutate(
          search_areas_at = glue::glue(
            '<a href="https://wastd.dbca.wa.gov.au/admin/observations/area/?q={prefix}"',
            ' target="_" rel="external"><strong>{prefix}</strong></a>'
          ),
          .before = code
        ) %>%
        dplyr::select(-site_datum, site_longitude, site_latitude) %>%
        dplyr::left_join(enc_by_sites(), by = c("code" = "place_code")) %>%
        dplyr::filter(observations > 0) %>%
        dplyr::arrange(-observations) %>%
        janitor::clean_names(case = "title")

  })

  located_places <- reactive({
    req(w2_data())
    req(enc_by_sites())
      w2_data()$sites %>%
        dplyr::filter(!is.na(site_latitude)) %>%
        dplyr::mutate(
          search_areas_at = glue::glue(
            '<a href="https://wastd.dbca.wa.gov.au/admin/observations/area/?q={prefix}"',
            ' target="_" rel="external"><strong>{prefix}</strong></a>'
          ),
          .before = code
        ) %>%
        dplyr::select(-site_datum, site_longitude, site_latitude) %>%
        # dplyr::left_join(sites_by_pc, by=c("code"="w2_location_code")) %>%
        dplyr::left_join(enc_by_sites(), by = c("code" = "place_code")) %>%
        dplyr::filter(observations > 0) %>%
        dplyr::arrange(-observations) %>%
        janitor::clean_names(case = "title")

  })

  invalid_coords <- reactive({
    req(w2_data())
      w2_data()$enc %>%
        dplyr::filter(
          longitude < -180 |
            longitude > 180 |
            latitude < -90 |
            latitude > 90 |
            is.na(latitude) |
            is.na(longitude)
        )

  })

  unlikely_coords <- reactive({
    req(w2_data())
      w2_data()$enc %>%
        dplyr::filter(
          longitude < 100 |
            longitude > 150 |
            latitude < -45 |
            latitude > 0
        )
  })

  w2_obs_wastd_sites <- reactive({
    req(w2_data())
    req(sites())

      w2_data()$enc %>%
        dplyr::filter(
          longitude > -180, longitude < 180, latitude > -90, latitude < 90
        ) %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
        sf::st_join(sites(), left = TRUE)

  })

  # Outputs -------------------------------------------------------------------#
  #
  output$wastd_map <- leaflet::renderLeaflet({
    req(wastd_data())
    wastdr::map_wastd(wastd_data(), cluster = TRUE)

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
    req(wastd_data())
    bs4ValueBox(
      value = wastd_data()$downloaded_on,
      subtitle = "WAStD data downloaded",
      color = "info",
      gradient=TRUE,
      icon = icon("download")
    )
  })

  output$w2_dl_on <- renderbs4ValueBox({
    req(w2_data())
    bs4ValueBox(
      value = w2_data()$downloaded_on,
      subtitle = "WAMTRAM data downloaded",
      color = "info",
      gradient=TRUE,
      icon = icon("download")
    )
  })

  output$sites_dl_on <- renderbs4ValueBox({
    req(wastd_sites())
    bs4ValueBox(
      value = wastd_sites()$downloaded_on,
      subtitle = "WAStD Sites downloaded",
      color = "info",
      gradient=TRUE,
      icon = icon("download")
    )
  })

  # W2 Places -----------------------------------------------------------------#
  output$w2_places_map <- leaflet::renderLeaflet({
    req(wastd_sites())
    req(w2_data())

    wastdr::map_wastd_wamtram_sites(
      wastd_sites()$localities, wastd_sites()$sites, w2_data()$sites
    )
  })

  output$located_places <- reactable::renderReactable({
    req(located_places())
    reactable::reactable(
      located_places(),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  output$homeless_places <- reactable::renderReactable({
    req(homeless_places())
    reactable::reactable(
      homeless_places(),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  # W2 Observations -----------------------------------------------------------#
  output$impossible_coords <- reactable::renderReactable({
    req(invalid_coords())
    reactable::reactable(
      invalid_coords(),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  output$unlikely_coords <- reactable::renderReactable({
    req(unlikely_coords())
    reactable::reactable(
      unlikely_coords(),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  # /outputs ------------------------------------------------------------------#
}

# TODO https://shiny.rstudio.com/reference/shiny/1.3.2/reactivePoll.html
# TODO https://shiny.rstudio.com/reference/shiny/1.3.2/reactiveFileReader.html
