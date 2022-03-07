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
      1000, # Poll data file for changes every 1 second
      NULL, # across sessions
      fn_wastd_sites, # relative to project or Dockerfile workdir
      readRDS # using function readRDS
    )
  }

  # WAStD data cross session file reader
  fn_wastd_data <- here::here("inst/wastd_data.rds")
  wastd_data_all <- if (!fs::file_exists(fn_wastd_data)) {
    NULL
  } else {
    reactiveFileReader(
      1000, # Poll data file for changes every 1 second
      NULL, # across sessions
      fn_wastd_data, # relative to project or Dockerfile workdir
      readRDS # using function readRDS
    )
  }

  # Filtered by WAStD Area from select input or URL param "site"
  wastd_data <- reactive({
    req(wastd_data_all())
    req(input$sel_wastd_area)

    if (input$sel_wastd_area == "Select a program") {
      return(NULL)
    } else {
      wastd_data_all() %>%
        wastdr::filter_wastd_turtledata(input$sel_wastd_area)
    }
  })

  # WAMTRAM data cross session file reader
  fn_w2_data <- here::here("inst/w2_data.rds")
  w2_data <- if (!fs::file_exists(fn_w2_data)) {
    NULL
  } else {
    reactiveFileReader(
      1000, # Poll data file for changes every 1 second
      NULL, # across sessions
      fn_w2_data, # relative to project or Dockerfile workdir
      readRDS # using function readRDS
    )
  }

  # Manual data download ------------------------------------------------------#
  waitress_wastd_sites <- waiter::Waitress$new("#sites_dl_on", theme = "overlay", infinite = TRUE)
  waitress_wastd_data <- waiter::Waitress$new("#wastd_dl_on", theme = "overlay", infinite = TRUE)
  waitress_w2_data <- waiter::Waitress$new("#w2_dl_on", theme = "overlay", infinite = TRUE)

  observeEvent(input$action_dl_wastd_sites, {
    waitress_wastd_sites$start()
    toast(
      title = "Updating WAStD sites",
      body = "This will take a few seconds...",
      options = list(autohide = TRUE, icon = "fas fa-update", class = "info")
    )
    wastdr::download_wastd_sites(save=fn_wastd_sites, compress=FALSE)
    toast(
      title = "Finished WAStD sites download",
      body = "Reloading ...",
      options = list(autohide = TRUE, icon = "fas fa-tick", class = "success")
    )
    waitress_wastd_sites$close()
  })

  observeEvent(input$action_dl_wastd_data, {
    waitress_wastd_data$start()
    "Downloading WAStD Data" %>% wastdr::wastdr_msg_info()
    toast(
      title = "Updating WAStD data",
      body = "This will take a few minutes...",
      options = list(autohide = TRUE, icon = "fas fa-update", class = "info")
    )
    # max_records = 1000 limit used for DEV
    wastdr::download_wastd_turtledata(save=fn_wastd_data, compress=FALSE)
    toast(
      title = "Finished WAStD data download",
      body = "Reloading ...",
      options = list(autohide = TRUE, icon = "fas fa-tick", class = "success")
    )
    "Finished downloading WAStD Data" %>% wastdr::wastdr_msg_success()
    waitress_wastd_data$close()
  })

  observeEvent(input$action_dl_w2_data, {
    waitress_w2_data$start()
    "Downloading WAMTRAM Data" %>% wastdr::wastdr_msg_info()
    if (wastdr::w2_online() == FALSE) {
      toast(
        title = "WAMTRAM not accessible",
        body = "WAMTRAM is only accessible from the DBCA intranet with valid credentials.",
        options = list(autohide = TRUE, icon = "fas fa-exclamation-triangle")
      )
      "Failed downloading WAStD Data" %>% wastdr::wastdr_msg_warn()
    } else {
      toast(
        title = "Updating WAMTRAM data",
        body = "This will take about a minute...",
        options = list(autohide = TRUE, icon = "fas fa-update", class = "warning")
      )

      wastdr::download_w2_data(save = fn_w2_data, compress=FALSE)

      toast(
        title = "Finished WAMTRAM data download",
        body = "Reloading ...",
        options = list(autohide = TRUE, icon = "fas fa-tick", class = "success")
      )
      "Finished downloading WAMTRAM Data" %>% wastdr::wastdr_msg_success()
    }
    waitress_w2_data$close()
  })

  # Automatic data download ---------------------------------------------------#
  # This currently deactivated as it feels it gets in the user's way
  # Issue 1: it runs on session start (wastd data takes ca 40 mins to dl)
  # Issue 2: it resets the UI when done which interrupts the user's workflow
  #
  # A better approach is to create a separate Docker image to run the
  # long-running data ETL and download tasks on a scheduled cron job,
  # and mount the named volume read-write from both running containers.
  #
  # WAStD sites ---------------------------------------------------------------#
  # expire_wastd_sites <- reactiveTimer(1000 * 60 * 1) # Expire every 1 min

  # observe({
  #   # expire_wastd_sites()
  #   invalidateLater(1000 * 60 * 60 * 1)
  #
  #   waitress_wastd_sites$start()
  #   "[{Sys.time()}] Downloading WAStD Sites to {fn_wastd_sites}" %>%
  #     glue::glue() %>%
  #     wastdr::wastdr_msg_info()
  #
  #   sites <- wastdr::download_wastd_sites()
  #   saveRDS(sites, file = fn_wastd_sites)
  #   waitress_wastd_sites$close()
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
  # expire_wamtram_data <- reactiveTimer(1000 * 60 * 60) # Expire every 1h
  #
  # observe({
  #   expire_wamtram_data()
  #
  #   if (wastdr::w2_online() == FALSE) {
  #     "WAMTRAM not accessible. Need to run in DBCA intranet with credentials in env vars." %>%
  #       glue::glue() %>%
  #       wastdr::wastdr_msg_info()
  #   } else {
  #     "[{Sys.time()}] Downloading WAMTRAM Data to {fn_w2_data}" %>%
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

  # Data filters --------------------------------------------------------------#
  #
  # WAStD Localities
  output$flt_wastd <- renderUI({
    req(wastd_data_all())
    selectInput(
      "sel_wastd_area",
      label = NULL,
      choices = c(
        "Select a program",
        wastd_data_all()$areas$area_name,
        "All turtle programs",
        "Other"
      )
    )
  })

  # Make WAStD Area selections bookmarkable via query parameter "site"
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[["site"]])) {
      updateSelectInput(session, "sel_wastd_area", selected = query[["site"]])
    }
  })

  # WAMTRAM select Locations, Places, enter ObsID
  output$flt_w2_data_loc <- renderUI({
    req(w2_data())
    selectInput(
      "w2_loc",
      label = "W2 Location:",
      choices = c("", sort(unique(w2_data()$enc$location_code)))
    )
  })

  output$flt_w2_data_plc <- renderUI({
    req(w2_data())
    selectInput("w2_plc", label = "W2 Place:", choices = c("", sort(unique(w2_data()$enc$place_code))))
  })

  output$flt_w2_data_obs <- renderUI({
    textInput("w2_oid", label = "Observation ID:", value = "")
  })

  #
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

  output$vb_total_emergences <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h3("1234"),
      subtitle = "Total Emergences",
      color = "indigo",
      gradient = TRUE,
      icon = icon("arrow-up")
    )
  })

  output$vb_proc_mis <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h3("1234 / 500"),
      subtitle = "Processed / Missed",
      color = "lime",
      gradient = TRUE,
      icon = icon("pencil-alt")
    )
  })

  output$vb_new_res_rem <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h3("100 / 200 / 100"),
      subtitle = "New / Resighted / Remigrant",
      color = "orange",
      gradient = TRUE,
      icon = icon("eye")
    )
  })

  output$vb_nesting_success <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h3("50%"),
      subtitle = "Nesting sucess",
      color = "teal",
      gradient = TRUE,
      icon = icon("thumbs-up")
    )
  })

  output$wastd_dl_on <- renderbs4ValueBox({
    req(wastd_data())
    bs4ValueBox(
      value = tags$h4(wastd_data()$downloaded_on %>% lubridate::with_tz("Australia/Perth")),
      subtitle = "WAStD data downloaded",
      color = "navy",
      gradient = TRUE,
      icon = icon("download")
    )
  })

  output$w2_dl_on <- renderbs4ValueBox({
    req(w2_data())
    bs4ValueBox(
      value = tags$h4(w2_data()$downloaded_on %>% lubridate::with_tz("Australia/Perth")),
      subtitle = "WAMTRAM data downloaded",
      color = "navy",
      gradient = TRUE,
      icon = icon("download")
    )
  })

  output$sites_dl_on <- renderbs4ValueBox({
    req(wastd_sites())
    bs4ValueBox(
      value = tags$h4(wastd_sites()$downloaded_on %>% lubridate::with_tz("Australia/Perth")),
      subtitle = "WAStD Sites downloaded",
      color = "navy",
      gradient = TRUE,
      icon = icon("download")
    )
  })

  # W2 Places -----------------------------------------------------------------#
  output$vb_place_loc_rate <- renderbs4ValueBox({
    req(located_places())
    req(wastd_sites())

    x <- round(100 * (nrow(located_places()) / nrow(w2_data()$sites)), 0)

    bs4ValueBox(
      value = tags$h3(x),
      subtitle = "% W2 places located",
      color = dplyr::case_when(
        x < 70 ~ "maroon",
        x < 90 ~ "orange",
        x < 95 ~ "olive",
        TRUE ~ "lime"
      ),
      gradient = TRUE,
      icon = icon("chart-area")
    )
  })

  output$vb_place_homeless_rate <- renderbs4ValueBox({
    req(homeless_places())
    req(wastd_sites())

    x <- round(100 * (nrow(homeless_places()) / nrow(w2_data()$sites)), 0)

    bs4ValueBox(
      value = tags$h3(x),
      subtitle = "% W2 places need coords",
      color = dplyr::case_when(
        x > 20 ~ "maroon",
        x > 10 ~ "orange",
        x > 5 ~ "olive",
        TRUE ~ "lime"
      ),
      gradient = TRUE,
      icon = icon("chart-area")
    )
  })

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
  #
  selected_place <- reactive({
    req(w2_data())
    req(input$w2_plc)
    w2_data()$sites %>% dplyr::filter(code == input$w2_plc)
  })
  output$txt_w2_plc_lat <- renderText({
    req(selected_place())
    glue::glue("Lat {round(selected_place()$site_latitude, 5)}")
  })
  output$txt_w2_plc_lon <- renderText({
    req(selected_place())
    glue::glue("Lon {round(selected_place()$site_longitude, 5)}")
  })

  output$vb_w2_plc_lat <- renderbs4ValueBox({
    req(wastd_sites())
    bs4ValueBox(
      value = tags$h3(round(selected_place()$site_latitude, 5)),
      subtitle = "W2 Site Lat",
      color = "navy",
      gradient = TRUE,
      icon = icon("crosshairs")
    )
  })

  output$vb_w2_plc_lon <- renderbs4ValueBox({
    req(wastd_sites())
    bs4ValueBox(
      value = tags$h3(round(selected_place()$site_longitude, 5)),
      subtitle = "W2 Site Lon",
      color = "navy",
      gradient = TRUE,
      icon = icon("crosshairs")
    )
  })

  output$map_w2_obs <- leaflet::renderLeaflet({
    req(w2_data())
    req(wastd_sites())

    wastdr::map_wamtram(
      w2_data(),
      # location = input$w2_loc,
      place = input$w2_plc,
      obs_id = input$w2_oid,
      wa_sites = wastd_sites()$sites
      # l_height="calc(100vh - 80px)"
    )
  })

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
