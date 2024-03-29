#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactiveFileReader reactive observe observeEvent renderUI
#'  selectInput textInput req parseQueryString updateSelectInput renderText HTML
#'  dateRangeInput downloadButton downloadHandler renderPlot
#' @importFrom stringr str_replace_all
#' @import bs4Dash
#' @noRd
app_server <- function(input, output, session) {

  # Data ----------------------------------------------------------------------#
  # ODK Import timestamp
  fn_odk <- here::here("inst/odk.txt")
  odk_imported <- if (!fs::file_exists(fn_odk)) {
    NULL
  } else {
    reactiveFileReader(
      1000, # Poll data file for changes every 1 second
      NULL, # across sessions
      fn_odk, # relative to project or Dockerfile workdir
      readLines # using function readLines
    )
  }

  # ODK Data
  fn_odkc <- here::here("inst/odkc_data.rds")
  odkc_data <- if (!fs::file_exists(fn_odkc)) {
    NULL
  } else {
    reactiveFileReader(
      1000, # Poll data file for changes every 1 second
      NULL, # across sessions
      fn_odkc, # relative to project or Dockerfile workdir
      readRDS # using function readRDS
    )
  }

  # WAStD areas cross session file reader
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
    shiny::reactiveFileReader(
      1000, # Poll data file for changes every 1 second
      NULL, # across sessions
      fn_wastd_data, # relative to project or Dockerfile workdir
      readRDS # using function readRDS
    )
  }

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
    wastdr::download_wastd_sites(save = fn_wastd_sites, compress = FALSE)
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
    wastdr::download_wastd_turtledata(save = fn_wastd_data, compress = FALSE)
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

      wastdr::download_w2_data(save = fn_w2_data, compress = FALSE)

      toast(
        title = "Finished WAMTRAM data download",
        body = "Reloading ...",
        options = list(autohide = TRUE, icon = "fas fa-tick", class = "success")
      )
      "Finished downloading WAMTRAM Data" %>% wastdr::wastdr_msg_success()
    }
    waitress_w2_data$close()
  })


  # Data filter UI elements ---------------------------------------------------#
  #

  areas <- reactive({
    req(wastd_data_all())$areas$area_name
  })

  output$flt_wastd_areas <- renderUI({
    selectInput(
      "sel_wastd_area",
      label = "WA Turtle Monitoring Locations",
      choices = c("Select a program", areas(), "All turtle programs", "Other")
    )
  })

  seasons <- reactive({
    sort(unique(req(wastd_data())$animals$season))
  })

  output$flt_wastd_seasons <- renderUI({
    selectInput(
      "sel_wastd_seasons",
      label = "Turtle Nesting Seasons (FY start)",
      multiple = TRUE,
      selected = seasons(),
      choices = seasons()
    )
  })

  # Make WAStD Area selections bookmarkable via query parameter "site"
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[["site"]])) {
      updateSelectInput(session, "sel_wastd_area", selected = query[["site"]])
    }
  })

  # sites_by_pc <- reactive({tidyr::separate_rows(req(wastd_sites())$sites, w2_place_code)})

  # Filtered and derived WAStD data -------------------------------------------#
  # WAStD data filtered by WAStD Area from select input or URL param "site"
  wastd_data <- reactive({
    req(input$sel_wastd_area)

    if (req(input$sel_wastd_area) == "Select a program") {
      return(NULL)
    } else {
      req(wastd_data_all()) %>%
        wastdr::filter_wastd_turtledata(
          area_name = input$sel_wastd_area,
          seasons = input$sel_wastd_seasons
        )
    }
  })

  wastd_emergences_area <- reactive({
    req(wastd_data()) %>% wastdr::total_emergences_per_area_season_species()
  })

  wastd_emergences_site <- reactive({
    req(wastd_data()) %>% wastdr::total_emergences_per_site_season_species()
  })

  wastd_nesting_area_season <- reactive({
    req(wastd_data()) %>% wastdr::nesting_success_per_area_season_species()
  })

  wastd_nesting_area_day <- reactive({
    req(wastd_data()) %>% wastdr::nesting_success_per_area_day_species()
  })

  wastd_hatching_emergence_success_area <- reactive({
    req(wastd_data())$nest_excavations %>%
      wastdr::hatching_emergence_success_area()
  })

  wastd_sighting_area_season <- reactive({
    req(wastd_data()) %>%
      wastdr::sighting_status_per_area_season_species()
  })

  # wastd_sighting_site_season <- reactive({
  #   req(wastd_data()) %>%
  #     wastdr::sighting_status_per_site_season_species() %>%
  #     dplyr::mutate(
  #       species = stringr::str_to_sentence(species) %>%
  #         stringr::str_replace("-", " ")
  #     ) %>%
  #     janitor::clean_names(case = "sentence")
  # })

  # wastd_hatching_emergence_success_site <- reactive({
  #   req(wastd_data())$nest_excavations %>%
  #     wastdr::hatching_emergence_success_site()
  # })

  wastd_dist <- reactive({
    req(wastd_data())$nest_dist %>% wastdr::disturbance_by_season()
  })

  # Derived WAMTRAM data ------------------------------------------------------#
  enc_by_sites <- reactive({
    req(w2_data())$enc %>%
      dplyr::group_by(place_code) %>%
      dplyr::tally(name = "observations") %>%
      dplyr::ungroup()
  })

  homeless_places <- reactive({
    req(w2_data())$sites %>%
      dplyr::filter(is.na(site_latitude)) %>%
      dplyr::mutate(
        search_areas_at = glue::glue(
          '<a href="https://wastd.dbca.wa.gov.au/admin/observations/area/?q={prefix}"',
          ' target="_" rel="external"><strong>{prefix}</strong></a>'
        ),
        .before = code
      ) %>%
      dplyr::select(-site_datum, site_longitude, site_latitude) %>%
      dplyr::left_join(req(enc_by_sites()), by = c("code" = "place_code")) %>%
      dplyr::filter(observations > 0) %>%
      dplyr::arrange(-observations) %>%
      janitor::clean_names(case = "title")
  })

  located_places <- reactive({
    req(w2_data())$sites %>%
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
      dplyr::left_join(req(enc_by_sites()), by = c("code" = "place_code")) %>%
      dplyr::filter(observations > 0) %>%
      dplyr::arrange(-observations) %>%
      janitor::clean_names(case = "title")
  })

  invalid_coords <- reactive({
    req(w2_data())$enc %>%
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
    req(w2_data())$enc %>%
      dplyr::filter(
        longitude < 100 |
          longitude > 150 |
          latitude < -45 |
          latitude > 0
      )
  })

  w2_enc <- reactive({
    x <- req(w2_data())$enc %>%
      dplyr::filter(
        longitude > -180, longitude < 180, latitude > -90, latitude < 90
      )

    if (!is.null(input$w2_loc) && input$w2_loc != "") {
      x <- dplyr::filter(x, location_code == input$w2_loc)
    }

    if (!is.null(input$w2_plc) && input$w2_plc != "") {
      x <- dplyr::filter(x, place_code == input$w2_plc)
    }

    x
  })

  w2_obs_wastd_sites <- reactive({
    req(w2_enc()) %>%
      dplyr::filter(
        longitude > -180, longitude < 180, latitude > -90, latitude < 90
      ) %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
      sf::st_join(req(wastd_sites())$sites, left = TRUE)
  })

  nrow_w2_enc_total <- reactive({
    req(w2_enc()) %>% nrow()
  })

  nrow_w2_enc_in_sites <- reactive({
    req(w2_obs_wastd_sites()) %>%
      dplyr::filter(!is.na(site_name)) %>%
      nrow()
  })

  nrow_w2_enc_no_sites <- reactive({
    req(w2_obs_wastd_sites()) %>%
      dplyr::filter(is.na(site_name)) %>%
      nrow()
  })

  pct_w2_enc_in_sites <- reactive({
    round(100 * nrow_w2_enc_in_sites() / nrow_w2_enc_total(), 2)
  })
  # End derived WAMTRAM data --------------------------------------------------#

  # ---------------------------------------------------------------------------#
  # Outputs
  # ---------------------------------------------------------------------------#
  #
  # Tab WAStD - Incidents -----------------------------------------------------#
  output$flt_mfi_daterange <- renderUI({
    dateRangeInput("mfi_daterange",
      label = "Date range for Incidents",
      start = "2022-01-01",
      format = "yyyy-mm-dd",
      startview = "year",
      weekstart = 1,
      language = "en-AU"
      # end = Sys.Date() + 2
    )
  })

  mfi_data_all <- reactive({
    req(wastd_data_all())$animals %>%
      wastdr::filter_realspecies() %>%
      wastdr::filter_dead()
  })

  mfi_data <- reactive({
    if (is.null(input$mfi_daterange[1])) {
      req(wastd_data_all()$animals)
    } else {
      mfi_data_all() %>%
        dplyr::filter(
          calendar_date_awst >= input$mfi_daterange[1],
          calendar_date_awst <= input$mfi_daterange[2]
        )
    }
  })

  output$mfi_map <- leaflet::renderLeaflet({
    if (is.null(input$mfi_daterange[1])) {
      wastdr::leaflet_basemap()
    } else {
      req(mfi_data()) %>%
        wastdr::map_mwi(sites = req(wastd_sites())$sites, split_species = FALSE)
    }
  })

  output$mfi_summary <- reactable::renderReactable({
    req(mfi_data()) %>%
      dplyr::group_by(season, taxon, cause_of_death) %>%
      dplyr::tally() %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        `Season (FY start)` = season,
        `Taxonomic Group` = taxon,
        `Incident type` = cause_of_death,
        `Tally` = n
      ) %>%
      reactable::reactable(
        sortable = TRUE,
        filterable = TRUE,
        defaultColDef = reactable::colDef(html = TRUE)
      )
  })

  # Tab WAStD - Nesting -------------------------------------------------------#
  output$wastd_map <- leaflet::renderLeaflet({
    if (is.null(wastd_data())) {
      wastdr::leaflet_basemap()
    } else {
      wastdr::map_wastd(req(wastd_data()), cluster = TRUE)
    }
  })

  output$odk_imported <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h4(
        req(odk_imported()) %>%
          lubridate::parse_date_time(orders = "ymdHMS") %>%
          lubridate::with_tz("Australia/Perth")
      ),
      subtitle = "ODK imported to WAStD",
      color = "navy",
      gradient = TRUE,
      icon = icon("download")
    )
  })

  output$wastd_dl_on <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h4(req(wastd_data_all())$downloaded_on %>%
        lubridate::with_tz("Australia/Perth")),
      subtitle = "WAStD data updated",
      color = "navy",
      gradient = TRUE,
      icon = icon("download")
    )
  })

  output$w2_dl_on <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h4(req(w2_data())$downloaded_on %>% lubridate::with_tz("Australia/Perth")),
      subtitle = "WAMTRAM data updated",
      footer = HTML(
        '<button id="action_dl_w2_data" type="button" ',
        'class="btn action-button btn-xs btn-outline-warning ml-2">',
        "Refresh</button>"
      ),
      color = "navy",
      gradient = TRUE,
      icon = icon("download")
    )
  })

  output$sites_dl_on <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h4(req(wastd_sites())$downloaded_on %>% lubridate::with_tz("Australia/Perth")),
      subtitle = "WAStD sites updated",
      footer = HTML(
        '<button id="action_dl_wastd_sites" type="button" ',
        'class="btn action-button btn-xs btn-outline-warning ml-2">',
        "Refresh</button>"
      ),
      color = "navy",
      gradient = TRUE,
      icon = icon("download")
    )
  })

  # TabPanel Emergences and Processing ----------------------------------------#
  ggplot_wastd_emerg_area_season_species <- reactive({
    req(wastd_emergences_area()) %>%
      wastdr::ggplot_total_emergences_per_area_season_species()
  })

  output$plt_emergences <- plotly::renderPlotly({
    req(ggplot_wastd_emerg_area_season_species()) %>% plotly::ggplotly()
  })

  output$tbl_total_emergences_proc_mis_area <- reactable::renderReactable({
    reactable::reactable(
      req(wastd_emergences_area()),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  output$tbl_total_emergences_proc_mis_site <- reactable::renderReactable({
    reactable::reactable(
      req(wastd_emergences_site()),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  # TabPanel Nesting Success --------------------------------------------------#
  ggplot_wastd_emerg_nesting_abs <- reactive({
    req(wastd_nesting_area_season()) %>%
      wastdr::ggplot_nesting_success_per_area_season_species()
  })

  ggplot_wastd_emerg_nesting_rel <- reactive({
    req(wastd_nesting_area_season()) %>%
      wastdr::ggplot_nesting_success_per_area_season_species_pct()
  })

  output$plt_emergences_nesting_abs <- plotly::renderPlotly({
    req(ggplot_wastd_emerg_nesting_abs()) %>% plotly::ggplotly()
  })

  output$plt_emergences_nesting_rel <- plotly::renderPlotly({
    req(ggplot_wastd_emerg_nesting_rel()) %>% plotly::ggplotly()
  })

  output$tbl_total_emergences_nesting_area_season <- reactable::renderReactable({
    reactable::reactable(
      req(wastd_nesting_area_season()),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  output$tbl_total_emergences_nesting_area_day <- reactable::renderReactable({
    reactable::reactable(
      req(wastd_nesting_area_day()),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  # TabPanel Recaptures -------------------------------------------------------#
  # Emergences split by sighting status: na, new, resighting, remigrant
  ggplot_wastd_sighting_area_season <- reactive({
    req(wastd_sighting_area_season()) %>%
      wastdr::ggplot_sighting_status_per_area_season_species()
  })

  output$plt_sighting_area_season <- plotly::renderPlotly({
    req(ggplot_wastd_sighting_area_season()) %>% plotly::ggplotly()
  })

  output$tbl_sighting_area_season <- reactable::renderReactable({
    reactable::reactable(
      req(wastd_sighting_area_season()),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })
  # Same for site: wastd_sighting_site_season()

  #
  # internesting interval
  # clutch frequency
  # morphometrics
  # injuries
  #
  # tabPanel HS/ES ------------------------------------------------------------#
  # req(wastd_hatching_emergence_success_area())
  # req(wastd_hatching_emergence_success_site())


  output$map_nests <- leaflet::renderLeaflet({
    if (is.null(wastd_data())) {
      wastdr::leaflet_basemap()
    } else {
      wastdr::map_nests(req(wastd_data())$nest_tags)
    }
  })

  ggplot_hatching_success <- reactive({
    req(wastd_data()) %>% wastdr::ggplot_hatching_success()
  })

  output$plt_hatching_success <- plotly::renderPlotly({
    req(ggplot_hatching_success()) %>% plotly::ggplotly()
  })

  ggplot_emergence_success <- reactive({
    req(wastd_data()) %>% wastdr::ggplot_emergence_success()
  })

  output$plt_emergence_success <- plotly::renderPlotly({
    req(ggplot_emergence_success()) %>% plotly::ggplotly()
  })

  output$tbl_hatching_success <- reactable::renderReactable({
    reactable::reactable(
      req(wastd_hatching_emergence_success_area()),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  # tabPanel Disturbance ------------------------------------------------------#
  output$map_dist <- leaflet::renderLeaflet({
    if (is.null(wastd_data())) {
      wastdr::leaflet_basemap()
    } else {
      wastdr::map_dist(req(wastd_data())$nest_dist)
    }
  })

  output$plt_dist <- plotly::renderPlotly({
    req(wastd_data()) %>%
      wastdr::ggplot_disturbance_by_season() %>%
      plotly::ggplotly()
  })

  output$tbl_dist <- reactable::renderReactable({
    reactable::reactable(
      req(wastd_dist()),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  # Tab Hatchling Misorientation ----------------------------------------------#
  output$map_fanangles <- leaflet::renderLeaflet({
    if (is.null(wastd_data())) {
      wastdr::leaflet_basemap()
    } else {
      wastdr::map_fanangles(req(wastd_data()))
    }
  })

  ggplot_wastd_fans <- reactive({
    req(wastd_data()) %>% wastdr::ggplot_hatchling_misorientation()
  })

  output$plt_fanangles <- plotly::renderPlotly({
    req(ggplot_wastd_fans()) %>% plotly::ggplotly()
  })

  # Tab Surveys ---------------------------------------------------------------#
  output$plt_svy_heatmap <- renderPlot({
    req(wastd_data())$surveys %>% wastdr::survey_count_heatmap()
  })

  output$tbl_svy_season <- reactable::renderReactable({
    req(wastd_data())$surveys %>%
      wastdr::survey_season_stats() %>%
      reactable::reactable()
  })

  # Tab ODKC ------------------------------------------------------------------#
  coldef_img <- reactable::colDef(
    cell = function(value) {
      if (basename(value) != "NA") {
        img_src <- fs::path("media", basename(value))
        image <- tags$img(
          src = img_src,
          alt = glue::glue("Image {value} hosted at {img_src}"),
          width = 100,
          height = 100
        )
        tags$a(href = img_src, target = "_", image)
      } else {
        "No image"
      }
    }
  )

  tt_reactable <- . %>% reactable::reactable(
    searchable = TRUE,
    sortable = TRUE,
    filterable = TRUE,
    columns = list(
      datasheet_photo_datasheet_front = coldef_img,
      datasheet_photo_datasheet_rear = coldef_img,
      ft1_ft1_photo = coldef_img,
      ft2_ft2_photo = coldef_img,
      ft3_ft3_photo = coldef_img
    )
  )

  output$tbl_tt_rej <- reactable::renderReactable({
    req(odkc_data())$tt_fix %>% tt_reactable()
  })

  output$tbl_tt_imp <- reactable::renderReactable({
    req(odkc_data())$tt %>% tt_reactable()
  })

  # Tab WAMTRAM - Places ------------------------------------------------------#
  output$vb_place_loc_rate <- renderbs4ValueBox({
    x <- round(100 * (nrow(req(located_places())) / nrow(req(w2_data())$sites)), 0)
    bs4ValueBox(
      value = tags$h3(x),
      subtitle = "% W2 places located in WAStD",
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
    x <- round(100 * (nrow(req(homeless_places())) / nrow(req(w2_data())$sites)), 0)

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
    wastdr::map_wastd_wamtram_sites(
      req(wastd_sites())$areas,
      req(wastd_sites())$sites,
      req(w2_data())$sites
    )
  })

  output$located_places <- reactable::renderReactable({
    reactable::reactable(
      req(located_places()),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  output$homeless_places <- reactable::renderReactable({
    reactable::reactable(
      req(homeless_places()),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  # Tab WAMTRAM - Observations ------------------------------------------------#
  output$flt_w2_data_loc <- renderUI({
    selectInput(
      "w2_loc",
      label = "W2 Location:",
      choices = c("", sort(unique(req(w2_data())$enc$location_code)))
    )
  })

  output$flt_w2_data_plc <- renderUI({
    selectInput(
      "w2_plc",
      label = "W2 Place:",
      choices = c("", sort(unique(req(w2_data())$enc$place_code)))
    )
  })

  output$flt_w2_data_obs <- renderUI({
    textInput("w2_oid", label = "Observation ID:", value = "")
  })

  output$vb_w2_total_obs <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h3(req(nrow_w2_enc_total())),
      subtitle = "Total Obs",
      color = "info",
      gradient = TRUE,
      icon = icon("pencil-alt")
    )
  })

  output$vb_w2_obs_in_sites <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h3(req(nrow_w2_enc_in_sites())),
      subtitle = "Obs in sites",
      color = "info",
      gradient = TRUE,
      icon = icon("pencil-alt")
    )
  })

  output$vb_w2_obs_no_sites <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h3(req(nrow_w2_enc_no_sites())),
      subtitle = "Obs outside of sites",
      color = "warning",
      gradient = TRUE,
      icon = icon("question")
    )
  })

  output$vb_w2_obs_pct_in_sites <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$h3(req(pct_w2_enc_in_sites())),
      subtitle = "Pct in Sites",
      color = ifelse(pct_w2_enc_in_sites() > 95, "success", "warning"),
      gradient = TRUE,
      icon = icon("percent")
    )
  })

  selected_place <- reactive({
    req(input$w2_plc)
    req(w2_data())$sites %>% dplyr::filter(code == input$w2_plc)
  })

  output$vb_w2_plc_lat <- renderbs4ValueBox({
    lat <- round(req(selected_place())$site_latitude, 5)
    bs4ValueBox(
      value = tags$h4(lat),
      subtitle = "W2 Site Lat",
      color = "navy",
      gradient = TRUE,
      icon = icon("crosshairs")
    )
  })

  output$vb_w2_plc_lon <- renderbs4ValueBox({
    lon <- round(req(selected_place())$site_longitude, 5)
    bs4ValueBox(
      value = tags$h4(lon),
      subtitle = "W2 Site Lon",
      color = "navy",
      gradient = TRUE,
      icon = icon("crosshairs")
    )
  })

  output$map_w2_obs <- leaflet::renderLeaflet({
    if (
      (!is.null(input$w2_loc) && input$w2_loc != "") |
        (!is.null(input$w2_plc) && input$w2_plc != "") |
        input$w2_oid != ""
    ) {
      wastdr::map_wamtram(
        req(w2_data()),
        location = input$w2_loc,
        place = input$w2_plc,
        obs_id = input$w2_oid,
        wa_sites = req(wastd_sites())$sites
        # l_height="calc(100vh - 80px)"
      )
    } else {
      wastdr::leaflet_basemap()
    }
  })

  output$impossible_coords <- reactable::renderReactable({
    reactable::reactable(
      req(invalid_coords()),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  output$unlikely_coords <- reactable::renderReactable({
    reactable::reactable(
      req(unlikely_coords()),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  output$homeless_obs <- reactable::renderReactable({
    reactable::reactable(
      req(w2_enc()), # TODO %>% dplyr::filter(is.na(site_name)),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  output$located_enc <- reactable::renderReactable({
    reactable::reactable(
      req(w2_enc()), # TODO %>% dplyr::filter(!is.na(site_name)),
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })

  # Download all the goodies --------------------------------------------------#
  output$export_wastd <- downloadHandler(
    filename = function() {
      glue::glue(
        "{wastd_data()$downloaded_on} {input$sel_wastd_area} turtle data.zip"
      ) %>%
        stringr::str_replace_all(":", "-")
    },
    content = function(file) {
      out <- fs::path(tempdir(), "wastd_export")

      # The main show: WAStD data export
      req(wastd_data()) %>%
        wastdr::export_wastd_turtledata(outdir = out, zip = FALSE)

      # Derived data summaries
      req(wastd_emergences_area()) %>%
        readr::write_csv(
          file = fs::path(out, "emergences_per_area_season_species.csv")
        )

      req(wastd_emergences_site()) %>%
        readr::write_csv(
          file = fs::path(out, "emergences_per_site_season_species.csv")
        )

      req(wastd_nesting_area_season()) %>%
        readr::write_csv(
          file = fs::path(out, "nesting_per_area_season_species.csv")
        )

      req(wastd_nesting_area_day()) %>%
        readr::write_csv(
          file = fs::path(out, "nesting_per_area_day_species.csv")
        )

      # TODO enable once WAStD reconstruct_animal_names is proven correct
      req(wastd_sighting_area_season()) %>%
        readr::write_csv(
          file = fs::path(out, "recaptures_per_area_day_species.csv")
        )

      req(wastd_hatching_emergence_success_area()) %>%
        readr::write_csv(
          file = fs::path(out, "hatching_emergence_success.csv")
        )

      req(wastd_dist()) %>%
        readr::write_csv(
          file = fs::path(out, "disturbance_predation.csv")
        )

      # Figures: save ggplot / plotly (refactor ggplot/plotly as reactive)
      # plt_emergences
      # plt_emergences_nesting_abs
      # plt_emergences_nesting_rel
      # plt_dist


      utils::zip(file, fs::dir_ls(out))
    },
    contentType = "application/zip"
  )

  output$btn_download_wastd <- renderUI({
    shiny::need(wastd_data(), message = "Loading data...")

    downloadButton(
      "export_wastd",
      glue::glue("Download WAStD data"),
      class = "dl-data btn btn-danger",
      title = "Download currently selected WAStD data",
      icon = icon("download")
    )
  })
  # /outputs ------------------------------------------------------------------#
}
