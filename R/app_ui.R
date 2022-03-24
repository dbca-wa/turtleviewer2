#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList uiOutput icon fluidRow tabPanel
#' @import bs4Dash
#' @import wastdr
#' @noRd
app_ui <- function(request) {
  golem::favicon("www/favicon.ico")

  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    waiter::autoWaiter(),
    dashboardPage(
      fullscreen = TRUE,
      scrollToTop = TRUE,
      # preloader = list(html = waiter::spin_chasing_dots(), color = "#333e48"),
      header = dashboardHeader(
        title = dashboardBrand(
          title = "DBCA Marine Data",
          color = "primary",
          href = "https://turtledata.dbca.wa.gov.au/"
          # image = "www/logo.svg"
        )
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          id = "sidebar_menu",
          menuItem(
            text = "Marine Fauna Incidents",
            tabName = "tab_incidents",
            icon = icon("skull-crossbones")
          ),
          menuItem(
            text = "WA Turtle Monitoring",
            tabName = "tab_wastd",
            startExpanded = TRUE,
            icon = icon("pizza-slice"),
            menuSubItem(
              text = "Turtle Nesting",
              tabName = "tab_turtle_nesting",
              icon = icon("home")
            ),
            # menuSubItem(
            #   text = "Turtle Hatching",
            #   tabName = "tab_turtle_hatching",
            #   icon = icon("egg")
            # ),
            # menuSubItem(
            #   text = "Disturbance",
            #   tabName = "tab_disturbance",
            #   icon = icon("bolt")
            # ),
            menuSubItem(
              text = "WAMTRAM Places",
              tabName = "tab_w2_places",
              icon = icon("map")
            ),
            menuSubItem(
              text = "WAMTRAM Observations",
              tabName = "tab_w2_observations",
              icon = icon("pencil-alt")
            )
          )
        )
      ),
      body = dashboardBody(
        waiter::useWaitress(),
        tabItems(
          tabItem(
            tabName = "tab_incidents",
            fluidRow(
              bs4Card(
                uiOutput("flt_mfi_daterange", class = "col-3"),
                title = "Filter and export Marine Fauna Incident Data",
                width = 12,
                id = "box_mfi_filter"
              )
            ),
            fluidRow(
              tabBox(
                tabPanel(
                  title = "Incidents by taxonomic group and incident type",
                  reactable::reactableOutput("mfi_summary"),
                  icon = icon("chart-area")
                ),
                width = 6
              ),
              bs4Card(
                leaflet::leafletOutput("mfi_map"),
                title = "Marine Fauna Incident Map",
                footer = "View Fullscreen for a bigger map",
                width = 6,
                # label = "label",
                id = "box_mfi_map"
              )
            )
          ),
          tabItem(
            tabName = "tab_turtle_nesting",
            fluidRow(
              bs4Card(
                fluidRow(
                  bs4ValueBoxOutput("odk_imported", width = 3),
                  bs4ValueBoxOutput("wastd_dl_on", width = 3),
                  bs4ValueBoxOutput("sites_dl_on", width = 3),
                  bs4ValueBoxOutput("w2_dl_on", width = 3)
                ),
                fluidRow(
                  uiOutput("flt_wastd_areas", class = "btn btn-xs col-3"),
                  uiOutput("flt_wastd_seasons", class = "btn btn-xs col-3"),
                  uiOutput("btn_download_wastd", class = "btn btn-xs col-3")
                ),
                title = "Filter and export Turtle Monitoring Data",
                width = 12
              )
            ),
            # WAStD Data Map --------------------------------------------------#
            fluidRow(
              bs4Card(
                leaflet::leafletOutput("wastd_map"),
                title = "Turtle Monitoring Overview",
                width = 12,
                id = "box_all_wastd_data"
              )
            ), #---------------------------------------------------------------#
            fluidRow(
              tabBox(
                tabPanel(
                  title = "Emergences and Processing",
                  plotly::plotlyOutput("plt_emergences", height = "600px"),
                  reactable::reactableOutput("tbl_total_emergences_proc_mis_area"),
                  reactable::reactableOutput("tbl_total_emergences_proc_mis_site"),
                  icon = icon("arrow-up")
                ),
                tabPanel(
                  title = "Nesting Success",
                  plotly::plotlyOutput("plt_emergences_nesting_abs", height = "600px"),
                  plotly::plotlyOutput("plt_emergences_nesting_rel", height = "600px"),
                  reactable::reactableOutput("tbl_total_emergences_nesting_area_season"),
                  reactable::reactableOutput("tbl_total_emergences_nesting_area_day"),
                  icon = icon("thumbs-up")
                ),
                tabPanel(
                  title = "Recaptures",
                  shiny::tags$p("Coming soon"),
                  # reactable::reactableOutput("tbl_processed_turtles_new_res_rem")
                  # shiny::plotOutput("plt_processed_turtles_new_res_rem")
                  icon = icon("redo")
                ),

                tabPanel(
                  title = "Hatching and Emergence Success",
                  plotly::plotlyOutput("plt_hatching_success", height = "600px"),
                  plotly::plotlyOutput("plt_emergence_success", height = "600px"),
                  reactable::reactableOutput("tbl_hatching_success"),
                  icon = icon("thumbs-up")
                ),
                tabPanel(
                  title = "Hatchling Misorientation",
                  shiny::tags$p("Coming soon"),
                  # fan angles
                  icon = icon("lightbulb")
                ),
                tabPanel(
                  title = "Disturbance and Predation",
                  plotly::plotlyOutput("plt_dist", height = "600px"),
                  reactable::reactableOutput("tbl_dist"),
                  icon = icon("bolt")
                ),

                width = 12,
                maximizable = TRUE
              )
            )
          ),
          # WAMTRAM Places
          tabItem(
            tabName = "tab_w2_places",
            boxLayout(
              bs4ValueBoxOutput("vb_place_loc_rate", width = 3),
              bs4ValueBoxOutput("vb_place_homeless_rate", width = 3),
              type = "group"
            ),
            fluidRow(
              bs4Card(
                leaflet::leafletOutput("w2_places_map"),
                title = "WAMTRAM places and WAStD sites",
                # footer= "footer",
                width = 12,
                maximizable = TRUE,
                # label = "label",
                id = "box_w2_places_map"
              )
            ),
            fluidRow(
              tabBox(
                tabPanel(
                  title = "W2 places located in WAStD",
                  reactable::reactableOutput("located_places"),
                  icon = icon("map")
                ),
                tabPanel(
                  title = "W2 places without coords",
                  reactable::reactableOutput("homeless_places"),
                  icon = icon("marker")
                ),
                width = 12,
                maximizable = TRUE
              )
            )
          ),
          # WAMTRAM Observations
          tabItem(
            tabName = "tab_w2_observations",
            fluidRow(
              bs4ValueBoxOutput("vb_w2_total_obs", width = 3),
              bs4ValueBoxOutput("vb_w2_obs_in_sites", width = 3),
              bs4ValueBoxOutput("vb_w2_obs_no_sites", width = 3),
              bs4ValueBoxOutput("vb_w2_obs_pct_in_sites", width = 3)
            ),
            fluidRow(
              bs4Card(
                uiOutput("flt_w2_data_loc", class = "m-2"),
                uiOutput("flt_w2_data_plc", class = "m-2"),
                uiOutput("flt_w2_data_obs", class = "m-2"),
                bs4ValueBoxOutput("vb_w2_plc_lat", width = 12),
                bs4ValueBoxOutput("vb_w2_plc_lon", width = 12),
                title = "Filter WAMTRAM 2 Observations",
                width = 4
              ),
              bs4Card(
                title = "WAMTRAM 2 Observations",
                footer = "Compare coordinates from outliers' popups to Site Lat/Lon",
                leaflet::leafletOutput("map_w2_obs"),
                width = 8
              )
            ),
            fluidRow(
              tabBox(
                tabPanel(
                  title = "Impossible W2 coordinates",
                  reactable::reactableOutput("impossible_coords"),
                  icon = icon("exclamation")
                ),
                tabPanel(
                  title = "Unlikely W2 coordinates",
                  reactable::reactableOutput("unlikely_coords"),
                  icon = icon("question")
                ),
                tabPanel(
                  title = "Homeless W2 Observations",
                  reactable::reactableOutput("homeless_obs"),
                  icon = icon("question")
                ),
                tabPanel(
                  title = "Located W2 Observations",
                  reactable::reactableOutput("located_enc"),
                  icon = icon("crosshairs")
                ),
                width = 12,
                maximizable = TRUE
              )
            )
          )
        ) # /tabItems
      ),
      # controlbar = dashboardControlbar(),
      title = "WA Turtle Data"
    )
  ) # /tagList
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import bs4Dash
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  add_resource_path("media", app_sys("app/media"))

  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "WA Turtle Data")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert())
  )
}
