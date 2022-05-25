#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList uiOutput plotOutput icon fluidRow tabPanel
#' @import bs4Dash
#' @import wastdr
#' @noRd
app_ui <- function(request) {
  golem::favicon("www/favicon.ico")
  map_footer <- paste(
    "Maximise map with fullscreen button top left.",
    "Toggle layers top right, zoom in to expand grouped markers.",
    "Click on markers to view popup with links to WAStD."
  )

  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    waiter::autoWaiter(),
    dashboardPage(
      title = "WA Turtle Data",
      fullscreen = TRUE,
      scrollToTop = TRUE,
      # preloader = list(html = waiter::spin_chasing_dots(), color = "#333e48"),
      header = dashboardHeader(
        title = dashboardBrand(
          title = "DBCA Marine Data",
          color = "primary",
          href = "https://turtledata.dbca.wa.gov.au/"
          # image = "www/logo-dbca.png"
        ),
        uiOutput("flt_wastd_areas", class = "btn btn-xs"),
        uiOutput("flt_wastd_seasons", class = "btn btn-xs"),
        uiOutput("btn_download_wastd", class = "btn btn-xs"),
        compact = TRUE,
        border = FALSE
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          id = "sidebar_menu",
          menuItem(
            text = "WA Turtle Monitoring",
            tabName = "tab_wastd",
            startExpanded = TRUE,
            icon = icon("pizza-slice"),
            menuSubItem(
              text = "Nesting",
              tabName = "tab_turtle_nesting",
              icon = icon("home")
            ),
            menuSubItem(
              text = "Emergence",
              tabName = "tab_turtle_hatch",
              icon = icon("thumbs-up")
            ),
            menuSubItem(
              text = "Misorientation",
              tabName = "tab_turtle_mis",
              icon = icon("lightbulb")
            ),
            menuSubItem(
              text = "Disturbance",
              tabName = "tab_turtle_dis",
              icon = icon("bolt")
            ),
            menuSubItem(
              text = "Surveys",
              tabName = "tab_turtle_svy",
              icon = icon("eye")
            ),
            menuSubItem(
              text = "ODKC",
              tabName = "tab_odkc",
              icon = icon("clipboard")
            )
          ),
          menuItem(
            text = "Legacy Tagging DB",
            tabName = "tab_incidents",
            icon = icon("tags"),
            menuSubItem(
              text = "Places",
              tabName = "tab_w2_places",
              icon = icon("map")
            ),
            menuSubItem(
              text = "Observations",
              tabName = "tab_w2_observations",
              icon = icon("pencil-alt")
            )
          ),
          menuItem(
            text = "Marine Fauna Incidents",
            icon = icon("skull-crossbones"),
            menuSubItem(
              text = "Incidents",
              tabName = "tab_incidents",
              icon = icon("map")
            )
          )
        )
      ),
      controlbar = bs4DashControlbar(
        tags$h3("Data currency", class = "mt-2 ml-2"),
        bs4ValueBoxOutput("odk_imported", width = 12),
        bs4ValueBoxOutput("wastd_dl_on", width = 12),
        bs4ValueBoxOutput("sites_dl_on", width = 12),
        bs4ValueBoxOutput("w2_dl_on", width = 12)
      ),
      body = dashboardBody(
        shinyWidgets::setBackgroundImage(src = "www/green_hatchling.jpg", shinydashboard = FALSE),
        waiter::useWaitress(),
        tabItems(
          tabItem(
            tabName = "tab_incidents",
            fluidRow(
              bs4Card(
                title = "Marine Fauna Incidents Dashboard",
                tags$p(
                  paste(
                    "A dedicated MFI dashboard could look similar to this tab.",
                    "It could be hosted separately to the turtle data dashboard.",
                    "Note that only the date range filter below applies to this tab."
                  )
                ),
                class = "mt-2",
                width = 12
              )
            ),
            fluidRow(
              bs4Card(
                title = "Incidents by taxonomic group and incident type",
                uiOutput("flt_mfi_daterange", class = "btn btn-xs"),
                reactable::reactableOutput("mfi_summary"),
                class = "mt-2",
                width = 6
              ),
              bs4Card(
                title = "Incident map",
                footer = map_footer,
                leaflet::leafletOutput("mfi_map", height = "calc(60vh)"),
                width = 6
              )
            )
          ),
          tabItem(
            tabName = "tab_turtle_nesting",
            # WAStD Data Map --------------------------------------------------#
            fluidRow(
              bs4Card(
                leaflet::leafletOutput("wastd_map"),
                title = "Turtle Monitoring Data Overview",
                footer = map_footer,
                width = 12,
                id = "box_all_wastd_data"
              )
            ),
            # WAStD Data Summaries --------------------------------------------#
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
                  plotly::plotlyOutput("plt_sighting_area_season", height = "600px"),
                  reactable::reactableOutput("tbl_sighting_area_season"),
                  # shiny::plotOutput("plt_processed_turtles_new_res_rem")
                  icon = icon("redo")
                ),
                width = 12,
                maximizable = TRUE
              )
            )
          ),
          tabItem(
            tabName = "tab_turtle_hatch",
            fluidRow(
              bs4Card(
                leaflet::leafletOutput("map_nests"),
                title = "Hatched Nests",
                footer = map_footer,
                width = 12
              )
            ),
            fluidRow(
              bs4Card(
                title = "Hatching and Emergence Success",
                plotly::plotlyOutput("plt_hatching_success", height = "600px"),
                plotly::plotlyOutput("plt_emergence_success", height = "600px"),
                reactable::reactableOutput("tbl_hatching_success"),
                width = 12
              )
            )
          ),
          tabItem(
            tabName = "tab_turtle_mis",
            fluidRow(
              bs4Card(
                leaflet::leafletOutput("map_fanangles"),
                title = "Turtle Hatchling Misorientation at Nest Emergence",
                footer = map_footer,
                width = 12
              )
            ),
            fluidRow(
              bs4Card(
                title = "Misorientation by species and season",
                plotly::plotlyOutput("plt_fanangles", height = "600px"),
                width = 12
              )
            )
          ),
          tabItem(
            tabName = "tab_turtle_dis",
            fluidRow(
              bs4Card(
                leaflet::leafletOutput("map_dist"),
                title = "General and Nest Disturbance and Predation",
                footer = map_footer,
                width = 12
              )
            ),
            fluidRow(
              bs4Card(
                title = "General and Nest Disturbance and Predation",
                plotly::plotlyOutput("plt_dist", height = "600px"),
                reactable::reactableOutput("tbl_dist"),
                width = 12
              )
            )
          ),
          tabItem(
            tabName = "tab_turtle_svy",
            fluidRow(
              bs4Card(
                title = "Survey Stats",
                reactable::reactableOutput("tbl_svy_season"),
                width = 12
              )
            ),
            fluidRow(
              bs4Card(
                title = "Survey effort",
                plotOutput("plt_svy_heatmap", height = "600px"),
                width = 12
              )
            )
          ),
          tabItem(
            tabName = "tab_odkc",
            fluidRow(
              bs4Card(
                title = "Rejected Turtle Tagging Records",
                reactable::reactableOutput("tbl_tt_rej"),
                width = 12
              ),
              bs4Card(
                title = "Imported Turtle Tagging Records",
                reactable::reactableOutput("tbl_tt_imp"),
                width = 12
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
      )
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
  add_resource_path("media", app_sys("inst/media"))

  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "WA Turtle Data"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert())
  )
}
