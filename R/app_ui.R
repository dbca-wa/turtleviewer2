#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import bs4Dash
#' @import shiny
#' @noRd
app_ui <- function(request) {
  golem::favicon("www/favicon.ico")

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    dashboardPage(
      fullscreen = TRUE,
      scrollToTop = TRUE,
      preloader = list(html = waiter::spin_chasing_dots(), color = "#333e48"),
      header = dashboardHeader(
        bs4Dash::actionButton(
          "action_dl_wastd_sites",
          "Update WAStD Sites",
          status = "primary",
          outline = TRUE,
          size = "xs",
          class = "m-1"
        ),
        bs4Dash::actionButton(
          "action_dl_wastd_data",
          "Update WAStD Data",
          status = "primary",
          outline = TRUE,
          size = "xs",
          class = "m-1"
        ),
        bs4Dash::actionButton(
          "action_dl_w2_data",
          "Update WAMTRAM data",
          status = "primary",
          outline = TRUE,
          size = "xs",
          class = "m-1"
        ),
        title = dashboardBrand(
          title = "WA Turtle Data",
          color = "primary",
          href = "https://turtledata.dbca.wa.gov.au/"
          # image = "www/logo.svg"
        )
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          id = "sidebar_menu",
          menuItem(
            text = "WA Sea Turtle DB",
            tabName = "tab_wastd",
            startExpanded = TRUE,
            menuSubItem(
              text = "Turtle Nesting",
              tabName = "tab_turtle_nesting",
              icon = icon("home")
            ),
            menuSubItem(
              text = "Turtle Hatching",
              tabName = "tab_turtle_hatching",
              icon = icon("egg")
            ),
            menuSubItem(
              text = "Disturbance",
              tabName = "tab_disturbance",
              icon = icon("bolt")
            ),
            menuSubItem(
              text = "Incidents",
              tabName = "tab_strandings",
              icon = icon("skull-crossbones")
            )
          ),
          menuItem(
            text = "Turtle Tagging DB",
            tabName = "tab_wamtram",
            startExpanded = TRUE,
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
          )
        )
      ),
      body = dashboardBody(
        tabItems(
          tabItem(
            tabName = "tab_turtle_nesting",
            fluidRow(
              boxLayout(
                bs4ValueBoxOutput("vb_total_emergences", width = 3),
                bs4ValueBoxOutput("vb_proc_mis", width = 3),
                bs4ValueBoxOutput("vb_new_res_rem", width = 3),
                bs4ValueBoxOutput("vb_nesting_success", width = 3),
                bs4ValueBoxOutput("sites_dl_on", width = 3),
                bs4ValueBoxOutput("wastd_dl_on", width = 3),
                bs4ValueBoxOutput("w2_dl_on", width = 3),
                type = "group"
              )
            ),
            fluidRow(
              bs4Card(
                leaflet::leafletOutput("wastd_map"),
                title = "All WAStD data",
                # footer= "footer",
                width = 12,
                maximizable = TRUE,
                # label = "label",
                id = "box_all_wastd_data"
              )
            ),
            fluidRow(
              tabBox(
                tabPanel(
                  title = "Total emergences",
                  # reactable::reactableOutput("tbl_total_emergences_proc_mis")
                  # shiny::plotOutput("plt_total_emergences_proc_mis")
                  icon = icon("arrow-up")
                ),
                tabPanel(
                  title = "Processed turtles",
                  # reactable::reactableOutput("tbl_processed_turtles_new_res_rem")
                  # shiny::plotOutput("plt_processed_turtles_new_res_rem")
                  icon = icon("eye")
                ),
                tabPanel(
                  title = "Nesting success",
                  # reactable::reactableOutput("tbl_nesting_success")
                  # shiny::plotOutput("plt_nesting_success")
                  icon = icon("thumbs-up")
                ),
                width = 12,
                maximizable = TRUE
              )
            )
          ),
          tabItem(
            tabName = "tab_turtle_hatching",
            shiny::tags$h3("Hatching and emergence success")
          ),
          tabItem(
            tabName = "tab_disturbance",
            shiny::tags$h3("Map of disturbances"),
            shiny::tags$h3("Disturbances by season and cause")
          ),
          tabItem(
            tabName = "tab_strandings",
            shiny::tags$h3("Map of Marine Wildlife Incidents"),
            shiny::tags$h3("Marine Wildlife Incidents by season and cause")
          ),
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
          tabItem(
            tabName = "tab_w2_observations",
            fluidRow(
              # uiOutput("flt_w2_data_loc"),
              uiOutput("flt_w2_data_plc"),
              uiOutput("flt_w2_data_obs")
            ),
            bs4Card(
              leaflet::leafletOutput("map_w2_obs")
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
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  add_resource_path("media", app_sys("app/media"))

  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "turtleviewer2")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert())
  )
}
