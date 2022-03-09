#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
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
        # Download button: WAStD Sites
        bs4Dash::actionButton(
          "action_dl_wastd_sites",
          "Update WAStD Sites",
          status = "primary",
          outline = TRUE,
          size = "xs",
          class = "btn btn-xs m-1"
        ),
        # Download button: WAStD Data
        bs4Dash::actionButton(
          "action_dl_wastd_data",
          "Update WAStD Data",
          status = "primary",
          outline = TRUE,
          size = "xs",
          class = "btn btn-xs m-1"
        ),
        # Download button: WAMTRAM Data
        bs4Dash::actionButton(
          "action_dl_w2_data",
          "Update WAMTRAM data",
          status = "primary",
          outline = TRUE,
          size = "xs",
          class = "btn btn-xs m-1"
        ),
        # Filter: WAStD Localities
        uiOutput(
          "flt_wastd",
          inline = TRUE,
          class = "btn btn-xs"
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
        waiter::useWaitress(),
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
            # WAStD Data Map --------------------------------------------------#
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
            ), #---------------------------------------------------------------#
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
                  #
                  # We calculate the **nesting success** as the fraction of confirmed nests
                  # (turtles observed laying during tagging plus "successful crawl" turtle
                  #   tracks with what is believed by the observer to be a nest) over the
                  # total number of nesting attempts (emergences).
                  # As nesting success are counted:
                  #
                  # * Tagging records with `clutch_completed` status
                  #
                  # * "Y" (confirmed completed clutch).
                  #
                  # * Tracks with `nest_type`
                  #
                  # * "successful-crawl" (believed to be a nest by the observer).
                  #
                  # As unknowns are counted:
                  #
                  #   * Tagging records with `clutch_completed` status
                  #
                  # * "P" (possible clutch, but not certain enough to confirm),
                  # * "U" (uncertain about clutch), and
                  # * "D" (did not check).
                  #
                  # * Tracks with `nest_type`
                  #
                  # * "track-not-assessed" (did not check for nest), and
                  # * "track-unsure" (checked but unsure).
                  #
                  # As non nesting are counted:
                  #
                  #   * Tagging records with `clutch_completed` status
                  #
                  # * "N" (confirmed no).
                  #
                  # * Tracks with `nest_type`
                  #
                  # * "false-crawl" (believed not to be a nest by the observer).
                  #
                  # When calculating the nesting success as fraction of successful over total nesting,
                  # we calculate three versions of nesting success.
                  #
                  # * The "pessimistic" success rate considers all unknowns as non nesting.
                  # * The "optimistic" success rate considers all unknowns as successful nesting.
                  # * The "split" success rate considers exactly half of all unknowns as successful nesting.
                  #
                  # Uncertain track count records include cases where the presence of a nest is unsure or not
                  # checked for. The tagging records cater for a range of unknowns, which again could
                  # well be successfully completed clutches.
                  #
                  # The **data** are biased (both ways) by the observers' individual ability to
                  # recognize a nest, and the opportunity to observe the clutch up to its successful
                  # end.
                  #
                  # Nesting success measured during night tagging can be assumed to be lowered by
                  # the disruption through monitoring disturbance.
                  # * Nesting success (optimistic) = yes + unsure / yes + unsure + no
                  # * Nesting success is lower during tagging because tagging disrupts nesting
                  # * Morning surveys of tracks with/without nests don't disturb nesting process, but
                  #   biased by observer capability to determine nest presence (barring excavation).
                  #
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
            # fluidRow(
            #   boxLayout(
            #     # bs4ValueBoxOutput("vb_hs", width = 3),
            #     # bs4ValueBoxOutput("vb_es", width = 3),
            #     type = "group"
            #   )
            # ),
            # fluidRow(
            # tabBox(
            #   tabPanel(
            #     title = "Hatching and Emergence Success",
            #     # shiny::plotOutput("")
            #     # reactable::reactableOutput("")
            #     icon = icon("table")
            #   ),
            #   # tabPanel(
            #   #   title = "Hatching and Emergence Success timeseries",
            #   #   # reactable::reactableOutput("")
            #   #   # shiny::plotOutput("")
            #   #   icon = icon("chart-area")
            #   # ),
            #   width = 12,
            #   maximizable = TRUE
            # )
            # )
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
              bs4Card(
                # uiOutput("flt_w2_data_loc"),
                uiOutput("flt_w2_data_plc", class = "m-2"),
                uiOutput("flt_w2_data_obs", class = "m-2"),
                bs4ValueBoxOutput("vb_w2_plc_lat", width = 12),
                bs4ValueBoxOutput("vb_w2_plc_lon", width = 12),
                width = 4
              ),
              bs4Card(
                title = "WAMTRAM 2 Observations",
                footer = "Select Place Code or Observation ID to filter data",
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
    bundle_resources(path = app_sys("app/www"), app_title = "WA Turtle Data")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert())
  )
}
