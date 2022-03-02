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
      header = dashboardHeader(
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
          ),
          bs4Dash::actionButton(
            "action_dl_wastd_sites",
            "Update WAStD Sites",
            status = "info"
          ),
          bs4Dash::actionButton(
            "action_dl_wastd_data",
            "Update WAStD Data",
            status = "info"
          ),
          bs4Dash::actionButton(
            "action_dl_w2_data",
            "Update WAMTRAM data",
            status = "info"
          )
        )
      ),
      body = dashboardBody(
        tabItems(
          tabItem(
            tabName = "tab_turtle_nesting",
            boxLayout(
              bs4ValueBoxOutput("total_emergences"),
              bs4ValueBoxOutput("processed"),
              bs4ValueBoxOutput("missed"),
              bs4ValueBoxOutput("wastd_dl_on"),
              bs4ValueBoxOutput("w2_dl_on"),
              bs4ValueBoxOutput("sites_dl_on"),
              type = "deck"
            ),
            shiny::tags$h3("Map of all data"),
            leaflet::leafletOutput("wastd_map"),
            shiny::tags$h3("Total emergences: Processed and missed"),
            shiny::tags$h3("Processed turtles: New, resightings, remigrants"),
            shiny::tags$h3("Total emergences: Nesting success")
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
            # boxLayout(
            # bs4ValueBoxOutput("total_emergences"),
            # bs4ValueBoxOutput("processed"),
            # bs4ValueBoxOutput("missed"),
            # bs4ValueBoxOutput("wastd_dl_on"),
            # bs4ValueBoxOutput("w2_dl_on"),
            # type="deck"
            # ),
            leaflet::leafletOutput("w2_places_map"),
            shiny::tags$h3("Located W2 places"),
            reactable::reactableOutput("located_places"),
            shiny::tags$h3("Homeless W2 places"),
            reactable::reactableOutput("homeless_places")
          ),
          tabItem(
            tabName = "tab_w2_observations",
            shiny::tags$h3("Impossible coordinates"),
            reactable::reactableOutput("impossible_coords"),
            shiny::tags$h3("Unlikely coordinates"),
            reactable::reactableOutput("unlikely_coords")
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
