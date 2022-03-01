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
        sidebarHeader("WAStD"),
        sidebarMenu(
          id = "sidebar_menu_wastd",
          menuItem(
            text = "Turtle Nesting",
            tabName = "tab_turtle_nesting"
          ),
          menuItem(
            text = "Turtle Hatching",
            tabName = "tab_turtle_hatching"
          ),
          menuItem(
            text = "Disturbance",
            tabName = "tab_disturbance"
          )
        ),
        sidebarHeader("WAMTRAM 2"),
        sidebarMenu(
          id = "sidebar_menu_wamtram",
          menuItem(
            text = "Places",
            tabName = "tab_w2_places"
          ),
          menuItem(
            text = "Observations",
            tabName = "tab_w2_observations"
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
              type="deck"
            ),
              leaflet::leafletOutput("wastd_map")
            ),

          tabItem(
            tabName = "tab_turtle_hatching"
          ),

          tabItem(
            tabName = "tab_disturbance"
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
            leaflet::leafletOutput("w2_places_map")
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
