#' Lumos app UI
#'
#' @import shiny
#'
#' @return app UI
#' @export
app_ui <- function() {

  shinydashboard::dashboardPage(
    title = "Lumos | Chris Brownlie",

    skin = "yellow",

    shinydisconnect::disconnectMessage2(),

    # Head
    header = shinydashboard::dashboardHeader(
      disable = TRUE
    ),


    # Side
    sidebar = shinydashboard::dashboardSidebar(
      disable = TRUE
    ),

    # Main
    body = shinydashboard::dashboardBody(

      # HTML Headers
      tags$head(
        # Include custom CSS
        tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
        # Include favicon
        tags$link(rel = "shortcut icon", href = "lumos.ico")
      ),


      # Main Content
      fluidRow(
        h1("LUMOS",
           class = "page-title")
      ),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            width = NULL,
            title = "Details",

            p("This app can be used to see how various candelabra lighting arrangements would
              look on a stage and how it would affect the level of lux at each point on the stage."),
            p("The options below can be altered to update the plot, which will show what the amount of light
              will be at each point on the stage, given the current configuration."),
            tags$ul("There are some important points/assumptions to note:",
                    tags$li("The stage is always quadrilateral"),
                    tags$li("It is lit by candelabras that are all circular, with candles evenly spaced around their circumference"),
                    tags$li("There are no other light sources"),
                    tags$li("All candelabra have a flat circular base, with candles resting on that base"),
                    tags$li("The lux from a candle at a point is calculate as (the candle's light in lumens)/ ((distance to the candle in m)^2)")),

            p("Use the options box on the right to change other parameters of the stage, and the box on the left below to add/remove/edit candelabras")
          )
        ),

        column(
          width = 6,
          shinydashboard::box(
            width = NULL,
            title = "Stage Options",

            p("Use the inputs below to alter the stage."),
            p(tags$b("NOTE: updating these will remove all candelabra from the stage")),

            numericInput("stage_width",
                         "Stage width (wing to wing) in cm",
                         value = 1200),
            numericInput("stage_depth",
                         "Stage depth (front to back) in cm",
                         value = 800),
            numericInput("unit_scaling",
                         "Size of each stage unit (NOTE: lower values will result in long processing times)",
                         value = 10),
            actionButton("update_stage",
                         "Update stage"),
            p("Note that updating the stage size will remove any candelabras
              that would not fit on the new stage."),

            checkboxInput("show_candelabra",
                          "Show candelabra on lux plot?",
                          value = TRUE),
            numericInput("max_lux",
                         "Maximum lux value",
                         value = 10,
                         min = 1)
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          shinydashboard::box(
            width = NULL,
            title = "Candelabras",
            h4("New candelabra:"),
            p(tags$i("Tip: click on the stage plot to set the x and y coordinates automatically")),
            numericInput("new_x_location",
                         "X coordinate of new candelabra",
                         value = 0),
            numericInput("new_y_location",
                         "Y coordinate of new candelabra",
                         value = 0),
            numericInput("new_n_candles",
                         "Number of candles in new candelabra",
                         value = 10,
                         min = 1),
            numericInput("new_lumens",
                         "Number of lumens emitted from *each candle* in the candelabra",
                         value = 10),
            numericInput("new_candle_height",
                         "Height of each candle from the base of the candelabra (in cm)",
                         value = 15),
            numericInput("new_radius",
                         "Radius of the candelabra (in cm)",
                         value = 30),
            numericInput("new_height",
                         "Height of the candelabra from the ground (in cm)",
                         value = 1000),

            actionButton("add_new_candelabra",
                         "Add new candelabra"),
            hr(),

            h4("Existing candelabras:"),
            uiOutput("existing_candelabras")
          )
        ),
        column(
          width = 8,
          fluidRow(
            shinydashboard::box(
            width = NULL,
            height = NULL,
            title = h1("Stage lighting"),
            p("On the plot below, black represents complete darkness
              and brighter yellow colours indicate higher levels of light.
              You can set the maximum lux level using the option above."),
            uiOutput("plot_container")
            )
          )
        )
      )
    )

  )

}
