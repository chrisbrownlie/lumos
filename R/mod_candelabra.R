#' Individual candelabra module UI
#'
#' @param id the ID of the module (and the ID of the candelabra
#' the module represents)
#'
#' @return candelabra UI (a list of details and some action buttons)
#'
#' @export
candelabra_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("candelabra"))
}

#' Individual candelabra module server
#'
#' @param id the ID of the module (and the ID of the candelabra
#' the module represents)
#' @param clb the candelabra object the module represents
#' @param lsp the last_selected_point reactive object
#' @param stg the stage object that the candelabra is on
#'
#' @return candelabra server
#'
#' @export
candelabra_server <- function(id,
                              clb,
                              lsp,
                              stg) {

  moduleServer(id,
               function(input,
                        output,
                        session) {
                 ns <- session$ns

                 clb_n <- reactive({
                   which(purrr::map(stg()$candelabras, ~.x$id)==id)
                 })

                 output$candelabra <- renderUI({
                   tags$li(
                     p("Candelabra ",
                       clb_n(),
                       tags$i(
                         " (ID: ",
                         id,
                        ")")),
                     tags$ul(
                       tags$li(paste0("X: ", clb$x_position)),
                       tags$li(paste0("Y: ", clb$y_position)),
                       tags$li(paste0("# candles: ", clb$n_candles)),
                       tags$li(paste0("Radius: ", clb$radius)),
                       tags$li(paste0("Height: ", clb$height)),
                       tags$li(paste0("Candle lumens: ", clb$candles[[1]]$lumens)),
                       tags$li(paste0("Candle height: ", clb$candles[[1]]$candle_height)),
                       tags$div(
                         style = "width:50%;float:left",
                         actionButton(ns("move_clb"),
                                      paste0("Move to (", paste(lsp(), collapse = ","), ")"))
                       ),
                       tags$div(
                         style = "width:50%;float:left",
                         actionButton(ns("remove_clb"),
                                      "Remove candelabra")
                       )
                     )
                   )
                 })

                 observeEvent(input$move_clb, {
                   stg()$move_candelabra(clb_n(),
                                       new_x = lsp()[1],
                                       new_y = lsp()[2])
                 })

                 observeEvent(input$remove_clb, {
                   stg()$remove_candelabra(clb_n())
                 })


               })
}
