#' Lumos app server
#'
#' @param input shiny inputs
#' @param output shiny outputs
#' @param session shiny session
#'
#' @return a shiny app server
#' @export
app_server <- function(input, output, session) {

  current_stage <- stage$new(0)$reactive()

  output$stage_light_plot <- plotly::renderPlotly(
    current_stage()$plot_light(show_candelabras = input$show_candelabra,
                               max_lux = input$max_lux) %>%
      event_register('plotly_click')
  )

  output$plot_container <- renderUI({
    with_spinner(
      plotlyOutput("stage_light_plot",
                   width = "100%",
                   height = "100%")
    )
  })

  last_selected_point <- reactive({
    c(plotly::event_data("plotly_click")$x,
      plotly::event_data("plotly_click")$y)
  })

  observe({
    if (length(last_selected_point())) {
      updateNumericInput(session = session,
                         inputId = "new_x_location",
                         value = last_selected_point()[[1]])
      updateNumericInput(session = session,
                         inputId = "new_y_location",
                         value = last_selected_point()[[2]])
    }
  })

  # Candelabra UI
  output$existing_candelabras <- renderUI({

    tags$ul(
      purrr::imap(
        current_stage()$candelabras,
        ~candelabra_ui(.x$id)
      )
    )

  })

  observeEvent(input$add_new_candelabra, {
    current_stage()$add_candelabra(
      x = input$new_x_location,
      y = input$new_y_location,
      r = input$new_radius,
      l = input$new_lumens,
      h = input$new_height,
      n = input$new_n_candles,
      ch = input$new_candle_height
    )

    new_clb <- current_stage()$candelabras[[length(current_stage()$candelabras)]]

    candelabra_server(id = new_clb$id,
                      lsp = last_selected_point,
                      clb = new_clb,
                      stg = current_stage)
  })

  observeEvent(input$update_stage, {
    current_stage()$change_size(
      w = input$stage_width,
      d = input$stage_depth,
      us = input$unit_scaling
    )
  })

}
