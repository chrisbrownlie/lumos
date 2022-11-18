#' Lumos app server
#'
#' @param input shiny inputs
#' @param output shiny outputs
#' @param session shiny session
#'
#' @return a shiny app server
#' @export
app_server <- function(input, output, session) {

  stage <- stage$new(0)$reactive()

  output$stage_light_plot <- plotly::renderPlotly(
    stage()$plot_light(show_candelabras = input$show_candelabra) %>%
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
        stage()$candelabras,
        function(clb, i) {
          tags$li(
            paste0("Candelabra ", i),
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
                actionButton(paste0("move_clb_", i),
                             paste0("Move to (", paste(last_selected_point(), collapse = ","), ")"))
              ),
              tags$div(
                style = "width:50%;float:left",
                actionButton(paste0("remove_clb_", i),
                             "Remove candelabra")
              )
            )
          )
        }
      )
    )

  })

  observeEvent(input$add_new_candelabra, {
    stage()$add_candelabra(
      x = input$new_x_location,
      y = input$new_y_location,
      r = input$new_radius,
      l = input$new_lumens,
      h = input$new_height,
      n = input$new_n_candles,
      ch = input$new_candle_height
    )
  })

  observeEvent(input$update_stage, {
    stage <- stage$new(0,
                       width = input$stage_width,
                       depth = input$stage_depth,
                       unit_scaling = input$unit_scaling)$reactive()
  })

  # TODO: convert to modules
  observe({
    print("running creation observer")
      lapply(
        seq_len(length(stage()$candelabras)),
        function(i, lsp = isolate(last_selected_point())) {
          observeEvent(input[[paste0("move_clb_", i)]], {
            print(paste0("running observer for candelabra ", i))
            stage()$move_candelabra(i, lsp[1], lsp[2])
          })
          observeEvent(input[[paste0("remove_clb_", i)]], {
            stage()$remove_candelabra(i)
          })
        }
      )
  })

}
