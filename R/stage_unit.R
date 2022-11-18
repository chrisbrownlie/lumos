stage_unit <- R6::R6Class(
  classname = "stage_unit",
  public = list(
    x_position = NULL,
    y_position = NULL,
    height = NULL,
    width = NULL,
    initialize = function(stage,
                          x_position,
                          y_position) {

      if (x_position < 0 | x_position > stage$width) stop("Invalid unit x_position")
      if (y_position < 0 | y_position > stage$depth) stop("Invalid unit y_position")

      self$x_position <- x_position
      self$y_position <- y_position
      self$height <- 0
      self$width <- stage$unit_scaling

      private$.parent_stage <- stage
      invisible(self)
    },

    incoming_lux_from = function(candle_id, candle_position) {

      if (candle_id %in% private$.known_candles && private$.known_candles[[candle_id]]$position == candle_position) {
        return(private$.known_candles[[candle_id]]$lux)
      } else {

        lux <- sld()

      }

    }
  ),
  private = list(
    .parent_stage = NULL
  ),
  active = list(
    unit_id = function(value) {
      if (missing(value)) {
        paste0(self$x_position, "-", self$y_position)
      } else {
        stop("Cannot set ID manually")
      }
    },
    incoming_lux = function(value) {
      if (missing(value)) {

        parent_candles <- private$.parent_stage$candle_positions

        lux <- sapply(
          seq_len(nrow(parent_candles)),
          \(i) {
            print(paste0("SU: ", self$unit_id, "/", i))
            parent_candles$lumens[i]/(
              sld(c(self$x_position,
                    self$y_position,
                    self$height),
                  c(parent_candles$x[i],
                    parent_candles$y[i],
                    parent_candles$z[i]))/100
            )
          }
        ) %>%
          sum()

      }
    }
  )
)
