#' candle class
#'
#' @return an object with class 'candle'
#' @export
candle <- R6::R6Class(
  classname = "candle",
  public = list(
    #' @field lumens the light that this candle emits in lumens
    lumens = NULL,

    #' @field x_offset the distance (cm) in the x direction of the candle
    #' from the centre of the candelabra in which it sits
    x_offset = NULL,

    #' @field y_offset the distance (cm) in the y direction of the candle
    #' from the centre of the candelabra in which it sits
    y_offset = NULL,

    #' @field candle_height the height of the candle (in cm) from the base
    #' of the candelabra in which it sits
    candle_height = NULL,

    #' @description create a new candle object
    #' @param candelabra the candelabra object in which the new candle should sit
    #' @param lumens the amount of light the new candle emits, in lumens
    #' @param x_offset the distance (cm) in the x direction between the new candle and
    #' the centre of the candelabra in which it sits
    #' @param y_offset the distance (cm) in the y direction between the new candle and
    #' the centre of the candelabra in which it sits
    #' @param candle_height the height of the candle in cm
    initialize = function(candelabra,
                          lumens,
                          x_offset,
                          y_offset,
                          candle_height) {

      private$.id <- uuid::UUIDgenerate()

      self$lumens <- lumens
      self$x_offset <- x_offset
      self$y_offset <- y_offset
      private$parent_candelabra <- candelabra

      self$candle_height <- candle_height

      invisible(self)
    }
  ),
  private = list(
    .id = NULL,
    parent_candelabra = NULL
  ),
  active = list(
    #' @field id a uniquely generated ID for the candle
    id = function(value) {
      if (missing(value)) {
        private$.id
      } else {
        stop("Cannot set id manually")
      }
    },

    #' @field candelabra the ID of the candelabra in which this candle sits
    candelabra = function(value) {
      if (missing(value)) {
        private$parent_candelabra$id
      } else {
        stop("Cannot set stage manually")
      }
    },

    #' @field stage the name of the stage on which this candle sits
    stage = function(value) {
      if (missing(value)) {
        private$parent_candelabra$parent_stage$alias
      } else {
        stop("Cannot set stage manually")
      }
    },

    #' @field x_position the overall x coordinate of the candle on the stage
    x_position = function(value) {
      if (missing(value)) {
        private$parent_candelabra$x_position + self$x_offset
      } else {
        stop("Cannot set candle position manually, move candelabra")
      }
    },

    #' @field y_position the overall y coordinate of the candle on the stage
    y_position = function(value) {
      if (missing(value)) {
        private$parent_candelabra$y_position + self$y_offset
      } else {
        stop("Cannot set candle position manually, move candelabra")
      }
    }
  )
)

#' @exportS3Method
print.candle <- function(x) {
  cli::cli_inform("{.cls lumos::candle}")
  cli::cli_inform("{.emph {x$id}} on { {x$stage}}")
  cli::cli_inform("Centred at [{ {round(x$x_position,2)}}, { {round(x$y_position,2)}}]")
  cli::cli_inform("Emitting {.val {x$lumens}} lumens")
  invisible(x)
}
