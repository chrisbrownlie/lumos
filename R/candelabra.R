#' candelabra class
#'
#' Each candelabra is made up of 1 or more candles
#'
#' @return an object with class 'candelabra'
#' @export
candelabra <- R6::R6Class(
  classname = "candelabra",
  public = list(

    #' @field x_position the x coordinate (in cm) of the centre of the candelabra
    #' on the stage
    x_position = NULL,

    #' @field y_position the y coordinate (in cm) of the centre of the candelabra
    #' on the stage
    y_position = NULL,

    #' @field n_candles the number of the candles in the candelabra
    n_candles = NULL,

    #' @field radius the radius (in cm) of the candelabra
    radius = NULL,

    #' @field height the height (in cm) of the base of the candelabra from the
    #' floor of the stage
    height = NULL,

    #' @description create a new object of class 'candelabra'
    #' @param stage the object with class 'stage' to which this candelabra belongs
    #' @param centre_position a numeric vector of length 2 giving the (x,y) coordinates
    #' of this candelabra on the stage
    #' @param n_candles the number of candle objects to initiate in the candelabra
    #' @param radius the radius of the candelabra in cm
    #'
    #' @return an object of class candelabra containing multiple objects of class candle
    initialize = function(stage,
                          centre_position,
                          n_candles,
                          radius,
                          candle_lumens,
                          candle_height,
                          height) {

      private$.id <- uuid::UUIDgenerate()
      private$parent_stage <- stage
      self$n_candles <- n_candles
      private$.candles <- lapply(seq_len(n_candles),
                        function(i,
                                 r = radius,
                                 cl = candle_lumens,
                                 ch = candle_height,
                                 nc = n_candles) {
                          candle$new(
                            candelabra = self,
                            lumens = cl,
                            x_offset = r*cos(2*pi*i/nc),
                            y_offset = r*sin(2*pi*i/nc),
                            candle_height = ch
                          )
                        })

      self$x_position <- centre_position[1]
      self$y_position <- centre_position[2]
      self$radius <- radius
      self$height <- height

      invisible(self)
    },

    #' @description move the candelabra to a new position on the stage
    #' @param x the new x coordinate of the candelabra
    #' @param y the new y coordinate of the candelabra
    #' @return updates the x_position and y_position fields
    move_to = function(x,y) {

      if (x < 0 | x > private$parent_stage$width) stop("invalid x")
      if (y < 0 | y > private$parent_stage$depth) stop("invalid y")

      self$x_position <- x
      self$y_position <- y
      invisible(self)
    }
  ),
  private = list(
    .id = NULL,
    .candles = NULL,
    parent_stage = NULL
  ),
  active = list(
    #' @field id returns the generated unique ID of the candelabra
    id = function(value) {
      if (missing(value)) {
        private$.id
      } else {
        stop("Cannot manually set id")
      }
    },
    #' @field stage returns the name of the stage on which this candelabra sits
    stage = function(value) {
      if (missing(value)) {
        private$parent_stage$alias
      } else {
        stop("Cannot manually set alias")
      }
    },
    #' @field candles returns a list of the candle objects contained in this candelabra
    candles = function(value) {
      if (missing(value)) {
        private$.candles
      } else {
        stop("Cannot manually set candles")
      }
    }
  )
)
#' @exportS3Method
print.candelabra <- function(x) {
  cli::cli_inform("{.cls lumos::candelabra}")
  cli::cli_inform("{.emph {x$id}} on { {x$stage}}")
  cli::cli_inform("Centred at [{ {x$x_position}}, { {x$y_position}}]")
  cli::cli_inform("{.val {length(x$candles)}} candles")
  invisible(x)
}
