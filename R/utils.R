#' Get lumos defaults
#'
#' @param what the name of the default value to get
#'
#' @return a default numeric value for 'what'
default <- function(what) {
  switch(what,
         "candle_height" = 10,
         "candelabra_candles" = 10,
         "candelabra_radius" = 30,
         "candle_lumens" = 10,
         "unit_scaling" = 10,
         stop("No default for this"))
}


#' Add shinycssloader to a UI element
#'
#' @param elem the ui element to add the spinner to
#'
#' @return the input element with a loading spinner when
#' computing
with_spinner <- function(elem) {
  shinycssloaders::withSpinner(
    elem
  )
}
