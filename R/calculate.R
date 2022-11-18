#' Calculate lux
#'
#' Given an amount of light emitted from a source in lumens and a distance to
#' the light source, calculate the level of light in lux
#'
#' @param lumens the amount of light being emitted from the source in lumens
#' @param distance the distance to the light source in cm
#'
#' @return a number indicating the lux that the light source gives
#' @export
calculate_lux <- function(lumens, distance) {
  lumens/((distance/100)^2)
}
v_calc_lux <- Vectorize(calculate_lux)

#' Calculate the straight line distance between two points
#'
#' @param x1 the x coordinate of the first point
#' @param y1 the y coordinate of the first point
#' @param z1 the z coordinate of the first point
#' @param x2 the x coordinate of the second point
#' @param y2 the y coordinate of the second point
#' @param z2 the z coordinate of the second point
#'
#' @return a single number indicating the straight line distance
#' between the two points
#' @export
sld <- function(x1,y1,z1,x2,y2,z2) {
  sqrt(sum((c(x1,y1,z1)-c(x2,y2,z2))^2))
}
v_sld <- Vectorize(sld)
