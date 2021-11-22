#' Get the length of the front triangle diagonal
#'
#' The front triangle is a misnomer, because it's a quadrilateral made up
#' by the seat tube ST, top tube TT, head tube HT and down tube DT. The
#' diagonal we want is the second one, going from the bottom bracket BB
#' to the point where the top tube meets the head tube.
#'
#' @param st_length The length of the seat tube ST in millimeters.
#' @param tt_length The length of the top tube TT in millimeters.
#' @param st_angle The angle between the seat tube and the horizontal.
#' @param tt_angle The angle between the top tube and the horizontal.
#'
#' @return A scalar equal to the length of the diagonal in millimeters.
#' @export
#'
#' @examples
#' get_bb_tt_diagonal(500, 530, 73, 71)
get_bb_tt_diagonal <- function(st_length,
                               tt_length,
                               st_angle,
                               tt_angle) {
  # some reasonable conditions
  stopifnot(st_length > 0)
  stopifnot(tt_length > 0)
  stopifnot(st_angle > 0 && st_angle < 90)
  stopifnot(tt_angle >= 0 && tt_angle < 90)

  st_tt_angle <- st_angle + tt_angle
  out <- sqrt(st_length^2 +
              tt_length^2 -
              2 * st_length * tt_length * cos(st_tt_angle * pi / 180))
  return(out)
}
