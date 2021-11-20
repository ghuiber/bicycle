#' Get the bottom bracket drop
#'
#' @param cs_st_angle The angle between the chain stay (cs) and seat tube (st) in degrees.
#' @param st_angle The angle between the seat tube and the horizontal.
#' @param cs_length Chain stay length in millimeters.
#' @param angle_btw_css Angle between chain stays, in degrees.
#'
#' @return A scalar equal to the bottom bracket drop in millimeters.
#' @export
#'
#' @examples
#' get_bb_drop(60, 74, 450, 14)
get_bb_drop <- function(cs_st_angle,
                        st_angle,
                        cs_length,
                        angle_btw_css) {
  # some reasonable conditions
  stopifnot(cs_st_angle > 0 && cs_st_angle < 180)
  stopifnot(angle_btw_css > 0 && angle_btw_css < 30)
  stopifnot(st_angle > 0 && st_angle < 90)
  stopifnot(cs_length > 0)

  # This is a 2d projection of the bike frame: its shadow
  # as if it were lying on the ground with the sun at noon.
  # The chain stay sticks out from the ground starting at the
  # bottom bracket and ending with the dropout in the air, at
  # half the angle between the chains stays. We want the length
  # of its shadow:
  cs_shadow <- cs_length * cos((angle_btw_css / 2) * pi / 180)

  # Derived from here (sketch it out on paper):
  # cs_angle <- 90 - (cs_st_angle + (90 - st_angle))
  cs_angle <- st_angle - cs_st_angle
  bb_drop <- cs_shadow * sin(cs_angle * pi / 180)
  return(bb_drop)
}
