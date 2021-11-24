#' Get front triangle dimensions
#'
#' The front triangle is a misnomer, because it's a quadrilateral made up
#' by four tubes: the seat tube ST, top tube TT, head tube HT and down tube DT.
#' Between these tubes there are four angles, and they must add up to 360 degrees.
#' A bike frame designer aims for a top tube length (`tt_length`) and
#' seat tube length (`st_length`) that fit a rider of a given height and
#' torso length, and also for a seat tube angle (`st_angle`) head
#' tube angle (`ht_angle`) and top tube angle (`tt_angle`) that give
#' the bike its desired riding characteristics.
#'
#' The angle between the seat tube and the downtube (`st_dt_angle`) is
#' pre-determined in lugged frame construction by how the bottom bracket
#' shell was cast. With these six dimensions in hand we can get the full
#' set of dimensions that describe the front triangle. This function
#' returns them as a tibble of five columns and three rows. Each tube is
#' described as a right triangle made up of the tube length as the hypotenuse
#' and the tube's horizontal and vertical projections as the legs. The elements
#' on each row are named accordingly. Why five columns for four tubes?
#'
#' The fifth column, ett_triangle, helps calculate the effective top tube length
#' when the top tube is slanted. For slanted top tubes the effective length is
#' the horizontal distance between the front of the top tube and an imaginary
#' extension of the seat tube equal to the vertical projection of the top tube
#' times the sine of the seat tube angle in radians. In other words, this
#' horizontal distance is equal to the horizontal projection of the top tube
#' plus the vertical projection of the top tube times the tangent of the seat
#' tube angle in radians. The second term of this addition is the third
#' element of the ett_triangle vector.
#'
#' @inheritParams get_bb_tt_diagonal
#' @param ht_angle Head tube angle with the horizontal, in degrees.
#' @param st_dt_angle Angle between the seat tube and the down tube, in degrees.
#'
#' @return A 3 x 5 tibble.
#' @export
#'
#' @examples
#' get_front_triangle_dims()
get_front_triangle_dims <- function(st_length = 500,
                                    tt_length = 500,
                                    st_angle = 71,
                                    tt_angle = 0,
                                    ht_angle = 71,
                                    st_dt_angle = 60) {
  stopifnot(st_length > 0 && tt_length > 0)
  stopifnot(st_angle > 0 && st_angle < 90)
  stopifnot(tt_angle >= 0 && tt_angle < 90)
  stopifnot(ht_angle > 0 && ht_angle < 90)
  stopifnot(st_dt_angle > 0 && st_dt_angle < 90)

  tt_ht_angle <- 180 - (ht_angle + tt_angle)
  dt_angle <- (90 - st_angle) + (90 - st_dt_angle) # 1
  ht_dt_angle <- dt_angle + ht_angle               # 2
  # neither 1 nor 2 may feel intuitively true to you.
  # if so, sketch the frame out on paper. they can be
  # both proved with some basic Euclidean geometry.
  # it's easier if you can look at the drawing.
  bb_tt_diagonal <- get_bb_tt_diagonal(st_length, tt_length, st_angle, tt_angle)
  tt_diagonal_angle <- acos(-(st_length^2 - (tt_length^2 + bb_tt_diagonal^2))/(2*tt_length*bb_tt_diagonal))*180/pi
  st_diagonal_angle <- acos(-(tt_length^2 - (st_length^2 + bb_tt_diagonal^2))/(2*st_length*bb_tt_diagonal))*180/pi
  diagonal_dt_angle <- st_dt_angle - st_diagonal_angle
  diagonal_ht_angle <- tt_ht_angle - tt_diagonal_angle


  dt_length <- sin(diagonal_ht_angle * pi / 180) * bb_tt_diagonal / sin(ht_dt_angle * pi / 180)
  dt_triangle <- c(length = dt_length,
                   vertical_projection = sin(dt_angle * pi / 180) * dt_length,
                   horizontal_projection = cos(dt_angle * pi / 180) * dt_length)

  ht_length <- sin(diagonal_dt_angle * pi / 180) * bb_tt_diagonal / sin(ht_dt_angle * pi / 180)
  ht_triangle <- c(length = ht_length,
                   vertical_projection = sin(ht_angle * pi / 180) * ht_length,
                   horizontal_projection = cos(ht_angle * pi / 180) * ht_length)

  st_triangle <- c(length = st_length,
                   vertical_projection = sin(st_angle * pi / 180) * st_length,
                   horizontal_projection = cos(st_angle * pi / 180) * st_length)

  tt_triangle <- c(length = tt_length,
                   vertical_projection = sin(tt_angle * pi / 180) * tt_length,
                   horizontal_projection = cos(tt_angle * pi / 180) * tt_length)

  # if the top tube has a slope, then its length is different
  # from the effective top tube (ett) length. add ett_triangle:
  tt_vp <- tt_triangle[['vertical_projection']]
  ett_triangle <- c(length = tt_vp / sin(st_angle * pi / 180),
                    vertical_projection = tt_vp,
                    horizontal_projection = tt_vp / tan(st_angle * pi / 180))

  tibble::tibble(st_triangle,
                 dt_triangle,
                 ht_triangle,
                 tt_triangle,
                 ett_triangle)
}

#' Get rear triangle dimensions
#'
#' The rear triangle is made up of the projection in the horizontal plane
#' of the chain stay, the seat stay, and the seat tube. The seat tube is
#' the same as its projection in the horizontal plane, but the other two
#' tubes stick out at an angle, ans they must accommodate the width of
#' the rear wheel hub, so there's some trigonometry involved.
#'
#' A bike frame designer aims for a seat tube length (`st_length`) that
#' fits a rider of a given height, and also for a seat tube angle (`st_angle`)
#' and chain stay length (`cs_length`) that give the bike its desired riding
#' characteristics, though `cs_length` is subject to some other constraints
#' described in the documentation for [bicycle::get_cs_length()].
#'
#' The angle between the chain stay and the seat tube (`cs_st_angle`) is
#' pre-determined in lugged frame construction by how the bottom bracket
#' shell was cast.
#'
#' This function returns the dimensions of the rear triangle using the
#' convention described in the documentation for [bicycle::get_front_triangle_dims()].
#'
#' @inheritParams get_front_triangle_dims
#' @inheritParams get_old_spacing
#' @param cs_st_angle Angle between chain stay and seat tube, in degrees.
#' @seealso [bicycle::get_cs_length()]
#' @seealso [bicycle::get_front_triangle_dims()]
#'
#' @return A 3 x 3 tibble.
#' @export
#'
#' @examples
#' get_rear_triangle_dims()
get_rear_triangle_dims <- function(st_length = 500,
                                   cs_length = 450,
                                   st_angle = 71,
                                   cs_st_angle = 61,
                                   angle_btw_css = 14) {
  # remember, this is a 2d projection of the bike frame: its
  # shadow as if it were lying on the ground with the sun at noon.
  # so the chain stay sticks out from the ground starting at the
  # BB toward the DO, at half the angle between the chains stays.
  # we want the length of its shadow:
  cs_shadow <- cs_length * cos((angle_btw_css / 2) * pi / 180)

  # derived from here (sketch it out on paper)
  # cs_angle <- 90 - (cs_st_angle + (90 - st_angle))
  cs_angle <- st_angle - cs_st_angle

  cs_triangle <- c(length = cs_shadow,
                   vertical_projection = cs_shadow * sin(cs_angle * pi / 180),
                   horizontal_projection = cs_shadow * cos(cs_angle * pi / 180))

  st_triangle <- c(length = st_length,
                   vertical_projection = st_length * sin(st_angle * pi / 180),
                   horizontal_projection = st_length * cos(st_angle * pi / 180))

  ss_hp <- cs_triangle['horizontal_projection'] - st_triangle['horizontal_projection']
  ss_vp <- st_triangle['vertical_projection'] - cs_triangle['vertical_projection']
  ss_shadow <- sqrt(ss_vp^2 + ss_hp^2)
  ss_triangle <- c(length = ss_shadow,
                   vertical_projection = ss_vp,
                   horizontal_projection = ss_hp)

  tibble::tibble(cs_triangle, st_triangle, ss_triangle)
}

#' Get the dimensions of the bicycle frame
#'
#' This is a convenience wrapper that puts together the
#' output of [bicycle::get_front_triangle_dims()] and
#' [bicycle::get_rear_triangle_dims()].
#'
#' @inheritParams get_front_triangle_dims
#' @inheritParams get_rear_triangle_dims
#' @seealso [bicycle::get_front_triangle_dims()]
#' @seealso [bicycle::get_rear_triangle_dims()]
#'
#' @return A 7 x 3 tibble.
#' @export
#'
#' @examples
#' wrap_frame_dims()
wrap_frame_dims <- function(st_length = 500,
                            tt_length = 500,
                            st_angle = 71,
                            tt_angle = 0,
                            ht_angle = 71,
                            st_dt_angle = 60,
                            cs_length = 450,
                            cs_st_angle = 59,
                            angle_btw_css = 14) {
  front_triangle <- get_front_triangle_dims(st_length = st_length,
                                            tt_length = tt_length,
                                            st_angle = st_angle,
                                            tt_angle = tt_angle,
                                            ht_angle = ht_angle,
                                            st_dt_angle = st_dt_angle)
  rear_triangle <- get_rear_triangle_dims(st_length = st_length,
                                          cs_length = cs_length,
                                          st_angle = st_angle,
                                          cs_st_angle = cs_st_angle,
                                          angle_btw_css = angle_btw_css)
  df <- front_triangle %>%
    dplyr::inner_join(rear_triangle)
  for (i in colnames(rear_triangle)) {
    names(df[[i]]) <- names(df$cs_triangle)
  }
  df
}
