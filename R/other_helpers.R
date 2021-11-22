#' Get seat tube to rear tire clearance
#'
#' Frame designs with shorter chain stays are for racing; longer
#' chain stays are for comfort. You can tell them apart because
#' the latter have a larger gap between the rear tire and the
#' seat tube. This function returns that gap, in millimeters,
#' for a given design tibble `df` and wheel diameter.
#'
#' @param df A tibble with frame dimensions from the front
#' and rear triangle, returned by [bicycle::wrap_frame_dims()] or
#' at a minimum the portion of it that comes from [bicycle::get_rear_triangle_dims()]
#' @param wheel_diameter Wheel diameter in millimeters; 622 is the bead seat diameter
#' of a 700c wheel. To see the effect of the tire width, add 2 x tire width in millimeters
#' -- i.e. specify `wheel_diameter = 622 + 2 * 32` for a 32 mm tire.
#'
#' @return A scalar, the seat tube to rear tire clearance in millimeters
#' @export
get_st_to_rear_tire_clearance <- function(df,
                                          wheel_diameter = 622) {
  # first, get horizontal distance
  # btw the rear DO and the ST

  # get the ST angle to the vertical:
  st_hp <- df$st_triangle[['horizontal_projection']]
  st_vp <- df$st_triangle[['vertical_projection']]
  st_angle <- atan(st_hp/st_vp)*180/pi
  # get the ss projections
  ss_hp <- df$ss_triangle[['horizontal_projection']]
  ss_vp <- df$ss_triangle[['vertical_projection']]

  # here's the horizontal distance you need:
  st_hd <- ss_hp + ss_vp * tan(st_angle * pi / 180)

  # this horizontal distance is a hypotenuse of the
  # right triangle that connects the read DO to the
  # seat tube. the tire clearance is the difference
  # between the leg that connects the DO to the ST
  # and half the wheel diameter. The angle between
  # this leg and the hypotenuse is the same as
  # st_angle, and you can prove this graphically.
  # so now we just need a cosine relationship to
  # get that leg length:
  (st_hd * cos(st_angle * pi / 180)) -
    (wheel_diameter / 2)
}

#' Get some useful frame metrics
#'
#' Once you have a frame design `df` you can use this to derive
#' some metrics that will help gauge the ride characteristics.
#'
#' @inheritParams overlay_the_bicycle
#' @inheritParams get_rear_triangle_dims
#' @inheritParams get_cs_length
#' @param crank_arm_length The crank arm length in millimeters. Typical values are 165, 170 and 175 for adult riders.
#' @param bbs_width Bottom bracket shell width. It's standard -- 68 mm is typical for road bikes.
#'
#' @return A named vector
#' @export
get_all_the_metrics <- function(df,
                                angle_btw_css = 14,
                                wheel_diameter = 622,
                                crank_arm_length = 170,
                                old_spacing = 130,
                                bbs_width = 68) {
  # start by recovering the chain stay length
  cs_shadow <- df$cs_triangle[['length']]
  cs_length <- cs_shadow / cos((angle_btw_css / 2) * pi / 180)

  wheelbase <- df$cs_triangle[['horizontal_projection']] +
    df$dt_triangle[['horizontal_projection']] +
    df$af_triangle[['horizontal_projection']]
  bb_height <- wheel_diameter / 2 - df$cs_triangle[['vertical_projection']]

  # toe overlap is distance between the tip of the crank arm
  # and the edge of the wheel when the crank arm is lined up
  # perfectly along the front-center line.
  a <- df$dt_triangle[['horizontal_projection']] +
    df$af_triangle[['horizontal_projection']]
  b <- df$cs_triangle[['vertical_projection']]
  front_center <- sqrt(a^2 + b^2)
  toe_overlap <- front_center - (crank_arm_length + wheel_diameter / 2)
  fork_trail <- calculate_fork_trail(df, wheel_diameter)
  st_to_rear_tire_clearance <- get_st_to_rear_tire_clearance(df, wheel_diameter)
  bbsc_offset <- get_bbsc_offset(old_spacing, angle_btw_css, cs_length, bbs_width)

  # return the goods
  c('Wheel base' = wheelbase,
    'BB height' = bb_height,
    'Front center' = front_center,
    'Toe overlap' = toe_overlap,
    'Fork trail' = fork_trail,
    'Seat tube to rear tire clearance' = st_to_rear_tire_clearance,
    'BBSC offset' = bbsc_offset)
}

#' Get all the things
#'
#' A wrapper for tighter control over design inputs. Returns a named
#' list of three objects: the frame design tibble, the ggplot drawing,
#' and a named vector of useful frame metrics.
#'
#' @inheritParams wrap_frame_dims
#' @inheritParams find_ht_extension_and_add_true_fork
#' @inheritParams draw_the_bicycle
#' @inheritParams get_all_the_metrics
#'
#' @return A named list
#' @export
big_bicycle <- function(st_length = 500,
                        tt_length = 500,
                        st_angle = 71,
                        tt_angle = 0,
                        ht_angle = 71,
                        st_dt_angle = 58,
                        cs_st_angle = 62,
                        cs_length = 450,
                        angle_btw_css = 14,
                        fork_rake = 45,
                        fork_cta_length = 390,
                        wheel_diameter = 622,
                        i = 350,
                        c = I('black'),
                        alpha_factor = 1,
                        bb_shell_diameter = 34.8,
                        crank_arm_length = 170,
                        old_spacing = 130,
                        bbs_width = 68) {
  df <- wrap_frame_dims(st_length = st_length,
                        tt_length = tt_length,
                        st_angle = st_angle,
                        tt_angle = tt_angle,
                        ht_angle = ht_angle,
                        st_dt_angle = st_dt_angle,
                        cs_length = cs_length,
                        cs_st_angle = cs_st_angle,
                        angle_btw_css = angle_btw_css) %>%
    find_ht_extension_and_add_true_fork(fork_rake = fork_rake,
                                        fork_cta_length = fork_cta_length)

  pic <- df %>%
    draw_the_bicycle(wheel_diameter = wheel_diameter,
                     alpha_factor = alpha_factor,
                     i = i,
                     c = c) %>%
    add_the_steering_axis_to_the_drawing(df,
                                         wheel_diameter = wheel_diameter,
                                         alpha_factor = alpha_factor,
                                         i = i,
                                         c = c) %>%
    add_the_fork_rake_to_the_drawing(df,
                                     wheel_diameter = wheel_diameter,
                                     alpha_factor = alpha_factor,
                                     i = i,
                                     c = c) %>%
    draw_the_true_fork(df,
                       wheel_diameter = wheel_diameter,
                       alpha_factor = alpha_factor,
                       i = i,
                       c = c) %>%
    add_the_fork_trail(df,
                       wheel_diameter = wheel_diameter,
                       alpha_factor = alpha_factor,
                       i = i,
                       c = c) %>%
    add_the_wheels(df,
                   bb_shell_diameter = bb_shell_diameter,
                   wheel_diameter = wheel_diameter,
                   alpha_factor = alpha_factor,
                   i = i,
                   c = c)

  metrics <- df %>%
    get_all_the_metrics(angle_btw_css = angle_btw_css,
                        wheel_diameter = wheel_diameter,
                        crank_arm_length = crank_arm_length,
                        old_spacing = old_spacing,
                        bbs_width = bbs_width) %>%
    round()

  list(data = df,
       picture = pic,
       metrics = metrics)
}
