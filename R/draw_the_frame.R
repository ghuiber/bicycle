#' Overlay a new bicycle frame onto an existing plot
#'
#' The bicycle frame is anchored on the plot at the rear dropout. Imagine it
#' standing on the floor with the rear wheel against the wall. If the floor is
#' the x axis and the wall is the y axis, then the (x, y) coordinates of the
#' rear dropout are (x = wheel diameter / 2, y = wheel diameter / 2). That
#' is the first point we draw and everything else is in reference to it.
#'
#' @param x An existing ggplot object. At a minimum, it's a scatter plot of
#' the rear dropout center. But it can be a bicycle frame of a previous design
#' already drawn, onto which you can now overlay a new design using the dims
#' in `df` for easy comparison.
#' @param df A frame design tibble, such as the minimal one drawn by [bicycle::wrap_frame_dims()]
#' or the one augmented by [bicycle::find_ht_extension_and_add_true_fork()].
#' @param wheel_diameter Wheel diameter in millimeters; 622 is the bead seat diameter
#' of a 700c wheel. To see the effect of the tire width, add 2 x tire width in millimeters
#' -- i.e. specify `wheel_diameter = 622 + 2 * 32` for a 32 mm tire.
#' @param c The color of the line segments.
#' @param alpha_factor The transparency of the line segments, `alpha`.
#'
#' @return A ggplot object.
#' @seealso [ggplot2::ggplot()]
#' @export
overlay_the_bicycle <- function(x,
                                df,
                                wheel_diameter = 622,
                                c = I('black'),
                                alpha_factor = 1) {
  # first, the easy parts: add the 3 tubes that
  # go into the bottom bracket. it's the same
  # recipe for all three because they form
  # triangles with a common corner in the BB.
  # all three are right triangles whose legs --
  # a horizontal projection (hp) and a vertical
  # projection (vp) of the tube of interest --
  # are parallel to the axes. so you draw the
  # tubes between the (x, y) coordinates of
  # the projections shifted by i from (0, 0)
  i <- wheel_diameter/2

  # add the chain stay CS:
  cs_plot <- x +
    geom_segment(aes(x = i,
                     y = i,
                     xend = i + cs_triangle['horizontal_projection'],
                     yend = i - cs_triangle['vertical_projection'],
                     colour = c),
                 alpha = alpha_factor,
                 data = df)

  # add the seat tube ST:
  st_plot <- cs_plot +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'],
                     y = i -
                       cs_triangle['vertical_projection'] +
                       st_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'],
                     yend = i -
                       cs_triangle['vertical_projection'],
                     colour = c),
                 alpha = alpha_factor,
                 data = df)

  # add the down tube DT. notice it points forward, so
  # move the x coordinate of its higher end to the right
  # with a + sign, not to the left as with the CS and ST:
  dt_plot <- st_plot +
    geom_segment(aes(x = i + cs_triangle['horizontal_projection'],
                     y = i - cs_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'],
                     yend = i -
                       cs_triangle['vertical_projection'] +
                       dt_triangle['vertical_projection'],
                     colour = c),
                 alpha = alpha_factor,
                 data = df)

  # add the seat stay SS. no need to draw its
  # projections because the intersecting CS hp
  # and ST vp will do the same job. in fact,
  # there's no need for any of the data in the
  # ss_triangle column because we have already
  # drawn the ends of the segment when we drew
  # the CS and the ST, so we can just join them.
  # but using the ss_triangle data is a good
  # way to check that the math is correct so far:
  # if the seat stay that start at the top of the
  # seat tube ends at the dropout exactly, then
  # all is well and we got the SS length right.
  ss_plot <- dt_plot +
    geom_segment(aes(x = i,
                     y = i,
                     xend = i +
                       ss_triangle['horizontal_projection'],
                     yend = i +
                       ss_triangle['vertical_projection'],
                     colour = c),
                 alpha = alpha_factor,
                 data = df)

  # add the top tube TT
  tt_plot <- ss_plot +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'],
                     y = i -
                       cs_triangle['vertical_projection'] +
                       st_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'] +
                       tt_triangle['horizontal_projection'],
                     yend = i -
                       cs_triangle['vertical_projection'] +
                       st_triangle['vertical_projection'] +
                       tt_triangle['vertical_projection'],
                     colour = c),
                 alpha = alpha_factor,
                 data = df)

  # add the head tube HT. like with the seat stay SS, we
  # can take a shortcut: the ends of this segment are drawn
  # already. they are the loose ends of the TT and DT. but
  # we'll use ht_triangle to make sure we got the dims right.
  ht_plot <- tt_plot +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'] +
                       tt_triangle['horizontal_projection'],
                     y = i -
                       cs_triangle['vertical_projection'] +
                       st_triangle['vertical_projection'] +
                       tt_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'] +
                       tt_triangle['horizontal_projection'] +
                       ht_triangle['horizontal_projection'],
                     yend = i -
                       cs_triangle['vertical_projection']+
                       st_triangle['vertical_projection'] +
                       tt_triangle['vertical_projection'] -
                       ht_triangle['vertical_projection'],
                     colour = c),
                 alpha = alpha_factor,
                 data = df)

  ht_plot
}

#' Draw a fresh frame onto a blank sheet
#'
#' This wraps [bicycle::overlay_the_bicycle()] for convenience.
#'
#' @inheritParams overlay_the_bicycle
#'
#' @return A ggplot object.
#' @seealso [ggplot2::ggplot()]
#' @export
draw_the_bicycle <- function(df,
                             wheel_diameter = 622,
                             c = I('black'),
                             alpha_factor = 1) {
  # start with the bottom bracket
  i = wheel_diameter / 2
  do_center <- tibble::tibble(x = i,
                              y = i)
  do_plot <- do_center %>%
    ggplot(aes(x = x,
               y = y,
               colour = c)) +
    geom_point(alpha = .5 * alpha_factor)

  do_plot %>%
    overlay_the_bicycle(df,
                        wheel_diameter,
                        c,
                        alpha_factor)
}

#' Add the effective top tube to a frame drawing
#'
#' Useful for slant top tube designs. For horizontal
#' top tube designs there's nothing to add and you
#' can check in `df`: the elements of the `ett_triangle`
#' vector will be all 0.
#'
#' @inheritParams overlay_the_bicycle
#'
#' @return A ggplot object.
#' @seealso [ggplot2::ggplot()]
#' @export
add_the_ett_to_the_drawing <- function(x,
                                       df,
                                       wheel_diameter = 622,
                                       c = I('black'),
                                       alpha_factor = 1) {
  # We start with the seat tube extension
  i <- wheel_diameter / 2
  st_ext_plot <- x +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'] -
                       ett_triangle['horizontal_projection'],
                     y = i -
                       cs_triangle['vertical_projection'] +
                       st_triangle['vertical_projection'] +
                       ett_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'],
                     yend = i -
                       cs_triangle['vertical_projection'] +
                       st_triangle['vertical_projection'],
                     colour = c),
                 alpha = 0.1 * alpha_factor,
                 data = df)

  # The we add the effective top tube
  ett_plot <- st_ext_plot +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'] -
                       ett_triangle['horizontal_projection'],
                     y = i -
                       cs_triangle['vertical_projection'] +
                       st_triangle['vertical_projection'] +
                       ett_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'] +
                       tt_triangle['horizontal_projection'],
                     yend = i -
                       cs_triangle['vertical_projection'] +
                       st_triangle['vertical_projection'] +
                       tt_triangle['vertical_projection'],
                     colour = c),
                 alpha = 0.1 * alpha_factor,
                 data = df)

  ett_plot
}

#' Add the steering axis to a frame drawing
#'
#' _Warning_: this requires a frame design tibble `df` that has the columns
#' added by [bicycle::find_ht_extension_and_add_true_fork()]. A
#' minimal `df` tibble as drawn by [bicycle::wrap_frame_dims()] will
#' not be sufficient.
#'
#' @inheritParams overlay_the_bicycle
#'
#' @return A ggplot object.
#' @seealso [ggplot2::ggplot()]
#' @export
add_the_steering_axis_to_the_drawing <- function(x,
                                                 df,
                                                 wheel_diameter = 622,
                                                 c = I('black'),
                                                 alpha_factor = 1) {
  # We start at the bottom of the head tube HT
  # (same as the top of the down tube DT, easier)
  # and draw the steering axis down to the height
  # of the dropouts first.
  i <- wheel_diameter / 2
  sa_check <- x +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'],
                     y = i -
                       cs_triangle['vertical_projection'] +
                       dt_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       sa_triangle['horizontal_projection'],
                     yend = i -
                       cs_triangle['vertical_projection'] +
                       dt_triangle['vertical_projection'] -
                       sa_triangle['vertical_projection'],
                     colour = c),
                 alpha = 0.1 * alpha_factor,
                 data = df)

  # Now we'll add the steering axis extension
  sa_ext <- sa_check +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       sa_triangle['horizontal_projection'],
                     y = i,
                     xend = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       sa_triangle['horizontal_projection'] +
                       sa_ext_triangle['horizontal_projection'],
                     yend = i -
                       sa_ext_triangle['vertical_projection'],
                     colour = c),
                 alpha = 0.1 * alpha_factor,
                 data = df)
  sa_ext
}

#' Add the fork rake to a frame drawing
#'
#' _Warning_: this requires a frame design tibble `df` that has the columns
#' added by [bicycle::find_ht_extension_and_add_true_fork()]. A
#' minimal `df` tibble as drawn by [bicycle::wrap_frame_dims()] will
#' not be sufficient.
#'
#' @inheritParams overlay_the_bicycle
#'
#' @return A ggplot object.
#' @seealso [ggplot2::ggplot()]
#' @export
add_the_fork_rake_to_the_drawing <- function(x,
                                             df,
                                             wheel_diameter = 622,
                                             c = I('black'),
                                             alpha_factor = 1) {
  # first, add the rake leg of the right triangle
  # whose long leg is the extended steering axis
  i <- wheel_diameter / 2
  rake <- x +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       sa_triangle['horizontal_projection'] +
                       sa_ext_triangle['horizontal_projection'],
                     y = i -
                       sa_ext_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       sa_triangle['horizontal_projection'] +
                       sa_ext_triangle['horizontal_projection'] +
                       rake_triangle['horizontal_projection'],
                     yend = i,
                     colour = c),
                 alpha = 0.1 * alpha_factor,
                 data = df)

  # next, draw the hypotenuse: the adjusted fork. you
  # could use the CS vertical projection for the yend
  # coordinate where you have to end up, as for the
  # rake leg above, but if you use the af dims now
  # this will be an opportunity to check that they are
  # correct. if they are, this will close the sa-r-af
  # triangle. here:
  rake +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'],
                     y = i -
                       cs_triangle['vertical_projection'] +
                       dt_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       af_triangle['horizontal_projection'],
                     yend = i -
                       cs_triangle['vertical_projection'] +
                       dt_triangle['vertical_projection'] -
                       af_triangle['vertical_projection'],
                     colour = c),
                 alpha = 0.1 * alpha_factor,
                 data = df)
}

#' Add the true fork to a frame drawing
#'
#' _Warning_: this requires a frame design tibble `df` that has the columns
#' added by [bicycle::find_ht_extension_and_add_true_fork()]. A
#' minimal `df` tibble as drawn by [bicycle::wrap_frame_dims()] will
#' not be sufficient. If all went well, when we draw a fork starting at
#' the bottom end of the HT extension, with a given rake and cta length,
#' it will end up exactly at the top end of the rake line.
#'
#' @inheritParams overlay_the_bicycle
#'
#' @return A ggplot object.
#' @seealso [ggplot2::ggplot()]
#' @export
draw_the_true_fork <- function(x,
                               df,
                               wheel_diameter = 622,
                               c = I('black'),
                               alpha_factor = 1) {
  # we'll first draw the HT extension, then the real fork
  i <- wheel_diameter / 2
  x +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'],
                     y = i -
                       cs_triangle['vertical_projection'] +
                       dt_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       ht_ext_triangle['horizontal_projection'],
                     yend = i -
                       cs_triangle['vertical_projection'] +
                       dt_triangle['vertical_projection'] -
                       ht_ext_triangle['vertical_projection'],
                     colour = c),
                 alpha = alpha_factor,
                 data = df) +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       ht_ext_triangle['horizontal_projection'],
                     y = i -
                       cs_triangle['vertical_projection'] +
                       dt_triangle['vertical_projection'] -
                       ht_ext_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       ht_ext_triangle['horizontal_projection'] +
                       f_triangle['horizontal_projection'],
                     yend = i -
                       cs_triangle['vertical_projection'] +
                       dt_triangle['vertical_projection'] -
                       ht_ext_triangle['vertical_projection'] -
                       f_triangle['vertical_projection'],
                     colour = c),
                 alpha = alpha_factor,
                 data = df)
}

#' Add the wheels to a frame drawing
#'
#' Adding the wheels to the frame plot may help you get an idea of the overall
#' proportions. Other than that, it's not a very useful thing to do.
#' _Warning_: this requires a frame design tibble `df` that has the columns
#' added by [bicycle::find_ht_extension_and_add_true_fork()]. A
#' minimal `df` tibble as drawn by [bicycle::wrap_frame_dims()] will
#' not be sufficient. If all went well, when we draw a fork starting at
#' the bottom end of the HT extension, with a given rake and cta length,
#' it will end up exactly at the top end of the rake line.
#'
#' @inheritParams overlay_the_bicycle
#' @param bb_shell_diameter Outer diameter of the BB shell in millimeters.
#'
#' @return A ggplot object.
#' @seealso [ggplot2::ggplot()]
#' @export
add_the_wheels <- function(x,
                           df,
                           bb_shell_diameter = 34.8,
                           wheel_diameter = 622,
                           c = I('black'),
                           alpha_factor = 1) {
  i <- wheel_diameter / 2
  bb_circle <- tibble(x = i +
                        df$cs_triangle[['horizontal_projection']],
                      y = i -
                        df$cs_triangle[['vertical_projection']],
                      r = bb_shell_diameter/2)
  rear_wheel <- tibble(x = i,
                       y = i,
                       r = i)
  front_wheel <- tibble(x = i +
                          df$cs_triangle[['horizontal_projection']] +
                          df$dt_triangle[['horizontal_projection']] +
                          df$f_triangle[['horizontal_projection']],
                        y = i,
                        r = i)

  # alpha here won't matter. geom_circle will draw
  # the wheels black for now, but maybe a future
  # release will enable setting alpha. see
  # https://github.com/thomasp85/ggforce/issues/180
  x +
    geom_circle(aes(x0 = x,
                    y0 = y,
                    r = r),
                alpha = 0.1 * alpha_factor,
                data = rear_wheel,
                inherit.aes = FALSE) +
    geom_circle(aes(x0 = x,
                    y0 = y,
                    r = r),
                alpha = 0.1 * alpha_factor,
                data = front_wheel,
                inherit.aes = FALSE) +
    geom_circle(aes(x0 = x,
                    y0 = y,
                    r = r),
                alpha = 0.1 * alpha_factor,
                data = bb_circle,
                inherit.aes = FALSE)
}

#' Add the fork trail to a frame drawing
#'
#' Once you have the fork rake, you might as well visualize the trail.
#' _Warning_: this requires a frame design tibble `df` that has the columns
#' added by [bicycle::find_ht_extension_and_add_true_fork()]. A
#' minimal `df` tibble as drawn by [bicycle::wrap_frame_dims()] will
#' not be sufficient. If all went well, when we draw a fork starting at
#' the bottom end of the HT extension, with a given rake and cta length,
#' it will end up exactly at the top end of the rake line.
#'
#' @inheritParams overlay_the_bicycle
#'
#' @return A ggplot object.
#' @seealso [ggplot2::ggplot()]
#' @export
add_the_fork_trail <- function(x,
                               df,
                               wheel_diameter = 622,
                               c = I('black'),
                               alpha_factor = 1) {
  i <- wheel_diameter / 2
  # recover the HT angle:
  ht_hp <- df$ht_triangle[['horizontal_projection']]
  ht_vp <- df$ht_triangle[['vertical_projection']]
  ht_angle <- 90 - atan(ht_hp/ht_vp)*180/pi

  # extend the steering axis
  h <- i
  sa_vp <- df$sa_triangle['vertical_projection']
  sa_hp <- df$sa_triangle['horizontal_projection']
  rake_point_y <- i -
    df$rake_triangle['vertical_projection']
  rake_point_x <- i +
    df$cs_triangle['horizontal_projection'] +
    df$dt_triangle['horizontal_projection'] +
    df$sa_triangle['horizontal_projection'] +
    df$sa_ext_triangle['horizontal_projection']

  leg_1 <- (i + df$rake_triangle['vertical_projection'])
  leg_2 <- leg_1 / tan(ht_angle * pi / 180)

  # 1. draw a vertical line from the fork DO to the ground
  spoke <- x +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       af_triangle['horizontal_projection'],
                     y = i,
                     xend = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       af_triangle['horizontal_projection'],
                     yend = 0,
                     colour = c),
                 alpha = 0.1 * alpha_factor,
                 data = df)

  # 2. extend the steering axis from the rake point to the ground
  trail <- spoke +
    geom_segment(aes(x = rake_point_x,
                     y = rake_point_y,
                     xend = rake_point_x + leg_2,
                     yend = 0,
                     colour = c),
                 alpha = 0.1 * alpha_factor)

  # 3. draw the horizontal line that connects the bottom of `spoke`
  #    and the bottom of `trail`
  ground <- trail +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       af_triangle['horizontal_projection'],
                     y = 0,
                     xend = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'] +
                       sa_triangle['horizontal_projection'] +
                       sa_ext_triangle['horizontal_projection'] +
                       leg_2,
                     yend = 0,
                     colour = c),
                 alpha = 0.1 * alpha_factor,
                 data = df)
  # here's the trail length, btw:
  trail_length <- (df$sa_triangle['horizontal_projection'] +
                     df$sa_ext_triangle['horizontal_projection'] +
                     leg_2) - df$af_triangle['horizontal_projection']
  ground
}

#' Add any tube extensions to the frame drawing
#'
#' If this is a welded frame, you can set the difference between
#' the top of the head tube and the point where the top tube joins
#' the head tube (measured center-to-center). You can also set the
#' difference between the top of the seat tube and the center of
#' the point where the top tube meets the seat tube. Both of these
#' differences are "tube extensions" that, if set, can be drawn.
#'
#' A complete `df` tibble as drawn by [bicycle::wrap_frame_dims()]
#' is necessary. If it contains no tube extensions, the `x` ggplot
#' object will be returned unchanged.
#'
#' @inheritParams overlay_the_bicycle
#'
#' @return A ggplot object.
#' @seealso [ggplot2::ggplot()]
#' @export
add_the_tube_extensions_to_the_drawing <- function(x,
                                                   df,
                                                   wheel_diameter = 622,
                                                   c = I('black'),
                                                   alpha_factor = 1) {
  tube_extensions <- intersect(names(df), c('tht_triangle', 'tst_triangle'))
  # If there's nothing to draw, exit here:
  if(length(tube_extensions) == 0) return(x)
  # `tube` is a string: one of the elements of tube_extensions
  # but with the first character dropped, so `ht_triangle` for `tht_triangle`
  draw_tube_extension <- function(tube) {
    i <- wheel_diameter / 2
    ttube <- paste0('t', tube)
    if(!(ttube %in% names(df))) return(x)
    x_y <- list(st_triangle = c(x = i +
                                  df$cs_triangle[['horizontal_projection']] -
                                  df$st_triangle[['horizontal_projection']],
                                y = i -
                                  df$cs_triangle[['vertical_projection']] +
                                  df$st_triangle[['vertical_projection']]),
                ht_triangle = c(x = i +
                                  df$cs_triangle[['horizontal_projection']] -
                                  df$st_triangle[['horizontal_projection']] +
                                  df$tt_triangle[['horizontal_projection']],
                                y = i -
                                  df$cs_triangle[['vertical_projection']] +
                                  df$dt_triangle[['vertical_projection']] +
                                  df$ht_triangle[['vertical_projection']])) %>%
      tibble::as_tibble()
    x_y_end <- c(x = x_y[[tube]][['x']] - df[[ttube]][['horizontal_projection']],
                 y = x_y[[tube]][['y']] + df[[ttube]][['vertical_projection']])
    x_y <- x_y %>%
      add_column(x_y_end = x_y_end)
    x +
      geom_segment(aes(x = .data[[tube]]['x'],
                       y = .data[[tube]]['y'],
                       xend = .data[['x_y_end']]['x'],
                       yend = .data[['x_y_end']]['y'],
                       colour = c),
                   alpha = alpha_factor,
                   data = x_y)

  }
   x <- draw_tube_extension('ht_triangle')
   x <- draw_tube_extension('st_triangle')
   x
}
