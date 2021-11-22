#' Overlay a new bicycle frame onto an existing plot
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
#' returns them as a tibble of four columns, one for each tube, and three
#' rows. Each tube is described as a right triangle made up of the
#' tube length as the hypotenuse and the tube's horizontal and vertical
#' projections as the legs. The elements on each row are named accordingly.
#'
#' @param x An existing ggplot object. At a minimum, it's a scatter plot of
#' the rear dropout center. But it can be a bicycle frame of a previous design
#' already drawn, onto which you can now overlay a new design using the dims
#' in `df` for easy comparison.
#' @param df A frame design tibble, such as drawn by [bicycle::wrap_frame_dims()].
#' @param wheel_diameter Wheel diameter in millimeters; 622 is the bead seat diameter
#' of a 700c wheel. To see the effect of the tire width, add 2 x tire width in millimeters
#' -- i.e. specify `wheel_diameter = 622 + 2 * 32` for a 32 mm tire.
#' @param i The offset from x = 0, so the frame fits nicely in the first quadrant.
#' @param c The color of the line segments.
#' @param alpha_factor The transparency of the line segments, `alpha`.
#'
#' @return A ggplot object.
#' @seealso [ggplot2::ggplot()]
#' @export
overlay_the_bicycle <- function(x,
                                df,
                                wheel_diameter = 622,
                                i = 350,
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
  # the projections shifted by i from x = 0
  # and by wheel_diameter / 2 from y = 0

  # add the chain stay CS:
  cs_plot <- x +
    geom_segment(aes(x = i,
                     y = wheel_diameter / 2,
                     xend = i + cs_triangle['horizontal_projection'],
                     yend = wheel_diameter / 2 - cs_triangle['vertical_projection'],
                     colour = c),
                 alpha = alpha_factor,
                 data = df)

  # add the seat tube ST:
  st_plot <- cs_plot +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'],
                     y = wheel_diameter / 2 -
                       cs_triangle['vertical_projection'] +
                       st_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'],
                     yend = wheel_diameter / 2 -
                       cs_triangle['vertical_projection'],
                     colour = c),
                 alpha = alpha_factor,
                 data = df)

  # add the down tube DT. notice it points forward, so
  # move the x coordinate of its higher end to the right
  # with a + sign, not to the left as with the CS and ST:
  dt_plot <- st_plot +
    geom_segment(aes(x = i + cs_triangle['horizontal_projection'],
                     y = wheel_diameter / 2 - cs_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] +
                       dt_triangle['horizontal_projection'],
                     yend = wheel_diameter / 2 -
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
                     y = wheel_diameter / 2,
                     xend = i +
                       ss_triangle['horizontal_projection'],
                     yend = wheel_diameter / 2 +
                       ss_triangle['vertical_projection'],
                     colour = c),
                 alpha = alpha_factor,
                 data = df)

  # add the top tube TT
  tt_plot <- ss_plot +
    geom_segment(aes(x = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'],
                     y = wheel_diameter / 2 -
                       cs_triangle['vertical_projection'] +
                       st_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'] +
                       tt_triangle['horizontal_projection'],
                     yend = wheel_diameter / 2 -
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
                     y = wheel_diameter / 2 -
                       cs_triangle['vertical_projection'] +
                       st_triangle['vertical_projection'] +
                       tt_triangle['vertical_projection'],
                     xend = i +
                       cs_triangle['horizontal_projection'] -
                       st_triangle['horizontal_projection'] +
                       tt_triangle['horizontal_projection'] +
                       ht_triangle['horizontal_projection'],
                     yend = wheel_diameter / 2 -
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
                             i = 350,
                             c = I('black'),
                             alpha_factor = 1) {
  # start with the bottom bracket
  do_center <- tibble::tibble(x = i,
                              y = wheel_diameter / 2)
  do_plot <- do_center %>%
    ggplot(aes(x = x,
               y = y,
               colour = c)) +
    geom_point(alpha = .5 * alpha_factor)

  do_plot %>%
    overlay_the_bicycle(df,
                        wheel_diameter,
                        i,
                        c,
                        alpha_factor)
}
