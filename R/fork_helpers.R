#' Add steering axis, adjusted fork and rake triangle
#'
#' The adjusted fork and the actual fork are two different
#' things. The former is the hypotenuse of a right triangle
#' whose legs are the steering axis and the rake line. The
#' latter is the hypotenuse of a right triangle whose short
#' leg is the rake line and whose long leg runs along the
#' steering axis, but is shorter. The difference is the
#' head tube extension.
#'
#' @param frame_dims A 7 x 3 tibble with frame dimensions from the front
#' and rear triangle, returned by [bicycle::wrap_frame_dims()].
#' @param fork_rake The fork rake in millimeters
#' @seealso [bicycle::wrap_frame_dims()]
#'
#' @return An 11 x 3 tibble.
#' @export
add_steering_axis <- function(frame_dims,
                              fork_rake = 45) {
  # frame_stats is the df you get from combining
  # the front and rear triangle stats. recover the HT angle:
  ht_hp <- frame_dims$ht_triangle[['horizontal_projection']]
  ht_vp <- frame_dims$ht_triangle[['vertical_projection']]
  ht_angle <- 90 - atan(ht_hp/ht_vp)*180/pi

  # step 1: project steering axis SA from HT out to DO height
  sa_vp <- frame_dims$dt_triangle[['vertical_projection']] -
    frame_dims$cs_triangle[['vertical_projection']]
  sa_hp <- sa_vp / tan(ht_angle * pi / 180)
  sa_length <- sa_vp / sin(ht_angle * pi / 180)

  # step 2: extend sa_hp to the front DO by a distance
  # equal to the hypotenuse of the right triangle formed
  # by the fork rake and an extension of the steering
  # axis down to the point where it makes a right angle
  # with the fork rake line. This triangle is similar
  # to the triangle whose hypotenuse is the SA. This
  # means that the length of the hypotenuse is proportional
  # to sa_length, as in
  af_hp_ext <- sa_length * fork_rake / sa_vp
  af_hp <- sa_hp + af_hp_ext
  # adjusted fork length from Pythagoras:
  af_length <- sqrt(sa_vp^2 + af_hp^2)

  # step 3: extend sa_length down to where it needs
  # to be to make a 90-degree angle with the rake line.
  # the short leg of the right triangle with the
  # hypotenuse equal to the horizontal distance
  # af_hp_ext is equal to
  af_l1 <- fork_rake * sa_hp / sa_vp

  # the long leg is just the rake. If I am right,
  # then the sum of the two squared is equal to
  # af_length^2 so Pythagoras should be TRUE but
  # for rounding -- so the ratio below should be ~ 1:
  message(paste('Pythagoras!', (af_l1^2 + fork_rake^2) / (af_hp_ext)^2))

  # now set up triangles for the fork rake
  # and for the SA extension. first, recover
  # the head tube angle from the SA projections
  ht_angle <- 90 - atan(sa_hp/sa_vp)*180/pi
  sa_ext_triangle <- c(length = af_l1,
                       vertical_projection = sin(ht_angle * pi / 180) * af_l1,
                       horizontal_projection = cos(ht_angle * pi / 180) * af_l1)
  rake_triangle <- c(length = fork_rake,
                     vertical_projection = cos(ht_angle * pi / 180) * fork_rake,
                     horizontal_projection = sin(ht_angle * pi / 180) * fork_rake)

  frame_dims <- frame_dims %>%
    add_column(sa_triangle = c(length = sa_length,
                               vertical_projection = sa_vp,
                               horizontal_projection = sa_hp)) %>%
    add_column(af_triangle = c(length = af_length,
                               vertical_projection = sa_vp,
                               horizontal_projection = af_hp)) %>%
    add_column(sa_ext_triangle = sa_ext_triangle,
               rake_triangle = rake_triangle)
  frame_dims
}

#' Add the true fork
#'
#' The fork is specified with a given crown-to-axle length and
#' rake. These determine ride characteristics, wheel base length,
#' and toe overlap. Depending on how long the fork is, the head
#' tube must extend below the point where it meets the down tube
#' so we will have to add that bit to the frame dimensions.
#'
#' @inheritParams add_steering_axis
#' @param fork_cta_length The fork crown-to-axle length in millimeters.
#' @seealso [bicycle::wrap_frame_dims()]
#'
#' @return A 13 x 3 tibble.
#' @export
find_ht_extension_and_add_true_fork <- function(frame_dims,
                                                fork_rake = 45,
                                                fork_cta_length = 390) {
  df <- frame_dims %>% add_steering_axis(fork_rake)
  stopifnot(fork_cta_length < df$af_triangle['length'])
  # the right triangle whose long leg is the SA + SA_ext
  # and short leg is the fork rake has a hypotenuse that
  # goes from the point where HT meets DT to the right
  # end of the fork rake. that hypotenuse is the adjusted
  # fork af, and the length of that is h as shown below,
  # after computing the length of the long leg as a:
  a <- df$sa_triangle[['length']] + df$sa_ext_triangle[['length']]
  h <- sqrt(a^2 + fork_rake^2)

  # but we already have the af length in df, so
  # we can check our math for deriving h above:
  message(paste('h is af', h / df$af_triangle[['length']]))

  # now, some algebra. let's call the fork rake r.
  # inside the right triangle described above, with
  # hypotenuse h, long leg a and short leg r, you
  # can draw another right triangle with the same
  # short leg r and a long leg that is a little shorter
  # than a defined above. We'll call it a - x for now.
  # The hypotenuse of this second right triangle is the
  # `fork_cta_length` so we'll call it f.
  # The difference x is the extra bit of HT you need
  # below the point where DT and HT meet, because f < h.
  # the currently calculated df$ht_triangle['length']
  # plus this x, to be determined, will be the true
  # length of the head tube. Here's how we get x:
  # - two right triangles, two Pythagoras theorems:
  #   h^2 = a^2 + r^2                     (1)
  #   f^2 = (a-x)^2 + r^2                 (2)
  # - subtract 2 from 1 and you get:
  #   (h+f)(h-f) = (a - a + x)(a + a - x) (3)
  # - or
  #   x^2 - 2ax + m = 0                   (4)
  #   where m = (h+f)(h-f)
  # so x is one of the 2 solutions to the quad
  # equation (4):
  m <- (h + fork_cta_length)*(h-fork_cta_length)
  delta = 4*(a^2 - m)
  x = c(a - sqrt(delta)/2, a + sqrt(delta)/2)
  # we'll take the plausible one: it should be
  # positive and small -- less than 50 mm.
  ht_ext_length <- x[(x > 0) & (x < 50)]
  ht_length <- df$ht_triangle[['length']]

  # Now add the HT extension and its projections:
  # they should be all proportional to those of the
  # HT, adjusted by the ratio ht_ext_length/ht_length.
  df <- df %>%
    add_column(ht_ext_triangle = .$ht_triangle * ht_ext_length/ht_length)

  # now add the true rake triangle:
  r_vp <- df$af_triangle[['vertical_projection']] - df$ht_ext_triangle[['vertical_projection']]
  r_hp <- sqrt(fork_cta_length^2 - r_vp^2)
  df %>%
    add_column(f_triangle = c(length = fork_cta_length,
                              vertical_projection = r_vp,
                              horizontal_projection = r_hp))
}

#' Calculate fork trail
#'
#' Given a tibble with all the frame dimensions you get
#' after you run both [bicycle::wrap_frame_dims()] and
#' [bicycle::find_ht_extension_and_add_true_fork()] you
#' can now calculate the fork trail.
#'
#' @param df A tibble with frame dimensions returned by [bicycle::find_ht_extension_and_add_true_fork()].
#' @param wheel_diameter Wheel diameter in millimeters; 622 is the bead seat diameter
#' of a 700c wheel. To see the effect of the tire width, add 2 x tire width in millimeters
#'
#' @return A scalar equal to the fork trail in millimeters
#' @export
calculate_fork_trail <- function(df,
                                 wheel_diameter = 622) {
  # recover the HT angle:
  ht_hp <- df$ht_triangle[['horizontal_projection']]
  ht_vp <- df$ht_triangle[['vertical_projection']]
  ht_angle <- 90 - atan(ht_hp/ht_vp)*180/pi

  # get legs of right triangle
  leg_1 <- (wheel_diameter / 2 + df$rake_triangle[['vertical_projection']])
  leg_2 <- leg_1 / tan(ht_angle * pi / 180)

  # return trail length
  df$sa_triangle[['horizontal_projection']] +
    df$sa_ext_triangle[['horizontal_projection']] +
    leg_2 -
    df$af_triangle[['horizontal_projection']]
}
