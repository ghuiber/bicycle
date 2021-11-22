#' Get the over-locknuts-dimension (OLD) spacing
#'
#' In the plane that is perpendicular onto the bike, the chain
#' stay is part of a trapezoid that is as wide as half the OLD
#' spacing at the dropout and half the bottom bracket shell
#' center offset at the bottom bracket. The OLD spacing is standard
#' (130 mm for road bikes, 135 mm for old-school mountain bikes,
#' 142 mm for newer mountain bikes, etc.). So the OLD spacing is
#' an input in frame design, not a derived dimension. This function
#' is only useful as a sanity check, to make sure that you hit the
#' right OLD number with a given set of values for its arguments.
#'
#' @param cs_length Chain stay length in millimeters.
#' @param angle_btw_css Angle between chain stays, in degrees.
#' @param bbsc_offset The bottom bracket shell offset in millimeters.
#'
#' @return A scalar equal to the OLD spacing in millimeters.
#' @export
#'
#' @examples
#' get_old_spacing(450, 10, 10)
get_old_spacing <- function(cs_length,
                            angle_btw_css,
                            bbsc_offset) {
  # some reasonable conditions
  stopifnot(cs_length > 0)
  stopifnot(angle_btw_css > 0 && angle_btw_css < 30)
  stopifnot(bbsc_offset > 0 && bbsc_offset < 20)

  # At a given angle between the chainstays,
  # the longer the chainstays the wider the
  # over-locknut-dimension (OLD) spacing.
  # For road bikes this should be 130 mm;
  # for old-school mountain bikes, 135 mm;
  # and for Surly gnot-rite, 132.5 mm.
  2 * (cs_length * sin((angle_btw_css / 2)*pi/180) + bbsc_offset)
}

#' Get the chain stay length
#'
#' In the plane that is perpendicular onto the bike, the chain
#' stay is part of a trapezoid that is as wide as half the OLD
#' spacing at the dropout and half the bottom bracket shell
#' center offset at the bottom bracket. The OLD spacing is standard
#' (130 mm for road bikes, 135 mm for old-school mountain bikes,
#' 142 mm for newer mountain bikes, etc.). So the OLD spacing is
#' an input in frame design, not a derived dimension. If you have
#' a bottom bracket shell with a given angle between chain stays
#' (`angle_btw_css`) and center offset (`bbsc_offset`) then you
#' must use this function to determine the chain stay length that
#' will produce the OLD intended for your frame.
#'
#' @inheritParams get_old_spacing
#' @param old_spacing Over-locknut-dimension (hub width) in millimeters.
#'
#' @return A scalar equal to the chain stay length in millimeters.
#' @export
#'
#' @examples
#' get_cs_length(130, 10, 10)
get_cs_length <- function(old_spacing,
                          angle_btw_css,
                          bbsc_offset) {
  # you can also guess at the cs length you need
  # if you have a BB shell with a given angle btw
  # chain stays and need a given old_spacing
  (old_spacing/2 - bbsc_offset) /sin((angle_btw_css / 2)*pi/180)
}

#' Get the bottom bracket shell offset `bbsc_offset`
#'
#' In the plane that is perpendicular onto the bike, the chain
#' stay is part of a trapezoid that is as wide as half the OLD
#' spacing at the dropout and half the bottom bracket shell
#' center offset at the bottom bracket. The OLD spacing is standard
#' (130 mm for road bikes, 135 mm for old-school mountain bikes,
#' 142 mm for newer mountain bikes, etc.). So is the width of the
#' bottom bracket (BB) shell (68 mm for road bikes, 73 mm for old-school
#' mountain bikes, etc.).
#'
#' Given a target OLD and a bottom bracket shell of a given width,
#' given angle between chain stays, and a chosen chain stay length,
#' you can derive the `bbsc_offset` that suits. When you build a
#' lugged frame, this function is just a sanity check, because your
#' bottom bracket shell lug will come with a given `bbsc_offset`).
#'
#' When you build a welded frame and you have a choice of `bbsc_offset`,
#' you must set it subject to the constraint that you must leave some
#' room between the centers of the chain stays and the faces of the BB
#' shell. This function forces you to leave 19 mm, which should be
#' enough for chain stays of typical outer diameter at the BB shell end.
#'
#' @inheritParams get_old_spacing
#' @inheritParams get_cs_length
#' @param bbs_width Bottom bracket shell width.
#'
#' @return A scalar equal to the bottom bracket shell offset in millimeters.
#' @export
#'
#' @examples
#' get_bbsc_offset(130, 14, 450, 73)
get_bbsc_offset <- function(old_spacing,
                            angle_btw_css,
                            cs_length,
                            bbs_width = 68) {
  # you can also guess at the bbsc offset
  bbsc_offset <- old_spacing/2 - cs_length * sin((angle_btw_css / 2)*pi/180)

  # but make sure you have room: leave at least
  # 19 mm between this offset and the BBS face
  stopifnot(bbsc_offset <= bbs_width / 2 - 19)
  bbsc_offset
}
