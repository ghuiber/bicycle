% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_helpers.R
\name{big_bicycle}
\alias{big_bicycle}
\title{Get all the things}
\usage{
big_bicycle(
  st_length = 500,
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
  c = I("black"),
  alpha_factor = 1,
  bb_shell_diameter = 34.8,
  crank_arm_length = 170,
  old_spacing = 130,
  bbs_width = 68
)
}
\arguments{
\item{st_length}{The length of the seat tube ST in millimeters.}

\item{tt_length}{The length of the top tube TT in millimeters.}

\item{st_angle}{The angle between the seat tube and the horizontal.}

\item{tt_angle}{The angle between the top tube and the horizontal.}

\item{ht_angle}{Head tube angle with the horizontal, in degrees.}

\item{st_dt_angle}{Angle between the seat tube and the down tube, in degrees.}

\item{cs_st_angle}{Angle between chain stay and seat tube, in degrees.}

\item{cs_length}{Chain stay length in millimeters.}

\item{angle_btw_css}{Angle between chain stays, in degrees.}

\item{fork_rake}{The fork rake in millimeters}

\item{fork_cta_length}{The fork crown-to-axle length in millimeters.}

\item{wheel_diameter}{Wheel diameter in millimeters; 622 is the bead seat diameter
of a 700c wheel. To see the effect of the tire width, add 2 x tire width in millimeters
-- i.e. specify \code{wheel_diameter = 622 + 2 * 32} for a 32 mm tire.}

\item{c}{The color of the line segments.}

\item{alpha_factor}{The transparency of the line segments, \code{alpha}.}

\item{bb_shell_diameter}{Outer diameter of the BB shell in millimeters.}

\item{crank_arm_length}{The crank arm length in millimeters. Typical values are 165, 170 and 175 for adult riders.}

\item{old_spacing}{Over-locknut-dimension (hub width) in millimeters.}

\item{bbs_width}{Bottom bracket shell width. It's standard -- 68 mm is typical for road bikes.}
}
\value{
A named list
}
\description{
A wrapper for tighter control over design inputs. Returns a named
list of three objects: the frame design tibble, the ggplot drawing,
and a named vector of useful frame metrics.
}
