% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/old_helpers.R
\name{get_bbsc_offset}
\alias{get_bbsc_offset}
\title{Get the bottom bracket shell offset \code{bbsc_offset}}
\usage{
get_bbsc_offset(old_spacing, angle_btw_css, cs_length, bbs_width = 68)
}
\arguments{
\item{old_spacing}{Over-locknut-dimension (hub width) in millimeters.}

\item{angle_btw_css}{Angle between chain stays, in degrees.}

\item{cs_length}{Chain stay length in millimeters.}

\item{bbs_width}{Bottom bracket shell width.}
}
\value{
A scalar equal to the bottom bracket shell offset in millimeters.
}
\description{
In the plane that is perpendicular onto the bike, the chain
stay is part of a trapezoid that is as wide as half the OLD
spacing at the dropout and half the bottom bracket shell
center offset at the bottom bracket. The OLD spacing is standard
(130 mm for road bikes, 135 mm for old-school mountain bikes,
142 mm for newer mountain bikes, etc.). So is the width of the
bottom bracket (BB) shell (68 mm for road bikes, 73 mm for old-school
mountain bikes, etc.).
}
\details{
Given a target OLD and a bottom bracket shell of a given width,
given angle between chain stays, and a chosen chain stay length,
you can derive the \code{bbsc_offset} that suits. When you build a
lugged frame, this function is just a sanity check, because your
bottom bracket shell lug will come with a given \code{bbsc_offset}).

When you build a welded frame and you have a choice of \code{bbsc_offset},
you must set it subject to the constraint that you must leave some
room between the centers of the chain stays and the faces of the BB
shell. This function forces you to leave 19 mm, which should be
enough for chain stays of typical outer diameter at the BB shell end.
}
\examples{
get_bbsc_offset(130, 14, 450, 73)
}
