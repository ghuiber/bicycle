#' bicycle: A package for designing bicycle frames
#'
#' The foo package provides three categories of important functions:
#' frame dimension calculations, frame design, and plotting.
#'
#' @section Frame dimension calculations:
#' The bicycle frame geometry is subject to some constraints, such as
#' when you form a triangle (between the chain stay, seat stay, and
#' seat tube) the angles must sum to 180 degrees. These constraints
#' are met when only some, not all, of the frame parameters are inputs,
#' chosen arbitrairly. The remaining frame parameters are outputs,
#' calculated such that the constraints are met. Functions in this
#' section allow you to derive some dimensions from others.
#'
#' @section Frame design:
#' This is where the frame tube dimensions are organized so you can
#' pass them along to plots or as inputs to CAD software.
#'
#' @section Plotting:
#' You can use tibbles returned from the frame design functions as
#' inputs to ggplot objects that can either sketch a frame, part of a
#' frame, or overlay two frames onto the same plot for a simple
#' pairwise visual comparison of standover clearance, wheelbase, etc.
#'
#'
#' @docType package
#' @name bicycle
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @import dplyr
#' @import tibble
#' @import ggplot2
#' @import ggforce
#' @import stringr
NULL
#> NULL
