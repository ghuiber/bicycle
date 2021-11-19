get_bb_drop <- function(cs_st_angle,
                        st_angle,
                        cs_length,
                        angle_btw_css) {
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
