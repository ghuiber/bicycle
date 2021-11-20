test_that("get_bb_drop() errors if any input < 0", {
  expect_error(get_bb_drop(60, 72, 430, -10))
})
