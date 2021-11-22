test_that("get_bb_tt_diagonal() errors if any angle >= 90", {
  expect_error(get_bb_tt_diagonal(500, 530, 73, 90))
})
