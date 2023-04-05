test_that("ILUC calculation works", {

  surplus     <- c(0.2, -0.35, 0.1, -0.05) ### fractional
  demand      <- c(1000, 2000, 100, 200) ### Mg T consumption
  ILUC_factor <- c(3, 5, 20, 80, 90)

  ILUC_carbon <- 0 - (surplus * demand) * ILUC_factor

  expect_equal(ILUC_carbon, c(-600, 3500, -200, 800, -18000))


})
