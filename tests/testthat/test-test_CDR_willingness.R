test_that("test CDR willingness calculation works", {

  area <- c(400, 300, 250, 50, 0)
  CDR  <- c(NA, NA, TRUE, TRUE, TRUE)

  a <- 40
  b <- 0.1

  CDR.frac <- (CDR * area) / sum(area)

  CDR.willingness <- c(1/(1+exp(-a * (CDR.frac - b))))

  expect_equal(round(CDR.willingness,3), c(NA, NA, 0.998, 0.119, 0.018))


  })
