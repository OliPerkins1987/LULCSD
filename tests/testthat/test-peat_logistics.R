test_that("peat logistics calculation works", {

 available.peat <- c(200, 50, 0, 0)
 peat.LULC      <- c(50, 60, 10, 0)

 peat.LULC      <- ifelse(peat.LULC > available.peat, available.peat, peat.LULC)

 expect_equal(peat.LULC, c(50, 50, 0, 0))

})
