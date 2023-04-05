test_that("income from carbon credit calculation works", {

  CDR         <- c(FALSE, FALSE, TRUE, TRUE, TRUE)
  idc         <- 50
  carbon_cost <- c(0, 0, 12.5, 100, 25)
  carbon_yield<- c(0, 0, 15, 0, 22) ## biomass carbon credits claimed by energy company
  carbon_income <- CDR * (idc-carbon_cost) * carbon_yield

  expect_equal(carbon_income, c(0, 0, 562.5, 0, 550.0))

})
