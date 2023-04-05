test_that("biodiversity income calculation works", {

  urban_fraction <- 0.001 * 2 ##0.1% if UK land area urbanised in a year, 2 biodiversity units per ha
  pbp            <- 15000/30  ### biodiversity price per unit -> income over 30 years
  pbc            <- 0.5       ### cost to produce unit of biodiversity
  s_biod_yield   <- c(2, 6, 50, 100) ### different biodiversity yields
  pbs            <- 10              ### biodiversity subsidy
  s_CDR          <- c(0, 1, 1, 1)    ### is the LU an environmental LUC (CDR) option?
  is.biomass     <- !(c(FALSE, TRUE, FALSE, FALSE))

  biodiversity_income <- (pbp*pbc) * s_biod_yield * urban_fraction +
    (pbs*s_CDR*s_biod_yield*is.biomass)

  expect_equal(biodiversity_income, c(1, 3, 525, 1050))


})
