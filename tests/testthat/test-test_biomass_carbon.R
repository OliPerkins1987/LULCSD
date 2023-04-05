test_that("biomass carbon calculation works", {

  yield <- 10 ## Tonnes per ha of dry mass
  area  <- 100 ### Kha of biomass production
  LCA   <- 0.5 * 0.47 ### 50% of biomass is carbon * 47% capture efficiency through BECCS

  ### 3.67 converts Carbon to CO2
  biomass_carbon <- yield * area * LCA * 3.67

  ### 'biomass carbon flux' Carbon released to atmosphere from soil under miscanthus planting
  SOC_delta <- 5

  ### net atmospheric carbon combination of removals/additions from soil carbon and removals in vegetation
  net_carbon<- (SOC_delta * area) - biomass_carbon

  expect_equal(net_carbon, 500-862.45)


  })
