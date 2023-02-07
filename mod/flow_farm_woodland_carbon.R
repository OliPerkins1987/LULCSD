

###################################################################################

### Captures yield tradeoffs of marginal lands

###################################################################################

farm_woodland_carbon <- function(s_farm_woodland_cropland, 
                           s_farm_woodland_grassland, s_lfa_farm_woodland_cropland, s_lfa_farm_woodland_grassland) {

  ################################
  ### calculate woodland Carbon seq
  ################################
  woodland <- list(s_farm_woodland_cropland, s_farm_woodland_grassland, 
                   s_lfa_farm_woodland_cropland, s_lfa_farm_woodland_grassland)
  
  woodland <- lapply(woodland, 
                     function(y) 0 - (y@s_parameters$s_area * y@s_parameters$s_carbon_yield))
  
  
  return(list(s_farm_woodland_cropland  = list(s_farm_woodland_carbon = woodland[[1]]), 
              s_farm_woodland_grassland = list(s_farm_woodland_carbon = woodland[[2]]), 
              s_lfa_farm_woodland_cropland  = list(s_farm_woodland_carbon = woodland[[3]]), 
              s_lfa_farm_woodland_grassland = list(s_farm_woodland_carbon = woodland[[4]])))
  
  
  }







