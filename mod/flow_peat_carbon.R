

###################################################################################

### Captures yield tradeoffs of marginal lands

###################################################################################

peat_carbon <- function(s_peat_cropland, 
                           s_peat_grassland, s_lfa_peat_cropland, s_lfa_peat_grassland) {

  ################################
  ### calculate woodland Carbon seq
  ################################
  peat <- list(s_peat_cropland, s_peat_grassland, 
                   s_lfa_peat_cropland, s_lfa_peat_grassland)
  
  peat <- lapply(peat, 
                     function(y) 0 - (y@s_parameters$s_area * y@s_parameters$s_carbon_yield))
  
  
  return(list(s_peat_cropland  = list(s_peat_carbon = peat[[1]]), 
              s_peat_grassland = list(s_peat_carbon = peat[[2]]), 
              s_lfa_peat_cropland  = list(s_peat_carbon = peat[[3]]), 
              s_lfa_peat_grassland = list(s_peat_carbon = peat[[4]])))
  
  
  }







