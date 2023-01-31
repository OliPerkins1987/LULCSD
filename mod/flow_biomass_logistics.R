

###################################################################################

### Captures yield tradeoffs of marginal lands

###################################################################################

biomass_logistics <- function(s_biomass_cropland, s_biomass_grassland, 
                              s_lfa_biomass_cropland, s_lfa_biomass_grassland) {

  ################################
  ### apply biomass constraint
  ################################
  
  biomass <- list(s_biomass_cropland, s_biomass_grassland, 
                  s_lfa_biomass_cropland, s_lfa_biomass_grassland)
  
  biomass <- lapply(biomass, function(z) {
    
   z@s_parameters$s_biomass_max - z@s_parameters$s_area
    
  })
  
  return(list(s_biomass_cropland  = list(s_logistics_constraint = biomass[[1]]), 
              s_biomass_grassland = list(s_logistics_constraint = biomass[[2]]), 
              s_lfa_biomass_cropland  = list(s_logistics_constraint = biomass[[3]]), 
              s_lfa_biomass_grassland = list(s_logistics_constraint = biomass[[4]])))
  
  
  }







