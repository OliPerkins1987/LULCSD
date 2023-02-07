

###################################################################################

### Captures yield tradeoffs of marginal lands

###################################################################################

biomass_carbon <- function(ff_) {

  ################################
  ### calculate biomass carbon flows
  ################################
  
  sf <- lapply(ff_, function(z) {
    
    Carbon.weight <- sapply(z, function(cs) {cs@s_parameters$s_biomass_carbon_flux})
    
    for(s in 1:length(z)) {
      
      if(z[[s]]@s_parameters$s_commodity == 'Biomass') {
        
        if(is.null(z[[s]]@s_parameters$s_LULC)) {
          
          ### 3.67: tC -> tCO2eq
          veg.carbon                           <- 3.67 * (z[[s]]@s_parameters$s_yield * z[[s]]@s_parameters$s_area * z[[s]]@s_parameters$s_lca_efficiency)
          
          z[[s]]@s_parameters$s_biomass_carbon <- ((z[[s]]@s_parameters$s_biomass_carbon_flux * z[[s]]@s_parameters$s_area) - veg.carbon)
          
        } else {
        
        ### 3.67: tC -> tCO2eq
        veg.carbon                           <- 3.67 * (z[[s]]@s_parameters$s_yield * z[[s]]@s_parameters$s_area * z[[s]]@s_parameters$s_lca_efficiency)
          
        biomass.area   <- z[[s]]@s_parameters$s_LULC
        biomass.area[s]<- z[[s]]@s_parameters$s_area
        
        z[[s]]@s_parameters$s_biomass_carbon <- (sum(biomass.area * Carbon.weight) - veg.carbon) 

        
        }
        
      } 
      
      z[[s]]
      
    }
    
    z
    
  })
    
  sf
  
  return(list(ff_ = list(unlist(sf), c('s_biomass_carbon'))))
  
  
  }







