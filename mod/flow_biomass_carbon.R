

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
          
          z[[s]]@s_parameters$s_biomass_carbon <- (z[[s]]@s_parameters$s_biomass_carbon_flux * z[[s]]@s_parameters$s_area) 
          
        } else {
        
        biomass.area   <- z[[s]]@s_parameters$s_LULC
        biomass.area[s]<- z[[s]]@s_parameters$s_area
        
        z[[s]]@s_parameters$s_biomass_carbon <- (sum(biomass.area * Carbon.weight))

        
        }
        
      } 
      
      z[[s]]
      
    }
    
    z
    
  })
    
  sf
  
  return(list(ff_ = list(unlist(sf), c('s_biomass_carbon'))))
  
  
  }







