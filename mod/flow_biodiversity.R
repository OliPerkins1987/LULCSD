

###################################################################################

### Captures yield tradeoffs of marginal lands

###################################################################################

biodiversity <- function(ff_) {

  ################################
  ### calculate biomass carbon flows
  ################################
  
  sf <- lapply(ff_, function(z) {
    
    for(s in 1:length(z)) {
      
      z[[s]]@s_parameters$s_biodiversity <- z[[s]]@s_parameters$s_biodiv_factor * z[[s]]@s_parameters$s_area
        
      } 
      
    z
    
  })
  
  
  return(list(ff_ = list(unlist(sf), c('s_biodiversity'))))
  
  
  }







