

###################################################################################

### Captures yield tradeoffs of marginal lands

###################################################################################

ILUC_carbon <- function(ff_) {

  
  sf <- lapply(ff_, function(z) {
    
    ILUC <- lapply(z, function(s) {
      
      if(s@s_parameters$s_surplus != 0) {
      
      s@s_parameters$s_ILUC_carbon <-  (0 - s@s_parameters$s_surplus * s@s_parameters$s_demand) * s@s_parameters$s_ILUC_factor
     
      } else {
        
        s@s_parameters$s_ILUC_carbon <- 0
        
      }
      
      s
      
    })
    
    ILUC
    
  })
  
  
  return(list(ff_ = list(unlist(sf), c('s_ILUC_carbon'))))
  
  
}







