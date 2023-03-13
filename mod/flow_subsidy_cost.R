

###################################################################################

### calculates total cost of agricultural subsidies

###################################################################################

subsidy_cost <- function(ff_, p_biodiversity_subsidy) {

  
  sf <- lapply(ff_, function(z) {
    
    subs <- lapply(z, function(s) {
      
        s@s_parameters$s_subsidy_cost <- s@s_parameters$s_area * s@s_parameters$s_subsidy + 
                           s@s_parameters$s_biodiversity_units * s@s_parameters$s_CDR * p_biodiversity_subsidy

        s
      
    })
    
    subs
    
  })
  
  
  return(list(ff_ = list(unlist(sf), c('s_subsidy_cost'))))
  
  
}







