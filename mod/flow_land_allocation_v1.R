

###################################################################################

### Calculate land use changes based on pressures

###################################################################################

### ff_ should be a list of stocks by family

la <- function(sf) {
  
  sf <- lapply(sf, function(x) {
    
    delta <- x@s_parameters$s_income_pressure
    
    delta <- ifelse(length(delta) == 0, 0, delta)
    
    x@s_parameters$s_area    <- x@s_parameters$s_area + delta
    
    x
    
  })
    
  sf
  
}

land_allocation <- function(ff_) {

  ff_     <- lapply(ff_, la)
  
  return(list(ff_ = list(unlist(ff_), c('s_area'))))
  
}



