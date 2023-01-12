

###################################################################################

### Calculate land use changes based on pressures

###################################################################################

### ff_ should be a list of stocks by family

la <- function(sf) {
  
  Proj.area <- unlist(lapply(sf, function(x) {
    
    delta <- x@s_parameters$s_income_pressure
    
    delta <- ifelse(length(delta) == 0, 0, delta)
    
    return((x@s_parameters$s_area + delta))

  }))
    
  ### stop area going negative for a system
  
  Proj.area <- Proj.area[!is.na(Proj.area)]
  
  if(any(Proj.area < 0)) {
    
    neg.area <- sum((Proj.area[which(Proj.area < 0)]))
    n.pos    <- length(Proj.area[Proj.area >= 0])
    
    Proj.area<- ifelse(Proj.area < 0, 0, Proj.area - (neg.area/n.pos))
    
  }
  
  for(i in 1:length(sf)) {
  
  sf[[i]]@s_parameters$s_area <- Proj.area[i]
  
  }
   
  sf
   
}

land_allocation <- function(ff_) {

  ff_     <- lapply(ff_, la)
  
  return(list(ff_ = list(unlist(ff_), c('s_area'))))
  
}



