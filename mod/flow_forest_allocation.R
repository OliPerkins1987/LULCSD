

###################################################################################

### Calculates allocation of nursery stocks to forest planting

###################################################################################


forest_la <- function(sf, plt) {
  
  Proj.area <- unlist(lapply(sf, function(x) {
    
    delta <- x@s_parameters$s_new_planting
    
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
  


forest_allocation <- function(s_forest, s_lfa_forest, p_land_trans) {
  
  s.fam   <- list(s_forest, s_lfa_forest)
  
  if(any(unlist(lapply(s.fam, function(z) {z@s_parameters$s_new_planting}))> 0)) {
  
  s.fam <- forest_la(s.fam, p_land_trans)
  
  }
  
  return(list(s_forest     = list(s_area = s.fam[[1]]@s_parameters$s_area), 
              s_lfa_forest = list(s_area = s.fam[[2]]@s_parameters$s_area)))
  
}



