

###################################################################################

### Calculates flows between dairy and cropland

###################################################################################

### ff_ should be a list of stocks by family

inter_la <- function(sf, plt) {
  
  for(i in 1:length(sf)) {
    
    sf[[i]][[1]]@s_parameters$crop_dairy <- ((sf[[i]][[1]]@s_parameters$s_income / sf[[i]][[2]]@s_parameters$s_income)-1) * plt
    sf[[i]][[2]]@s_parameters$crop_dairy <- ((sf[[i]][[2]]@s_parameters$s_income / sf[[i]][[1]]@s_parameters$s_income)-1) * plt
    
    small <- which.min(unlist(lapply(sf[[i]], function(x) {x@s_parameters$crop_dairy})))
    big   <- which.max(unlist(lapply(sf[[i]], function(x) {x@s_parameters$crop_dairy})))
    
    sf[[i]][[small]]@s_parameters$crop_dairy <- 0 - (sf[[i]][[big]]@s_parameters$crop_dairy * sf[[i]][[small]]@s_parameters$s_area)
    sf[[i]][[big]]@s_parameters$crop_dairy   <- sf[[i]][[big]]@s_parameters$crop_dairy * sf[[i]][[small]]@s_parameters$s_area
    


  Proj.area <- unlist(lapply(sf[[i]], function(x) {
    
    delta <- x@s_parameters$crop_dairy
    
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
  
  for(j in 1:length(sf[[i]])) {
  
    sf[[i]][[j]]@s_parameters$s_area <- Proj.area[j]
  
    }
   
   }
  
  sf
   
}

inter_system_allocation <- function(s_cereals, s_lfa_cereals, 
                                    s_dairy, s_lfa_dairy, p_land_trans) {
  
  s.fam   <- list('Other' = list(s_cereals, s_dairy), 
                  'LFA'   = list(s_lfa_cereals, s_lfa_dairy))
  
  if(any(unlist(lapply(unlist(s.fam), function(z) {z@s_parameters$s_income}))> 0)) {
  
  s.fam   <- inter_la(s.fam, plt = p_land_trans)
  
  }
  
  return(list(s_cereals     = list(s_area = s.fam[[1]][[1]]@s_parameters$s_area), 
              s_dairy        = list(s_area = s.fam[[1]][[2]]@s_parameters$s_area),
              s_lfa_cereals = list(s_area = s.fam[[2]][[1]]@s_parameters$s_area), 
              s_lfa_dairy    = list(s_area = s.fam[[2]][[2]]@s_parameters$s_area)))
  
}



