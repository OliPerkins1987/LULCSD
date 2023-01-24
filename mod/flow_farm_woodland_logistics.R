

###################################################################################

### Simple process for calculating production by commodity

###################################################################################

farm_woodland_logistics <- function(s_forest, s_farm_woodland_cropland, 
        s_farm_woodland_grassland, s_lfa_farm_woodland_cropland, s_lfa_farm_woodland_grassland) {

  ################################
  ### apply woodland constraint
  ################################
  
  woodland <- list(s_farm_woodland_cropland, s_farm_woodland_grassland, 
                   s_lfa_farm_woodland_cropland, s_lfa_farm_woodland_grassland)
  
  woodland <- lapply(woodland, 
                     function(y) y@s_parameters$s_income_pressure)
  
  wood.thresh <- s.list[grepl('farm_woodland', sapply(s.list, 
                                                      function(z) {z@s_name}))][[1]]@s_parameters$s_logistics_constraint
  
  wood.tot    <- sum(unlist(woodland[which(unlist(woodland) > 0)]))
  
  if(wood.tot > wood.thresh) {
    
    woodland <- lapply(woodland, function(z) ifelse(z > 0, z / (wood.tot/wood.thresh), z))

    for(w in 1:length(woodland)) {
      
      woodland[[w]] <- woodland[[w]]
      
    }
    
    
  } 
  
  
  return(list(s_farm_woodland_cropland  = list(s_logistics_constraint = woodland[[1]]), 
              s_farm_woodland_grassland = list(s_logistics_constraint = woodland[[2]]), 
              s_lfa_farm_woodland_cropland  = list(s_logistics_constraint = woodland[[3]]), 
              s_lfa_farm_woodland_grassland = list(s_logistics_constraint = woodland[[4]])))
  
  
  }







