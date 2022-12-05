

###################################################################################

### Simple process for translating price difference to cost pressure

###################################################################################

#calculates income pressure on land use allocation

income_pressure <- function(sf) {
  
  sf <- lapply(sf, function(x) {
    
    income <- unlist(lapply(x, function(z) {z@s_parameters$s_income}))
    
    lapply(x, function(y, ip = income) {
      
      y@s_parameters$s_income_pressure <- 1 - (y@s_parameters$s_income / mean(ip, na.rm= T))
      
      y
    })
    
  })
  
  
  sf
  
}


lc_stock <- function(sf) {
  
  #helper to find supply of less competitive land system for change
  
  land_stock <- lapply(sf, function(x) {
    
                  which.min(unlist(lapply(x, function(y) {
    
                    y@s_parameters$s_income
    
                      })))
  
                        })

 
  
   
}
  
### converts income pressure to a hectares change

land_pressure <- function(p_land_trans = 0.0035, ...) {

  l <- list(...)

  s.fam   <- stock_by_family(l)
  s.fam   <- income_pressure(s.fam)
  s.small <- lc_stock(s.fam) 
  
  for(i in 1:length(s.fam)) {
    
    delta_area <- s.fam[[i]][[s.small[[i]]]]@s_parameters$s_area
    
    for(j in 1:length(s.fam[[i]])) {
    
    s.fam[[i]][[j]]@s_parameters$s_income_pressure <- (s.fam[[i]][[j]]@s_parameters$s_income_pressure) * 
                                                          p_land_trans * delta_area
    
      }
  
  }

    return(unname(unlist(s.fam)))
  
}



