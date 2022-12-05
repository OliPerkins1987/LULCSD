

###################################################################################

### Simple process for translating price difference to cost pressure

###################################################################################

income_pressure <- function(sf) {
  
  sf <- lapply(sf, function(x) {
    
    income <- unlist(lapply(x, function(z) {z@s_parameters$s_income}))
    
    lapply(x, function(y, ip = income) {
      
      y@s_parameters$s_income_pressure <- y@s_parameters$s_income / mean(ip, na.rm= T)
      
      y
    })
    
  })
  
  
  sf
  
}


land_pressure <- function(s_forest, 
                          s_cereals,
                          s_veg,
                          s_dairy, 
                          s_rg, p_baseline_land_trans = 14.771*1000) {

  s.fam <- stock_by_family(mod = mod)
  s.fam <- income_pressure(s.fam)
  s.fam <- lapply(unlist(s.fam), function(x) {
    
            x@s_parameters$s_income_pressure <- (1-x@s_parameters$s_income_pressure) * p_baseline_land_trans
            x
    
                })
  
    
    return(unlist(s.fam))
  
  }
