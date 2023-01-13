

###################################################################################

### Economic return for land conversion to CDR

###################################################################################

### converts income pressure to a hectares change
### ff_ should be a list of stocks by family

CDR_income <- function(ff_, i_demand_carbon) {

  #calculates income per ha of land converted to CDR
  
  calc_CDR_income <- function(sf, idc) {
    
    sf <- lapply(sf, function(x) {
      
      lapply(x, function(y) {
        
        s <- y@s_parameters

        y@s_parameters$s_carbon_income <- s$s_CDR * idc
        
        y
        
      })
      
    })
    
    
    return(sf)
    
  }
  
  
  
  s.fam    <- calc_CDR_income(ff_, i_demand_carbon@i_current_val)

    return(list(ff_ = list(unlist(s.fam), c('s_carbon_income'))))
  
}



