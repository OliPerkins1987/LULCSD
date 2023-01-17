
###################################################################################

### Calculates land fraction farmers are open to converting to CDR

###################################################################################

### how much land is covered by the given CDR?

calc_CDR_frac <- function(sf) {
  
  sf <- lapply(sf, function(x) {
          
          area.CDR <- unlist(lapply(x, function(z) {z@s_parameters$s_area}))
    
          lapply(x, function(y) {
            
            if(y@s_parameters$s_CDR == 1) { 
            
            s                         <- y@s_parameters
            y@s_parameters$s_CDR_frac <- s$s_area / sum(area.CDR, na.rm = T)
            
            } else if(y@s_parameters$s_CDR == 0) {
              
              y@s_parameters$s_CDR_frac <- NA
              
            }
            
            
            y
            
          })

      })
  
  
  return(sf)
  
}


calc_CDR_will <- function(sf, p_CDR) {
  
  sf <- lapply(sf, function(x) {
    
    lapply(x, function(y, area.CDR = area.CDR) {
      
      if(y@s_parameters$s_CDR == 1) { 
        
        s   <- y@s_parameters
        
        ### sigmoid uptake curve
        y@s_parameters$s_CDR_will <- c(1/(1+exp(-p_CDR[1] * (s$s_CDR_frac - p_CDR[2]))))

      } else if(y@s_parameters$s_CDR == 0) {
        
        y@s_parameters$s_CDR_will <- NA
        
      }
      
      
      y
      
    })
    
  })
  
  
  return(sf)
  
}


##################################################################################

### converts behavioural drivers to a theoretical hectares' CDR uptake
### ff_ should be a list of stocks by family

##################################################################################

CDR_willingness <- function(ff_, p_CDR_uptake_a, p_CDR_uptake_b) {

  s.fam   <- calc_CDR_frac(ff_)
  s.fam   <- calc_CDR_will(s.fam, p_CDR = c(p_CDR_uptake_a, p_CDR_uptake_b))

  return(list(ff_ = list(unlist(s.fam), c('s_CDR_will'))))
  
}



