
###################################################################################

### Calculates land fraction farmers are open to converting to CDR

###################################################################################

### uptake of enviro schemes

sum_CDR_uptake <- function(sf) {
  
  sf <- lapply(sf, function(x) {
    
    CDR_will_tot <- sum(unlist(lapply(x, function(z) {
      
      if(z@s_parameters$s_CDR == 1) {
        
        return(z@s_parameters$s_CDR_will)
        
      } else if (z@s_parameters$s_CDR == 0) {
        
        return(as.numeric(0))
        
      }
      
    })), na.rm = T)
    
    lapply(x, function(y) {
      
      s <- y@s_parameters
      
      y@s_parameters$CDR_tot <- CDR_will_tot
      
      y
      
    })
    
  })
  
  
  return(sf)
  
}

### calculate tig parameter update

calc_tig <- function(sf, b_tig) {
  
  sf <- lapply(sf, function(x) {
    
          lapply(x, function(y) {
            
            s <- y@s_parameters
            
            tig.increment <- b_tig + s$CDR_tot
            
            y@s_parameters$s_trust_in_govt <- s$s_trust_in_govt + tig.increment
            
            y
            
          })

      })
  
  
  return(sf)
  
}


##################################################################################

### converts behavioural drivers to a theoretical hectares' CDR uptake
### ff_ should be a list of stocks by family

##################################################################################

trust_in_govt <- function(ff_, p_baseline_tig_update) {

  
  s.fam   <- sum_CDR_uptake(ff_)
  s.fam   <- calc_tig(s.fam, p_baseline_tig_update)

  
  return(list(ff_ = list(unlist(s.fam), c('s_trust_in_govt'))))
  
  
}



