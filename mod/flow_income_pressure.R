

###################################################################################

### Simple process for translating price difference to pressure on land

###################################################################################

### currently assumes that the area of land available per system is ~static
#? how to change this?

### How should stocks be fed demand information? Internally?

#calculates income pressure on land use allocation
#calculates ratio of income per farm to mean income per farm for the land system

calc_income <- function(sf) {
  
  sf <- lapply(sf, function(x) {
    
          lapply(x, function(y) {
            
            s <- y@s_parameters
            
            if(is.null(s$s_size)) {
              
              xs <- unlist(lapply(x, function(z) {z@s_parameters$s_size}))
              xs <- mean(xs[!is.null(xs)], na.rm = T)
              
              s$s_size <- xs
              
            }
            
            y@s_parameters$s_income <- ((s$s_yield * s$s_price * s$s_margin) + s$s_subsidy + s$s_carbon_income) * s$s_size 
            
            y
            
          })

      })
  
  
  return(sf)
  
}


calc_ip <- function(sf) {
  
  sf <- lapply(sf, function(x) {
    
    income <- unlist(lapply(x, function(z) {z@s_parameters$s_income}))
    income <- income[which(sapply(x, function(z) {z@s_parameters$s_area})> 0)]
    
    lapply(x, function(y, ip = income) {
      
      y@s_parameters$s_income_pressure <- (y@s_parameters$s_income / mean(ip, na.rm= T)) - 1
      
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

  lapply(land_stock, function(l) {
    
    ifelse(length(l) == 0, 0, l)
    
        })
  
 }
  
### converts income pressure to a hectares change
### ff_ should be a list of stocks by family

income_pressure <- function(ff_, p_land_trans) {

  s.fam   <- calc_income(ff_)
  s.fam   <- calc_ip(s.fam)
  s.small <- lc_stock(s.fam) 
  
  ###
  
  ### convert price pressure to hectares' change
  for(i in 1:length(s.fam)) {
    
    for(j in 1:length(s.fam[[i]])) {
    
      if(unlist(s.small[[i]]) == 0) {
        
        s.fam[[i]][[j]]@s_parameters$s_income_pressure <- 0
        
      } else {
      
    s.fam[[i]][[j]]@s_parameters$s_income_pressure <- (s.fam[[i]][[j]]@s_parameters$s_income_pressure * 
                                                          p_land_trans *
                                                           s.fam[[i]][[unlist(s.small[[i]])]]@s_parameters$s_area)
    
        }
    
      }
  
  }

  ### how do we ensure results are returned in the right order?
  
    return(list(ff_ = list(unlist(s.fam), c('s_income', 's_income_pressure'))))
  
}



