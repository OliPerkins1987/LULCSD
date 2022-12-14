

###################################################################################

### Simple process for calculating production by commodity

###################################################################################

production <- function(ff_, p_price_weight) {

  ### calculates production by land system
  
  produce <- function(s) {
    
    ### where s is a land system
    s@s_parameters$s_production <- s@s_parameters$s_area * s@s_parameters$s_yield * s@s_parameters$s_prod_adjust
    
    return(s)
    
  }
  
  ### production ratio to consumption
  
  calc_surplus <- function(sf) {

    ### a bit fiddly...
    # regroup land systems by production type e.g. cereals (rather than land cover, e.g. cropland)
    # calculate combined production and surplus
    # push back grouped by land cover
    
    ### regroup
    lc    <- unique(gsub('lfa_', '', gsub('s_', '', unlist(lapply(unlist(sf), function(x){x@s_name})))))
    sf.lc <- split(unlist(sf), factor(lc, levels = unique(lc), ordered = T)) 
    
    ### calculate surplus
    p   <- lapply(sf.lc, function(x) {
            sum(unlist(lapply(x, function(y) {y@s_parameters$s_production})))})
                     
    for(i in 1:length(p)) {
      
      for(j in 1:length(sf.lc[[i]])) {
        
        sf.lc[[i]][[j]]@s_parameters$s_production_combined <- p[[i]]
        
        
        surp <- (((sf.lc[[i]][[j]]@s_parameters$s_production_combined - sf.lc[[i]][[j]]@s_parameters$s_demand) / sf.lc[[i]][[j]]@s_parameters$s_demand))
        
        sf.lc[[i]][[j]]@s_parameters$s_surplus <- surp
          
        }
      
      }

    ### return grouped by land cover
    sf <- split(unlist(sf.lc), unlist(lapply(unlist(sf.lc), function(x) {x@s_family})))
    return(sf)
    
  }
  
  
  ### update price
  calc_price <- function(sf, p_pw = p_price_weight) {
    
    sf <- lapply(sf, function(x) {
      
      x@s_parameters$s_price <- x@s_parameters$s_price + (x@s_parameters$s_price  * (0-x@s_parameters$s_surplus) * p_pw)
      
      x@s_parameters$s_price <- ifelse(x@s_parameters$s_price < 0, 0, x@s_parameters$s_price)
      
      x
      
    })
    
    return(sf)
    
  }
  
  
  #####################
  # run flow
  #####################
  
  for(i in 1:length(ff_)) {
    
    ff_[[i]]  <- lapply(ff_[[i]], produce)

   }
  
  ff_ <- calc_surplus(ff_)
  ff_ <- lapply(ff_, calc_price)
  
  
  return(list(ff_ = list(unlist(ff_), c('s_production', 's_surplus','s_price'))))
  
  
  }







