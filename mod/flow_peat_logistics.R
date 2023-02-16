

###################################################################################

### Captures yield tradeoffs of marginal lands

###################################################################################

peat_logistics <- function(ff_) {

  ################################
  ### apply biomass constraint
  ################################
  
  s.fam <- lapply(ff_, function(z) {
    
    s.names <- sapply(z, function(s) {s@s_parameters$s_commodity})
    
    if('Peat' %in% s.names) {
      
      Peat.key <- which(s.names == 'Peat')
      
      z <- lapply(z, function(s) {
      
        if(s@s_parameters$s_commodity != 'Peat' & !is.null(s@s_parameters$s_LULC[Peat.key])) {
        
        s@s_parameters$s_available_peat_area <- s@s_parameters$s_available_peat_area + s@s_parameters$s_LULC[Peat.key]
        
        s@s_parameters$s_available_peat_area <- ifelse(s@s_parameters$s_available_peat_area < 0, 0, s@s_parameters$s_available_peat_area)
        
        if( s@s_parameters$s_available_peat_area > s@s_parameters$s_peat_max) {
          
          s@s_parameters$s_peat_max <- s@s_parameters$s_available_peat_area
          
        }
      
        }
        
        s
      
      })
      
      Peat.flow <- unlist(sapply(z, function(s) {s@s_parameters$s_peat_max - s@s_parameters$s_available_peat_area}))
      is.cdr    <- sapply(z, function(s) {s@s_parameters$s_CDR == 1})
      
      z <- lapply(z, function(s) {
        
        if(s@s_parameters$s_commodity == 'Peat') {
          
          s@s_parameters$s_logistics_constraint <- ifelse(Peat.flow < 0, 0, Peat.flow)
          s@s_parameters$s_logistics_constraint <- ifelse(is.cdr, 0, s@s_parameters$s_logistics_constraint)
          
        }
        
        s
        
        
      })
      
    }
    
    z
    
  })

  
  return(list(ff_ = list(unlist(s.fam), c('s_available_peat_area', 's_logistics_constraint'))))
  
  
  }







