

###################################################################################

### Constrains peat restoration by its availability

###################################################################################

peat_logistics <- function(ff_) {

  ################################
  ### apply biomass constraint
  ################################
  
  s.fam <- lapply(ff_, function(z) {
    
    s.names <- sapply(z, function(s) {s@s_parameters$s_commodity})
    
    ### update peat available for restoring
    
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
      
      ### constraint peat flows
      Peat.available <- unlist(sapply(z, function(s) {s@s_parameters$s_available_peat_area}))
      
      z <- lapply(z, function(s) {
        
        if(s@s_parameters$s_commodity == 'Peat') {
          
          s@s_parameters$s_logistics_constraint <- Peat.available
          
        }
        
        s
        
        
      })
      
    }
    
    z
    
  })

  
  return(list(ff_ = list(unlist(s.fam), c('s_available_peat_area', 's_logistics_constraint'))))
  
  
  }







