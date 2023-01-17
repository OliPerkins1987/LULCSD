

###################################################################################

### Calculate land use changes based on pressures

###################################################################################

### makes an empty transition matrix for populating

mk_trans_matrix <- function(s_list, nr) {

  stopifnot('Invalid transition matrix size' = (nr %in% c(length(s_list), 1)))
  
    df <- setNames(data.frame(matrix(nrow = nr, ncol = length(s_list))), 
               sapply(s_list, function(s) {s@s_name}))

    if(nr == length(s_list)) {
    
    row.names(df) <- colnames(df)

    }
    
    df

}

#######################################################

### behavioural transition matrices

#######################################################

get_behaviour_pars <- function(sf, behaviour_offset, 
                               tig_beta, p_max_delta, p_lt) {
  
  sf <- lapply(sf, function(x) {
    
  df <- list(willingness = mk_trans_matrix(x, nr = length(x)), 
          CDR_max        = mk_trans_matrix(x, nr = length(x)), 
          tig            = mk_trans_matrix(x, nr = length(x)))
    
    
    #################################
    ### populate behavioural matrix
    #################################
    
    for(i in 1:length(x)) {
      
      ### CDR-specific willingness
      
      CDR.key   <- which(sapply(x, function(z) {z@s_parameters$s_CDR}) == 1)
      trans.val <- sapply(x[CDR.key], function(z) {z@s_parameters$s_CDR_will})
      
      if(x[[i]]@s_parameters$s_CDR == 0) {df$willingness[CDR.key, i] <- trans.val + behaviour_offset}
      if(x[[i]]@s_parameters$s_CDR == 1) {df$willingness[, i]        <- x[[i]]@s_parameters$s_CDR_will + behaviour_offset}
      
      ### max land conversion - as a ceiling on conversion
      if(x[[i]]@s_parameters$s_CDR == 0) {df$CDR_max[i, CDR.key] <- (x[[i]]@s_parameters$s_CDR_hectares / 
                                              x[[i]]@s_parameters$s_size) * x[[i]]@s_parameters$s_area / p_max_delta}
      
      ### trust in govt
      if(x[[i]]@s_parameters$s_CDR == 1) {
      
      df$tig[, i] <- x[[i]]@s_parameters$s_trust_in_govt / 
                        (x[[i]]@s_parameters$s_trust_in_govt + tig_beta)
      
      df$tig[, i] <- 1-(x[[i]]@s_parameters$s_subsidy/x[[i]]@s_parameters$s_income) * x[[i]]@s_parameters$s_trust_in_govt
      
        }
        
      }
    
    ### set non-CDR transitions to 1
    df$willingness <- data.frame(apply(df$willingness, 2, function(z){ifelse(is.na(z), 1, z)}))  
    
    ### set ceiling on non_CDR transitions to inf
    df$CDR_max     <- data.frame(apply(df$CDR_max, 2, function(z){ifelse(is.na(z), 99999, z)}))  
    
    ### set non-CDR ig in govt to 1
    df$tig         <- data.frame(apply(df$tig, 2, function(z){ifelse(is.na(z), 1, z)})) 
    
    df
  
    })  
  
  return(sf)
  
}


###########################################################

### Economic transition matrices

###########################################################

calc_econ_flows <- function(sf) {
  
  sf <- lapply(sf, function(x) {
    
    df <- mk_trans_matrix(x, nr = length(x))
    
    #################################
    ### populate econ flows matrix
    #################################
    
    for(i in 1:length(x)) {
      
      ### CDR-specific willingness
      
      econ.val  <- sapply(x, function(z) {z@s_parameters$s_income_pressure})
      
      if(econ.val[i] < 0 & !is.na(econ.val[i])) {
        
        econ.distr <- ifelse(econ.val > 0, econ.val/sum(econ.val[econ.val>0], na.rm = T), 0)
        df[, i]    <- econ.val[i] * econ.distr
        
      } else if (econ.val[i] > 0 & !is.na(econ.val[i])) {
        
        econ.distr <- ifelse(econ.val < 0, econ.val/sum(econ.val[econ.val<0], na.rm = T), 0)
        df[, i]    <- econ.val[i] * econ.distr
        
      }
      
    }
    
    df <- data.frame(apply(df, 2, function(z) {ifelse(is.na(z), 0, z)}))
    df
    
  })
  
  return(sf)
  
}




###############################################################

### combine sub-models

###############################################################


combine_mods <- function(sf, behaviour, econ) {
  
  for(i in 1:length(sf)) {
    
    ### apply initial constraint
    LULC <- econ[[i]] * behaviour[[i]]$willingness
    
    ### apply trust in govt constraint
    LULC <- LULC * behaviour[[i]]$tig
    
    ### apply ceiling on CDR uptake
    for(j in 1:length(sf[[i]])) {
      
      LULC[, j] <- ifelse(LULC[, j] > behaviour[[i]]$CDR_max[, j], 
                          behaviour[[i]]$CDR_max[, j], LULC[, j])
      
      LULC[j, ] <- ifelse(LULC[j, ] < 0 & abs(LULC[j, ]) > LULC[, j], 0-LULC[, j], LULC[j, ])

    }
    
    for(j in 1:length(sf[[i]])) {
      
      sf[[i]][[j]]@s_parameters$s_LULC <- sum(LULC[, j])
      
    }
    
  }
  
  sf
  
}




########################################################

### calculate LULC

########################################################

calc_la <- function(sf) {
  
  Proj.area <- unlist(lapply(sf, function(x) {
    
    delta <- x@s_parameters$s_LULC
    
    delta <- ifelse(length(delta) == 0, 0, delta)
    
    return((x@s_parameters$s_area + delta))

  }))
    
  ### stop area going negative for a system
  
  Proj.area <- Proj.area[!is.na(Proj.area)]
  
  if(any(Proj.area < 0)) {
    
    neg.area <- sum((Proj.area[which(Proj.area < 0)]))
    n.pos    <- length(Proj.area[Proj.area >= 0])
    
    Proj.area<- ifelse(Proj.area < 0, 0, Proj.area - (neg.area/n.pos))
    
  }
  
  for(i in 1:length(sf)) {
  
  sf[[i]]@s_parameters$s_area <- Proj.area[i]
  
  }
   
  sf
   
}

###############################################################

### Run function

###############################################################

land_allocation <- function(ff_, p_behaviour_intercept, p_tig_beta, p_max_CDR_delta, p_land_trans) {
  
  
  if(any(sapply(unlist(ff_), function(z) {!is.null(z@s_parameters$s_CDR_will)}))) {
  
  b.df    <- get_behaviour_pars(ff_, p_behaviour_intercept,p_tig_beta, p_max_CDR_delta, p_land_trans)
  econ.df <- calc_econ_flows(ff_)
  s.fam   <- combine_mods(ff_, b.df, econ.df)
  
  } else {
    
    s.fam <- ff_
    
  }
  
  s.fam   <- lapply(s.fam, calc_la)
  
  return(list(ff_ = list(unlist(s.fam), c('s_area'))))
  
}



