

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
      df$tig[, i] <- ifelse(df$tig[, i] < 0 & !is.na(df$tig[, i]), 0, df$tig[, i])
      
      df$tig[i, ] <- df$tig[, i]
      
        }
        
      }
    
    ### set non-CDR & intra-CDR transitions to 1
    df$willingness                    <- data.frame(apply(df$willingness, 2, function(z){ifelse(is.na(z), 1, z)}))  
    df$willingness[CDR.key, CDR.key]  <- 1
    
    ### set ceiling on non_CDR transitions to inf
    df$CDR_max     <- data.frame(apply(df$CDR_max, 2, function(z){ifelse(is.na(z), 99999, z)}))  
    
    ### set non-CDR ig in govt to 1
    df$tig                   <- data.frame(apply(df$tig, 2, function(z){ifelse(is.na(z), 1, z)})) 
    df$tig[CDR.key, CDR.key] <- 1
    
    df
  
    })  
  
  return(sf)
  
}

############################################################

### Get Environmental constraints

############################################################

get_envrionmental_constraint <- function(sf) {


  env.con <- lapply(sf, function(x) {
  
    df <- mk_trans_matrix(x, nr = length(x))
  
    #################################
    ### populate econ flows matrix
    #################################
  
    for(i in 1:length(x)) {
    
      df[, i]  <- x[[i]]@s_parameters$s_env_suit
    
    }
  
    df <- data.frame(df)
  
    df
  
  })

return(env.con)

}

############################################################

### Get logistics constraints

############################################################

get_logistics_pars <- function(sf) {
  
  logic_con <- lapply(sf, function(x) {
    
    df <- mk_trans_matrix(x, nr = 1)
    
    #########################################
    ### populate logstics_contraint matrix
    #########################################
    
    for(i in 1:length(x)) {
      
      ### treat peat separately
      if(!(x[[i]]@s_parameters$s_commodity == 'Peat')) {
      
      ### CDR-specific logistics
      df[, i] <- ifelse(is.null(x[[i]]@s_parameters$s_logistics_constraint), NA, 
                        x[[i]]@s_parameters$s_logistics_constraint)
      
      }
      
    }
    
    df <- data.frame(t(apply(df, 2, function(z) {ifelse(is.na(z), Inf, 
                                                  ifelse(z < 0, 0, z))})))
    df
    
  })
  
  return(logic_con)
  
}

###########################################################

### Account for peat

###########################################################

get_peat_limits <- function(sf) {
  
  logic_con <- lapply(sf, function(x) {
    
    df       <- mk_trans_matrix(x, nr = 1)
    Peat.key <- which(sapply(x, function(s) {s@s_parameters$s_commodity}) == 'Peat')
    
    #########################################
    ### populate logstics_contraint matrix
    #########################################
    
    if(length(x[[Peat.key]]@s_parameters$s_logistics_constraint) == ncol(df)-1) {
    
      df[, -Peat.key] <- x[[Peat.key]]@s_parameters$s_logistics_constraint
    
    }
    
    df[1, ] <- ifelse(is.na(df[1, ]), Inf,
                 ifelse(df[1, ] < 0, 0, df[1, ]))
    df
    
  })
  
  
}


###########################################################

### Economic transition matrices

###########################################################

calc_econ_flows <- function(sf) {
  
  econ.flows <- lapply(sf, function(x) {
    
    df <- mk_trans_matrix(x, nr = length(x))
    
    #################################
    ### populate econ flows matrix
    #################################
    
    for(i in 1:length(x)) {
      
      df[, i]  <- x[[i]]@s_parameters$s_income_pressure
      
    }
    
    df <- data.frame(df)
    
    df
    
  })
  
  return(econ.flows)
  
}


############################################################

### Container for scenario specific political decisions

############################################################


mk_political_constraints <- function(sf, pp) {
  
  pol.con <- lapply(sf, function(x) {
    
    df <- mk_trans_matrix(x, nr = length(x))
    
    #################################
    ### populate matrix
    #################################
    
    for(i in 1:length(pp)) {
      
      .key  <- which(grepl(names(pp)[i], colnames(df)))
      .peat <- which(grepl('peat', row.names(df)))
      .crop <- which(grepl('cereal', row.names(df)))
      .veg  <- which(grepl('veg', row.names(df)))
      
      ### infinite unless specified
      df[, .key]  <- Inf
      
      ### no trees or biomass on peat
      if(pp[[i]]$peat == FALSE) {
        
        df[.peat, .key] <- 0
        
      }
      
      if(pp[[i]]$prime == FALSE) {
        
        
        df[.crop, .key] <- ifelse(x[[.key]]@s_parameters$s_income_pressure[.crop] > 0, 
                                  x[[.key]]@s_parameters$s_income_pressure[.crop] * 0.25, 
                                  x[[.key]]@s_parameters$s_income_pressure[.crop])
        
        
        df[.veg, .key] <- ifelse(x[[.key]]@s_parameters$s_income_pressure[.veg] > 0, 
                                 0, x[[.key]]@s_parameters$s_income_pressure[.veg])
        
      }
      
    }
    
    df <- data.frame(apply(df, 2, function(z) {ifelse(is.na(z), Inf, z)}))
    
    df
    
  })
  
  return(pol.con)
  
}



###############################################################

### combine sub-models

###############################################################

combine_mods <- function(sf, behaviour, econ, logistics, enviro, peat, pol) {
  
  
  for(i in 1:length(sf)) {
    
    ###############################################
    
    ### behaviour
    
    ###############################################
    
    CDR.key                    <- sapply(sf[[i]], function(z) {z@s_parameters$s_CDR == 1})
    LULC <- econ[[i]]
    
    ### apply CDR uptake constraint
    for(j in 1:length(sf[[i]])) {
      
      LULC[CDR.key, j] <- ifelse((LULC[CDR.key, j] <0) & !(j %in% CDR.key), LULC[CDR.key, j] * behaviour[[i]]$willingness[CDR.key, j], 
                                 LULC[CDR.key, j])
      
      LULC[j, CDR.key] <- ifelse((LULC[j, CDR.key] >0) & !(j %in% CDR.key), LULC[j, CDR.key] * behaviour[[i]]$willingness[j, CDR.key], 
                                 LULC[j, CDR.key])
      
      
    }

    ### apply trust in govt constraint
    for(j in 1:length(sf[[i]])) {
    
    LULC[CDR.key, j] <- ifelse((LULC[CDR.key, j] <0) & !(j %in% CDR.key), LULC[CDR.key, j] * behaviour[[i]]$tig[CDR.key, j], 
                               LULC[CDR.key, j])
    
    LULC[j, CDR.key] <- ifelse((LULC[j, CDR.key] >0) & !(j %in% CDR.key), LULC[j, CDR.key] * behaviour[[i]]$tig[j, CDR.key], 
                               LULC[j, CDR.key])
    
    }
    
    ### apply ceiling on CDR uptake
    for(j in 1:length(sf[[i]])) {
      
      LULC[, j] <- ifelse(LULC[, j] > behaviour[[i]]$CDR_max[, j], 
                          behaviour[[i]]$CDR_max[, j], LULC[, j])
      
      LULC[j, ] <- ifelse(LULC[j, ] < 0 & abs(LULC[j, ]) > LULC[, j], 0-LULC[, j], LULC[j, ])

    }
    
    
    ##################################################
    
    ### Logistics
    
    ##################################################
    
    ### apply logistics ceilling for woodland and biomass
    for(j in 1:length(sf[[i]])) {
      
      if(sum(LULC[, j][which(LULC[, j] > 0)]) > logistics[[i]][, j]) {
        
        LULC[, j] <- ifelse(LULC[, j] > 0, 
                       LULC[, j] / (sum(LULC[, j][which(LULC[, j] > 0)])/ logistics[[i]][, j]), 
                       LULC[, j])
        
        LULC[j, ] <- (0 - LULC[, j])
      }
      
    }
    
    ### account for distribution of peat
    Peat.key <- which(sapply(sf[[i]], function(s) {s@s_parameters$s_commodity}) == 'Peat')
    
    for(j in 1:length(sf[[i]])) {
      
      if((0- LULC[Peat.key, j]) > peat[[i]][, j]) {
        
        LULC[Peat.key, j] <- (0 - peat[[i]][, j])
        
        LULC[j, Peat.key] <- peat[[i]][, j]
        
      }
      
    }
    
    ##################################################
    
    ## Apply environmental constraint
    
    ##################################################
    
    for(j in 1:length(sf[[i]])) {

        LULC[, j] <- ifelse(LULC[, j] > 0, 
                            LULC[, j] * enviro[[i]][, j], 
                            LULC[, j])
        
        LULC[j, ] <- (0 - LULC[, j])
      
        }
      
    
    
    ##################################################
    
    ## Apply political constraints
    
    ##################################################
    
    for(j in 1:length(sf[[i]])) {
      
      LULC[, j] <- ifelse(LULC[, j] > pol[[i]][, j], 
                          pol[[i]][, j], 
                          LULC[, j])
      
      LULC[j, ] <- (0 - LULC[, j])
      
    }
    
    ##################################################
    
    ### update LULC
    
    ##################################################
    
    for(j in 1:length(sf[[i]])) {
      
      sf[[i]][[j]]@s_parameters$s_LULC <- as.numeric(LULC[, j])
      
      }
    
    
    
    }
    
    
  
  sf
  
}




########################################################

### calculate LULC

########################################################

calc_la <- function(sf) {
  
  Proj.area <- unlist(lapply(sf, function(x) {
    
    delta <- sum(x@s_parameters$s_LULC)
    
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

land_allocation <- function(ff_, p_behaviour_intercept, p_tig_beta, p_max_CDR_delta, 
                            p_politics_woodland_peat, p_politics_woodland_prime,
                            p_politics_biomass_peat, p_politics_biomass_prime) {
  
  ### catches case where model is run from 1st step without initial values
  if(any(sapply(unlist(ff_), function(z) {!is.null(z@s_parameters$s_CDR_will)}))) {
    
    b.df    <- get_behaviour_pars(ff_, p_behaviour_intercept,p_tig_beta, p_max_CDR_delta)
    l.df    <- get_logistics_pars(ff_)
    env.df  <- get_envrionmental_constraint(ff_)
    p.df    <- get_peat_limits(ff_)
    politics<- mk_political_constraints(ff_, 
                                        pp = list('woodland' = list('peat' = p_politics_woodland_peat, 'prime' = p_politics_woodland_prime), 
                                                  'biomass'  = list('peat' = p_politics_biomass_peat, 'prime' = p_politics_biomass_prime)))
    econ.df <- calc_econ_flows(ff_)
    s.fam   <- combine_mods(ff_, b.df, econ.df, l.df, env.df, p.df, politics)
    
  } else {
    
    s.fam <- ff_
    
  }
  
  s.fam   <- lapply(s.fam, calc_la)
  
  return(list(ff_ = list(unlist(s.fam), c('s_area', 's_LULC'))))
  
}

