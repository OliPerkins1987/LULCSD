
###################################################################################

### Calculates land fraction farmers are open to converting to CDR

###################################################################################

### distribution of sizes

calc_size_dist <- function(sf, psr) {
  
  sf <- lapply(sf, function(x) {
    
          lapply(x, function(y) {
            
            s <- y@s_parameters
            
            y@s_parameters$s_size_dist <- qgamma(seq(0.001, 0.999, 1/1000), s$s_size * psr, rate = psr) 
            
            y
            
          })

      })
  
  
  return(sf)
  
}

### distribution of incomes

calc_income_dist <- function(sf) {
  
  sf <- lapply(sf, function(x) {
    
    lapply(x, function(y) {
      
      s <- y@s_parameters
      
      y@s_parameters$s_income_dist <- ((s$s_yield * s$s_price * s$s_margin) + s$s_subsidy) * s$s_size_dist
      
      y
      
    })
    
  })
  
  
  return(sf)
  
}


### distribution of ages

calc_age_dist <- function(samps, ifa, pas) {
  
  a_dist <- qnorm(seq(0.001, 0.999, 1/samps), ifa, pas)
  
  return(a_dist)
  
}

### calculate boolean willigness (0-1) 

calc_CDR_bool <- function(sf, ifa) {

  sf <- lapply(sf, function(x) {
    
    lapply(x, function(y) {
    
      s      <- y@s_parameters
    
      ha.dat <- data.frame(income = s$s_income_dist, 
                           size   = s$s_size_dist, 
                           age    = ifa)
    
      probs  <- list()
      
      ### !!!this is not elegant
      ### parameters from empirical tree model
      
      set.seed(1987)
      
      n1 <- length(which(ha.dat$size <= 20))/nrow(ha.dat)
      n2 <- length(which(ha.dat$income[1:(floor(0.8* nrow(ha.dat)))][
             sample.int(floor(0.8* nrow(ha.dat)), nrow(ha.dat), replace = T)] < 75000))/nrow(ha.dat)
      n3 <- length(which(ha.dat$age < 75))/nrow(ha.dat)
      n4 <- length(which(ha.dat$age < 55))/nrow(ha.dat)

      n5 <- length(which(ha.dat$age < 65))/nrow(ha.dat)
      
      n6 <- !grepl('lfa', y@s_name)
      n7 <- length(which(ha.dat$income < 100000))/nrow(ha.dat)
      n8 <- length(which(ha.dat$income < 75000))/nrow(ha.dat)
      
      n9  <- length(which(ha.dat$size < 50))/nrow(ha.dat)
      n10 <- length(which(ha.dat$income[(floor(0.2* nrow(ha.dat))):nrow(ha.dat)][
              sample.int(floor(0.8* nrow(ha.dat)), nrow(ha.dat), replace = T)] < 75000))/nrow(ha.dat)

      probs[[1]] <- (n1*(1-n2))
      probs[[2]] <- (n1*(n2)*(1-n3))
      probs[[3]] <- (n1*(n2)*(n3)*(n4))
      probs[[4]] <- (n1*(n2)*(n3)*(1-n4))  

      probs[[5]] <- ((1-n1) * (n5) * n6 * n7)
      probs[[6]] <- ((1-n1) * (n5) * n6 * (1-n7))
      probs[[7]] <- ((1-n1) * (n5) * (1-n6) * (n8))
      probs[[8]] <- ((1-n1) * (n5) * (1-n6) * (1-n8))
                  
      probs[[9]] <- ((1-n1) * (1-n5) * (n9))
      probs[[10]]<- ((1-n1) * (1-n5) * (1-n9) * n10)
      probs[[11]]<- ((1-n1) * (1-n5) * (1-n9) * (1-n10))
      
      
      probs <- unlist(probs) * c(0.3125, 0.520, 0.8794, 1, 
                                 0.4714, 1, 0.8537, 0.4737, 
                                 0.2941, 0.3866, 0.5769)
      
      y@s_parameters$CDR_bool    <- probs

      y
    })
    
  })
  
  return(sf)
  
}

### calculate rate | willingness (ha)

calc_CDR_rate <- function(sf, ifa) {
  
  
  sf <- lapply(sf, function(x) {
    
    lapply(x, function(y) {
      
      s      <- y@s_parameters
      
      ha.dat <- data.frame(income = s$s_income_dist, 
                           size   = s$s_size_dist, 
                           age    = ifa)
      
      probs  <- list()
      
      ### !!!this is not elegant
      ### parameters from empirical tree model
      
      set.seed(1987)
      
      n1 <- length(which(ha.dat$size < 25))/nrow(ha.dat)
      n2 <- length(which(ha.dat$age  < 75))/nrow(ha.dat)
      
      
      probs[[1]] <- (n1*(n2))
      probs[[2]] <- (n1*(1-n2))
      probs[[3]] <- (1-n1)
      
      probs      <- unlist(probs) * c(0.2363, 0.1268, 0.1188)
      
      ### adjust rate of CDR for grazing
      
      graze.par  <- ifelse(grepl('grassland', y@s_family), 1.465, 0.682)
      
      y@s_parameters$CDR_rate <- probs * graze.par
      
      y
    })
    
  })
  
  return(sf)
  
  
}

######################
### CDR hectares calc
######################

calc_CDR_hectares <- function(sf, ifa) {
  
  sf <- lapply(sf, function(x) {
    
    lapply(x, function(y) {
      
      s      <- y@s_parameters
      
      ha.dat <- data.frame(income = s$s_income_dist, 
                           size   = s$s_size_dist, 
                           age    = ifa)
      
      ha  <- list()
      
      ### !!!this is not elegant
      ### parameters from empirical tree model
      
      set.seed(1987)
      
      ha[[1]] <- sum(mean(ha.dat$size[ha.dat$size < 25]) * s$CDR_rate[1:2]) * sum(s$CDR_bool[1:4]) 
      ha[[2]] <- sum(mean(ha.dat$size[ha.dat$size >= 25]) * s$CDR_rate[3])  * sum(s$CDR_bool[5:11]) 
      
      y@s_parameters$s_CDR_hectares <- sum(unlist(ha)) / mean(ha.dat$size)

    })
    
  })
  
  return(sf)
  
  
}


##################################################################################

### converts behavioural drivers to a theoretical hectares' CDR uptake
### ff_ should be a list of stocks by family

##################################################################################

land_willingness <- function(ff_, i_farmer_age,
                      p_size_rate, p_age_spread) {

  s.fam   <- calc_size_dist(ff_, psr = p_size_rate)
  s.fam   <- calc_income_dist(s.fam)
  ifa     <- calc_age_dist(1000, i_farmer_age@i_current_val, p_age_spread)
  
  s.fam   <- calc_CDR_bool(s.fam, ifa)
  s.fam   <- calc_CDR_rate(s.fam, ifa)
  
  s.fam   <- calc_CDR_hectares(s.fam, ifa)
  
  return(list(ff_ = list(unlist(s.fam), c('s_CDR_hectares'))))
  
}



