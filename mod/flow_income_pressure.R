

###################################################################################

### Simple process for translating price difference to pressure on land

###################################################################################

### currently assumes that the area of land available per system is ~static
#? how to change this?

### How should stocks be fed demand information? Internally?

#calculates income pressure on land use allocation
#calculates ratio of income per farm to mean income per farm for the land system

calc_income <- function(sf, p_CDR_s) {

  sf <- lapply(sf, function(x) {

          lapply(x, function(y) {

            s <- y@s_parameters

            if(is.null(s$s_size)) {

              xs <- unlist(lapply(x, function(z) {z@s_parameters$s_size}))
              xs <- mean(xs[!is.null(xs)], na.rm = T)

              xs <- ifelse(s$s_CDR == 1, xs * p_CDR_s, xs)

              s$s_size              <- xs
              y@s_parameters$s_size <- xs

            }

            .inc                    <- ((s$s_yield * s$s_price * s$s_margin) + s$s_subsidy + s$s_carbon_income +
                                          s$s_biodiversity_income) * s$s_size
            y@s_parameters$s_income <- ifelse(.inc < 0, 0, .inc)

            y

          })

      })


  return(sf)

}


calc_ip <- function(sf) {

  sf <- lapply(sf, function(x) {

    income       <- unlist(lapply(x, function(z) {z@s_parameters$s_income}))
    inc.per.ha   <- income / unlist(lapply(x, function(z) {z@s_parameters$s_size}))
    areas        <- unlist(lapply(x, function(z) {z@s_parameters$s_area}))

    lapply(x, function(y) {

      ### income pressure is mean of ratios of farm income & per hectare income
      y@s_parameters$s_income_pressure <- apply(cbind(y@s_parameters$s_income / income,
                                            ifelse(is.finite((y@s_parameters$s_income / y@s_parameters$s_size) /inc.per.ha),
                                                   (y@s_parameters$s_income / y@s_parameters$s_size) /inc.per.ha, 0)), 1, mean)

      ### catch where area == 0
      y@s_parameters$s_income_pressure <- ifelse(is.finite(y@s_parameters$s_income_pressure),
                                                 y@s_parameters$s_income_pressure, 0)

      y

    })


  })

  sf

}


lc_flow <- function(sf, plt) {

  #calculate econ flows between systems

  for(i in 1:length(sf)) {

    econ.LULC <- list()
    econ.vals <- sapply(sf[[i]], function(z) {z@s_parameters$s_income_pressure})
    areas     <- sapply(sf[[i]], function(z) {z@s_parameters$s_area})

    inc.mat   <- apply(sapply(sf[[i]], function(z) {z@s_parameters$s_income_pressure}), 2, function(z) {ifelse(z<1 & z != 0, -1/z, z)})

    for(j in 1:ncol(inc.mat)) {

      for(k in 1:nrow(inc.mat)) {

        inc.mat[k, j] <- ifelse(inc.mat[k, j] > 1, inc.mat[k, j] * areas[k],
                          ifelse(inc.mat[k, j] < 1, inc.mat[k, j] * areas[j],
                            0)) * plt

      }

      sf[[i]][[j]]@s_parameters$s_income_pressure <- as.numeric(inc.mat[, j])

    }



    }

  return(sf)

 }

### converts income pressure to a hectares change
### ff_ should be a list of stocks by family

income_pressure <- function(ff_, p_land_trans, p_CDR_size) {

  s.fam   <- calc_income(ff_, p_CDR_size)
  s.fam   <- calc_ip(s.fam)
  s.fam   <- lc_flow(s.fam, p_land_trans)

  return(list(ff_ = list(unlist(s.fam), c('s_income', 's_income_pressure'))))

}



