

###################################################################################

### Economic return for biodiversity credits

###################################################################################

### converts income pressure to a hectares change
### ff_ should be a list of stocks by family

biodiversity_income <- function(ff_, p_biodiversity_price, p_biodiversity_cost, p_biodiversity_subsidy) {

  #calculates income per ha of land converted to CDR

  calc.area <- function(sf) {

    s <- unlist(sf)

    tot.area <- sum(unlist(lapply(s, function(z) {

      z@s_parameters$s_area

    })), na.rm = T)

    urb.area <- sum(unlist(lapply(s, function(z) {

      if(z@s_family == 'urban') {

      ### 2 is the baseline number of biodiversity units required to be purchased per ha

      t <- z@s_parameters$s_area * 2

      } else {

      t <-  0

      }

      t

    })), na.rm = T)

    return(urb.area / tot.area)

  }

  calc_biod_income <- function(sf, pbp, pbc, pbs, urban_fraction) {

    sf <- lapply(sf, function(x) {

      lapply(x, function(y) {

        if(y@s_family != 'urban') {

        s <- y@s_parameters

        s_biod_yield   <- ifelse(is.null(s$s_biodiversity_units), 0, s$s_biodiversity_units)

        y@s_parameters$s_biodiversity_income <- (pbp*pbc) * s_biod_yield * urban_fraction +
                                                    (pbs*s$s_CDR*s_biod_yield*!grepl('biomass', tolower(s$s_commodity)))

        y

        }

      })

    })


    return(sf)

  }


  urb.frac <- calc.area(ff_)
  s.fam    <- calc_biod_income(ff_, p_biodiversity_price, p_biodiversity_cost, p_biodiversity_subsidy, urb.frac)

    return(list(ff_ = list(unlist(s.fam), c('s_biodiversity_income'))))

}



