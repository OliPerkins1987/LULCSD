

###################################################################################

### Example flow functions for rhapsody package

###################################################################################

deforest <- function(s_soybean, s_forest, p_land_trans) {

  ssb <- s_soybean@s_parameters
  sf  <- s_forest@s_parameters

  ### calculate implied area of land use change
  delta_area <- ssb$price * ssb$area * p_land_trans

  ### prevent forest area < 0
  delta_area <- ifelse((sf$area - delta_area) < 0, sf$area, delta_area)

  ### update area
  ssb$area <- ssb$area + delta_area
  sf$area  <- sf$area - delta_area

  return(list(s_soybean = list(area = ssb$area),
              s_forest = list(area = sf$area)))

}


produce_soy <- function(s_soybean, i_soybean_demand, p_price_update) {

  ### calculate soybean production
  ssb            <- s_soybean@s_parameters
  ssb$production <- ssb$yield * ssb$area

  ### calculate price as a function of net surplus (deficit)
  ssb$price      <- ssb$price + (1-(i_soybean_demand@i_current_val/ssb$production))*p_price_update

  return(list(s_soybean = list(production = ssb$production, price = ssb$price)))

}


