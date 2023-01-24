

###################################################################################

### Simple process for calculating production by commodity

###################################################################################

forest_production <- function(s_forest, s_lfa_forest,
                       p_price_weight, p_logging_rate, p_planting_rate) {
  
    ### produce timber from forests: assumes all logging is replanted
    s_forest@s_parameters$s_production     <- s_forest@s_parameters$s_price * p_logging_rate
    s_lfa_forest@s_parameters$s_production <- s_lfa_forest@s_parameters$s_price * p_logging_rate
    
    ### plant new forests
    s_forest@s_parameters$s_new_planting     <- s_forest@s_parameters$s_price * p_planting_rate
    s_lfa_forest@s_parameters$s_new_planting <- s_lfa_forest@s_parameters$s_price * p_planting_rate
    
    ### allocate nursery stocks for restocking & planting
    s_forest@s_parameters$s_nursery_available<- s_forest@s_parameters$s_nursery_stock - s_forest@s_parameters$s_production - s_forest@s_parameters$s_new_planting 
                                                          ##s_lfa_forest@s_parameters$s_new_planting - s_lfa_forest@s_parameters$s_production
    
    s_lfa_forest@s_parameters$s_s_nursery_available<- s_forest@s_parameters$s_nursery_available
    
    ### calculate surplus
    s_forest@s_parameters$s_surplus     <- ((s_forest@s_parameters$s_production)  - 
                                              s_forest@s_parameters$s_demand) / s_forest@s_parameters$s_demand
    s_lfa_forest@s_parameters$s_surplus <- s_forest@s_parameters$s_surplus    

    ### update price
    s_forest@s_parameters$s_price     <- s_forest@s_parameters$s_price + (s_forest@s_parameters$s_price  * (0-s_forest@s_parameters$s_surplus) * p_price_weight)
    s_lfa_forest@s_parameters$s_price <- s_forest@s_parameters$s_price
    
  return(list(s_forest       = list(s_price        = s_forest@s_parameters$s_price, 
                                    s_nursery_available = s_forest@s_parameters$s_nursery_available,
                                    s_new_planting = s_forest@s_parameters$s_new_planting, 
                                    s_production   = s_forest@s_parameters$s_production), 
              s_lfa_forest   =  list(s_price       = s_lfa_forest@s_parameters$s_price, 
                                     s_nursery_available = s_lfa_forest@s_parameters$s_nursery_available, 
                                     s_new_planting= s_lfa_forest@s_parameters$s_new_planting, 
                                     s_production  = s_lfa_forest@s_parameters$s_production)))
  
  
  }







