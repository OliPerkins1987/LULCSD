

###################################################################################

### Calculates urban growth ~ population growth

##################################################################################

pop_growth <- function(i_population, s_urban) {
  
  
  return(list(s_urban = list(s_pop_delta = i_population@i_current_val)))
  
  
}



urban_growth <- function(s_cereals, s_urban, p_urban_growth) {
  
  s.cereal <- s_cereals@s_parameters$s_area - (s_urban@s_parameters$s_pop_delta * p_urban_growth)
  s.urb    <- s_urban@s_parameters$s_area   + (s_urban@s_parameters$s_pop_delta * p_urban_growth)
  
  return(list(s_cereals     = list(s_area = s.cereal), 
              s_urban       = list(s_area = s.urb)))
  
}



