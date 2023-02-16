

###################################################################################

### Calculates urban growth ~ population growth

##################################################################################

delta_sa <- function(i_setaside, s_setaside) {
  
  
  return(list(s_setaside = list(s_delta_sa = i_setaside@i_current_val)))
  
  
}



set_aside <- function(s_cereals, s_lfa_cereals, s_setaside) {
  
  tot.cereal   <- s_cereals@s_parameters$s_area + s_lfa_cereals@s_parameters$s_area
  
  s.cereal     <- s_cereals@s_parameters$s_area - (
                    s_cereals@s_parameters$s_area / tot.cereal) * s_setaside@s_parameters$s_delta_sa
  
  lfa.cereal   <- s_lfa_cereals@s_parameters$s_area - (
                    s_lfa_cereals@s_parameters$s_area / tot.cereal) * s_setaside@s_parameters$s_delta_sa
  
  return(list(s_cereals           = list(s_area = s.cereal), 
              s_lfa_cereals       = list(s_area = lfa.cereal), 
              s_setaside         = list(s_area = s_setaside@s_parameters$s_area + s_setaside@s_parameters$s_delta_sa)))
  
}



