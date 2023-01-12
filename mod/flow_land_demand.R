

###################################################################################

### Simple process for translating price difference to pressure on land

###################################################################################

### passes commodity demand to relevant sub-system

demand_cereals <- function(s_cereals, s_lfa_cereals, i_demand_cereals) {
  
  
  ### pass
  
  return(list(s_cereals = list(s_demand = i_demand_cereals@i_current_val), 
              s_lfa_cereals = list(s_demand = i_demand_cereals@i_current_val)))
  
}


demand_veg <- function(s_veg, s_lfa_veg, i_demand_veg) {
  
  
  ### pass
  
  return(list(s_veg = list(s_demand = i_demand_veg@i_current_val), 
              s_lfa_veg = list(s_demand = i_demand_veg@i_current_val)))
  
}

demand_dairy <- function(s_dairy, s_lfa_dairy, i_demand_dairy) {
  
  
  
  ### pass
  
  return(list(s_dairy = list(s_demand = i_demand_dairy@i_current_val), 
              s_lfa_dairy = list(s_demand = i_demand_dairy@i_current_val)))
  
}

demand_rg <- function(s_rg, s_lfa_rg, i_demand_rg) {
  
  
  ### pass
  
  return(list(s_rg = list(s_demand = i_demand_rg@i_current_val), 
              s_lfa_rg = list(s_demand = i_demand_rg@i_current_val)))
  
}


demand_hardwood <- function(s_forest, s_lfa_forest, i_demand_hardwood) {

  
  ### pass
  
  return(list(s_forest = list(s_demand = i_demand_hardwood@i_current_val), 
              s_lfa_forest = list(s_demand = i_demand_hardwood@i_current_val)))
  
}



demand_softwood <- function(s_farm_woodland_cropland, s_farm_woodland_grassland, 
                            s_lfa_farm_woodland_cropland, s_lfa_farm_woodland_grassland, i_demand_softwood) {
  
  ### pass
  
  return(list(s_farm_woodland_cropland           = list(s_demand = i_demand_softwood@i_current_val), 
              s_farm_woodland_grassland          = list(s_demand = i_demand_softwood@i_current_val),
              s_lfa_farm_woodland_cropland       = list(s_demand = i_demand_softwood@i_current_val), 
              s_lfa_farm_woodland_grassland      = list(s_demand = i_demand_softwood@i_current_val)))
  
}




demand_biomass <- function(s_biomass_cropland, s_biomass_grassland, 
                           s_lfa_biomass_cropland, s_lfa_biomass_grassland, i_demand_biomass) {
  
  
  ### pass
  
  return(list(s_biomass_cropland           = list(s_demand = i_demand_biomass@i_current_val), 
              s_biomass_grassland          = list(s_demand = i_demand_biomass@i_current_val),
              s_lfa_biomass_cropland       = list(s_demand = i_demand_biomass@i_current_val), 
              s_lfa_biomass_grassland      = list(s_demand = i_demand_biomass@i_current_val)))
  
}







