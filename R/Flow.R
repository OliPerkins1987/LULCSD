
###############################################################################

### Defines core class for a flow

###############################################################################

#' @export
flow <- setClass('flow', 
          slots=list(f_name="character", f_id="character",
           f_from_stocks = "character", f_to_stocks = "character", 
            f_function = "character", f_values = "list"))


### helpers
#' @export
setGeneric("check_flow_function", function(x) standardGeneric("check_flow_function"))
setMethod("check_flow_function", "flow", function(x){typeof(get(x@f_function)) == 'closure'})
    
setGeneric("parse_flow_args", function(x, taxon) standardGeneric("parse_flow_args"))
setMethod("parse_flow_args", "flow", function(x, taxon) {
  
  for(f in 1:length(x@f_values$inputs)) {
    
    if(taxon[f] == 'inputs') {
      
      x@f_values$inputs[[f]] <- list(current = x@f_values$input[[f]]@i_current_val, 
                                    data = x@f_values$input[[f]]@i_data)
      
    } else if(taxon[f] == 'stocks') {
      
      x@f_values$inputs[[f]] <- x@f_values$input[[f]]@s_parameters
      
    }
    
  }
  
  x
  
})


### getters
setGeneric("get_flow_args", function(x, mod, parse_args) standardGeneric("get_flow_args"))
setMethod("get_flow_args", "flow", function(x, mod, parse_args = T) {
  
  f_args            <- names(formals(get(x@f_function)))
  tax               <- arg_parser(mod, f_args)
  
  result <- sapply(1:length(tax), function(z) {
    
    if(tax[z] == 'inputs') {
      
      r <- mod@inputs[[which(unlist(lapply(mod@inputs, function(i) (i@i_name))) == f_args[z])]]
      
    } else if(tax[z] == 'parameters') {
      
      r <- mod@parameters[[f_args[z]]]
      
    } else if(tax[z] == 'stocks') { 

      r   <- mod@stocks[[which(unlist(lapply(mod@stocks, function(s) (s@s_name))) == f_args[z])]]

    }
    
    r        <- list(r)
    names(r) <- f_args[z]
    r
    
  })
  
  x@f_values$inputs <- result
  
  if(parse_args == T) {
    
    x <- parse_flow_args(x, tax)
    
  }
  
  x

})

### run flow
setGeneric("run_flow", function(x, ...) standardGeneric("run_flow"))
setMethod("run_flow", "flow", function(x, ...) {

  x@f_values$outputs <- do.call(x@f_function, x@f_values$inputs)
  
  x
  
})
      
### setters
setGeneric("set_flow_output", function(x, mod) standardGeneric("set_flow_output"))
setMethod("set_flow_output", "flow", function(x, mod) {
  
  f_outs <- names(x@f_values$outputs)
  tax    <- arg_parser(mod, f_outs)
  
  for(z in 1:length(tax)) {
    
    if(tax[z] == 'stocks') {
      
      for(out in 1:length(x@f_values$outputs[[z]])) {
      
      mod@stocks[[which(unlist(lapply(mod@stocks, 
          function(s) (s@s_name))) == f_outs[z])]]@s_parameters[names(
            x@f_values$outputs[[z]][out])] <- x@f_values$outputs[[z]][[out]]
      
          } 
      
    } else if(tax[z] == 'outputs') {
          
      for(out in 1:length(x@f_values$outputs[[z]])) {
        
        mod@outputs[[which(unlist(lapply(mod@outputs, 
          function(o) (o@o_name))) == f_outs[z])]]@o_current_val[names(
            x@f_values$outputs[[z]][out])] <- x@f_values$outputs[[z]][[out]]
        
      } 
      
        }
      
      }

  mod
  
  
})

###########################################################################

### Define a family of flows

###########################################################################

### a family flow takes inputs based on their stock family not stock name

setClass('family_flow', contains = 'flow')

##########################
## getter for family flow
##########################

setGeneric("get_ff_args", function(x, mod, parse_args) standardGeneric("get_ff_args"))
setMethod("get_ff_args", "family_flow", function(x, mod, parse_args = F) {
  
  f_args            <- names(formals(get(x@f_function)))
  tax               <- arg_parser(mod, f_args)
  
  result <- sapply(1:length(tax), function(z) {
    
    if(tax[z] == 'inputs') {
      
      r <- mod@inputs[[which(unlist(lapply(mod@inputs, function(i) (i@i_name))) == f_args[z])]]
      r        <- list(r)
      names(r) <- f_args[z]
      
    } else if(tax[z] == 'parameters') {
      
      r <- mod@parameters[[f_args[z]]]
      r        <- list(r)
      names(r) <- f_args[z]
      
    } else if(tax[z] == 'family_flows') { 
      
      r   <- list()

      for(s in 1:length(x@f_from_stocks)) {
      
      r[[s]]        <- mod@stocks[which(unlist(lapply(mod@stocks, function(s) (s@s_family))) == x@f_from_stocks[s])]
      names(r)[[s]] <- x@f_from_stocks[s]
      
    }
    
    r        <- list(r)
    names(r) <- 'ff_'
    
    }
    
    r
    
  })
  
  x@f_values$inputs <- result
  
  if(parse_args == T) {
    
    x <- parse_flow_args(x, tax)
    
  }
  
  x
  
})

############################
### setter for family flow
############################

setGeneric("set_ff_output", function(x, mod) standardGeneric("set_ff_output"))
setMethod("set_ff_output", "family_flow", function(x, mod) {
  
  f_outs <- names(x@f_values$outputs)
  tax    <- arg_parser(mod, f_outs)
  
  for(z in 1:length(tax)) {
    
    if(tax[z] == 'stocks') {
      
      for(out in 1:length(x@f_values$outputs[[z]])) {
        
        mod@stocks[[which(unlist(lapply(mod@stocks, 
                                        function(s) (s@s_name))) == f_outs[z])]]@s_parameters[names(
                                          x@f_values$outputs[[z]][out])] <- x@f_values$outputs[[z]][[out]]
        
      } 
      
    } else if(tax[z] == 'outputs') {
      
      for(out in 1:length(x@f_values$outputs[[z]])) {
        
        mod@outputs[[which(unlist(lapply(mod@outputs, 
            function(o) (o@o_name))) == f_outs[z])]]@o_current_val[names(
              x@f_values$outputs[[z]][out])] <- x@f_values$outputs[[z]][[out]]
        
      } 
      
      ### family flow outputs should 
      
    } else if(tax[z] == 'family_flows') {
      
      fields <- x@f_values$outputs[[z]][[2]]
      
       for(s in x@f_values$outputs[[z]][[1]]) {
         
         sn <- which(unlist(lapply(mod@stocks, function(y) {y@s_name})) == s@s_name)
         
         for(f in fields) {
           
           mod@stocks[[sn]]@s_parameters[f] <- s@s_parameters[f]
           
         }
         
       }
      
    }
    
  }
  
  mod
  
  
})


