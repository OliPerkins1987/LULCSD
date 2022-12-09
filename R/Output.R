
###############################################################################

### Defines core class for an input

###############################################################################

#' @export
setClass('output', 
    slots=list(o_name="character", o_id="character",
      o_from_stocks = "character", o_from_var = "character",
      o_current_val = "numeric", o_outputs = "list"))


### method for updating by timestep
#' @export
setGeneric('get_output', function(x, mod) standardGeneric("get_output"))
setMethod('get_output', 'output', function(x, mod){
  
  from  <- arg_parser(mod, x@o_from_stocks)
  o     <- numeric()
  
  for(f in 1:length(from)) {
  
    if(from[f] != 'stocks') {stop('Model outputs must be drawn from a model stock')}
  
    
    o[f] <- mod@stocks[[which(unlist(
                      lapply(mod@stocks, 
                        function(x) {x@s_name})) == x@o_from_stocks[f])]]@s_parameters[x@o_from_var[f]]
  
  }
  
  o <- unlist(o)
  
  if(length(o) > 1) {
    
    o <- sum(unlist(o))
    
  }
  
  x@o_current_val             <- o
  x@o_outputs[[(1 +(mod@run$step - mod@run$first_step))]] <- x@o_current_val 
  
  x
  
})
  
