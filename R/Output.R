
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
  
  from            <- arg_parser(mod, x@o_from_stocks)
  
  if(from != 'stocks') {stop('Model outputs must be drawn from a model stock')}
  
  x@o_current_val <- mod@stocks[[which(unlist(
                      lapply(mod@stocks, 
                        function(x) {x@s_name})) == x@o_from_stocks)]]@s_parameters[[x@o_from_var]]
  
  
  x@o_outputs[[mod@run$step]] <- x@o_current_val 
  
  x
  
})
  
