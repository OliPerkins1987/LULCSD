
###############################################################################

### Defines core class for a stock

###############################################################################

#' @export
setClass('stock', 
  slots=list(s_name="character", 
   s_id="character", s_family = "character",
   s_flows_out = "character", s_flows_in = "character", 
    s_internal_pars, s_parameters = "list"))

#' @export
setGeneric("load_stock_data", function(x, data) standardGeneric("load_stock_data"))
setMethod("load_stock_data", "stock", function(x, data) {
      x@s_parameters$internal <- data
      x})


#' @export
setGeneric('update_internal_par', function(x, mod) standardGeneric("update_internal_par"))
setMethod('update_internal_par', 'stock', function(x, mod){
  
  step <- mod@run$step
  df   <- x@s_parameters$internal
  
  if(nchar(x@s_internal_pars[1]) != 0 & length(x@s_internal_pars) != 0) {
    
    for(i in 1:length(x@s_internal_pars)) {
    
      if(length(dim(df)) == 2) {
    
        x@i_current_val <- df[df[, 1] == step, which(names(df)) == x@s_internal_pars[i]]
    
  } else if(length(dim(df)) == 3) {
    
    x@i_current_val <- df[[which(names(df)) == x@s_internal_pars[i]]][, , step]
    
  } else {
    
    stop('Input data must be either a 2-d or 3-d array or array-like')
    
      }
  
    }
  
  }
  
  x
  
})





###########################################################################

### Define a family of stocks

###########################################################################

#' @export
setClass('family_stock', contains = 'stock',
  slots=list(s_family_name="character", s_family_id="character", 
    s_family_members = "character",
     s_family_flows_in = "character", s_family_flows_out = "character"))



