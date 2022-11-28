
###############################################################################

### Defines core class for a stock

###############################################################################

#' @export
setClass('stock', 
  slots=list(s_name="character", s_id="character",
   s_flows_out = "character", s_flows_in = "character", 
    s_parameters = "list"))

#' @export
setGeneric("load_stock_data", function(x, data) standardGeneric("load_stock_data"))
setMethod("load_stock_data", "stock", function(x, data) {
      x@s_parameters$data <- data
      x})



###########################################################################

### Define a family of stocks

###########################################################################

#' @export
setClass('family_stock', contains = 'stock',
  slots=list(s_family_name="character", s_family_id="character", 
    s_family_members = "character",
     s_family_flows_in = "character", s_family_flows_out = "character"))



