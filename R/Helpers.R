
#####################################################################################

### Generic helpers to enable modelling

#####################################################################################

stock_by_family <- function(args) {

  s.syst <- split(args, unlist(lapply(args, function(x) {x@s_family}))) 
  s.syst

}
