
#####################################################################################

### Generic helpers to enable modelling

#####################################################################################

stock_by_family <- function(mod) {

  args   <- evalq(as.list(environment()), envir = -2)
  tax    <- arg_parser(mod, names(args))
  s      <- args[tax == "stocks"]
  s.syst <- split(s, unlist(lapply(s, function(x) {x@s_family}))) 
  s.syst

}
