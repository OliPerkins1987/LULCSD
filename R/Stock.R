
###############################################################################

### Defines core class for a stock

###############################################################################

#' @export
.stock <- setClass('stock', 
          slots=list(s_name="character", 
           s_id="character", s_family = "character",
            s_flows_out = "character", s_flows_in = "character", 
             s_internal_pars = "character", s_parameters = "list"))

init_stock <- function(name, id, family, flows_out, flows_in, 
                       internal_pars, parameters = list()) {
  
  s <- .stock(s_name = name, s_id = id, s_family = family, s_flows_out = flows_out, 
              s_internal_pars = internal_pars)

  ## deal with case of no internal parameters
  s@s_internal_pars <- ifelse(identical(s@s_internal_pars, character(0)), '', s@s_internal_pars)
    
  s
  
}


###############################################################################

### Generics for loading data, initialising and updating internal pars

###############################################################################

#' @export
setGeneric("load_stock_data", function(x, path) standardGeneric("load_stock_data"))
setMethod("load_stock_data", "stock", function(x, path) {
  
  if(x@s_internal_pars[1] != "") {
  
    for(p in 1:length(x@s_internal_pars)) {
      
      x@s_parameters[x@s_internal_pars[p]] <- 0
      
    }
    
    if(is.na(path)) {
    
      path <- paste0(x@s_name, '.csv')

    }
  
    x@s_parameters$internal <- read.csv(path)
    
  }
    
  x
  
})

#' @export
setGeneric('init_pars', function(x, isv) standardGeneric('init_pars'))
setMethod('init_pars', 'stock', function(x, isv) {
  
  r <- which(isv[, 1] == x@s_name)
  
  for(i in r) {
    
    x@s_parameters[isv[i, 2]] <- isv[i,3]
    
  }
  
  x
  
})



#' @export
setGeneric('update_internal_par', function(x, mod) standardGeneric("update_internal_par"))
setMethod('update_internal_par', 'stock', function(x, mod){
  
  if(!any(grepl('internal', names(x@s_parameters))) | nchar(x@s_internal_pars[1]) == 0) {
    
    warning('Trying to update stock with no internal parameters')
    return(x)
    
  }
  
  step <- mod@run$step
  df   <- x@s_parameters$internal

  for(i in 1:length(x@s_internal_pars)) {
    
      if(length(dim(df)) == 2) {
    
        x@s_parameters[x@s_internal_pars[i]] <- df[df[, 1] == step, which(names(df) == x@s_internal_pars[i])]
    
  } else if(length(dim(df)) == 3) {
    
    x@s_parameters[x@s_internal_pars[i]] <- df[[which(names(df)) == x@s_internal_pars[i]]][, , step]
    
  } else {
    
    stop('Input data must be either a 2-d or 3-d array or array-like')
    
      }
  
    }

  x
  
})


##########################################
#### placeholder for spatial stock
##########################################


#' @export
setClass('spatial_stock', contains = 'stock')

setGeneric("load_spatial_stock", function(x, path) standardGeneric("load_spatial_stock"))
setMethod("load_spatial_stock", "stock", function(x, path) {
  
  
  if(is.na(path)) {
    
    path <- paste0(x@s_name, '.nc')
    
  }
  
  x@s_parameters$internal           <- raster::brick(path)[]
  col                               <- ncol(x@s_parameters$internal)
  r                                 <- nrow(x@s_parameters$internal)
  x@s_parameters$internal           <- cbind(rep(1:col, each = r, array(x@i_data, 
                                         dim = col*nrow(x@i_data))))
  
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



