
###############################################################################

### Defines core class for a stock

###############################################################################

#' @export
.stock <- setClass('stock',
          slots=list(s_name="character",
           s_id="character", s_family = "character",
            s_flows_out = "character", s_flows_in = "character",
             s_internal_pars = "character", s_parameters = "list"))


### Initialise a stock
#'Initialise a model stock
#'@param name A character string; if using standard taxonomy starting with 's_'
#'@param id A character string giving a unique object identifier
#'@param family A character string assigning the stock to a family of stocks
#'@param flows_out A character naming stocks and outputs that are downstream in the model structure
#'@param flows_in A character naming stocks and inputs that are uptream in the model structure
#'@param internal_pars A character vector naming the stocks internal parameters
#'@param parameters An empty list to be filled by model intial parameters
#'@returns A rhapsody stock object
#'@examples
#'s_soybeans <- init_stock(s_name = 's_soybean',
#'     s_family = 'cropland', s_id = 's1',
#'          s_flow_out = 's_soybean', s_flow_in = 'i_soybean_demand',
#'            s_parameters = c('area', 'yield', 'price'), s_internal_pars = '')

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

#'Load stock internal parameter data set
#'@param x A rhapsody stock
#'@param path Either NA, which uses defaults to 'stockname.csv', or a file path giving the location of the stock internal data
#'@returns A rhapsody stock object
#'@examples
#'s_soybeans <- load_stock_data(s_soybeans, NA)

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



#'Load stock parameters: these are those that are dynamically updated by other model objects
#'@param x A rhapsody stock
#'@param isv A dataframe, typically that passed to the initial_parameters argument of init_model
#'@returns A rhapsody stock object
#'@examples
#'s_soybeans <- init_pars(s_soybeans, start_vals)

#' @export
setGeneric('init_pars', function(x, isv) standardGeneric('init_pars'))
setMethod('init_pars', 'stock', function(x, isv) {

  r <- which(isv[, 1] == x@s_name)

  for(i in r) {

    x@s_parameters[isv[i, 2]] <- isv[i,3]

  }

  x

})



#'Load stock parameters: these are those that are dynamically updated by other model objects
#'@param x A rhapsody stock object
#'@param mod A rhapsody model object
#'@returns A rhapsody stock object
#'@examples
#'s_soybeans <- update_internal_par(s_soybeans, sd.mod)

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



