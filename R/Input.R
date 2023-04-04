
###############################################################################

### Defines core class for an input

###############################################################################

#' @export
.input <- setClass('input',
           slots=list(i_name="character", i_id="character",
            i_to = "character", i_dat_type = "character",
            i_data = "array", i_current_val = "numeric"))

#' @export
init_input <- function(name, id, to, dat_type, data = array(),
                       current_val = numeric()
                       ) {

  x <- .input(i_name = name, i_id = id, i_to = to, i_dat_type = dat_type, i_data = data)

}


### method for loading data
#' @export
setGeneric("load_input", function(x, path) standardGeneric("load_input"))
setMethod("load_input", "input", function(x, path) {

  if(is.na(path)) {

    path <- x@i_name

    path <- ifelse(x@i_dat_type == 'spatial', paste0(path, '.nc'), paste0(path, '.csv'))

  } else {


    path <- paste0(path, '/', x@i_name)

    path <- ifelse(x@i_dat_type == 'spatial', paste0(path, '.nc'), paste0(path, '.csv'))

  }

  if(x@i_dat_type == 'spatial') {

    ### NB currently flips spatial object into 2-d array - ? best approach ?

    x@i_data            <- brick(path)[]
    col                 <- ncol(x@i_data)
    r                   <- nrow(x@i_data)
    x@i_data            <- cbind(rep(1:col, each = r, array(x@i_data,
                                       dim = col*nrow(x@i_data))))


  } else{

    tmp      <- read.csv(path)
    x@i_data <- array(unlist(tmp),
                   dim = c(nrow(tmp), ncol(tmp)))

  }

  x

    })


### method for updating by timestep
setGeneric('update_input', function(x, mod) standardGeneric("update_input"))
setMethod('update_input', 'input', function(x, mod){

  step <- mod@run$step

  if(length(dim(x@i_data)) == 2) {

    x@i_current_val <- x@i_data[x@i_data[, 1] == step, 2]

  } else if(length(dim(x@i_data)) == 3) {

    x@i_current_val <- x@i_data[, , step]

  } else {

    stop('Input data must be either a 2-d or 3-d array')

    }

  x

})




