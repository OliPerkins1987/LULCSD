

.model <- setClass('model',
            slots=list(stocks = "list",
              flows  = "list",
                inputs  = "list",
                 outputs = "list",
                  parameters = "list",
                   run = "list",
                    taxon = "list"))


#'Initiliase a model - the core class for running a rhapsody system dynamics model. For a more detailed explanation see 'model' vignette
#'@param stock_list A list of stocks; typically the result of stocks_from_key
#'@param flow_list A list of flows; typically the result of flows_from_key
#'@param input_list A list of inputs; typically the result of inputs_from_key
#'@param output_list A list of outputs; typically the result of outputs_from_key
#'@param parameter_list A named list of model parameters, each beginning with the 'p_' taxonomic identifier
#'@param initial_stock_vals A three column dataframe; the 1st column specifies the relevant stock, the second the relevant stock parameter, and the third the initialising value
#'@param run a named list containing two integers: nsteps and step. These specify the number of model timesteps and the name -e.g. the year or month - of the 1st model timestep
#'@param taxon a named list used to specify model naming conventions; it is recommended to leave these as defaults.
#'@param load_stock_dat a logical - should model stock parameter data be loaded on initialisation?
#'@returns A rhapsody model object
#'@examples
#'mod    <- init_model(stocks_from_key(s_key), flows_from_key(f_key),
#'            inputs_from_key(i_key), outputs_from_key(o_key),
#'              initial_stock_vals = start_vals,
#'               list(p_land_conversion = 0.05, p_price_update = 0.01),
#'                run = list(nsteps = 31, step = 1990))
#'
#' @export
init_model <- function(stock_list, flows_list, input_list, output_list,
                       parameter_list, initial_stock_vals, #model parameters and initial values of stocks
                       run   = list(nsteps = 1, step = 1),
                       taxon = list('s_' = 'stocks',
                       'f_' = 'flows', 'ff' = 'family_flows', 'p_' = 'parameters',
                          'i_' = 'inputs', 'o_' = 'outputs'), load_stock_dat = T) {

  mod <- .model(stocks = stock_list,
                flows  = flows_list,
                inputs = input_list,
                outputs = output_list,
                parameters = parameter_list,
                run = run, taxon = taxon)

  if(load_stock_dat == T) {

    mod@stocks <- lapply(mod@stocks, load_stock_data, path = NA)

  }

  mod@stocks <- lapply(mod@stocks, init_pars, isv = initial_stock_vals)

  mod

}


########################################################

### Model methods

########################################################

### generic helper

#' @export
setGeneric("arg_parser", function(x, args) standardGeneric("arg_parser"))
setMethod("arg_parser", "model", function(x, args) {

  tax <- sapply(args, function(y) {

        x@taxon[[which(names(x@taxon) == substr(y, 1, nchar(names(x@taxon))))]]

      })

})


### scheduler
#'The scheduler updates the inputs and stock internal parameters; it then runs the model flows and captures model outputs
#'@param x A rhapsody model object
#'@returns A rhapsody model object
#'@examples
#'sd.mod    <- scheduler(sd.mod)
#' @export
setGeneric("scheduler", function(x, ...) standardGeneric("scheduler"))
setMethod("scheduler", "model", function(x, ...) {

  ### update inputs
  x@inputs <- lapply(x@inputs, update_input, x)

  ### update stock pars
  x@stocks <- lapply(x@stocks, update_internal_par, x)

  ####################################

  ### run flows

  ####################################

  for(i in 1:length(x@flows)) {

    ### get
    if(class(x@flows[[i]])[1] == 'family_flow') {

    x@flows[[i]]<- get_ff_args(x@flows[[i]], x, parse_args = F)

    } else {

    x@flows[[i]]<- get_flow_args(x@flows[[i]], x, parse_args = F)

    }

    ### run
    x@flows[[i]]<- run_flow(x@flows[[i]])


    ### set
    if(class(x@flows[[i]])[1] == 'family_flow') {

    x <- set_ff_output(x@flows[[i]], x)

    } else {

    x <- set_flow_output(x@flows[[i]], x)

    }

  }


  ### update outputs
  x@outputs <- lapply(x@outputs, get_output, x)

  x

})


### run the model
#'run_mod executes the scheduler using the information in the runs argument passed to a rhapsody model
#'@param x A rhapsody model object
#'@returns A rhapsody model object
#'@examples
#'sd.mod    <- run_mod(sd.mod)
#' @export
setGeneric("run_mod", function(x) "run_mod")
setMethod("run_mod", "model", function(x) {

  x@run$first_step <- x@run$step

  for(i in 1:x@run$nsteps) {

    x <- scheduler(x)

    x@run$step <- x@run$step + 1

  }

  x

})

### reset model timestep to 1
#'Helper for resetting the model timestep to 1
#'@param x A rhapsody model object
#'@returns A rhapsody model object
#'@examples
#'sd.mod    <- run_mod(sd.mod)
#'sd.mod$parameters$p_land_trans <- 0.01 ### some arbitrary change
#'sd.mod    <- reset_time(sd.mod)
#'sd.mod    <- run_mod(sd.mod)
#' @export
setGeneric("reset_time", function(x) "reset_time")
setMethod("reset_time", "model", function(x) {

  x@run$step <- x@run$first_step

  x

})


#'Runs a model multiple times with its first set of inputs; used to find convergence / equilibrium model state
#'@param x A rhapsody model object
#'@param n An integer
#'@returns A rhapsody model object
#'@examples
#'sd.mod    <- spin_up(sd.mod)
#'@export
setGeneric("spin_up",function(x, n) "spin_up")
setMethod("spin_up", "model", function(x, n){

  ### update inputs
  x@inputs <- lapply(x@inputs, update_input, x)

  ### update stock pars
  x@stocks <- lapply(x@stocks, update_internal_par, x)

  #out <- list()

  for(spin in 1:n) {




  for(i in 1:length(x@flows)) {

    ### get
    if(class(x@flows[[i]])[1] == 'family_flow') {

      x@flows[[i]]<- get_ff_args(x@flows[[i]], x, parse_args = F)

    } else {

      x@flows[[i]]<- get_flow_args(x@flows[[i]], x, parse_args = F)

    }

    ### run
    x@flows[[i]]<- run_flow(x@flows[[i]])


    ### set
    if(class(x@flows[[i]])[1] == 'family_flow') {

      x <- set_ff_output(x@flows[[i]], x)

    } else {

      x <- set_flow_output(x@flows[[i]], x)

      }

    }


  }



  x

  return(out)

})





