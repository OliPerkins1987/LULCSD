

.model <- setClass('model', 
            slots=list(stocks = "list", 
              flows  = "list", 
                inputs  = "list", 
                 outputs = "list",
                  parameters = "list",
                   run = "list",
                    taxon = "list"))

#' @export
init_model <- function(stock_list, flows_list, input_list, output_list, parameter_list,
                       run   = list(nsteps = 1, step = 1),
                       taxon = list('s_' = 'stocks', 
                       'f_' = 'flows','p_' = 'parameters', 
                          'i_' = 'inputs', 'o_' = 'outputs')) {
  
  mod <- .model(stocks = stock_list, 
                flows  = flows_list, 
                inputs = input_list, 
                outputs = output_list,
                parameters = parameter_list, 
                run = run, taxon = taxon)
  
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

#' @export
setGeneric("scheduler", function(x, ...) standardGeneric("scheduler"))
setMethod("scheduler", "model", function(x, ...) {
  
  ### update inputs
  
  x@inputs      <- lapply(x@inputs, update_input, x)
  
  
  ### run flows
  for(i in 1:length(x@flows)) {
    
    x@flows[[i]]<- get_flow_args(x@flows[[i]], x)
    x@flows[[i]]<- run_flow(x@flows[[i]])
    x           <- set_flow_output(x@flows[[i]], x)
    
  }
  
  
  ### stock functions
  ### PASS
  
  
  ### update outputs
  x@outputs <- lapply(x@outputs, get_output, x)
  
  x
  
})

#' @export
setGeneric("run_mod", function(x) "run_mod")
setMethod("run_mod", "model", function(x) {

  x@run$first_step <- x@run$step
  
  for(i in x@run$step:x@run$nsteps) {
    
    x <- scheduler(x)
    
    x@run$step <- x@run$step + 1
    
  }
  
  x
  
})

#' @export
setGeneric("reset_time", function(x) "reset_time")
setMethod("reset_time", "model", function(x) {
  
  x@run$step <- x@run$first_step
  
  x
  
})


