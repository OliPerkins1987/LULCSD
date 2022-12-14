

.model <- setClass('model', 
            slots=list(stocks = "list", 
              flows  = "list", 
                inputs  = "list", 
                 outputs = "list",
                  parameters = "list",
                   run = "list",
                    taxon = "list"))

#' @export
init_model <- function(stock_list, flows_list, input_list, output_list, 
                       parameter_list, initial_stock_vals, #model parameters and initial values of stocks
                       run   = list(nsteps = 1, step = 1),
                       taxon = list('s_' = 'stocks', 
                       'f_' = 'flows', 'ff' = 'family_flows', 'p_' = 'parameters', 
                          'i_' = 'inputs', 'o_' = 'outputs'), load_dat = T) {
  
  mod <- .model(stocks = stock_list, 
                flows  = flows_list, 
                inputs = input_list, 
                outputs = output_list,
                parameters = parameter_list, 
                run = run, taxon = taxon)
  
  if(load_dat == T) {
    
    mod@inputs <- lapply(mod@inputs, load_input, path = NA)
    mod@stocks <- lapply(mod@stocks, load_stock_data, path = NA)
    mod@stocks <- lapply(mod@stocks, init_pars, isv = initial_stock_vals)
      
  }
  
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

#' @export
setGeneric("reset_time", function(x) "reset_time")
setMethod("reset_time", "model", function(x) {
  
  x@run$step <- x@run$first_step
  
  x
  
})

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
    
    
    #out[[spin]] <- list(price = unlist(lapply(1:4, function(i){x@stocks[[i]]@s_parameters$s_price})), 
    #                 area  = unlist(lapply(1:4, function(i){x@stocks[[i]]@s_parameters$s_area})))
    
    #print(x@stocks[[2]]@s_parameters$s_area + x@stocks[[1]]@s_parameters$s_area)
    
  }
  
  
  
  x
  
  return(out)
  
})





