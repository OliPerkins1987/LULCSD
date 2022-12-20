
#' @export
.experiment <- setClass('experiment', 
                slots=list(mod = "model", parameter_set= "data.frame", initial_stock_vals = "data.frame",
                  n_runs = 'numeric', results = 'list', n_cores = 'numeric'))

Experiment <- function(.model, .parameter_set, .initial,
                       .n_runs, .n_cores = 1) {
  
     e <- .experiment(mod= .model, parameter_set = .parameter_set, initial_stock_vals = .initial,
                      n_runs = .n_runs, n_cores = .n_cores, results = list())
  
     e
     
}



########################################################

### Experiment methods

########################################################

setGeneric("single_experiment", function(e, run_numb) standardGeneric("single_experiment"))
setMethod("single_experiment", "experiment", function(e, run_numb){
  
  e@mod@stocks <- lapply(e@mod@stocks, init_pars, isv = e@initial_stock_vals)
  
  for(p in 1:length(e@mod@parameters)) {
    
    e@mod@parameters[[p]] <- e@parameter_set[run_numb, p]
    
  }
  
  e@mod                 <- run_mod(e@mod)
  e@results[[run_numb]] <- e@mod@outputs
  
  e
  
})



setGeneric("run_experiment", function(e) standardGeneric("run_experiment"))
setMethod("run_experiment", "experiment", function(e) {
  
 if(e@n_cores > 1) {
   
   cl <- foreach::makeCluster(3)
   foreach::registerDoParallel(cl)
   
   foreach::foreach(run = 1:e@n_runs) %dopar% {
     
     e            <- single_experiment(e, run)
     e@mod        <- reset_time(e@mod)
     
   }
   
   
 } else if (e@n_cores == 1) {
   
   for(run in 1:e@n_runs) {
     
     e     <- single_experiment(e, run)
     e@mod <- reset_time(e@mod)
     
   }
   
 } else {
   
   stop('The number of cores assigned must be an integer > 0')
   
    }
  
  e
  
})



###############################################################################

### Post - processing

###############################################################################

setGeneric("select_pars", function(e, data, method = 'best', ...) standardGeneric("select_pars"))
setMethod("select_pars", "experiment", function(e, data, method = 'best', ...) {
  
  
  kwargs <- list(...)
  results<- setNames(lapply(1:length(e@mod@outputs), function(i) {
             data.frame(matrix(nrow = nrow(data), ncol = 1))}), 
              sapply(e@mod@outputs, function(x) {x@o_name}))
  
  
  ### ?filter outputs for history matching
  
  if(".subset" %in% names(kwargs)) {
    results <- results[names(results) %in% kwargs$.subset]}
  
  
  ### combine data and model outputs
  
  var.cols <- which(sapply(e@mod@outputs, function(z) {z@o_name}) %in% names(results))
  
  for(i in 1:length(results)) {
    
    results[[i]][, 1] <- data[, i]
    
    for(j in 1:e@n_runs) {
      
      results[[i]]         <- cbind(results[[i]], 
                                data.frame(unlist(e@results[[j]][[var.cols[i]]]@o_outputs)))
      
    }
    
    colnames(results[[i]]) <- c('Data', paste0('run_', 1:e@n_runs))
    
  }
  
  
  ###########################################
  
  ### rejection sampling
  
  ###########################################
  
  ### ?best n runs?
  
  if(method == 'best') {
  
    ### requires .frac (numeric [0-1]) and .metric (string c(rmse, mae)) arguments
    ### !only rmse implemented so far
    
    if(is.null(kwargs$.frac)) {
      
      warning('Null value passed to parameter fraction retained; choosing top 10%')
      
      kwargs$.frac <- 0.1
      
    }
    
    
    results <- lapply(results, function(x) {
      

      r <- list()
      
      for(i in 2:ncol(x)) {
        
        ### calculate rmse
        r[[(i-1)]] <- sqrt(mean((x[, i] - x[, 1])^2)) / mean(x[, 1])
        
      }
      
      names(r) <- 1:length(r)
      r        <- unlist(r)
      r        <- unlist(r[order(unlist(r))][1:floor(kwargs$.frac * length(r))])
      
      })
    
    
    gc()
    
    return(list(runs   = as.numeric(unlist(lapply(results, names))), 
                metric = results))
      
  } else if(method == 'tolerance') {
  
  
    ### ? runs <= threshold
  
    ### requires .tolerance (0-inf) & .metric (string c(rmse, mae)) arguments
    ### !only rmse implemented so far
    
    if(is.null(kwargs$.tolerance)) {
      
      warning('Null value passed to error tolerance; choosing top 10% of variable means')
      
      kwargs$.tolerance <- 0.1
      
    }
    
    results <- lapply(results, function(x) {
      
      
      r <- list()
      
      for(i in 2:ncol(x)) {
        
        ### calculate rmse
        r[[(i-1)]] <- sqrt(mean((x[, i] - x[, 1])^2)) / mean(x[, 1])
        
      }
      
      names(r) <- 1:length(r)
      r        <- unlist(r)
      r        <- r[r <= kwargs$.tolerance]
      
    })
    
    
    gc()
    
    return(list(runs   = as.numeric(unlist(lapply(results, names))), 
                metric = results))
    
  }
  
  
})











