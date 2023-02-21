
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

setGeneric("build_scenario", function(e, base_t, finish_t, na.constant,
            inputs, stock_pars, initial_values) standardGeneric("build_scenario"))

setMethod("build_scenario", "experiment", function(e, base_t, finish_t, na.constant = TRUE,
              inputs = NULL, stock_pars = NULL, initial_values = NULL) {
  
  ### perturb stock internal parameters
  if(!is.null(inputs)) {
    
    i.names <- sapply(e@mod@inputs, function(z) {z@i_name})
    
    for(i in 1:ncol(inputs)) {
      
      i.match   <- which(i.names == colnames(inputs)[i])
      
      if(length(i.match) > 0) {
        
      t.match   <- which(e@mod@inputs[[i.match]]@i_data[, 1] == base_t)
      base.val  <- e@mod@inputs[[i.match]]@i_data[t.match, 2]
      
      e@mod@inputs[[i.match]]@i_data <- array(unlist(data.frame('time_step' = base_t:finish_t, 'value' = inputs[, i] * base.val)), 
                                        dim = c(length(base_t:finish_t), 2))
      
      }
        
    }
    
  }
  
  ### perturb stock internal parameters
  
  if(!is.null(stock_pars)) {
    
    s.names <- sapply(e@mod@stocks, function(z) {z@s_name})
      
    ### names of stock_pars should be name of an internal stock parameter
    
    for(i in 1:length(stock_pars)) {
      
      s.par_key <- names(stock_pars)[i] 
      s.dat     <- stock_pars[[i]]
      
      ### colnames names should match column names of a model stock
      
      for(j in 1:ncol(s.dat)) {
        
        s.match   <- which(s.names == colnames(s.dat)[j])
        
        if(length(s.match) > 0) {
          
          par.match <- which(colnames(e@mod@stocks[[s.match]]@s_parameters$internal) == s.par_key)
          t.match   <- which(e@mod@stocks[[s.match]]@s_parameters$internal[, 1] == base_t)
          base.val  <- e@mod@stocks[[s.match]]@s_parameters$internal[t.match, par.match]
          
          ### create container for new data
          
          if(!finish_t %in% e@mod@stocks[[s.match]]@s_parameters$internal[, 1]) {
            
            new.rows           <- data.frame(matrix(nrow = length(base_t:finish_t), 
                                      ncol = ncol(e@mod@stocks[[s.match]]@s_parameters$internal)))
            new.rows[, 1]      <- base_t:finish_t
            colnames(new.rows) <- colnames(e@mod@stocks[[s.match]]@s_parameters$internal)
            
            e@mod@stocks[[s.match]]@s_parameters$internal <- rbind(e@mod@stocks[[s.match]]@s_parameters$internal, 
                                                                   new.rows)
            
            e@mod@stocks[[s.match]]@s_parameters$internal <- e@mod@stocks[[s.match]]@s_parameters$internal[
                                                              !duplicated(e@mod@stocks[[s.match]]@s_parameters$internal[, 1]), ]
            
          }
          
          ### fill scenario data
          
          end.match                       <- which(e@mod@stocks[[s.match]]@s_parameters$internal[, 1] == finish_t)
          
          e@mod@stocks[[s.match]]@s_parameters$internal[
            t.match:end.match, par.match] <- e@mod@stocks[[s.match]]@s_parameters$internal[
                                                t.match, par.match] * s.dat[, j]
          
          
        }
        
      }
      
      
    } 
   
    
  }
 
  if(na.constant == TRUE) {
    
    if(!is.null(stock_pars)) {
    
    e@mod@stocks <- lapply(e@mod@stocks, function(z) {
      
      int.pars <- z@s_parameters$internal 
      
      if(length(int.pars) > 0) {
      
        for(k in 1:ncol(int.pars)) {
        
          int.pars[is.na(int.pars[, k]), k] <- int.pars[which(int.pars[, 1] == base_t), k][1]
        
        }
      
        z@s_parameters$internal <- int.pars
      
      }
      
      z
        })
    
    
    }
  }
       
  e
  
})

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
   
   cl <- foreach::makeCluster(e@n_cores)
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
  
  if(!is.null(kwargs$.scale) & kwargs$.scale == T) {
    
    results <- lapply(results, function(x) {data.frame(apply(x, 2, function(y) {(y-mean(x[, 1]))/sd(x[, 1])}))})
    
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
        r[[(i-1)]] <- sqrt(mean((x[, i] - x[, 1])^2))
        
      }
      
      names(r) <- 1:length(r)
      r        <- unlist(r)
      
      })
    

    results       <- setNames(apply(data.frame(results), 1, mean), 1:nrow(data.frame(results)))
    results       <- results[order(results)][1:floor(kwargs$.frac * length(results))]
     
    gc()
    
    return(list(runs   = names(results), 
                metric = results))
      
  } else if(method == 'tolerance') {
  
  
    ### ? runs <= threshold
  
    ### requires .tolerance (0-inf) & .metric (string c(rmse, mae)) arguments
    ### !only rmse implemented so far
    
    if(is.null(kwargs$.tolerance)) {
      
      warning('Null value passed to error tolerance; choosing 10% of variable means')
      
      kwargs$.tolerance <- 0.1
      
    }
    
    results <- lapply(results, function(x) {
      
      
      r <- list()
      
      for(i in 2:ncol(x)) {
        
        ### calculate rmse
        r[[(i-1)]] <- sqrt(mean((x[, i] - x[, 1])^2)) / sd(x[, 1])
        
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











