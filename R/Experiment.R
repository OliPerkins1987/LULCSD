


.experiment <- setClass('experiment',
                slots=list(mod = "model", parameter_set= "data.frame", initial_stock_vals = "data.frame",
                  n_runs = 'numeric', results = 'list', n_cores = 'numeric'))


### Experiment class
#'Experiment class: scenario building, batch running & parameter selection
#'@param .model A rhapsody model object
#'@param .parameter_set A data frame with 1 column per model parameter
#'@param .initial A data frame to set initial stock parameters, similar to that in the init_model function
#'@param .n_runs Integer, number of model runs to conduct from parameter set
#'@param .n_cores Integer, used to set cores for parallel processing
#'
#'@returns A rhapsody experiment object
#'@examples
#'
#'Priors <- data.frame(p_land_trans = runif(1000, 0.00001, 0.001), p_price_update = runif(1000, 0.01, 0.1))
#'sd.exp <- Experiment(sd.mod, Priors, start_vals, nrow(Priors))

#' @export
Experiment <- function(.model, .parameter_set, .initial,
                       .n_runs, .n_cores = 1) {

     e <- .experiment(mod= .model, parameter_set = .parameter_set, initial_stock_vals = .initial,
                      n_runs = .n_runs, n_cores = .n_cores, results = list())

     e

}



########################################################

### Experiment methods

########################################################

### Build a model scenario
#'Experiment method for building future scenarios for a model
#'@param e A rhapsody experiment object
#'@param base_t An integer denoting the starting timestep for the scenario
#'@param finish_t An integer denoting the finishing timestep for the scenario
#'@param na.constant Logical: should inputs and stock parameters not included in scenario input files be held constant?
#'@param inputs A dataframe: each column should be named after a model input
#'@param stock_pars A list of dataframes: each dataframe in the list should be named after an internal stock parameter, with the column names corresponding to the relevant model stock
#'@param initial_values A dataframe to initialise stock parameters
#'@returns A rhapsody experiment object
#'@examples
#'inputs     <- data.frame(Year = 2020:2050, i_soybean_demand = seq(1, 2, by = 1/30))
#'stock_pars <- list(s_yield = data.frame(Year = 2020:2050, soybeans = seq(1, 1.25, by = 0.25/30)))
#'sd.exp     <- build_scenario(sd.exp, base_t = 2020, finish_t = 2050, na.constant = T, inputs = inputs, stock_pars=  stock_pars, initial_values= start_vals)
#' @export

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


### Run an experiment one time
#'Function for running a model experiment a single time. Useful for checking outputs before a large batch-run
#'@param e A rhapsody experiment object
#'@param run_numb Integer - used to pick a parameter set from within the declared ranges
#'
#'@returns A rhapsody experiment object
#'@examples
#'
#'sd.exp <- single_experiment(sd.exp, 128)
#' @export
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

### Run an experiment one time
#'Function for running a model experiment a single time. Useful for checking outputs before a large batch-run
#'@param e A rhapsody experiment object
#'@returns A rhapsody experiment object, with results stored in the results slot
#'@examples
#'
#'sd.exp <- run_experiment(sd.exp)
#' @export
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

### select experiment parameters
#'Select model parameters through history matching
#'@param e A rhapsody experiment object
#'@param data A dataframe containing observations
#'@param method A character string; either 'best' or 'tolerance'. Currently only best is implemented
#'@param .subset A character string specifying which model outputs to history match
#'@param .frac A double (0-1); determines what fraction of model parameters to select if 'best' is chosen method
#'@param .scale A logical: should observations & model outputs be scaled before rmse is calculated?
#'@returns A named list with two parts: $runs, which gives the row number of the selected runs; and $metric, which reports the model selection metric for the chosen runs
#'@examples
#'
#'### fake data for history matching
#'dat              <- data.frame(soybean_area = seq(1000, 500000, by = (500000-1000)/30), forest_area = seq(1000000, 500000, by = 0-(500000/30)))
#'noise            <- sapply(seq(1000, 500000, by = (500000-1000)/30), function(z) {rnorm(1, z, z/3)})
#'dat$soybean_area <- dat$soybean_area + noise
#'dat$forest_area  <- dat$forest_area - noise
#'
#'### filter the parameters based on their fit to the observations
#'result <- select_pars(sd.exp, data = dat, method = 'best', .frac = 0.01, .scale = T,
#'                      .subset = sapply(sd.exp@mod@outputs, function(z) {z@o_name})[c(1:2)])
#'
#'### look at the results
#'summary(Priors)
#'summary(Priors[result$runs, ])

#' @export
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











