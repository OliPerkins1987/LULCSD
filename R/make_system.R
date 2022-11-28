
###############################################################################

### Builds a system from a list of stocks and flows

###############################################################################

#' @export
stocks_from_key <- function(key, parse_flows = T) {
  
  s_stocks<- list()
  
  for(r in 1:nrow(key)) {
    
    if(key$s_type[r] == 'stock') {
            
          s <- new('stock')
          
    } else if(key$s_type[r] == 'family_stock') {
      
          s <- new('family_stock')
          
          s@s_family_members <- key$s_name[key$s_family_id == key$s_family_id[r]]
 
    }
    
    for(col in colnames(key)) {
      
      if(!grepl('parameters', col)) {
      
      attr(s, col) <- key[[col]][r]
      
      } else if(grepl('parameters', col)) {
        
      attr(s, col)[key[[col]][r]] <- 0
        
      }
      
    }
    
    
    if(parse_flows == T) {
      
      cols <- colnames(key)[grepl('flows', colnames(key))]
      
      for(col in cols) {
      
        attr(s, col) <- strsplit(attr(s, col), ',')[[1]]
        attr(s, col) <- gsub(' ', '', attr(s, col))

          }
        
    }

    s_stocks[[r]] <- s
    
  }
    

  s_stocks
  
  
}


##############################################################################

### Flows from key

##############################################################################

#' @export
flows_from_key <- function(key, parse_stocks = T) {
  
  f_flows <- list()
  
  for(r in 1:nrow(key)) {
    
    if(key$f_type[r] == 'flow') {
      
      f <- new('flow')
      
    } else if(key$f_type[r] == 'family_flow') {
      
      f <- new('family_flow')
      
    }
    
    for(col in colnames(key)) {
      
      attr(f, col) <- key[[col]][r]
      
    }
    
    
    if(parse_stocks == T) {
      
      cols <- colnames(key)[grepl('stocks', colnames(key))]
      
      for(col in cols) {
        
        attr(f, col) <- strsplit(attr(f, col), ',')[[1]]
        attr(f, col) <- gsub(' ', '', attr(f, col))
        
      }
      
    }
    
    f_flows[[r]] <- f
    
  }

  
  f_flows
  
  
}



##############################################################################

### Inputs from key

##############################################################################

#' @export
inputs_from_key <- function(key, path = NA, data_type = 'generic') {
  
  i_inputs <- list()
  
  for(r in 1:nrow(key)) {
    
    if(key$i_type[r] == 'input') {
      
      i <- init_input(name = character(), id = character(), 
                      to = character(), dat_type = data_type)
      
    } 
    
    for(col in colnames(key)) {
      
      attr(i, col) <- key[[col]][r]
      
    }

    
    i             <- load_input(i, path = NA)
    i_inputs[[r]] <- i
    
  }
  
  
  i_inputs
  
  
}


##############################################################################

### Outputs from key

##############################################################################

#' @export
outputs_from_key <- function(key) {
  
  o_outputs<- list()
  
  for(r in 1:nrow(key)) {
    
    if(key$o_type[r] == 'output') {
      
      i <- new('output')
      
    } 
    
    for(col in colnames(key)) {
      
      attr(i, col) <- key[[col]][r]
      
    }
    
    o_outputs[[r]] <- i
    
  }
  
  
  o_outputs
  
  
}


##############################################################################

### Make system

##############################################################################

#' @export
make_system <- function(stocks_list, flows_list, tidy = F) {
  
  g  <- character()
 
  for(f in 1:length(flows_list)) {
    
    tmp_edges <- sapply(flows_list[[f]]@f_from_stocks, function(x) {
      
      sapply(flows_list[[f]]@f_to_stocks, function(y){
        
        c(x, y)})
      
            })
    
    
    g <- c(g, tmp_edges)
    
    
    
  }
  
  if(tidy == T) {
    
    g <- substr(g, 3, nchar(g))
    
  }
  
  g <- igraph::graph(g)
   
  g

  
}
  



