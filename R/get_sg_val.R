
get_sg_val <- 
  function(df, c, v) {
    
    value <- 
      (df %>% 
         filter(code %in% as.character(c), var %in% as.character(v)))$val
    
    if(length(value) == 0) {
      return(NA)
    } else {
      return(value)
    }
  }


