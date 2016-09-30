#' Calculating one new variable according to the expression.
#' 
calc_sg_val <-
  function(data,
           calc_c,
           calc_v,
           expr,
           vars_list = c("forma", "god", "final_code"),
           parallel = FALSE) {
    
    require(plyr) 
    require(dplyr)
    require(stringr)
    
    expr <- simpl_sg_expr(expression = expr, df_name = "df")

    ddply(data,
          .variables = vars_list,
          function(df) {
            df
            df %>%
              select_(.dots = vars_list) %>%
              distinct() %>%
              mutate(code = calc_c,
                     var = calc_v,
                     val = eval(parse(text = expr)))
            
          },
          .parallel = parallel) %>%
      tbl_df() %>% 
      bind_rows(data)
    
  }