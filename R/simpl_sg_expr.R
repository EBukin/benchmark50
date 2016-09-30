simpl_sg_expr <-
  function(expression, df_name = "df") {
    require(plyr)
    require(dplyr)
    require(stringr)
    
    # expr <- "[123_G1] + ([233_G12] - [235_G17]) / [223_G10]"
    pat <- "\\[\\d{1,}_\\D{1,2}\\d{1,2}\\]"
    
    all_cases <-
      c(1:(as.data.frame(str_locate_all(expression, pat)[[1]]) %>% nrow()))
    l_ply(all_cases,
          function(x) {
            one_bit <- str_sub(expression, str_locate(expression, pat))
            code <- str_match(one_bit, "\\d{1,}(?=_)")[[1]]
            var <- str_match(one_bit, "[a-zA-Z]{1,2}\\d{1,2}")[[1]]
            one_bit <-
              str_c('get_sg_val(df=', df_name, ',c=\"', code, '\",v=\"', var, '\")')
            str_sub(expression, str_locate(expression, pat)) <<-
              one_bit
          })
    return(expression)
  }