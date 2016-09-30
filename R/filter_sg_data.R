filter_sg_data <-
  function(df,
           codes,
           vars) {
    require(dplyr)
    left_join(data.frame(code = as.character(codes), var = as.character(vars), stringsAsFactors = FALSE),
              df,
              by = c("code", "var")) %>%
      filter(!is.na(final_code)) %>% 
      tbl_df()
  }