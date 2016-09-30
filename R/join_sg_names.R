#' Add variables names 
join_sg_names <-
  function(data, lang = "eng", namesPath = "data/code_names.txt") {
  if (any("eng", "ENG", "Eng", "E")) {
    data %>%
      left_join(
        read.delim(
          file = namesPath,
          header = TRUE,
          sep = "\t",
          stringsAsFactors = FALSE,
          colClasses = "character"
        ),
        by = "code"
      ) %>%
      select(-codeNameUkr)
  } else if (any("UKR", "ukr", "Ukr", "U")) {
    data %>%
      left_join(
        read.delim(
          file = namesPath,
          header = TRUE,
          sep = "\t",
          stringsAsFactors = FALSE,
          colClasses = "character"
        ),
        by = "code"
      ) %>%
      select(-codeNameEng)
    
  } else {
    data %>%
      left_join(
        read.delim(
          file = namesPath,
          header = TRUE,
          sep = "\t",
          stringsAsFactors = FALSE,
          colClasses = "character"
        ),
        by = "code"
      )
  }
  
}