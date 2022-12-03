



dots2args <- function(...){
  # takes dots and returns a named character vector
  # If an argument is unnamed it is assumed to be the name with no value
  dots <- rlang::enquos(...)
  dots_values <- sapply(dots, rlang::quo_get_expr)
  dots_names <- names(dots)
  dots_without_values <- "" == dots_names
  dots_names[dots_without_values] <- dots_values[dots_without_values]
  dots_values[dots_without_values] <- ""
  S3_arguments <- paste0(dots_names, ifelse(dots_without_values, "", " = "), dots_values)
  names(S3_arguments) = dots_names
  S3_arguments
}


grepl_files <- function(pattern, path, ...){

  files <- list.files(path, full.names = TRUE)
  matched_files <- sapply(files, function(file){
    file_text <- readLines(file)
    any(grepl(pattern, file_text, ...))
  })

  files[matched_files]
}
