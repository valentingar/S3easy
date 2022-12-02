



dots2args <- function(...){
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
