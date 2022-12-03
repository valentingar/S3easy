



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

generate_arguments_header <- function(arguments_name){
  arguments_text <- paste0(paste0("#' @param ", arguments_name), collapse = "\n")
}


grepl_files <- function(pattern, path, ...){

  files <- list.files(path, full.names = TRUE)
  matched_files <- sapply(files, function(file){
    file_text <- readLines(file)
    any(grepl(pattern, file_text, ...))
  })

  files[matched_files]
}


control_output <- function(name,
                           S3_output,
                           S3_overwrite,
                           functions_text,
                           header_text){

  output_file_text <- styler::style_text(paste0(c(header_text, functions_text), collapse = ""))
  functions_text <- styler::style_text(functions_text)

  if (S3_output == "file") {
    # check for existing file at file path
    file_path <- paste0(usethis::proj_path("R", name), ".R")
    if (file.exists(file_path) & !S3_overwrite){
      stop("There already exists a file at ", file_path, " and S3_overwrite is FALSE")
    }

    # check for existing function definition
    files_with_function_definition <- find_function(name)
    if (length(files_with_function_definition) > 0 && !all(files_with_function_definition == file_path)){
      message("class already defined? At:\n", files_with_function_definition)
      continue_user_input <- readline("continue anyway? (y/N)")
      if (!(continue_user_input == "y")){
        return(invisible(functions_text))
      }
    }

    writeLines(output_file_text, file_path)
    usethis::edit_file(file_path)
  } else if (S3_output == "clipboard") {
    clipr::write_clip(functions_text)
  }

  invisible(functions_text)

}
