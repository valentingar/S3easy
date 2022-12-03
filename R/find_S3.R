#'Find S3 classes and methods
#'
#'These functions help finding the class and function definitions for S3 classes
#'in the active project.
#'
#'@inheritParams new_S3
#'
#'
#'@export

find_S3class <- function(S3_name){
  search_pattern <- paste0("( *", S3_name," *\\<\\-)")
  files_with_function_definition <- grepl_files(search_pattern, proj_path("/R"), perl = TRUE)
}


