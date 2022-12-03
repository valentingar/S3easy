#' Create a new S3 class
#'
#' This function adds a new S3 class in a new file to the R/ directory of your
#' package project and sets up the general structure following the workflow
#' described in Advaced R 2 by Hadley Wickham.
#'
#' @param S3_name The name of the class to be created
#' @param ... The base object and any attributes of the new class. The first
#'   entry is the base object. Enter in the form of 'name = default_value'
#'   (without quotations). These will be added to the constructor and helper
#'   functions as arguments and to the call to structure(). H as attribute.
#'   Remove arguments from the helper function that are not entered by the user,
#'   but created within the helper function.
#' @param S3_inherited_class All parent classes of the new class. Enter in the
#'   form of 'parent_class, grandparent_class'
#' @param S3_output Control the output of the function: Either 'clipboard' or
#'   'file' or 'console'. Default 'clipboard'
#' @param S3_overwrite Control whether to overwrite an existing file. Default
#'   FALSE.
#'
#' @importFrom usethis use_r
#'
#' @export

new_S3 <- function(S3_name = NULL,
                   ...,
                   S3_inherited_class = "",
                   S3_output = "clipboard",
                   S3_overwrite = FALSE) {

  S3_output <- match.arg(S3_output, c("clipboard", "file", "none"))
  stopifnot("S3_overwrite must be TRUE/FALSE" = is.logical(S3_overwrite))

  class_arguments <- dots2args(...)
  class_arguments_name <- names(class_arguments)

  header_text <- generate_S3header(
    S3_name,
    class_arguments_name,
    S3_inherited_class
  )

  functions_text <- generate_S3functions(
    S3_name,
    class_arguments,
    class_arguments_name,
    S3_inherited_class
  )

  output_file_text <- styler::style_text(paste0(c(header_text, functions_text), collapse = ""))
  functions_text <- styler::style_text(functions_text)

  if (S3_output == "file") {
    # check for existing file at file path
    file_path <- paste0(usethis::proj_path("R", S3_name), ".R")
    if (file.exists(file_path) & !S3_overwrite){
      stop("There already exists a file at ", file_path, " and S3_overwrite is FALSE")
    }

    # check for existing function definition
    files_with_function_definition <- find_S3class(S3_name)
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


### HELPER FUNCTIONS -----------------

# Roxygen2 header
generate_S3header <- function(S3_name,
                              class_arguments_name,
                              S3_inherited_class) {
  arguments_text <- paste0(paste0("#' @param ", class_arguments_name), collapse = "\n")

  header_text <- paste0(
    "#' ", S3_name, "\n#'\n",
    "#' ", "The class ", S3_name,
    ifelse(all(S3_inherited_class == ""),
      "",
      paste0(" is a subclass of ", S3_inherited_class, " and")
    ),
    " represents...\n#'\n",
    arguments_text, "\n#'\n",
    "#' @export"
  )
  header_text
}

# Function code
generate_S3functions <- function(S3_name,
                                 class_arguments,
                                 class_arguments_name,
                                 S3_inherited_class = "") {
  argument_is_dots <- class_arguments_name == "..."
  got_dots <- any(argument_is_dots)

  stopifnot("Must have at least one argument: the base type" =
              length(class_arguments_name[!argument_is_dots]) > 0)

  ### generate function arguments ###
  if (any(!(class_arguments[argument_is_dots] == ""))) {
    message("removed default value for dots ('...').")
  }
  class_arguments[argument_is_dots] <- "..."
  class_arguments_collapse <- paste0(class_arguments, collapse = ",\n")
  class_arguments_nodots <- class_arguments[!argument_is_dots]
  class_arguments_nodots_collapse <- paste0(class_arguments_nodots, collapse = ",\n")

  ### Generate structure arguments ###
  # handle dots ...
  if (got_dots) {
    message("Dots ('...') will be added as function argument to the helper,
            but not to attributes of S3 class.")
    class_arguments_name <- class_arguments_name[!argument_is_dots]
  }

  # in structure() the first element is the base type
  class_arguments_name_structure_collapse <- paste0(
    class_arguments_name[-1], " = ",
    class_arguments_name[-1],
    collapse = ",\n"
  )

  # in call to constructure, all arguments are passed
  class_arguments_name_collapse <- paste0(
    class_arguments_name, " = ",
    class_arguments_name,
    collapse = ",\n"
  )

  # handle inherited classes
  if (all(S3_inherited_class == "")) {
    classes_collapse <- paste0("'", S3_name, "'")
  } else {
    S3_inherited_class_tidy <- gsub(" ", "", S3_inherited_class)
    S3_inherited_class_tidy <- unlist(strsplit(S3_inherited_class_tidy, ","))
    valid_S3_inherited_classes <- grepl(
      "^((([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|[.])$",
      S3_inherited_class_tidy
    )
    stopifnot("Provided S3_inherited_class not valid.\n
              Please enter in the form 'parent_class, grandparent_class'" = all(valid_S3_inherited_classes))
    stopifnot("class cannot inherit itself!" = !(S3_name %in% S3_inherited_class_tidy))
    classes_collapse <- paste0("c('", paste0(c(S3_name, S3_inherited_class_tidy), collapse = "', '"), "')")
  }

  constructor <-
    paste0(
      "new_", S3_name, " <- function(\n",
      class_arguments_nodots_collapse,
      "){\n",
      "# This is the low-lever constructor\n",
      "# It should be fast and efficient and is normally not user-facing:\n",
      "# -Outsource any complex coercion to the helper\n",
      "# -Remove function arguments that are not obligatory to create the object\n",
      "x <- structure(\n",
      class_arguments_name[1], ", # this is the base object\n",
      class_arguments_name_structure_collapse,",\n",
      "class = ", classes_collapse, "\n",
      ")\n",
      "x\n",
      "}\n"
    )

  helper <- paste0(
    S3_name, " <- function(\n",
    class_arguments_collapse,
    "){\n",
    "# Add code to coerce your object and validate the user input.\n",
    "x <- new_", S3_name, "(",
    class_arguments_name_collapse,
    ")\n",
    "x <- validate_", S3_name, "(x)\n",
    "x\n",
    "}\n"
  )

  validator <- paste0(
    "validate_", S3_name, " <- function(x){\n",
    "#add tests to check your object is created correctly here \n",
    "x\n",
    "}\n"
  )

  output_text <- paste0(
    "\n#### HELPER ####\n",
    helper,
    "\n#### CONSTRUCOR ####\n",
    constructor,
    "\n#### VALIDATOR ####\n",
    validator
  )
}
