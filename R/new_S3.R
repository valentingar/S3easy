#' Create a new S3 class
#'
#' This function adds a new S3 class in a new file to the R/ directory of your package project
#' and sets up the general structure following the workflow described in
#' Advaced R 2 by Hadley Wickham.
#'
#' @param name The name of the class to be created
#' @param ... Any attributes of the new class. Enter in the form of 'name = default_value'
#' (without quotations). These will be added to the constructor and helper functions as arguments and
#' to the call to structure() as attribute. Remove arguments from the helper function that are not
#' entered by the user, but created within the helper function.
#' @param inherited_class All parent classes of the new class. Enter in the form of 'parent_class, grandparent_class'
#'
#' @importFrom usethis use_r
#'
#' @export

new_S3 <- function(name = NULL, ..., inherited_class = ""){
  file_path <- usethis::proj_path("R", name)

  class_arguments <- dots2args(...)
  class_arguments_name <- names(class_arguments)

  header_text <- generate_S3header(name,
                                   class_arguments_name,
                                   inherited_class)

  functions_text <- generate_S3functions(
    name,
    class_arguments,
    class_arguments_name,
    inherited_class)

  output_text <- styler::style_text(paste0(c(header_text, functions_text), collapse = ""))

  writeLines(output_text, paste0(file_path, ".R"))
  usethis::edit_file(paste0(file_path, ".R"))
  output_text
}


### HELPER FUNCTIONS -----------------

# Roxygen2 header
generate_S3header <- function(
    name,
    class_arguments_name,
    inherited_class){

  arguments_text <- paste0(paste0("#' @param ", class_arguments_name), collapse = "\n")

  header_text <- paste0("#' ", name, "\n#'\n",
                        "#' ", "The class ", name,
                        ifelse(inherited_class == "",
                               "",
                               paste0(" is a subclass of ", inherited_class, " and")),
                        " represents...\n#'\n",
                        arguments_text, "\n#'\n",
                        "#' @export"
                        )
  header_text
}

# Function code
generate_S3functions <- function(
    name,
    class_arguments,
    class_arguments_name,
    inherited_class = ""){

  argument_is_dots <- class_arguments_name == "..."
  got_dots = any(argument_is_dots)

  ### generate function arguments ###
  if (any(!(class_arguments[argument_is_dots] == ""))){
    message("removed default value for dots ('...').")
  }
  class_arguments[argument_is_dots] <- '...'
  class_arguments_collapse <- paste0(class_arguments, collapse = ",\n")

  ### Generate structure arguments ###
  # handle dots ...
  if (got_dots){
    message("Dots ('...') will be added as function argument, but not to attributes of S3 class.")
    class_arguments_name <- class_arguments_name[!argument_is_dots]
  }

  class_arguments_name_collapse <- paste0(
    class_arguments_name, " = ",
    class_arguments_name,
    collapse = ",\n")


  # Handle no argument
  no_arguments <- length(class_arguments_name) == 0
  if (no_arguments){
    class_arguments_name_collapse <- ""
    class_arguments_collapse <- ""
  }

  # handle inherited classes
  if (inherited_class == ""){
    classes_collapse <- paste0("'", name, "'")
  } else{
    inherited_class_tidy <- gsub(" ","", inherited_class)
    inherited_class_tidy <- unlist(strsplit(inherited_class_tidy, ","))
    valid_inherited_classes <- grepl("^((([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|[.])$",
                                     inherited_class_tidy)
    stopifnot("Provided inherited_class not valid.\n
              Please enter in the form 'parent_class, grandparent_class'" = all(valid_inherited_classes))
    stopifnot("class cannot inherit itself!" = !(name %in% inherited_class_tidy))
    classes_collapse <- paste0("c('", paste0(c(name, inherited_class_tidy), collapse = "', '"),"')")
  }

  constructor <-
    paste0("new_", name, " <- function(\n",
           class_arguments_collapse,
           "){\n",
           "# This is the low-lever constructor\n",
           "# It should be fast and efficient and is normally not user-facing:\n",
           "# -Outsource any complex coercion to the helper\n",
           "# -Remove function arguments that are not obligatory to create the object\n",
           "x <- structure(\n",
           ".Data = NULL , # edit object value to your needs\n",
           class_arguments_name_collapse,
           ifelse(no_arguments, "", ",\n"),
           "class = ", classes_collapse, "\n",
           ")\n",
           "x\n",
           "}\n"
    )

  helper <- paste0(name, " <- function(\n",
                   class_arguments_collapse,
                   "){\n",
                   "# Add code to coerce your object and validate the user input.\n",
                   "x <- new_", name,"(",
                   class_arguments_name_collapse,
                   ifelse(got_dots, ",\n...\n", ""),
                   ")\n",
                   "x <- validate_", name, "(x)\n",
                   "x\n",
                   "}\n"
  )

  validator <- paste0("validate_", name, " <- function(x){\n",
                      "#add tests to check your object is created correctly here \n",
                      "x\n",
                      "}\n")

  output_text <- paste0(
    "\n#### HELPER ####\n",
    helper,
    "\n#### CONSTRUCOR ####\n",
    constructor,
    "\n#### VALIDATOR ####\n",
    validator)
}


