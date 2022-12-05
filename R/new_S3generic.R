#' Create a new S3 generic
#'
#' These functions help set up the base structure for a new S3 generic
#'
#' @param name The name of the new generic
#' @param ... The input parameters of the new generic.
#' @inheritParams new_S3class
#'
#' @export

new_S3generic <- function(name,
                          ...,
                          S3_output = "clipboard",
                          S3_overwrite = FALSE){

  arguments <- dots2args(...)
  arguments_collapse <- paste0(arguments, collapse = ",\n")
  arguments_name <- names(arguments)

  functions_text <- paste0(name, " <- function(\n",
                        arguments_collapse,
                        "){\n",
                        "UseMethod('",name,"')\n",
                        "}")

  arguments_text <- generate_arguments_header(arguments_name)
  header_text <- paste0("#' ", name, "\n",
                        "#'\n",
                        "#' Describe your generic here\n",
                        "#'\n",
                        arguments_text, "\n#'\n",
                        "#' @export\n"
                        )

  control_output(name,
                 S3_output,
                 S3_overwrite,
                 S3_add_method = FALSE,
                 functions_text,
                 header_text)
}
