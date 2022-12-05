#' Define new S3 methods
#'
#' This function helps you to generate new methods for existing generics. It
#' automatically applies a structure that matches the generic.
#'
#' @param S3_generic A generic function. Enter as a call to that function,
#' @param S3_name The name of the class for which this method is written
#'   without parantheses: S3_generic = myfun
#' @inheritParams new_S3class
#'
#' @export

new_S3method <- function(S3_generic,
                         S3_name,
                         S3_output = "clipboard"
){
  stopifnot("Please provide an actual function to S3_generic: S3_generic = my_fun"
            = is.function(S3_generic))
  stopifnot("S3_generic is no S3 generic function!" = utils::isS3stdGeneric(S3_generic))
  arguments <- formals(S3_generic)
  arguments_name <- names(arguments)

  arguments_collapse <- paste0(paste0(arguments_name, " = ", arguments),
                               collapse = ",\n")
  arguments_collapse <- gsub("= ,", ",", arguments_collapse)
  arguments_collapse <- gsub("= $", "", arguments_collapse, perl = TRUE)

  generic_text <- substitute(S3_generic)

  function_text <- paste0(
    "\n#' @exportS3Method\n",
    generic_text,".",S3_name, " <- function(\n",
    arguments_collapse,"\n",
    ") {\n",
    "}\n"
  )

  arguments_text <- generate_arguments_header(arguments_name)
  header_text <- paste0("#' ", generic_text, "\n",
                        "#'\n",
                        "#' Describe your generic here\n",
                        "#'\n",
                        arguments_text, "\n#'\n",
                        "#' @export\n"
  )

  control_output(paste0(generic_text,".",S3_name),
                 S3_output,
                 S3_overwrite = TRUE,
                 S3_add_method = TRUE,
                 function_text,
                 header_text = header_text)

}
