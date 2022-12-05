#' Define new S3 methods
#'
#' This function helps you to generate new methods for existing generics. It
#' automatically applies a structure that matches the generic.
#'
#' @param S3_name The name of the class for which this method is written
#' @param S3_generic A generic function. Enter as a call to that function,
#'   without parantheses: S3_generic = myfun
#' @inheritParams new_S3
#'
#' @export

new_S3method <- function(S3_name,
                         S3_generic,
                         S3_output = "clipboard",
                         S3_overwrite = FALSE
){
  stopifnot("Please provide an actual function to S3_generic: S3_generic = my_fun"
            = is.function(S3_generic))
  stopifnot("S3_generic is no S3 generic function!" = utils::isS3stdGeneric(S3_generic))
  arguments <- formals(S3_generic)
  arguments_name <- names(arguments)

  arguments_collapse <- paste0(paste0(arguments_name, " = ", arguments),
                               collapse = ",\n")
  arguments_collapse <- gsub("= ,", ",", arguments_collapse)

  generic_text <- substitute(S3_generic)

  function_text <- paste0(
    generic_text,".",S3_name, " <- function(\n",
    arguments_collapse,"\n",
    ") {\n",
    "}\n"
  )

  control_output(generic_text,
                 S3_output,
                 S3_overwrite,
                 function_text,
                 header_text = "#' @exportS3Method\n")

}
