
<!-- README.md is generated from README.Rmd. Please edit that file -->

# S3easy

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/S3easy)](https://CRAN.R-project.org/package=S3easy)
<!-- badges: end -->

S3 easy is a development tool that helps you adding new S3 classes,
generics and methods easily and consistently in the style of
[usethis](https://usethis.r-lib.org/index.html). It consists of three
functions: - **new_S3class()** helps creating constructor, helper and
validator functions in one go. - **new_S3generic()** helps creating new
S3 generic functions. - **new_S3method()** helps creating a new S3
method for an *existing generic*.

The package tries to enforce the suggestions for good S3 practice from
the respective chapter in Hadley Wickham’s [*Advanced
R*](https://adv-r.hadley.nz/index.html). However, the intention is not
to provide *working code*, but the necessary skeleton. Output will still
have to be validated and edited by the user.

All functions support both clipboard and file output. File output
ensures that no code is overwritten (without the consent of the user)
and that no inconsistencies arise (multiple function definitions in one
package).´

## Installation

You can install the development version of S3easy like so:

``` r
devtools::install_github("valentingar/S3easy")
```

## Usage

### Create a new class

Create a new class with `new_S3class()`. You must provide a name
(`test_class`) and at least a base object (`my_base_object`). Both the
attributes and the base object of your new class are entered in the
`...` argument of `new_S3class()`. The first entry is interpreted as the
base object. You can add default values with
`attribute = default_value`. Even dots (`...`) are supported for the
helper function. You can add inherited classes as a character, however
S3easy will not check that this is actually correct! Finally, control
the output with `S3_output`

``` r
library(S3easy)

new_S3class("test_class",
            my_base_object = list(),
            attribute_1 = "default_value",
            attribute_2 = numeric(),
            attrubute_3,
            '...',
            S3_inherited_class = "parent_class, grandparent_class",
            S3_output = "file"
            )
```

The result will be a new file at (“R/test_class.R”) with the following
content:

``` r
#' test_class
#'
#' The class test_class is a subclass of parent_class, grandparent_class and represents...
#'
#' @param my_base_object
#' @param attribute_1
#' @param attribute_2
#' @param attrubute_3
#' @param ...
#'
#' @export
#### HELPER ####
test_class <- function(my_base_object = list(),
                       attribute_1 = default_value,
                       attribute_2 = numeric(),
                       attrubute_3,
                       ...) {
  # Add code to coerce your object and validate the user input.
  x <- new_test_class(
    my_base_object = my_base_object,
    attribute_1 = attribute_1,
    attribute_2 = attribute_2,
    attrubute_3 = attrubute_3
  )
  x <- validate_test_class(x)
  x
}

#### CONSTRUCOR ####
new_test_class <- function(my_base_object = list(),
                           attribute_1 = default_value,
                           attribute_2 = numeric(),
                           attrubute_3) {
  # This is the low-lever constructor
  # It should be fast and efficient and is normally not user-facing:
  # -Outsource any complex coercion to the helper
  # -Remove function arguments that are not obligatory to create the object
  x <- structure(
    my_base_object, # this is the base object
    attribute_1 = attribute_1,
    attribute_2 = attribute_2,
    attrubute_3 = attrubute_3,
    class = c("test_class", "parent_class", "grandparent_class")
  )
  x
}

#### VALIDATOR ####
validate_test_class <- function(x) {
  # add tests to check your object is created correctly here
  x
}
```

You can also get the same output (without the Roxygen Header) to the
clipboard:

``` r
library(S3easy)

new_S3class("test_class",
            my_base_object = list(),
            attribute_1 = "default_value",
            attribute_2 = numeric(),
            attrubute_3,
            '...',
            S3_inherited_class = "parent_class, grandparent_class",
            S3_output = "clipboard"
            )
```

### Add a new generic

To add a new generic, simply run `new_S3generic()`. Again, the arguments
of the function can be controlled by adding them to the `...`, including
optional default values.

``` r
new_S3generic("test_fun",
              base_argument,
              argument_1 = 42, 
              argument_2 = list(),
              '...',
              S3_output = "file")
```

Here, a new file is created at “R/test_fun.R” that looks like this:

``` r
#' test_fun
#'
#' Describe your generic here
#'
#' @param base_argument
#' @param argument_1
#' @param argument_2
#' @param ...
#'
#' @export
test_fun <- function(base_argument,
                     argument_1 = 42,
                     argument_2 = list(),
                     ...) {
  UseMethod("test_fun")
}
```

### Add methods to generics

Adding methods is a bit different, as it requires input of an actual,
working S3 generic. This ensures, that all methods have at least the
same arguments as the generic. You can add methods by running
`new_S3method()`. If we want to use the previously generated generic, we
will have to load it first:

``` r
devtools::load_all()

new_S3method(test_fun, # the generic function
             "test_class", # the class for which to add the method
             S3_output = "file")
```

In this case, the following code will be added to end of “R/test_fun.R”

``` r

#' @exportS3Method
test_fun.test_class <- function(base_argument,
                                argument_1 = 42,
                                argument_2 = list(),
                                ...) {
}
```

If we add methods to generics that are not defined in the package, a new
file will be generated.

``` r

new_S3method(print, # the generic function
             "test_class", # the class for which to add the method
             S3_output = "file")
```

will create a new file at “R/print.R” with the following content:

``` r
#' print
#'
#' Describe your generic here
#'
#' @param x
#' @param ...
#'
#' @export

#' @exportS3Method
print.test_class <- function(x,
                             ...) {
}
```
