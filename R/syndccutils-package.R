#' @rawNamespace import(ggplot2, except = vars)
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom purrr keep has_element map map2 map_df map_chr map_int map_lgl
#'   flatten flatten_chr set_names walk2
#' @importFrom rlang UQ UQS :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines, thanks
## Jenny Bryan
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
