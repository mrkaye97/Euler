#' ut
#' A function to simplify adding dependencies
#' @param pack the package to add to the description
#' @return NULL (invisible)
#' @importFrom usethis use_package
#' @importFrom utils packageVersion
ut <- function(pack) {
  use_package(pack, min_version = packageVersion(pack))
  return(invisible(NULL))
}

#' multiples_sum
#' get the sum of the multiples of the numbers specified less than the max value specified
#' @param numbers the numbers whose multiples to include in the sum
#' @param max_value the largest number to consider
#' @return the sum
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom purrr reduce map
#' @import dplyr
multiples_sum <- function(numbers = c(3,5), max_value = 1000) {
  numbers %>%
    map(~ tibble(number = 1:(max_value-1)) %>%
          mutate(s = ifelse(.data$number %% .x == 0, .data$number, 0))) %>%
    reduce(left_join, by = 'number') %>%
    rowwise() %>%
    filter(any(c_across(-.data$number) == .data$number)) %>%
    ungroup() %>%
    summarize(x = sum(.data$number)) %>%
    pull(.data$x)
}
