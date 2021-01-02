#' Problem 1
#' If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
#' Find the sum of all the multiples of 3 or 5 below 1000.
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
    mutate(rs = sum(c_across(everything()))) %>%
    ungroup() %>%
    transmute(x = ifelse(.data$rs == .data$number, 0, .data$number)) %>%
    summarize(s = sum(.data$x))  %>%
    pull(.data$s)
}
