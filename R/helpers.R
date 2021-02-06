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

#' fibs
#' get fibonacci numbers
#' @param n the index of the fibonacci number ot return (default: 10)
#' @param x1 the first number in the sequence (default: 0)
#' @param x2 the second number in the sequence (default: 1)
#' @return numeric: the nth fibonacci number
fibs <- function(n = 10, x1 = 0, x2 = 1) {
  fib_tbl <- c(x1, x2, rep(NA, n))

  fib_mem <- function(n){
    stopifnot(n > 0)

    if(!is.na(fib_tbl[n])){
      fib_tbl[n]
    } else {
      fib_tbl[n - 1] <<- fib_mem(n - 1)
      fib_tbl[n - 2] <<- fib_mem(n - 2)
      fib_tbl[n - 1] + fib_tbl[n - 2]
    }
  }

  return(fib_mem(n))
}

#' get_factors
#' get the factors of a number
#' @param n the number
#' @return numeric: the factors
#' @importFrom purrr keep
get_factors <- function(n) {
  1:n %>%
    keep(function(x) n %% x == 0)
}

#' is_prime
#' check if a number is prime
#' @param n the number
#' @return boolean: TRUE if prime, FALSE otherwise
is_prime <- function(n) {
  if (length(get_factors(n)) == 2) TRUE else FALSE
}

#' prime_factors
#' get the prime factors of a number
#' @param n the number
#' @return numeric: the prime factors
#' @importFrom purrr keep
prime_factors <- function(n) {
  get_factors(n) %>%
    keep(function(x) is_prime(x))
}

#' get_squares
#'
#' get squares (either even, odd, or all)
#' @importFrom magrittr %>%
#' @importFrom purrr map_dbl keep
#' @importFrom rlang eval_tidy expr
#' @param n_max the number of odd squares to get
#' @param type 'odd' for the odd squares, 'even' for the even squares, 'all' for all squares
#' @return the numbers
get_squares <- function(n_max = 10, type = 'all') {
  .x <- NULL
  predicate <- switch(
    type,
    'all' = expr(.x %% 2 >= 0),
    'odd' = expr(.x %% 2 != 0),
    'even' = expr(.x %% 2 == 0),
    stop("Type needs to be one of 'even', 'odd', or 'all'")
  )

  1:n_max %>%
    keep(~ eval_tidy(predicate)) %>%
    map_dbl(~ .x ** 2)
}

#' letter_to_number
#'
#' convert a letter to a number
#' @param letter the letter to convert
#' @return the number
letter_to_number <- function(letter) {
  switch(
    letter,
    "A" = 1,
    "B" = 2,
    "C" = 3,
    "D" = 4,
    "E" = 5,
    "F" = 6,
    "G" = 7,
    "H" = 8,
    "I" = 9,
    "J" = 10,
    "K" = 11,
    "L" = 12,
    "M" = 13,
    "N" = 14,
    "O" = 15,
    "P" = 16,
    "Q" = 17,
    "R" = 18,
    "S" = 19,
    "T" = 20,
    "U" = 21,
    "V" = 22,
    "W" = 23,
    "X" = 24,
    "Y" = 25,
    "Z" = 26,
    "I didn't recognize that letter!"
  )
}

#' get_word_values
#'
#' convert a word to a number based on the num of its letters
#' @param word the letter to convert
#' @importFrom purrr map_dbl
#' @return the number
get_word_values <- function(word) {
  word %>%
    map_dbl(letter_to_number) %>%
    sum()
}



