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

#' @importFrom numbers isPrime
#' @importFrom memoise memoize
isPrimeMemo <- memoize(isPrime)

#' get_digits
#'
#' get the digits of a number
#' @param number the number to get permutations of
#' @return the digits
get_digits <- function(number) {
  number %>%
    as.character() %>%
    str_split('') %>%
    unlist() %>%
    as.numeric()
}

#' get_number_digit_permutation
#'
#' get all circular variants of a number
#' @param digits the digits of the number to permute
#' @param n number of digits to move
#' @return the number
get_number_digit_permutation <- function(n, digits) {
  N <- length(digits)
  digit_indices <- (n + (1:N)) %% N + 1

  digits[digit_indices] %>%
    paste0(collapse = '') %>%
    as.numeric()
}

#' get_all_circular_permutations
#'
#' get all of the circular permutations of a number
#' @param number the number
#' @importFrom purrr map_dbl
get_all_circular_permutations <- function(number) {
  digits <- get_digits(number)

  1:length(digits) %>%
    map_dbl(get_number_digit_permutation, digits)
}

#' get_nth_triangle
#'
#' get the nth triangle number
#' @param n the nth number
#' @return the nth triangle number
get_nth_triangle <- function(n) {
  .5 * n * (n + 1)
}

#' get_triangle_numbers
#'
#' get all of the triangle numbers up to t(n)
#' @param n the max n
#' @importFrom purrr map_dbl
get_triangle_numbers <- function(n) {
  1:n %>%
    map_dbl(get_nth_triangle)
}

#' triangle_greater_than_max
#'
#' test if a triangle number is greater than some specified maximum value
#' @param n the number to test (i.e. the 10th triangle number)
#' @param max_num the max to test against
#' @return a boolean (true if the nth triangle number is bigger than the max to test)
triangle_greater_than_max <- function(n, max_num) {
  triangle <- get_nth_triangle(n)
  if (triangle > max_num) {
    TRUE
  } else {
    FALSE
  }
}

#' Roll a die
#'
#' @param sides the number of sides of the die
#' @return the value of the die roll
roll_die <- function(sides = 4) {
  sample(1:sides, 1)
}

#' Monopoply Roll
#'
#' Rolling two dice
#'
#' @param sides the number of sides on the dice
#' @return a numeric vector of the die rolls
monopoly_roll <- function(sides = 4) {
  roll_1 <- roll_die(sides)
  roll_2 <- roll_die(sides)

  c(
    roll_1,
    roll_2
  )
}

#' Initialize community chest deck
#' @return a character vector of randomly shuffled community chest cards
init_community_chest <- function() {
  deck <- c(
    "Go to jail",
    "Advance to go"
  )


  deck <- c(
    deck,
    rep("Nothing", 14)
  )

  sample(deck, size = length(deck))
}

#' Initialize chance deck
#' @return a character vector of randomly shuffled chance cards
init_chance <- function() {
  deck <- c(
    "Advance to go",
    "Go to jail",
    "Go to C1",
    "Go to E3",
    "Go to H2",
    "Go to R1",
    "Go to next R",
    "Go to next R",
    "Go to next U",
    "Go back 3 squares"
  )

  deck <- c(
    deck,
    rep("Nothing", 6)
  )

  sample(deck, size = length(deck))
}

#' Draw a card from chance or community chest
#' @param deck the deck to draw from
#' @return A list with the drawn card and the new deck
draw_card <- function(deck) {
  list(
    draw = deck[1],
    new_deck = c(deck[2:length(deck)], deck[1])
  )
}



#' Go to the next railroad
#'
#' @importFrom purrr when
#' @param current_square the index of the current square
#' @return The square of the next railroad
next_railroad <- function(current_square) {
  current_square %>%
    when(
      . < 5 ~ 5,
      . < 15 ~ 15,
      . < 25 ~ 25,
      . < 35 ~ 35,
      TRUE ~ 5
    )
}

#' Go to the next utility
#'
#' @param current_square the index of the current square
#' @param return The square of the next utility
next_utility <- function(current_square) {
  current_square %>%
    when(
      . < 12 ~ 12,
      . < 28 ~ 28,
      TRUE ~ 12
    )
}

play_turn <- function(current_square, cc_deck, chance_deck, num_rolls = 1, num_die_sides = 4) {

  roll <- monopoly_roll(sides = num_die_sides)
  doubles <- length(unique(roll)) == 1

  if (doubles & num_rolls == 3) {
    new_square <- 10
  } else {
    new_square <- (current_square + sum(roll)) %% 40

    if (new_square == 30) {
      new_square <- 10
    } else {

      if (new_square %in% chance_squares) {
        draw <- draw_card(chance_deck)
        chance_deck <- draw$new_deck

        new_square <- draw %>%
          pluck("draw") %>%
          switch(
            "Go to H2" = 39,
            "Go to R1" = 5,
            "Go to next R" = next_railroad(new_square),
            "Go to E3" = 24,
            "Go to jail" = 10,
            "Go to C1" = 11,
            "Go to next U" = next_utility(new_square),
            "Go back 3 squares" = new_square - 3,
            "Advance to go" = 0,
            new_square
          )
      }

      if (new_square %in% cc_squares) {
        draw <- draw_card(cc_deck)
        cc_deck <- draw$new_deck

        new_square <- draw %>%
          pluck("draw") %>%
          switch(
            "Go to jail" = 10,
            "Advance to go" = 0,
            new_square
          )
      }

      if (doubles) {
        tmp <- play_turn(
          current_square = new_square,
          cc_deck = cc_deck,
          chance_deck = chance_deck,
          num_rolls = num_rolls + 1
        )

        new_square <- tmp$current_square
        chance_deck <- tmp$chance_deck
        cc_deck <- tmp$cc_deck
      }
    }
  }

  list(
    current_square = new_square,
    chance_deck = chance_deck,
    cc_deck = cc_deck
  )
}
