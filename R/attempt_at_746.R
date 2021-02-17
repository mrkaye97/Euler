# library(numbers)
#
# get_valid_combinations <- function(n) {
#   2 * factorial(2 * n) ^ 2
# }
#
# get_illegal <- function(n) {
#   2 * n * get_valid_combinations(1) ** n
# }
#
# get_illegal(1)
# get_illegal(2)
# get_illegal(3)
#
# n <- 3
# get_valid_combinations(n) - get_valid_combinations(1) * get_valid_combinations(n-1) + get_valid_combinations(2) * get_valid_combinations(n-2)
#
# generate_people <- function(n) {
#   c('m', 'd', 'b', 's') %>%
#     map(rep, n) %>%
#     transpose() %>%
#     imap(function(x,y) paste0(x,y)) %>%
#     unlist()
# }
#
# generate_seating_arrangement <- function(people) {
#   N <- length(people)
#   arrangement <- character(N)
#   for (i in 1:N) {
#     if (i == 1) {
#       tmp <- sample(people, 1)
#     } else {
#       if (grepl('s', arrangement[i-1]) | (grepl('m', arrangement[i-1]))) {
#         tmp <- people %>%
#           discard(function(x) (grepl('s', x) | grepl('m', x))) %>%
#           sample(1)
#       } else {
#         tmp <- people %>%
#           keep(function(x) (grepl('s', x) | grepl('m', x))) %>%
#           sample(1)
#       }
#     }
#     people <- people[people != tmp]
#     arrangement[i] <- tmp
#   }
#
#   arrangement
# }
#
# num_families_together <- function(arrangement) {
#   arr_nums <- arrangement %>%
#     map(str_sub, 2L, 2L) %>%
#     unlist()
#
#   N <- length(arr_nums)
#
#   result <- 0
#
#   for (i in 1:(N - 3)) {
#
#     tmp <- (arr_nums[i] == arr_nums[i + 1] & arr_nums[i + 2] == arr_nums[i + 3] & arr_nums[i] == arr_nums[i + 3])
#
#     if (tmp) {
#       i <- i + 4
#       result <- result + 1
#     }
#   }
#
#   return(result)
# }
#
# N <- 10000
# result <- numeric(N)
# n <- 2
# for (i in 1:N) {
#   print(i)
#   people <- generate_people(n)
#   arrangement <- generate_seating_arrangement(people)
#
#   result[i] <- num_families_together(arrangement)
# }
#
# result %>%
#   table() %>%
#   prop.table()
#
# get_valid_combinations(n) - get_valid_combinations(n) * num_fam_together / N

