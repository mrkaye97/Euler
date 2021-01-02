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
