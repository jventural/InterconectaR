#' @title Structure Groups
#' @description Creates a named list of sequential integer groups.
#' @param names Character vector of group names.
#' @param values Integer vector of group sizes.
#' @return A named list of integer sequences.
#' @export
structure_groups <- function(names, values) {
  if(length(names) != length(values)) {
    stop("The length of names and values must be the same.")
  }

  groups_list <- list()

  start <- 1
  for(i in seq_along(names)) {
    end <- start + values[i] - 1
    groups_list[[names[i]]] <- seq(start, end)
    start <- end + 1
  }

  groups <- structure(groups_list, Names = names)

  return(groups)
}
