#' @title Collumn map profile
#' @description Function to create a profile for a process map based on a collumn.
#' @param value The type of representation to be used: absolute or relative.
#' @export frequency


frequency <- function(value = c("absolute", "relative")) {
	value <- match.arg(value)
	attr(value, "perspective") <- "frequency"
	return(value)
}

