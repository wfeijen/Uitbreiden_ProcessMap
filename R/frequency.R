#' @title Frequency map profile
#' @description Function to create a frequency profile for a process map.
#' @param value The type of frequency value to be used: absolute or relative.
#' @export frequency


frequency <- function(value = c("absolute", "relative"),colomnName = NULL) {
	value <- match.arg(value)
	attr(value, "perspective") <- "frequency"
	if (is.null(colomnName)) { 
	    attr(value, "colomnName") <- paste0("frequency_",value)
	} else {
	    attr(value, "colomnName") <- colomnName
	}
	return(value)
}

