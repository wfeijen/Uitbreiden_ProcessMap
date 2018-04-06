#' @title Aggregate map profile
#' @description Function to create a aggregate profile for a process map using the values in one of the collumns of the eventlog.
#' @param FUN A summary function to be called on the process time of a specific activity, e.g. mean, median, min, max
#' @param colomnName The name of the column to be aggregated
#' @export aggregate




columnAggregate <- function(FUN = mean, colomnName = NULL) {
    if (is.null(colomnName)) 
        stop("No column name specified")
    else{
        attr(FUN, "colomnName") <- colomnName
        attr(FUN, "perspective") <- "colomAgregate"
    }
	return(FUN)
}
