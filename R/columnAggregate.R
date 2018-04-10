#' @title Aggregate map profile
#' @description Function to create a aggregate profile for a process map using the values in one of the collumns of the eventlog.
#' @param FUN A summary function to be called on the process time of a specific activity, e.g. mean, median, min, max
#' @param colomnName The name of the column to be aggregated
#' @param edgeOperation The operation to calculate a single value from the from and the to column in an edge
#' @export aggregate




columnAggregate <- function(FUN = mean, colomnName = NULL, edgeOperation = c("mean", "min", "max", "minus", "plus","from","to")) {
    if (is.null(colomnName)) 
        stop("No column name specified")
    else{
        attr(FUN, "colomnName") <- colomnName
        attr(FUN, "perspective") <- "colomAgregate"
        attr(FUN, "edgeOperation") <- edgeOperation
    }
	return(FUN)
}
