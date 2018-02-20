#' @title Agregate map profile
#' @description Function to create a aggregate profile for a process map using the values in one of the collumns of the eventlog.
#' @param FUN A summary function to be called on the process time of a specific activity, e.g. mean, median, min, max
#' @param units The time unit in which processing time should be presented (mins, hours, days, weeks)
#' @param flow_time The time to depict on the flows: the inter start time is the time between the start timestamp of consecutive activity instances,
#' the idle time is the time between the end and start time of consecutive activity instances.
#' @export agregate




agregate <- function(FUN = mean, units = "days", flow_time = c("inter_start_time","idle_time")) {
	flow_time <- match.arg(flow_time)
	attr(FUN, "flow_time") <- flow_time
	attr(FUN, "perspective") <- "agregate"
	attr(FUN, "units") <- units
	return(FUN)
}
