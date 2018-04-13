#' @title fill_activities
#'
#'
#' @description A function for filling the activities in a process map DiagrammeR object.
#' @param processMap The process map object for which we will add values to the attributes
#' @param column Defines the column containing the metric to be used
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' map <- process_map(patients, render = F)
#' 
#' }
#' @export enrichedProcessMap


# We aim at maximum flexibility by:
# 1 visualizing multiple metrics by:
#	a- edge with
#	b- edge color black to red
#   c- edge color black to blue
# 2 simplifieng the graph by leaving edges out based on metric. This can be done in two ways:
#	1- as a percentage of all edges (I.E. 0.5 shows half of the edges)
#	2- as an absolute number>1 (I.E. 10 shows at most 10 edges)