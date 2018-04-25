#' @title color_activities
#'
#'
#' @description A function for filling the activities in a process map DiagrammeR object
#' @param diagrammerProcessMap The diagrammer object for which we will add values to the attributes
#' @param column Defines the column containing the metric to be used
#' @param colorCaption Defines the color for to be used for the caption
#' @param colorLowerbound Defines the color lower bound to be used for the coloring scale of the activities
#' @param colorUpperbound Defines the color upper bound to be used for the coloring scale of the activities
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' map <- process_map(patients, render = F)
#' 
#' }
#' @export color_activities

color_activities <- function( diagrammerProcessMap,
                              column,
                              colorCaption  = "black",
                              colorLowerbound= "gray50",
                              colorUpperbound = "gray100" ){
    diagrammerProcessMap %>%
        rescale_node_attrs(
            node_attr_from = !!column,
            to_lower_bound = colorLowerbound,
            to_upper_bound = colorUpperbound,
            node_attr_to = fillcolor ) %>%
        rescale_node_attrs(
            node_attr_from = !!column,
            to_lower_bound = colorCaption,
            to_upper_bound = colorCaption,
            node_attr_to = fontcolor) %>%
        rescale_node_attrs(
            node_attr_from = !!column,
            to_lower_bound = colorCaption,
            to_upper_bound = colorCaption,
            node_attr_to = color) 
}