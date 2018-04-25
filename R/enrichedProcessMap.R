#' @title enrichedProcessMap
#'
#'
#' @description A function for creating a process map of an event log.
#' @param eventlog The event log object for which to create a process map
#' @param type Defines the type of metric shown by the with of the edges, which can be created with the functions frequency and performance. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time.
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' process_map(patients)
#' }
#' @export enrichedProcessMap


# We aim at maximum flexibility by:
# 1 visualizing multiple metrics by:
#	a- edge with
#	b- edge color black to red
#   c- edge color black to blue
# 2 simplifieng the graph by leaving edges out based on metric. This can be done in two ways:
#	1- as a percentage of all edges (I.E. 0.5 shows half of the edges)
#	2- as an absolute number > 1 (I.E. 10 shows at most 10 edges)

CreateBaseLog<-function(eventlog){
    tempEventLog <- eventlog %>%
                    as.data.table() %>%
                    droplevels %>%
                    mutate(act = !!activity_id_(eventlog),
                           aid = !!activity_instance_id_(eventlog),
                           case = !!case_id_(eventlog),
                           time = !!timestamp_(eventlog))
        
    base_log <- tempEventLog %>%
                as.data.table() %>%
                group_by(act, aid, case) %>%
                summarize(start_time = min(time),
                          end_time = max(time))
    
    tempEventLogAdditionalAttributes <- tempEventLog[!duplicated(
                                        select(tempEventLog,act,aid,case))
                                        ,]
    
    base_log <-  inner_join(base_log,tempEventLogAdditionalAttributes)
    

    base_log  %>%
        bind_rows(GetEndPoints(baseLog = base_log)) -> base_log
}

GetEndPoints<-function(baseLog){
    baseLog %>%
        group_by(case) %>%
        arrange(start_time) %>%
        slice(c(1,n())) %>%
        mutate(act = c("Start","End")) %>%
        mutate(start_time = recode(act, "End" = end_time, .default = start_time)) %>%
        mutate(end_time = recode(act, "Start" = start_time, .default = end_time))
}

GetBasicNodes <- function(precedence) {
    precedence %>%
        group_by(act, from_id) %>%
        summarize(n = as.double(n())) %>%
        ungroup() %>%
        mutate(activity_name = 0) %>%
        mutate(shape = if_start_or_end(act,"circle","rectangle"),
               tooltip = act,
               activity_name = if_start_or_end(act, act, tooltip)) %>%
        na.omit()
}

GetBasicEdges <- function(precedence) {
    precedence %>%
        ungroup() %>%
        group_by(act, from_id, next_act, to_id) %>%
        summarize(n = as.double(n())) %>%
        na.omit() %>%
        group_by(act, from_id) %>%
        mutate(activity_name = 0) %>%
        ungroup() %>%
        mutate(penwidth = rescale(activity_name, to = c(1,5)))
}

edges_performance <- function(precedence, aggregationInstructions) {
    flow_time <- attr(aggregationInstructions, "flow_time")
    temp <- precedence %>%
        ungroup() %>%
        mutate(time = case_when(flow_time == "inter_start_time" ~ as.double(next_start_time - start_time, units = attr(aggregationInstructions, "units")),
                                flow_time == "idle_time" ~ as.double(next_start_time - end_time, units = attr(aggregationInstructions, "units")))) %>%
        group_by(act, next_act, from_id, to_id) %>%
        summarize(aggr = aggregationInstructions(time)) %>%
        na.omit() %>%
        ungroup() %>%
        mutate(temp = aggr) %>%
        na.omit() %>%
        select(temp)
    colnames(temp)<-c(attr(aggregationInstructions, "columnName"))
    return(temp)
}

nodes_performance <- function(precedence, aggregationInstructions) {
    columnName <-  attr(aggregationInstructions, "columnName")
    temp <- precedence %>%
        mutate(duration = as.double(end_time-start_time
                                    , units = attr(aggregationInstructions, "units"))) %>%
        group_by(act, from_id) %>%
        summarize(aggr = aggregationInstructions(duration)) %>%
        ungroup () %>%
        mutate(temp = aggr) %>%
        na.omit() %>%
        select(temp)
    colnames(temp)<-c(columnName)
    return(temp)
}

edges_columnAgregate <- function(precedence, aggregationInstructions) {
    columnName <-  attr(aggregationInstructions, "columnNameIn")
    columnNamex <- paste0("^",columnName,".x$")
    columnNamey <- paste0("^",columnName,".y$")
    edgeOperation <- attr(aggregationInstructions, "edgeOperation")
    
    columnValues <- precedence %>%
        select(act, aid, columnName) 
    
    p <- precedence %>%
        inner_join(columnValues, by = c( "case" = "case","next_act" = "act", "next_aid" = "aid")) #
    names(p) <- sub(columnNamex, "aggrFirst", names(p))
    names(p) <- sub(columnNamey, "aggrSecond", names(p))
    p$calcColumn <- case_when(
        edgeOperation == "mean" ~  as.double(rowMeans(data.frame(p$aggrFirst,p$aggrSecond))),
        edgeOperation == "min" ~  as.double(do.call(pmin, data.frame(p$aggrFirst,p$aggrSecond))),
        edgeOperation == "max" ~  as.double(do.call(pmax, data.frame(p$aggrFirst,p$aggrSecond))),
        edgeOperation == "minus" ~  as.double(p$aggrFirst - p$aggrSecond),
        edgeOperation == "plus" ~ as.double(p$aggrFirst + p$aggrSecond),
        edgeOperation == "from" ~  as.double(p$aggrFirst),
        edgeOperation == "to" ~  as.double(p$aggrSecond),
        TRUE ~  as.double(rowMeans(data.frame(p$aggrFirst,p$aggrSecond)))
    )
    
    temp <- p %>%
        ungroup() %>%
        group_by(act, from_id, next_act, to_id) %>%
        summarize(aggr = aggregationInstructions(calcColumn)) %>%
        ungroup() %>%
        mutate(temp = aggr) %>%
        na.omit() %>%
        select(temp)
    colnames(temp)<-c(attr(aggregationInstructions, "columnNameOut"))
    return(temp)
}

nodes_columnAgregate <- function(precedence, aggregationInstructions) {
    names(precedence) <- sub(attr(aggregationInstructions, "columnNameIn"), "aggrCol", names(precedence))
    temp <- precedence %>%
        group_by(act, from_id) %>%
        summarize(aggr = aggregationInstructions(aggrCol)) %>%
        ungroup () %>%
        mutate(temp = aggr) %>%
        na.omit() %>%
        select(temp)
    colnames(temp)<-c(attr(aggregationInstructions, "columnNameOut"))
    return(temp)
}

edges_frequency <- function(precedence, aggregationInstructions) {
    temp <- precedence %>%
        ungroup() %>%
        group_by(act, from_id, next_act, to_id) %>%
        summarize(n = as.double(n())) %>%
        ungroup() %>%
        mutate(temp = case_when(aggregationInstructions == "relative" ~ round(100*n/sum(n),2),
                                 aggregationInstructions == "absolute" ~ n)) %>%
        na.omit() %>%
        select(temp)
    colnames(temp)<-c(attr(aggregationInstructions, "columnName"))
    return(temp)
}

nodes_frequency <- function(nodes, precedence, aggregationInstructions) {
    temp <- precedence %>%
        group_by(act, from_id) %>%
        inner_join(nodes) %>%
        summarize(n = as.double(n())) %>%
        ungroup() %>%
        mutate(temp = case_when(aggregationInstructions == "relative" ~ round(100*n/sum(n),2),
                                aggregationInstructions == "absolute" ~ n)) %>%
        na.omit() %>%
        select(temp)
    colnames(temp)<-c(attr(aggregationInstructions, "columnName"))
    return(temp)
}

if_start_or_end <- function(node, true, false) {
    ifelse(node %in% c("Start","End"), true, false)
}
if_start <- function(node, true, false) {
    ifelse(node %in% c("Start"), true, false)
}

GetBasePrecedence<-function(base_log,eventlog){
    base_log %>%
        ungroup() %>%
        count(act) %>%
        mutate(node_id = 1:n()) -> base_nodes
    
suppressWarnings(base_log %>%
                     ungroup() %>%
                     mutate(act = ordered(act
                                            , levels = c("Start", as.character(activity_labels(eventlog))
                                            , "End"))) %>%
                     group_by(case) %>%
                     arrange(start_time, act) %>%
                     mutate(next_act = lead(act),
                            next_start_time = lead(start_time),
                            next_end_time = lead(end_time),
                            next_aid = lead(aid)) %>%
                     full_join(base_nodes, by = c("act" = "act")) %>%
                     rename(from_id = node_id) %>%
                     full_join(base_nodes, by = c("next_act" = "act")) %>%
                     rename(to_id = node_id) %>%
                     select(-n.x, -n.y))
}

getNodesAggregation <- function(aggregationInstruction,nodes,base_precedence)
{
    perspective <- attr(aggregationInstruction, "perspective")
    if(perspective == "frequency") 
        nodes_frequency(nodes,base_precedence, aggregationInstruction)
    else if(perspective == "performance") 
        nodes_performance(base_precedence, aggregationInstruction)
    else if(perspective == "columnAgregate") 
        nodes_columnAgregate(base_precedence, aggregationInstruction)
}

getEdgesAggregation <- function(aggregationInstruction,base_precedence)
{
    perspective <- attr(aggregationInstruction, "perspective")
    if(perspective == "frequency") 
        edges_frequency(base_precedence, aggregationInstruction)
    else if(perspective == "performance")
        edges_performance(base_precedence, aggregationInstruction)
    else if(perspective == "columnAgregate")
        edges_columnAgregate(base_precedence, aggregationInstruction)
}

enrichedProcessMap <- function(eventlog , aggregationInstructions =  list(frequency("absolute"))) {
        base_log<-CreateBaseLog(eventlog = eventlog)
        base_precedence<- GetBasePrecedence(base_log,eventlog = eventlog)
        nodes<-GetBasicNodes(base_precedence)
        edges<-GetBasicEdges(base_precedence)
    

        create_node_df(n = nrow(nodes),
                       label = nodes$activity_name,
                       shape = nodes$shape,
                       style = "rounded,filled",
                       tooltip = nodes$tooltip,
                       penwidth = 1.5,
                       fontname = "Arial") -> nodes_df
        aggregatedColumns<-as.data.table(lapply(aggregationInstructions,getNodesAggregation,nodes,base_precedence))
        aggregatedColumns$activity_name <- nodes$activity_name
        
        nodes_df<-cbind(nodes_df,aggregatedColumns)
        
        min_level <- min(nodes_df$color_level)
        max_level <- max(nodes_df$color_level[nodes_df$color_level < Inf])
        
        create_edge_df(from = edges$from_id,
                       to = edges$to_id,
                       activity_name = edges$activity_name,
                       penwidth = edges$penwidth,
                       fontname = "Arial") -> edges_df
        
        aggregatedEdges<-as.data.table(lapply(aggregationInstructions,getEdgesAggregation,base_precedence))
        edges_df <- cbind(edges_df, aggregatedEdges)
        
        create_graph(nodes_df, edges_df)  %>%
            add_global_graph_attrs(attr = "rankdir", value = "TB",attr_type = "graph") %>%
            add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") 
}

    