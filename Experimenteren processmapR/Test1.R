library(bupaR)
library(edeaR)
library(processmonitR)
library(plotluck)
library(DiagrammeR)
#library(DiagrammeRsvg)

csvLog<-read.table("../3.0 RefundProcess.csv",header = TRUE, sep = ";",na = c("n.v.t.","onbekend"))
csvLog$status = "complete"
csvLog$activity_instance = 1:nrow(csvLog)
csvLog$timestamp<-as.POSIXct(strptime(csvLog$Complete.timestamp, "%d-%m-%Y  %H:%M"))
eventLog1<-eventlog(csvLog,
					case_id = "Order.Nr.",
					activity_id = "Status",
					activity_instance_id = "activity_instance",
					lifecycle_id = "status",
					timestamp = "timestamp",
					resource_id = "Employee"
)

refundIssued<-n_cases((gefilteredeEventLog<-filter_activity_presence(eventLog1,activities = c("Refund issued","Special refund issued"),method ="one_of")))
aantalzonderProductReceived<-n_cases(gefilteredeEventLog<-filter_precedence(gefilteredeEventLog,antecedents=c("Product received"), consequents=c("Refund issued","Special refund issued"),precedence_type = "eventually_follows",filter_method ="one_of",reverse=TRUE))
paste0("Er zijn ",refundIssued, " gevallen met refunds. Daarvan is van ",aantalzonderProductReceived," geen product ontvangen voor de refund")

#debugonce(process_map_styled)
process_map_styled(gefilteredeEventLog, metric_type = frequency("relative") )



map<-process_map_styled(gefilteredeEventLog, metric_type = frequency("relative"),render = F )
map$nodes_df$nieuw<-sample(1:10,nrow(map$nodes_df), replace = T)+sample(1:10,nrow(map$nodes_df), replace = T)
map$edges_df$nieuw<-sample(1:10,nrow(map$edges_df), replace = T)+sample(1:10,nrow(map$edges_df), replace = T)
map$edges_df$label<-map$edges_df$nieuw
map$edges_df$color[1]=rgb(0, 1, 0, 0)
map.nodes<-map$nodes_df
map.edges<-map$edges_df
map%>%export_graph(file_name = "../globalPerformanceGraph.png",  file_type = "PNG")
map$edges_df<-map$edges_df[map$edges_df$nieuw>10.0,]
map%>%export_graph(file_name = "../globalPerformanceGraph_naFilteren.png",  file_type = "PNG")
map%>%render_graph(layout = "visNetwork")
map%>%render_graph(layout = "circle")
#map$nodes_df$size=0.001
map <-
	map %>%
	add_global_graph_attrs(
		attr = "fontsize",
		value = 1,
		attr_type = "edge")%>%
	add_global_graph_attrs(
		attr = "fontsize",
		value = 0.1,
		attr_type = "node")
get_node_df(map)
get_edge_df(map)
map%>%render_graph(layout = "circle")
map%>%render_graph()


