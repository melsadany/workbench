create_importance_network <- function(domain_feature_analysis, top_n = 5) {
  
  # Extract top features for each domain
  network_data <- map_dfr(names(domain_feature_analysis), function(domain) {
    top_features <- domain_feature_analysis[[domain]]$top_features$vip_top[1:top_n]
    data.frame(from = top_features,
               to = domain,
               importance = 1:length(top_features),  # Rank-based importance
               stringsAsFactors = FALSE)
  })
  
  # Create network plot
  require(igraph)
  require(ggraph)
  
  graph <- graph_from_data_frame(network_data, directed = TRUE)
  
  # network_plot <- ggraph(graph, layout = "fr") +
  #   geom_edge_link(aes(linewidth = importance), arrow = arrow(length = unit(1.5, 'mm')), linewidth=0.3,end_cap = circle(3, 'mm')) +
  #   geom_node_point(aes(color = ifelse(name %in% names(domain_feature_analysis), "Domain", "Feature"),
  #                       # shape = ifelse(name %in% names(domain_feature_analysis), "Domain", "Feature"),
  #                       size = ifelse(name %in% names(domain_feature_analysis), "Domain", "Feature"))) +
  #   geom_node_text(aes(label = str_replace_all(name,"_|-"," ")), check_overlap = T,repel = TRUE, size = 3) +
  #   scale_color_manual(values = c("Domain" = redblu.col.2[1], "Feature" = redblu.col.2[2]),name="") +
  #   # scale_shape_manual(values = c("Domain" = 19, "Feature" = 1),name="") +
  #   scale_size_manual(values = c("Domain" = 5, "Feature" = 2),name="") +
  #   scale_edge_alpha(range = c(0.3,0.1))+ guides(color="none",shape="none",size="none")+
  #   scale_edge_size(range = c(7:1))+
  #   bw.theme+theme(panel.border = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
  #   labs(x="",y="")
  library(tidygraph)
  prior=create_layout(graph,"lgl")
  network_plot <- ggraph(as_tbl_graph(graph), 'metro',x = prior$x, y = prior$y, grid_space = 1, max_movement = 80) + 
    geom_edge_link(width = 2,alpha=0.5) + 
    geom_node_point(aes(color = ifelse(name %in% names(domain_feature_analysis), "Domain", "Feature")),size = 6) + 
    geom_edge_link(color = 'white', width = 1) + 
    geom_node_point(color = 'white', size = 3) +
    scale_color_manual(values = c("Domain" = redblu.col.2[1], "Feature" = redblu.col.2[2]),name="") +
    geom_node_text(aes(label = str_replace_all(name,"_|-"," ")), check_overlap = T,repel = T, size = 3.5) +
    guides(color="none")+
    bw.theme+theme(panel.border = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
    labs(x="",y="")
  return(network_plot)
}
