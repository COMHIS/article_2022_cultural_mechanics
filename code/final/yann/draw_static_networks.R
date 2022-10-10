
library(ggraph)
library(igraph)
library(tidygraph)

g = graph_list[[1]] %>% 
  mutate(node_id = 1:nrow1700) %>% 
  mutate(degree = centrality_degree(mode = 'all')) %>% 
  mutate(in_degree = centrality_degree(mode = 'in')) %>% 
  mutate(out_degree = centrality_degree(mode = 'out'))%>%
  left_join(asFrame1700, by = c('node_id' = 'id'))%>% 
  left_join(name_list, by ='name') %>% 
  mutate(label = coalesce(corrected_label, label)) %>%
  mutate(attributes.which_city = ifelse(in_degree>out_degree, 'London', 'Edinburgh'))%>% 
  select(-x, -y) 

reduce = graph_list[[1]] %>% 
  mutate(node_id = 1:nrow1700) %>% 
  mutate(degree = centrality_degree(mode = 'all')) %>% as_tibble() %>% arrange(desc(degree)) %>% head(13) %>% pull(name)

p = g %>%
  ggraph(layout = layout_df1700, circular = FALSE) +
  geom_edge_link(aes(width = weight,end_cap = circle(node2.degree + 15, "pt")),
                 edge_colour = "grey25",
                 arrow = arrow(
                   length = unit(0.05, "inches"),
                   type = "closed"
                 )) + 
  geom_node_point(aes(size = degree, fill = attributes.which_city), pch = 21) +
  geom_node_text(aes(label = label), size = 2.5, repel = F) + 
  theme_void() + 
  theme(legend.position = 'bottom') + 
  scale_size_area(max_size = 25,guide="none")  + 
  scale_edge_width(range = c(.5,3),guide="none") + labs(fill = 'City')

ggsave('output/figures/final/figure_9.pdf', plot = p,width = 7, height = 7)

g = graph_list[[2]] %>% 
  mutate(node_id = 1:nrow1720) %>% 
  mutate(degree = centrality_degree(mode = 'all')) %>% 
  mutate(in_degree = centrality_degree(mode = 'in')) %>% 
  mutate(out_degree = centrality_degree(mode = 'out')) %>% 
  left_join(asFrame1720, by = c('node_id' = 'id')) %>%
  mutate(attributes.which_city = ifelse(in_degree>out_degree, 'London', 'Edinburgh')) %>% 
  select(-x, -y) 

p = g %>%
  ggraph(layout = layout_df1720, circular = FALSE) +
  geom_edge_link(aes(width = weight,end_cap = circle(node2.degree + 10, "pt")),
                 edge_colour = "grey25",
                 arrow = arrow(
                   length = unit(0.1, "inches"),
                   type = "closed"
                 )) + 
  geom_node_point(aes(size = I(degree), fill = attributes.which_city), pch = 21) +
  geom_node_text(aes(label = label), size = 3, repel = F) + 
  theme_void() + 
  theme(legend.position = 'bottom', panel.background = element_blank())  + 
  scale_size_area(max_size = 15,guide="none")  + 
  scale_edge_width(range = c(.5,3),guide="none") + labs(fill = 'City')

ggsave('output/figures/final/figure_10.pdf', plot = p)

g = graph_list[[3]] %>% 
  mutate(node_id = 1:nrow1740) %>% 
  mutate(degree = centrality_degree(mode = 'all'))%>% 
  mutate(in_degree = centrality_degree(mode = 'in')) %>% 
  mutate(out_degree = centrality_degree(mode = 'out')) %>%
  left_join(asFrame1740, by = c('node_id' = 'id')) %>%
  mutate(attributes.which_city = ifelse(in_degree>out_degree, 'London', 'Edinburgh')) %>% 
  select(-x, -y) 

p = g %>%
  ggraph(layout = layout_df1740, circular = FALSE) +
  geom_edge_link(aes(width = weight,end_cap = circle(node2.degree + 10, "pt")),
                 edge_colour = "grey25",
                 arrow = arrow(
                   length = unit(0.1, "inches"),
                   type = "closed"
                 )) + 
  geom_node_point(aes(size = I(degree), fill = attributes.which_city), pch = 21) +
  geom_node_text(aes(label = label), size = 3.5, repel = F) + 
  theme_void() + 
  theme(legend.position = 'bottom') + 
  scale_size_area(max_size = 15,guide="none")  + 
  scale_edge_width(range = c(.5,3),guide="none") + labs(fill = 'City')


ggsave('output/figures/final/figure_11.pdf', plot = p,width = 13.18, height = 13.32)

g = graph_list[[4]] %>% 
  mutate(node_id = 1:nrow1760) %>% 
  mutate(degree = centrality_degree(mode = 'all')) %>% 
  mutate(in_degree = centrality_degree(mode = 'in')) %>% 
  mutate(out_degree = centrality_degree(mode = 'out'))%>%
  left_join(asFrame1760, by = c('node_id' = 'id'))%>%
  mutate(attributes.which_city = ifelse(in_degree>out_degree, 'London', 'Edinburgh'))  %>% 
  select(-x, -y) 

p = g %>%
  ggraph(layout = layout_df1760, circular = FALSE) +
  geom_edge_link(aes(width = weight,end_cap = circle(node2.degree + 10, "pt")),
                 edge_colour = "grey25",
                 arrow = arrow(
                   length = unit(0.1, "inches"),
                   type = "closed"
                 )) + 
  geom_node_point(aes(size = I(degree), fill = attributes.which_city), pch = 21) +
  geom_node_text(aes(label = label), size = 4, repel = F) + 
  theme_void() + 
  theme(legend.position = 'bottom')  + 
  scale_size_area(max_size = 15,guide="none")  + 
  scale_edge_width(range = c(.5,3),guide="none") + labs(fill = 'City')

ggsave('output/figures/final/figure_12.pdf', plot = p,width = 13.18, height = 13.32)


# 
# g = graph_list[[5]] %>% 
#   mutate(node_id = 1:nrow1780) %>% 
#   mutate(degree = centrality_degree(mode = 'all'))%>% 
#   mutate(in_degree = centrality_degree(mode = 'in')) %>% 
#   mutate(out_degree = centrality_degree(mode = 'out')) %>%
#   left_join(asFrame1780, by = c('node_id' = 'id')) %>%
#   mutate(attributes.which_city = ifelse(in_degree>out_degree, 'London', 'Edinburgh'))%>% 
#   select(-x, -y) 
# # 
# p = g %>%
#   ggraph(layout = layout_df1780, circular = FALSE) +
#   geom_edge_link(aes(width = weight,end_cap = circle(node2.degree + 10, "pt")),
#                  edge_colour = "grey25",
#                  arrow = arrow(
#                    length = unit(0.05, "inches"),
#                    type = "closed"
#                  )) + 
#   geom_node_point(aes(size = I(degree), fill = attributes.which_city), pch = 21) +
#   geom_node_text(aes(label = label), size = 4, repel = F) + 
#   theme_void() + 
#   theme(legend.position = 'bottom') + 
#   scale_size_area(max_size = 15,guide="none")  + 
#   scale_edge_width(range = c(.5,3),guide="none") + labs(fill = 'City')
# 
# ggsave('output/figures/final/1780_net.pdf', plot = p,width = 13.18, height = 13.32)