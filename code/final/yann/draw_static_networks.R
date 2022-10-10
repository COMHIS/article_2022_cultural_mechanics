
library(ggraph)
library(igraph)
library(tidygraph)


## Example bipartite diagram:

g = tibble(from =c("Book2", "Book2", "Book2", "Book3", "Book3", "Book1", "Book1"),to =  c('Kincaid', 'Creech', 'Strahan', "Strahan", "Millar", "Strahan", "Millar"), 
     ) %>% 
  graph_from_data_frame()

V(g)$type <- bipartite_mapping(g)$type

p1 = g %>% ggraph('bipartite') + geom_edge_link() + 
  geom_node_point(size = 12) + geom_node_label(aes(label = name),  size = 5, family = 'Times') + coord_flip() + theme_void() + 
  scale_x_continuous(expand = c(.15, .15))+ 
  scale_y_continuous(expand = c(.15, .15)) + theme(text = element_text(family = "Times"))

b_g = bipartite.projection(g)

p2 = b_g[[2]] %>% ggraph() + geom_edge_link(aes(width = weight)) + 
  geom_node_point(size = 12) + geom_node_label(aes(label = name),  size = 5, family = 'Times') + coord_flip() + theme_void() + 
  scale_x_continuous(expand = c(.1, .2))+ 
  scale_y_continuous(expand = c(.1, .1)) + 
  theme(text = element_text(family = "Times"))

p3 = cowplot::plot_grid(p1, p2)

ggsave('output/figures/final/figure_1.pdf', width = 8, height = 4)

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
  mutate(degree = centrality_degree(mode = 'all')) %>% 
  as_tibble() %>% 
  arrange(desc(degree)) %>% 
  head(13) %>% 
  pull(name)

all_names = g %>% as_tibble() %>% pull(name)

highlight = c('robertfreebairn_0', '54200258', '71442890')

p = g %>%
  ggraph(layout = layout_df1700, circular = FALSE) +
  geom_edge_link(aes(width = weight,end_cap = circle(node2.degree + 15, "pt")),
                 edge_colour = "grey25",
                 arrow = arrow(
                   length = unit(0.05, "inches"),
                   type = "closed"
                 )) + 
  geom_node_point(aes(size = degree, fill = attributes.which_city), pch = 21, color = 'black')+
  geom_node_label(family = "Times", aes(label = label),
                  fill = ifelse(all_names %in%highlight, 'black', 'white'), 
                  size = ifelse(all_names %in%highlight, 4,2), repel = T, 
                  color = ifelse(all_names %in%highlight, 'white', 'black')) + 
  theme_void() + 
  theme(legend.position = 'bottom', text = element_text(family = "Times")) + 
  scale_size_area(max_size = 25, guide="none")  + 
  scale_edge_width(range = c(.5,3),guide="none") + 
  labs(fill = 'City') + 
  scale_fill_manual(values = c('black', 'white')) + 
  guides(fill = guide_legend(override.aes = list(size=10)))

ggsave('output/figures/final/figure_8.pdf', plot = p,width = 7, height = 7)

g = graph_list[[2]] %>% 
  mutate(node_id = 1:nrow1720) %>% 
  mutate(degree = centrality_degree(mode = 'all')) %>% 
  mutate(in_degree = centrality_degree(mode = 'in')) %>% 
  mutate(out_degree = centrality_degree(mode = 'out')) %>% 
  left_join(asFrame1720, by = c('node_id' = 'id')) %>%
  mutate(attributes.which_city = ifelse(in_degree>out_degree, 'London', 'Edinburgh')) %>% 
  select(-x, -y) 

all_names = g %>% as_tibble() %>% pull(name)

highlight = c('101583814', '77108557', 'wilhamilton_0', '15008766')

p = g %>%
  ggraph(layout = layout_df1720, circular = FALSE) +
  geom_edge_link(aes(width = weight,end_cap = circle(node2.degree + 10, "pt")),
                 edge_colour = "grey25",
                 arrow = arrow(
                   length = unit(0.1, "inches"),
                   type = "closed"
                 )) + 
  geom_node_point(aes(size = I(degree), fill = attributes.which_city), pch = 21, color = 'black') +
  geom_node_label(family = "Times", aes(label = ifelse(degree >1, label, NA)),
                  fill = ifelse(all_names %in%highlight, 'black', 'white'), 
                  size = ifelse(all_names %in%highlight, 4,1.5), repel = T, 
                  color = ifelse(all_names %in%highlight, 'white', 'black'))  + 
  theme_void() + 
  theme(legend.position = 'bottom', panel.background = element_blank(), 
        text = element_text(family = "Times"))  + 
  scale_size_area(max_size = 15,guide="none")  + 
  scale_edge_width(range = c(.5,3),guide="none") + 
  labs(fill = 'City') + 
  scale_fill_manual(values = c('black', 'white')) + 
  guides(fill = guide_legend(override.aes = list(size=10)))

ggsave('output/figures/final/figure_9.pdf', plot = p)

g = graph_list[[3]] %>% 
  mutate(node_id = 1:nrow1740) %>% 
  mutate(degree = centrality_degree(mode = 'all'))%>% 
  mutate(in_degree = centrality_degree(mode = 'in')) %>% 
  mutate(out_degree = centrality_degree(mode = 'out')) %>%
  left_join(asFrame1740, by = c('node_id' = 'id')) %>%
  mutate(attributes.which_city = ifelse(in_degree>out_degree, 'London', 'Edinburgh')) %>% 
  select(-x, -y) 

all_names = g %>% as_tibble() %>% pull(name)

highlight = c('32051268', '39241855', '75336707', 'wilhamilton_0', 'NV4519', '15008766')

p = g %>%
  ggraph(layout = layout_df1740, circular = FALSE) +
  geom_edge_link(aes(width = weight,end_cap = circle(node2.degree + 10, "pt")),
                 edge_colour = "grey25",
                 arrow = arrow(
                   length = unit(0.1, "inches"),
                   type = "closed"
                 )) + 
  geom_node_point(aes(size = I(degree), fill = attributes.which_city), pch = 21, color = 'black') +
  geom_node_label(family = "Times", aes(label = ifelse(degree >1, label, NA)),
                  fill = ifelse(all_names %in%highlight, 'black', 'white'), 
                  size = ifelse(all_names %in%highlight, 6,2), repel = T, 
                  color = ifelse(all_names %in%highlight, 'white', 'black')) + 
  theme_void() + 
  theme(legend.position = 'bottom', text = element_text(size = 14, family = 'Times')) + 
  scale_size_area(max_size = 15,guide="none")  + 
  scale_edge_width(range = c(.5,3),guide="none") + 
  labs(fill = 'City') + 
  scale_fill_manual(values = c('black', 'white')) + 
  guides(fill = guide_legend(override.aes = list(size=10)))


ggsave('output/figures/final/figure_10.pdf', plot = p,width = 13.18, height = 13.32)

g = graph_list[[4]] %>% 
  mutate(node_id = 1:nrow1760) %>% 
  mutate(degree = centrality_degree(mode = 'all')) %>% 
  mutate(in_degree = centrality_degree(mode = 'in')) %>% 
  mutate(out_degree = centrality_degree(mode = 'out'))%>%
  left_join(asFrame1760, by = c('node_id' = 'id'))%>%
  mutate(attributes.which_city = ifelse(in_degree>out_degree, 'London', 'Edinburgh'))  %>% 
  select(-x, -y) 

all_names = g %>% as_tibble() %>% pull(name)

highlight = c('NV4519', '39467138', '49278054', '282772606', '39241855', 'johnbell_1', 'williamsands_0', 'jamescochran_0', 'jamesdickson_0', 'gavinalston_0', '39273848')


p = g %>%
  ggraph(layout = layout_df1760, circular = FALSE) +
  geom_edge_link(aes(width = weight,end_cap = circle(node2.degree + 10, "pt")),
                 edge_colour = "grey25",
                 arrow = arrow(
                   length = unit(0.1, "inches"),
                   type = "closed"
                 )) + 
  geom_node_point(aes(size = I(degree), fill = attributes.which_city), pch = 21, color = 'black') +
  geom_node_label(family = "Times", aes(label = ifelse(degree >1, label, NA)),
                  fill = ifelse(all_names %in%highlight, 'black', 'white'), 
                  size = ifelse(all_names %in%highlight, 5,2.5), repel = T, 
                  color = ifelse(all_names %in%highlight, 'white', 'black')) + 
  theme_void() + 
  theme(legend.position = 'bottom')  + 
  scale_size_area(max_size = 15,guide="none")  + 
  scale_edge_width(range = c(.5,3),guide="none") + 
  labs(fill = 'City') + 
  scale_fill_manual(values = c('black', 'white')) + 
  guides(fill = guide_legend(override.aes = list(size=10)))

ggsave('output/figures/final/figure_11.pdf', plot = p,width = 13.18, height = 13.32)
