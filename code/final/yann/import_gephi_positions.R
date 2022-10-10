# create a random network


library(jsonlite)
node_json1700 = jsonlite::read_json('data/work/1700_json.json')
nodes_list1700 = node_json1700$nodes
asFrame1700  <- do.call("rbind", lapply(nodes_list1700, as.data.frame)) %>% mutate(id = as.numeric(id))

node_json1720 = jsonlite::read_json('data/work/1720_json.json')
nodes_list1720 = node_json1720$nodes
asFrame1720  <- do.call("rbind", lapply(nodes_list1720, as.data.frame)) %>% mutate(id = as.numeric(id))

node_json1740 = jsonlite::read_json('data/work/1740_json.json')
nodes_list1740 = node_json1740$nodes
asFrame1740  <- do.call("rbind", lapply(nodes_list1740, as.data.frame)) %>% mutate(id = as.numeric(id))

node_json1760 = jsonlite::read_json('data/work/1760_json.json')
nodes_list1760 = node_json1760$nodes
asFrame1760  <- do.call("rbind", lapply(nodes_list1760, as.data.frame)) %>% mutate(id = as.numeric(id))

node_json1780 = jsonlite::read_json('data/work/1780_json.json')
nodes_list1780 = node_json1780$nodes
asFrame1780  <- do.call("rbind", lapply(nodes_list1780, as.data.frame)) %>% mutate(id = as.numeric(id))

nrow1700 =  graph_list[[1]] %>% as_tibble() %>% nrow()
layout_df1700 = graph_list[[1]] %>% mutate(node_id = 1:nrow1700) %>% 
  left_join(asFrame1700, by = c('name' = 'attributes.name')) %>% as_tibble() %>% select(x,y)

nrow1720 =  graph_list[[2]] %>% as_tibble() %>% nrow()
layout_df1720 = graph_list[[2]] %>% mutate(node_id = 1:nrow1720) %>% 
  left_join(asFrame1720, by = c('node_id' = 'id')) %>% as_tibble() %>% select(x,y)

nrow1740 =  graph_list[[3]] %>% as_tibble() %>% nrow()
layout_df1740 = graph_list[[3]] %>% mutate(node_id = 1:nrow1740) %>% 
  left_join(asFrame1740, by = c('node_id' = 'id')) %>% as_tibble() %>% select(x,y)

nrow1760 =  graph_list[[4]] %>% as_tibble() %>% nrow()
layout_df1760 = graph_list[[4]] %>% mutate(node_id = 1:nrow1760) %>% 
  left_join(asFrame1760, by = c('node_id' = 'id')) %>% as_tibble() %>% select(x,y)

nrow1780 =  graph_list[[5]] %>% as_tibble() %>% nrow()
layout_df1780 = graph_list[[5]] %>% mutate(node_id = 1:nrow1780) %>% 
  left_join(asFrame1780, by = c('node_id' = 'id')) %>% as_tibble() %>% select(x,y)


