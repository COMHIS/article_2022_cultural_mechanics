library(visNetwork)


all_links = estc_actor_links %>% 
  left_join(estc_actors %>% select(actor_id, is_organization)) %>% 
  filter(is_organization== FALSE) %>% 
  mutate(actor_id = ifelse(actor_id %in% millar,  millar[1],
                           ifelse(actor_id %in% creech,  creech[1],
                                  ifelse(actor_id %in% kincaid,  kincaid[1],
                                         ifelse(actor_id %in% donaldson, donaldson[1],
                                                ifelse( actor_id %in% bell,  bell[1],
                                                        ifelse(actor_id %in% cadell, cadell[1],
                                                               ifelse(actor_id %in% elliot, elliot[1], 
                                                                      ifelse(actor_id %in% dodsley, dodsley[1],
                                                                             ifelse(actor_id %in% cooper_t, cooper_t[1],
                                                                                    ifelse(actor_id %in% cooper_m, cooper_m[1],
                                                                                           ifelse(actor_id %in% hawkins, hawkins[1],
                                                                                                  ifelse(actor_id %in% wood_j, wood_j[1],
                                                                                                         ifelse(actor_id %in% strahan, strahan[1],
                                                                                                                ifelse(actor_id %in% freebairn_r, freebairn_r[1],
                                                                                                                       ifelse(actor_id %in% dodd, dodd[1],
                                                                                                                              ifelse(actor_id %in% hamilton, hamilton[1], 
                                                                                                                                     ifelse(actor_id %in% e_c_dilly, e_c_dilly[1],
                                                                                                                                            ifelse(actor_id %in% ramsay, ramsay[1],
                                                                                                                                                   ifelse(actor_id %in% longman, longman[1], actor_id)))))))))))))))))))) %>% 
  filter(actor_role_printer == T | actor_role_publisher == TRUE | actor_role_bookseller == T) %>%
  left_join(estc_core %>% 
              select(estc_id, publication_year, publication_place, work_id))



for_net = imprint_places %>% 
  mutate(actor_id = ifelse(actor_id %in% millar,  millar[1],
                           ifelse(actor_id %in% creech,  creech[1],
                                  ifelse(actor_id %in% kincaid,  kincaid[1],
                                         ifelse(actor_id %in% donaldson, donaldson[1],
                                                ifelse( actor_id %in% bell,  bell[1],
                                                        ifelse(actor_id %in% cadell, cadell[1],
                                                               ifelse(actor_id %in% elliot, elliot[1], 
                                                                      ifelse(actor_id %in% dodsley, dodsley[1],
                                                                             ifelse(actor_id %in% cooper_t, cooper_t[1],
                                                                                    ifelse(actor_id %in% cooper_m, cooper_m[1],
                                                                                           ifelse(actor_id %in% hawkins, hawkins[1],
                                                                                                  ifelse(actor_id %in% wood_j, wood_j[1],
                                                                                                         ifelse(actor_id %in% strahan, strahan[1],
                                                                                                                ifelse(actor_id %in% freebairn_r, freebairn_r[1],
                                                                                                                       ifelse(actor_id %in% dodd, dodd[1],
                                                                                                                              ifelse(actor_id %in% hamilton, hamilton[1], 
                                                                                                                                     ifelse(actor_id %in% e_c_dilly, e_c_dilly[1],
                                                                                                                                            ifelse(actor_id %in% ramsay, ramsay[1],
                                                                                                                                                   ifelse(actor_id %in% longman, longman[1], actor_id))))))))))))))))))))%>%
  mutate(combined = coalesce(city_or_assumed_address, actor_addresses)) %>% 
  left_join(corrected_names, by= c('actor_id' = 'name')) %>% 
  mutate(actor_id = coalesce(corrected_id, actor_id)) %>% filter(actor_id != 'X')

i = 1760

list_of_vn = list()

for(i in seq(1700, 1780, 20)){
  
 
  
  
  
  
  sample = all_links %>% 
    filter(work_id !='X-the lounger. a periodical paper') %>% 
    filter(publication_year %in% i:(i+19)) %>% 
    left_join(corrected_names, by= c('actor_id' = 'name')) %>% 
    mutate(actor_id = coalesce(corrected_id, actor_id)) %>% filter(actor_id != 'X')
  
  sample_all = all_links  %>% 
    filter(work_id !='X-the lounger. a periodical paper') %>% 
    #filter(publication_year %in% i:(i+19)) %>% 
    left_join(corrected_names, by= c('actor_id' = 'name')) %>% 
    mutate(actor_id = coalesce(corrected_id, actor_id)) %>% filter(actor_id != 'X')
  
  for_net_sample = for_net 
  
  
  all_links1 = sample %>% 
    left_join(for_net_sample %>% select(estc_id, actor_id, combined), by = c('estc_id', 'actor_id')) %>% 
    mutate(actor_publication_location = coalesce(combined, publication_place)) %>% 
    select(work_id, actor_id, publication_year, publication_place, estc_id,actor_publication_location) %>% 
    filter(publication_place %in% c('London', 'Edinburgh'))
  
  all_links2 = sample_all %>% 
    left_join(for_net_sample %>% select(estc_id, actor_id, combined), by = c('estc_id', 'actor_id')) %>% 
    mutate(actor_publication_location = coalesce(combined, publication_place)) %>% 
    select(work_id, actor_id, publication_year, publication_place, estc_id,actor_publication_location) %>% 
    filter(publication_place %in% c('London', 'Edinburgh'))
  
  full_links = all_links1 %>% left_join(all_links2, by = 'work_id')
  
  if(i<1720){ all = full_links %>% 
    distinct(actor_id.x, actor_id.y, estc_id.x, estc_id.y, .keep_all = T) %>% 
    filter(actor_id.x != actor_id.y) %>% 
    filter(estc_id.x %in% first_edition$estc_id) %>% 
    filter(publication_place.x == 'Edinburgh') %>% 
    filter(publication_year.x<publication_year.y) %>% 
    filter(publication_year.x > (publication_year.y-20)) %>% 
    filter(estc_id.x != estc_id.y) %>% 
    filter(actor_publication_location.x != actor_publication_location.y) %>% 
    #left_join(final_with_predicted, by = 'work_id') %>% 
    left_join(estc_actors %>% 
                select(actor_id, name_unified), by = c('actor_id.x' = 'actor_id')) %>% 
    left_join(estc_actors %>% 
                select(actor_id, name_unified), by = c('actor_id.y' = 'actor_id'))
  
  } else{  all = full_links %>% 
    distinct(actor_id.x, actor_id.y, estc_id.x, estc_id.y, .keep_all = T) %>% 
    filter(actor_id.x != actor_id.y) %>% 
    filter(estc_id.x %in% first_edition$estc_id) %>% 
    filter(publication_place.x == 'Edinburgh') %>% 
    filter(publication_year.x<publication_year.y) %>% 
    filter(publication_year.x > (publication_year.y-20)) %>% 
    filter(estc_id.x != estc_id.y) %>% 
    filter(publication_place.x != publication_place.y)%>% 
    filter(actor_publication_location.x != actor_publication_location.y)  %>% 
    #left_join(final_with_predicted, by = 'work_id') %>% 
    left_join(estc_actors %>% 
                select(actor_id, name_unified), by = c('actor_id.x' = 'actor_id')) %>% 
    left_join(estc_actors %>% 
                select(actor_id, name_unified), by = c('actor_id.y' = 'actor_id'))
  
  }
  
  
  
  
  node_list  = all  %>%
    count(actor_id.x, actor_id.y, name = 'weight') %>%
    #  filter(weight>1) %>% 
    igraph::graph_from_data_frame(directed = T) %>% 
    as_tbl_graph() %>% 
    mutate(in_degree = centrality_degree(weights = weight, mode = 'in')) %>% 
    mutate(out_degree = centrality_degree(weights = weight, mode = 'out')) %>%
    mutate(color = ifelse(in_degree>out_degree, "London", "Edinburgh")) %>% 
    as_tibble()
  
  work_id_names = estc_core %>% distinct(work_id, .keep_all = T) %>% select(work_id, short_title)
  authors = estc_actor_links %>% filter(actor_role_author == T) %>% 
    select(actor_id, estc_id, actor_name_primary) %>% left_join(estc_core %>% select(estc_id, work_id)) %>% distinct(work_id, .keep_all = T) %>% 
    select(actor_id, work_id, actor_name_primary)
  
  for_vn = all %>% 
    select(work_id, actor_id.x, actor_id.y, estc_id.x, estc_id.y, publication_year.x, publication_year.y, publication_place.x, publication_place.y) %>% 
    distinct(work_id, actor_id.x, actor_id.y,estc_id.x, .keep_all = T)%>% 
    left_join(estc_core %>% select(estc_id, short_title), by = c('estc_id.x' = 'estc_id')) %>% 
    left_join(estc_core %>% select(estc_id, short_title), by = c('estc_id.y' = 'estc_id')) %>% 
    arrange(publication_year.y) %>% left_join(authors %>% select(work_id, actor_name_primary)) %>% 
    mutate(publication_year.x = paste0("<a href='http://estc.bl.uk/", estc_id.x, "' target = '_blank'>", publication_year.x, "</a>" )) %>% 
    mutate(publication_year.y = paste0("<a href='http://estc.bl.uk/", estc_id.y, "' target = '_blank'>", publication_year.y, "</a>" )) %>%
    group_by(actor_id.x, actor_id.y, work_id) %>% 
    summarise(london_titles = paste0("Title: <i>", short_title.x, "</i>. Author: ", actor_name_primary, ". First Edinburgh edition: ", publication_year.x, ". Sub. London editions: ", paste0(publication_year.y, collapse = ', ')), n = n()) %>% 
    distinct(actor_id.x, actor_id.y, london_titles, .keep_all = T) %>% group_by(actor_id.x, actor_id.y) %>% 
    summarise(london_eds = paste0(london_titles, collapse = '<br><br>'), n = sum(n)) %>% arrange(desc(n)) %>%
    left_join(estc_actors %>% select(actor_id, name_unified), by = c('actor_id.x' = 'actor_id'))%>%
    left_join(estc_actors %>% select(actor_id, name_unified), by = c('actor_id.y' = 'actor_id')) %>% 
    mutate(title = paste0("<b>", name_unified.x, " -> ", name_unified.y, "</b><br>", london_eds)) %>% 
    graph_from_data_frame() %>% as_tbl_graph() %>% 
    activate(edges) %>% 
    mutate(width = n) %>% 
    activate(nodes) %>% 
    left_join(estc_actors %>% 
                select(actor_id, name_unified), by = c('name' = 'actor_id')) %>% 
    left_join(name_list, by ='name') %>% 
    mutate(name_unified = coalesce(corrected_label, name_unified)) %>% 
    mutate(in_degree = centrality_degree( mode = 'in')) %>% 
    mutate(out_degree = centrality_degree( mode = 'out')) %>%
    mutate(color = ifelse(in_degree>out_degree, "#F8766D", "#00B6EB")) %>% 
    mutate(group = color) %>% 
    mutate(name = paste0(name_unified, " (", name, ")")) %>% 
    mutate(size = 10*sqrt(centrality_degree(weights = n, mode = 'all')))
  
  list_of_vn[[as.character(i)]] = visNetwork::visIgraph(for_vn, physics = F) %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) 
  
}



save(list_of_vn, file = 'list_of_vn')


