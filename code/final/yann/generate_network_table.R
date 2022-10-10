full_scottish_authors = read_csv('../r_projects/publisher_network/full_scottish_authors.csv')

first_ed_year = estc_core %>% 
  group_by(work_id) %>% 
  slice_min(order_by= publication_year, n = 1) %>% 
  select(work_id, publication_year)

estc_raw = read_csv('../../estc-data-unified/estc-cleaned-initial/estc_processed.csv')

estc_publisher_info = estc_raw %>% 
  mutate(estc_id = trimws(str_remove_all(system_control_number, "\\(CU-RivES\\)"))) %>% 
  select(estc_id, publisher)

sher_authors = author_info %>% filter(Sher == 'yes')


info_list = list()
group_list = list()
work_info = list()
edge_list = list()




for(i in seq(1700, 1780, 20)){
  
  
  
  
  
  sample = all_links %>% 
    #filter(work_id %in% scottish_core_works) %>% 
    filter(publication_year %in% i:(i+19)) %>% 
    left_join(corrected_names, by= c('actor_id' = 'name')) %>% 
    mutate(actor_id = coalesce(corrected_id, actor_id)) %>% filter(actor_id != 'X')
  
  sample_all = all_links  %>%
    #filter(work_id %in% scottish_core_works)%>% 
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
  
  
  edge_list[[as.character(i)]] = all
  
  node_list  = all  %>%
    count(actor_id.x, actor_id.y, name = 'weight') %>%
    #  filter(weight>1) %>% 
    igraph::graph_from_data_frame(directed = T) %>% 
    as_tbl_graph() %>% 
    mutate(in_degree = centrality_degree(weights = weight, mode = 'in'))%>% 
    mutate(all_degree = centrality_degree(weights = weight, mode = 'all')) %>% 
    mutate(out_degree = centrality_degree(weights = weight, mode = 'out')) %>% 
    as_tibble()
  
  
  works_names = all %>% select(actor_id = actor_id.x, work_id) %>% rbind(all %>% select(actor_id = actor_id.y, work_id)) %>% distinct(actor_id, work_id)
  
  work_id_names = estc_core %>% distinct(work_id, .keep_all = T) %>% select(work_id, short_title)
  authors = estc_actor_links %>% filter(actor_role_author == T) %>% 
    select(actor_id, estc_id, actor_name_primary) %>% 
    left_join(estc_core %>% select(estc_id, work_id)) %>% distinct(work_id, .keep_all = T) %>% 
    select(actor_id, work_id, actor_name_primary)
  
  
  
  info_list[[as.character(i)]] =  node_list %>% 
    left_join(works_names, by = c('name' = 'actor_id')) %>% 
    left_join(estc_core %>% select(estc_id, work_id)) %>% 
    distinct(name, estc_id, .keep_all = T) %>% 
    count(name, in_degree, out_degree, all_degree, work_id) %>% 
    left_join(work_id_names) %>% 
    left_join(first_ed_year) %>% 
    arrange(desc(n)) %>% 
    left_join(authors, by = 'work_id') %>% 
    mutate(title = paste0(actor_name_primary," | ", short_title," (", publication_year, ") | ", n)) %>% 
    group_by(name) %>% 
    summarise(info = paste0(unique(title), collapse = '; '), in_degree = in_degree, out_degree = out_degree, all_degree = all_degree) %>% 
    left_join(estc_actors %>% 
                select(actor_id, name_unified), by = c('name' = 'actor_id')) %>% 
    distinct(name, name_unified, info, in_degree, out_degree, all_degree) %>% 
    #left_join(df_membership_list[[as.character(i)]], by = c('name' = 'names')) %>% arrange(desc(out_degree)) %>% 
    #mutate(all_info = paste0(name, "", info, "|", name_unified)) %>% group_by(membership) %>% summarise(all_all = paste0(all_info, collapse = '; ')) %>% 
    select(name_unified, all_degree, in_degree, out_degree, info) %>% arrange(desc(all_degree))
  
  
  group_list[[as.character(i)]] =  node_list %>% 
    left_join(works_names, by = c('name' = 'actor_id')) %>% 
    left_join(estc_core %>% select(estc_id, work_id)) %>% 
    distinct(name, estc_id, .keep_all = T) %>% 
    count(name, in_degree, out_degree, all_degree, work_id) %>% 
    left_join(work_id_names) %>% 
    left_join(first_ed_year) %>% 
    arrange(desc(n)) %>% 
    left_join(authors, by = 'work_id') %>% 
    mutate(title = paste0(actor_name_primary," | ", short_title," (", publication_year, ") | ", n)) %>% 
    group_by(name) %>% 
    summarise(info = paste0(unique(title), collapse = '; '), 
              in_degree = in_degree, out_degree = out_degree, all_degree = all_degree) %>% 
    left_join(estc_actors %>% 
                select(actor_id, name_unified), by = c('name' = 'actor_id')) %>% 
    distinct(name, name_unified, info, in_degree, out_degree, all_degree) %>% 
    left_join(df_membership_list[[as.character(i)]], by = c('name' = 'names')) %>% arrange(desc(out_degree)) %>% 
    mutate(all_info = paste0(name_unified, " (", all_degree, ")(", in_degree, ")(", out_degree, ")")) %>% 
    group_by(membership) %>% 
    summarise(all_all = paste0(all_info, collapse = '; ')) 
  
  
  node_list  = all  %>%
    count(actor_id.x, actor_id.y, name = 'weight') %>%
    #  filter(weight>1) %>% 
    igraph::graph_from_data_frame(directed = T) %>% 
    as_tbl_graph() %>% 
    mutate(in_degree = centrality_degree(weights = weight, mode = 'in'))%>% 
    mutate(out_degree = centrality_degree(weights = weight, mode = 'out')) %>% 
    as_tibble()
  
  work_id_names = estc_core %>% distinct(work_id, .keep_all = T) %>% select(work_id, short_title)
  authors = estc_actor_links %>% filter(actor_role_author == T) %>% 
    select(actor_id, estc_id, actor_name_primary) %>% left_join(estc_core %>% select(estc_id, work_id)) %>% distinct(work_id, .keep_all = T) %>% 
    select(actor_id, work_id, actor_name_primary)
  
  work_info[[as.character(i)]]  =  all %>% 
    select(work_id, actor_id.x, actor_id.y, estc_id.x, estc_id.y, publication_year.x, publication_year.y, publication_place.x, publication_place.y, name_unified.x, name_unified.y) %>% 
    left_join(estc_core %>% select(estc_id, short_title), by = c('estc_id.x' = 'estc_id')) %>% 
    left_join(estc_publisher_info, by = c('estc_id.x' = 'estc_id')) %>% 
    left_join(estc_publisher_info, by = c('estc_id.y' = 'estc_id'))%>% 
    left_join(estc_core %>% select(estc_id, short_title), by = c('estc_id.y' = 'estc_id')) %>% 
    arrange(publication_year.y) %>% 
    left_join(authors %>% select(work_id, actor_name_primary, author_id = actor_id)) %>% 
    mutate(scottish = ifelse(author_id %in% full_scottish_authors$actor_id, 'yes', 'no'))%>% 
    mutate(sher_list = ifelse(author_id %in% sher_authors$actor_id, 'yes', 'no')) %>% 
    mutate(publication_year.x = paste0("<a href='http://estc.bl.uk/", estc_id.x, "' target = '_blank'>", publication_year.x, "</a>" )) %>% 
    mutate(publication_year.y = paste0("<a href='http://estc.bl.uk/", estc_id.y, "' target = '_blank'>", publication_year.y, "</a>", " (", publisher.y, ")" )) %>%
    mutate(edges = paste0(name_unified.x, " -> ", name_unified.y)) %>% 
    group_by(work_id) %>% 
    summarise(london_titles = paste0("<b>Title: </b><i>", short_title.x, "</i>. <b>Author: </b>", actor_name_primary, ". <b>First Edinburgh edition: </b>", publication_year.x, ". <b>Ed. Imprint: </b>", publisher.x, ". <b>Sub. London editions: </b>", paste0(unique(publication_year.y), collapse = ', ')), connections = paste0(edges, collapse = ', '), total_london = length(unique(publication_year.y)), scottish_author = scottish, in_sher = sher_list) %>% 
    distinct(london_titles, .keep_all = T) 
  
}

info_list = data.table::rbindlist(info_list, idcol = 'year_start')
group_list = data.table::rbindlist(group_list, idcol = 'year_start')
edge_list = data.table::rbindlist(edge_list, idcol = 'year_start')


info_table_DT = work_info %>% 
  data.table::rbindlist(idcol = 'year_start') %>% 
  # filter(scottish_author == 'yes') %>% 
  DT::datatable(escape = F)

save(work_info, file = 'info_table_file')
