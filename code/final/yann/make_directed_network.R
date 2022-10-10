library(tidyverse)
library(tidytable)
library(igraph)
library(tidygraph)
library(ggraph)

#estcr::load_estc()
load('../r_projects/publisher_network/estc_actor_links')
load('../r_projects/publisher_network/estc_actors')
load('../r_projects/publisher_network/estc_core')

source('code/final/yann/load_alternative_actor_names.R')

all_links = estc_actor_links %>% 
  left_join(estc_actors %>% select(actor_id, is_organization)) %>% 
  filter(is_organization== FALSE)%>% 
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
                                                                                                                                                   ifelse(actor_id %in% longman, longman[1],
                                                                                                                                                          ifelse(actor_id %in% murray, murray,actor_id)))))))))))))))))))))%>% 
  filter(actor_role_printer == T | actor_role_publisher == TRUE | actor_role_bookseller == T) %>%
  left_join(estc_core %>% 
              select(estc_id, publication_year, publication_place, work_id))

authors = estc_actor_links %>% filter(actor_role_author == T) %>%
  select(actor_id, estc_id, actor_name_primary) %>% 
  left_join(estc_core %>% select(estc_id, work_id)) %>% 
  distinct(work_id, .keep_all = T) %>%
  select(actor_id, work_id, actor_name_primary)

after_1700 = estc_core %>% group_by(work_id) %>% 
  slice_min(publication_year,n = 1) 

after_1700 = after_1700 %>% filter(publication_year >1700) %>% pull(work_id)


labels_to_use = read_csv('data/work/labels_to_use.csv')

author_info = read_csv('data/work/full_scottish_authors.csv')

all_scottish_authors = author_info %>% filter(!scottish %in% c('no', '?'))



scottish_core_authors = estc_core %>% filter(work_id %in% after_1700) %>% left_join(labels_to_use, by = 'work_id') %>% 
  filter(sub_category %in% c('historygeneral', 
                             'philosophyhumanunderstandinglogicmetaphysicsetc', 
                             'philosophymoralphilosophy',
                             'philosophypoliticalphilosophy',
                             'scientificimprovementeconomyandtrade', 
                             'scientificimprovementgeographycartographyastronomyandnavigation', 
                             'scientificimprovementmathematics', 
                             'scientificimprovementmedicineandanatomy', 
                             'scientificimprovementnaturalhistory',
                             'scientificimprovementnaturalphilosophy', 
                             'educationgeneral') | main_category %in% 'Literature') %>% 
  left_join(authors, by = 'work_id') %>% 
  filter(actor_id %in% all_scottish_authors$actor_id) %>% distinct(work_id, .keep_all = T) %>% 
  count(actor_id) %>% 
  filter(n>1) %>% 
  left_join(estc_actors %>% select(actor_id, name_unified), by = 'actor_id') %>% arrange(desc(n)) %>% pull(actor_id)

scottish_core_works = estc_core %>% left_join(authors, by = 'work_id') %>% 
  filter(actor_id %in% scottish_core_authors) %>% 
  left_join(labels_to_use, by = 'work_id') %>% 
  filter(sub_category %in% c('historygeneral', 
                             'philosophyhumanunderstandinglogicmetaphysicsetc', 
                             'philosophymoralphilosophy',
                             'philosophypoliticalphilosophy',
                             'scientificimprovementeconomyandtrade', 
                             'scientificimprovementgeographycartographyastronomyandnavigation', 
                             'scientificimprovementmathematics', 
                             'scientificimprovementmedicineandanatomy', 
                             'scientificimprovementnaturalhistory','scientificimprovementnaturalphilosophy', 
                             'educationgeneral') | main_category %in% 'Literature') %>% count(work_id) %>% filter(n>1) %>% 
  pull(work_id)



corrected_names = read_csv('data/work/merged_node_list.csv')

imprint_places = read_csv('data/work/ed_london_addresses.csv') %>% select(-c(10:11))


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
                                                                                                                                                   ifelse(actor_id %in% longman, longman[1],
                                                                                                                                                          ifelse(actor_id %in% murray, murray,actor_id)))))))))))))))))))))%>%
  mutate(combined = coalesce(city_or_assumed_address, actor_addresses))%>% 
  #filter(publication_year %in% i:(i+19))%>% 
  left_join(corrected_names, by= c('actor_id' = 'name')) %>% 
  mutate(actor_id = coalesce(corrected_id, actor_id)) %>% filter(actor_id != 'X')

first_edition = estc_core %>% 
  slice_min.(.by = work_id, order_by = publication_year, n=  1)

graph_list = list()

for(i in seq(1700, 1780, 20)){
  
  
  
  
  sample = all_links %>% 
    filter(!work_id %in% c('X-the lounger. a periodical paper', '6163-medical essays and observations published')) %>% 
    filter(publication_year %in% i:(i+19)) %>% 
    left_join(corrected_names, by= c('actor_id' = 'name')) %>% 
    mutate(actor_id = coalesce(corrected_id, actor_id)) %>% filter(actor_id != 'X')
  
  sample_all = all_links  %>% 
    filter(!work_id %in% c('X-the lounger. a periodical paper', '6163-medical essays and observations published')) %>% 
    #filter(publication_year %in% i:(i+19)) %>% 
    left_join(corrected_names, by= c('actor_id' = 'name')) %>% 
    mutate(actor_id = coalesce(corrected_id, actor_id)) %>% filter(actor_id != 'X')
  
  for_net_sample = for_net 
  
  
  all_links1 = sample %>% 
    left_join(for_net_sample %>% 
                select(estc_id, actor_id, combined), by = c('estc_id', 'actor_id')) %>% 
    mutate(actor_publication_location = coalesce(combined, publication_place)) %>% 
    select(work_id, actor_id, publication_year, publication_place, estc_id,actor_publication_location) %>% 
    filter(publication_place %in% c('London', 'Edinburgh'))
  
  all_links2 = sample_all %>% 
    left_join(for_net_sample %>% select(estc_id, actor_id, combined), by = c('estc_id', 'actor_id')) %>% 
    mutate(actor_publication_location = coalesce(combined, publication_place)) %>% 
    select(work_id, actor_id, publication_year, publication_place, estc_id,actor_publication_location) %>% 
    filter(publication_place %in% c('London', 'Edinburgh'))
  
  full_links = all_links1 %>% left_join(all_links2, by = 'work_id')
  
  if(i<1720){ all = full_links  %>% 
    filter(!work_id %in% c('X-the lounger. a periodical paper', '6163-medical essays and observations published'))%>% 
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
  
  } else{  all = full_links  %>% 
    filter(!work_id %in% c('X-the lounger. a periodical paper', '1225-anatomy of humane body'))%>% 
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
  
  graph_list[[as.character(i)]]  = all  %>%
    count(actor_id.x, actor_id.y, name = 'weight') %>%
    #  filter(weight>1) %>% 
    igraph::graph_from_data_frame(directed = T) %>% as_tbl_graph() 
  
}



# Next run leiden_alg.rmarkdown (because it has python in it)

rmarkdown::render('code/final/yann/run_leiden_on_graph.Rmd')

df_membership_list = list('1700' = df_with_mem1700,'1720' = df_with_mem1720, '1740' = df_with_mem1740, '1760' = df_with_mem1760, '1780' = df_with_mem1780)

name_list = corrected_names %>% distinct(name, .keep_all = T) %>% 
  mutate(name_corrected = coalesce(corrected_label, name_unified)) %>% 
  mutate(corrected_label = snakecase::to_title_case(corrected_label)) %>% 
  select(name, corrected_label, born)

which_city_list = list()

plot_list = list()

all_pub_places = estc_actor_links %>% 
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
                                                                                                                                                   ifelse(actor_id %in% longman, longman[1],
                                                                                                                                                          ifelse(actor_id %in% murray, murray,actor_id)))))))))))))))))))))%>% 
  select(actor_id, estc_id) %>% 
  left_join(estc_core %>% 
              select(estc_id, publication_place, publication_year))

which_city = all_pub_places %>% 
  #filter(work_id %in% scottish_core_works) %>% 
  left_join(corrected_names, by= c('actor_id' = 'name')) %>% 
  mutate(actor_id = coalesce(corrected_id, actor_id)) %>% filter(actor_id != 'X') %>% 
  distinct(estc_id, actor_id, publication_place) %>% 
  filter(publication_place %in% c('London', 'Edinburgh')) %>% 
  count(actor_id, publication_place) %>% group_by(actor_id) %>% 
  slice_max(order_by = n, n = 1) %>% 
  rename(which_city = publication_place) %>% 
  select(-n) %>% 
  distinct(actor_id, .keep_all = T)

which_city2 = imprint_places %>% 
  #filter(work_id %in% scottish_core_works) %>% 
  left_join(corrected_names, by= c('actor_id' = 'name')) %>% 
  mutate(actor_id = coalesce(corrected_id, actor_id)) %>% filter(actor_id != 'X') %>% 
  distinct(estc_id, actor_id, publication_place) %>% 
  filter(publication_place %in% c('London', 'Edinburgh')) %>% 
  count(actor_id, publication_place) %>% group_by(actor_id) %>% 
  slice_max(order_by = n, n = 1) %>% 
  rename(which_city = publication_place) %>% 
  select(-n) %>% 
  distinct(actor_id, .keep_all = T)

which_city = which_city %>% left_join(which_city2, by = 'actor_id') %>% mutate(which_city = coalesce(which_city.y, which_city.x)) %>% 
  select(actor_id, which_city)

graph_list[[1]] %>% activate(edges) %>% 
  as_tibble() %>% 
  rename(Source = from, Target = to) %>% 
  write_csv('1700_edges.csv')



graph_list[[1]] %>% 
  left_join(df_membership_list[[1]], by = c('name' = 'names')) %>% 
  as_tibble() %>% 
  mutate(Id = 1:nrow(.)) %>% 
  left_join(estc_actors %>% 
              select(actor_id, actor_gender, year_pub_first_estc, viaf_link, name_unified), by = c('name' = 'actor_id')) %>% 
  left_join(name_list, by ='name') %>% 
  mutate(name_unified = coalesce(corrected_label, name_unified)) %>%
  left_join(which_city, by = c('name' = 'actor_id')) %>%  
  select(Id, membership, Label = name_unified, which_city, name) %>% 
  
  write_csv('1700_nodes.csv')

graph_list[[2]] %>% activate(edges) %>% as_tibble() %>% rename(Source = from, Target = to) %>% 
  write_csv('1720_edges.csv')



graph_list[[2]] %>% 
  left_join(df_membership_list[[2]], by = c('name' = 'names')) %>% 
  as_tibble() %>% mutate(Id = 1:nrow(.)) %>%
  left_join(which_city, by = c('name' = 'actor_id')) %>% 
  left_join(estc_actors %>% 
              select(actor_id, actor_gender, year_pub_first_estc, viaf_link, name_unified), by = c('name' = 'actor_id')) %>% 
  left_join(name_list, by ='name') %>% 
  mutate(name_unified = coalesce(corrected_label, name_unified))%>% select(Id, membership, Label = name_unified, which_city) %>% 
  
  write_csv('1720_nodes.csv')


graph_list[[3]] %>% activate(edges) %>% as_tibble() %>% rename(Source = from, Target = to) %>% 
  write_csv('1740_edges.csv')



graph_list[[3]] %>% 
  left_join(df_membership_list[[3]], by = c('name' = 'names')) %>% 
  as_tibble() %>% mutate(Id = 1:nrow(.))  %>%
  left_join(which_city, by = c('name' = 'actor_id'))%>% 
  left_join(estc_actors %>% 
              select(actor_id, actor_gender, year_pub_first_estc, viaf_link, name_unified), by = c('name' = 'actor_id')) %>% 
  left_join(name_list, by ='name') %>% 
  mutate(name_unified = coalesce(corrected_label, name_unified))%>% 
  select(Id, membership, Label = name_unified, which_city) %>% 
  
  write_csv('1740_nodes.csv')

graph_list[[4]] %>% activate(edges) %>% as_tibble() %>% rename(Source = from, Target = to) %>% 
  write_csv('1760_edges.csv')



graph_list[[4]] %>% 
  left_join(df_membership_list[[4]], by = c('name' = 'names')) %>% 
  as_tibble() %>% mutate(Id = 1:nrow(.)) %>%
  left_join(which_city, by = c('name' = 'actor_id'))%>% 
  left_join(estc_actors %>% 
              select(actor_id, actor_gender, year_pub_first_estc, viaf_link, name_unified), by = c('name' = 'actor_id')) %>% 
  left_join(name_list, by ='name') %>% 
  mutate(name_unified = coalesce(corrected_label, name_unified))%>% select(Id, membership, Label = name_unified, which_city) %>% 
  
  write_csv('1760_nodes.csv')

graph_list[[5]] %>% activate(edges) %>% as_tibble() %>% rename(Source = from, Target = to) %>% 
  write_csv('1780_edges.csv')



graph_list[[5]] %>% 
  left_join(df_membership_list[[5]], by = c('name' = 'names')) %>% 
  as_tibble() %>% mutate(Id = 1:nrow(.)) %>%
  left_join(which_city, by = c('name' = 'actor_id'))%>% 
  left_join(estc_actors %>% 
              select(actor_id, actor_gender, year_pub_first_estc, viaf_link, name_unified), by = c('name' = 'actor_id')) %>% 
  left_join(name_list, by ='name') %>% mutate(name_unified = coalesce(corrected_label, name_unified))%>% 
  select(Id, membership, Label = name_unified, which_city) %>% 
  
  write_csv('1780_nodes.csv')

