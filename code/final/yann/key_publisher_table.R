scottish_core_editions = estc_core %>% filter(work_id %in% scottish_core_works) %>% pull(estc_id)

estc_actor_with_changes = estc_actor_links %>% 
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
                                                                                                                                                   ifelse(actor_id %in% longman, longman[1], actor_id)))))))))))))))))))) 

tot1 = estc_actor_with_changes %>% 
  filter(actor_role_printer == T | actor_role_publisher == TRUE | actor_role_bookseller == T)  %>% 
  left_join(estc_core %>% select(estc_id, work_id)) %>% 
  filter(work_id %in% scottish_core_works) %>% 
  left_join(labels_to_use, by = 'work_id') %>% 
  count(actor_id, name = 'Total') %>%
  arrange(desc(Total))



tot2 = estc_actor_with_changes %>% 
  filter(actor_role_printer == T | actor_role_publisher == TRUE | actor_role_bookseller == T)%>% 
  left_join(estc_core %>% select(estc_id, work_id)) %>% 
  filter(work_id %in% scottish_core_works) %>% 
  filter(estc_id %in% first_edition$estc_id) %>% 
  left_join(labels_to_use, by = 'work_id')  %>% 
  count(actor_id, name = 'Total')
  
  
  
 tot3 = tot1 %>% left_join(tot2, by = 'actor_id') %>% 
  mutate(Total = paste0(Total.x, " (", Total.y, ")"))


j = estc_actor_with_changes %>% 
  filter(actor_role_publisher == TRUE | actor_role_bookseller == TRUE | actor_role_printer == TRUE) %>% 
  filter(estc_id %in% scottish_core_editions) %>% 
  left_join(estc_core %>% 
              distinct(estc_id, work_id)) %>% 
  inner_join(labels_to_use, by = 'work_id') %>% 
  mutate(first_ed = ifelse(estc_id %in% first_edition$estc_id, 'yes','no')) %>% 
  count(actor_id, main_category)

jj = estc_actor_with_changes %>% 
  filter(actor_role_publisher == TRUE | actor_role_bookseller == TRUE | actor_role_printer == TRUE) %>% 
  filter(estc_id %in% scottish_core_editions) %>% 
  left_join(estc_core %>% 
              distinct(estc_id, work_id)) %>% 
  inner_join(labels_to_use, by = 'work_id') %>% 
  mutate(first_ed = ifelse(estc_id %in% first_edition$estc_id, 'yes','no')) %>% 
  filter(first_ed == 'yes') %>% 
  count(actor_id, main_category)

j %>% left_join(jj, by = c('actor_id', 'main_category')) %>% 
  mutate(totals = ifelse(!is.na(n.y), paste0(n.x, " (", n.y, ")"), n.x)) %>% 
  select(-n.x, -n.y) %>% 
  pivot_wider(names_from = main_category, values_from = totals) %>% 
  left_join(tot3, by  = 'actor_id') %>% slice_max(Total.x, n = 20) %>% 
  left_join(estc_actors %>% 
              select(actor_id, name_unified)) %>% 
  select(name_unified, everything(),-actor_id, -Total.x, -Total.y) %>% write_csv('updated_totals.csv')

                                                                    