

wer1 <-estc_core %>%
  group_by(work_id) %>%
  slice(which.min(publication_year))

shs1$size <- wer1$document_type[match(shs1$work_id, wer1$work_id)]

sts3 <- shs1 %>% filter(!size == "Pamphlet")

sts3 <- sts3 %>% drop_na(format)

sts3 <- sts3 %>% drop_na(size)

###

top <- names(rev(sort(table(sts3$format)))[1:3])
af <- sts3 %>%
  mutate(top_gatherings = format %in% top) %>%
  mutate(gatherings_new = ifelse(top_gatherings, format, "Other")) %>%
  group_by(gatherings_new) 

af <- af %>% filter(publication_decade <1800)


af4 <- af %>% group_by(publication_decade, gatherings_new) %>%
  arrange(publication_decade) %>%
  summarize(n = n()) 

### binding these

af3$type <- "further_editions"

af4$type <- "first_editions"

g1 <- rbind(af3, af4)

g12<- g1 %>%
  pivot_wider(names_from = type, values_from = n, values_fill = 0)

g12$further_editions = g12$further_editions - g12$first_editions

g12<- g12 %>% pivot_longer(cols=c('further_editions', 'first_editions'),
                           names_to='type',
                           values_to='count') 

###

can4 <- ggplot(g12, aes(x=publication_decade, y=count, fill=type)) + geom_bar(stat = "identity", width = 5, position = "dodge")+
  labs(x = "Publication decade",
       y = "n",
       title = paste("Book formats of Scottish Enlightenment keyworks (no pamphlet-sized)")) + facet_wrap(~gatherings_new, scales = "free") 

can4
