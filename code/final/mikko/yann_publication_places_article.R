#shs1 <- read.csv(file="key_scottish_works.csv", header = T, stringsAsFactors = FALSE)

shs2 <- subset(estc_core, (work_id %in% shs1$work_id))

myvars <- as.character(c("estc_id", "publication_place", "publication_year", "publication_decade", "work_id", "short_title"))
shs2 = shs2[myvars]

###

shs2 <- shs2 %>% drop_na(publication_place)

shs2 <- shs2 %>% filter(publication_year > 1699)

top <- names(rev(sort(table(shs2$publication_place)))[1:2])
af <- shs2 %>% group_by(publication_decade, publication_place) %>%
  summarize(n = n()) %>%
  mutate(top_place = publication_place %in% top) %>%
  mutate(place = ifelse(top_place, publication_place, "Other")) %>%
  group_by(place) %>%
  mutate(f = n/sum(n)) %>%
  arrange(place)

### plot

af1 <- af %>% group_by(publication_decade, place) %>%
  summarize(sum = sum(f))

can8 <- ggplot(af1, aes(publication_decade, sum))  + 
  ggpattern::geom_col_pattern(aes(pattern = place, fill = place
  ), position = 'fill', color = 'black') + 
  theme_bw() + 
  theme(text = element_text(family = 'Times', size =14), legend.position = 'bottom') + 
  scale_x_continuous(breaks = seq(1700, 1800, 10)) +
  ggpattern::scale_pattern_manual(values=c('crosshatch', 'none', 'none')) +
  scale_fill_manual(values = c('white', 'white', 'black')) + 
  labs(y = "Proportion", x = NULL, fill = "Place:", pattern = "Place:")+ 
  theme(legend.key.size = unit(1.5, 'cm'))





ggsave('output/figures/final/figure_5.pdf', plot = can8, width = 10, height = 6)
