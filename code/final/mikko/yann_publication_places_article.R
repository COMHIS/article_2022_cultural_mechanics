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

can8 <- ggplot(af1, aes(x = publication_decade, y = sum, fill = place))  + 
  geom_bar(stat = "identity", colour="black", position=position_fill(), width=10) + 
  scale_x_continuous(breaks = c(1700, 1710, 1720, 1730, 1740, 1750, 1760, 1770, 1780, 1790)) +
  labs(x = "Publication decade",
       y = "percentage",
       title = paste("Comparing Edinburgh, London and other places of publication"))+ theme_minimal()


ggsave('output/figures/final/figure_5.jpg', plot = can8, width = 10, height = 6)
