shs1 <- read.csv(file="data/final/key_scottish_works.csv", header = T, stringsAsFactors = FALSE)

shs2 <- subset(estc_core, (work_id %in% shs1$work_id))

myvars <- as.character(c("estc_id", "publication_place", "publication_year", "publication_decade", "work_id", "short_title"))
shs2 = shs2[myvars]

shs2$topic <- shs1$topic[match(shs2$work_id, shs1$work_id)]

shs2 <- shs2 %>% filter(publication_year < 1800)

quarter <- function (x) {
  x <- as.numeric(as.character(x))
  floor(x/25) * 25
}

shs2$publication_quarter <- quarter(shs2$publication_year)

af1 <- shs2 %>% group_by(publication_quarter, topic) %>%
  arrange(publication_quarter) %>%
  summarize(n = n()) %>%
  group_by(publication_quarter) %>%
  mutate(f = n/sum(n))

### plot

can8 <- ggplot(af1, aes(x = publication_quarter, y = f, fill = topic))  + 
  ggpattern::geom_col_pattern( color = 'black',
                              pattern_color = "black",
                              pattern_fill = "white",
                              aes(pattern = topic)) + 
  scale_x_continuous(breaks=c(1700, 1725, 1750, 1775, 1800)) +
  labs(x = "Publication quarter",
       y = "Topic percentage", pattern = "Topic:", fill = "Topic:") +
  theme_minimal() + 
  ggpattern::scale_pattern_manual(values=c('none', 'circle', 'none', 'wave', 'weave')) +
  #ggpattern::scale_pattern_type_manual(values=c(NA, NA, NA, NA, NA), guide = 'none') + 
  #ggpattern::scale_pattern_density_manual(values = c(1,1,.1,.1,.1), guide = 'none') + 
  scale_fill_manual(values = c('black', 'white', 'white', 'white', 'white')) + 
  theme(legend.key.size = unit(1.5, 'cm'))+ 
  ggpattern::scale_pattern_color_discrete(guide = 'none') + 
  ggpattern::scale_pattern_fill_discrete(guide = 'none') + 
  theme(legend.position = 'bottom', text = element_text( size = 14, family = "Times"))


ggsave('output/figures/final/figure_6.pdf', plot = can8, width = 10, height = 6)
