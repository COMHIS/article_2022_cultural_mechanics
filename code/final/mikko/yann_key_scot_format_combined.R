shs1 <- read.csv(file="data/final/key_scottish_works.csv", header = T, stringsAsFactors = FALSE)

wer1 <-estc_core %>%
  group_by(work_id) %>%
  slice(which.min(publication_year))

shs1$size <- wer1$document_type[match(shs1$work_id, wer1$work_id)]

sts3 <- shs1 %>% filter(!size == "Pamphlet")

sts3 <- sts3 %>% drop_na(format)

sts3 <- sts3 %>% drop_na(size)

###

af3 <- subset(estc_core, (work_id %in% sts3$work_id))

myvars <- as.character(c("estc_id", "publication_place", "publication_year", "publication_decade", "work_id", "short_title"))
af3 = af3[myvars]

af3$format <- sts3$format[match(af3$work_id, sts3$work_id)]

### 

af3 <- af3 %>% filter(publication_year < 1800)

af3 <- af3 %>% group_by(publication_decade, format) %>%
  arrange(publication_decade) %>%
  summarize(n = n()) 

###

af4 <- sts3 %>% filter(imprint_date < 1800)

decade <- function (x) {
  x <- as.numeric(as.character(x))
  floor(x/10) * 10
}
af4$publication_decade <- decade(af4$imprint_date)

af4 <- af4 %>% group_by(publication_decade, format) %>%
  arrange(publication_decade) %>%
  summarize(n = n()) 
### binding these

af3$type <- "further_editions"

af4$type <- "first_editions"

g1 <- rbind(af3, af4)

g12<- g1 %>%
  pivot_wider(names_from = type, values_from = n, values_fill = 0)

g12$further_editions = g12$further_editions - g12$first_editions

g12<- g12 %>% rename(`Further editions` = further_editions, `First editions` = first_editions) %>% 
  pivot_longer(cols=c('Further editions', 'First editions'),
                           names_to='type',
                           values_to='count') 

top <- names(rev(sort(table(g12$format)))[1:4])
g13 <- g12 %>%
  mutate(top_gatherings = format %in% top) %>%
  mutate(gatherings_new = ifelse(top_gatherings, format, "Other")) %>%
  group_by(gatherings_new) 

###

can4 <- ggplot(g13, aes(x=publication_decade, y=count, fill=type)) + 
  geom_bar(stat = "identity", width = 5, position = "dodge", color = 'black')+
  scale_x_continuous(breaks = seq(1700, 1800, 20))  +  
  labs(x = "Publication decade",
       y = "Editions", fill = 'Type:') + 
  theme_bw() + 
  theme(legend.position = 'bottom', text = element_text(family = 'Times', size =14)) + 
  scale_fill_manual(values = c('black', 'white')) + 
  facet_wrap(~gatherings_new, scales = "free_y") 

ggsave('output/figures/final/figure_4.pdf', plot = can4, width = 10, height = 6)
