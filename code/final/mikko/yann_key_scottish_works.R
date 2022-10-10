library(tidyverse)
load('../r_projects/publisher_network/estc_core')

shs1 <- read.csv(file="data/final/key_scottish_works.csv", header = T, stringsAsFactors = FALSE)

shs2 <- subset(estc_core, (work_id %in% shs1$work_id))

myvars <- as.character(c("estc_id", "publication_place", "publication_year", "publication_decade", "work_id", "short_title"))
shs2 = shs2[myvars]

shs2$topic <- shs1$topic[match(shs2$work_id, shs1$work_id)]

### simple timeline of works, both work_id level (first work) + following editions

shs2 <- shs2 %>% filter(publication_year < 1800)

shs22 <- shs2 %>% group_by(publication_decade) %>%
  arrange(publication_decade) %>%
  summarize(n = n()) 

## first editions only

shs1 <- shs1 %>% filter(imprint_date < 1800)

decade <- function (x) {
  x <- as.numeric(as.character(x))
  floor(x/10) * 10
}

shs1$publication_decade <- decade(shs1$imprint_date)

shs11 <- shs1 %>% group_by(publication_decade) %>%
  arrange(publication_decade) %>%
  summarize(n = n()) 

### binding these

shs22$type <- "further_editions"

shs11$type <- "first_editions"

g1 <- rbind(shs11, shs22)

g12<- g1 %>%
  pivot_wider(names_from = type, values_from = n, values_fill = 0)

g12$further_editions = g12$further_editions - g12$first_editions

g12<- g12 %>% pivot_longer(cols=c('further_editions', 'first_editions'),
                    names_to='type',
                    values_to='count') 

## plot

can4 <- ggplot(g12, aes(x=publication_decade, y=count, fill=type)) + 
  geom_bar(stat = "identity", width = 5, position = "dodge") +
  scale_x_continuous(breaks = seq(1700, 1800, 10))  +  
  labs(x = "Publication decade",
       y = "n",
       title = paste("All editions of Scottish Enlightenment keyworks")) 

ggsave('output/figures/final/figure_3.jpg', plot = can4, width = 10, height = 6)
