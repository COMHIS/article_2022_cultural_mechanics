library(tidyverse)

shs1 <- read.csv(file="data/final/key_scottish_works.csv", header = T, stringsAsFactors = FALSE)

yryr1 <- shs1 %>% group_by(author) %>%
  arrange(author) %>%
  summarize(n = n()) 

yryr2 <- shs1 %>% group_by(author) %>%
  summarize(sum = sum(popularity))

df = merge(x=yryr1,y=yryr2,by="author")

### dominant topic

yrry3 <- shs1 %>% group_by(author, topic) %>%
  summarize(n = n())

yrry4 <- yrry3 %>% group_by(author) %>% slice_max(n)

##

df$genre <- yrry4$topic[match(df$author, yrry4$author)]

####

df1 <- df %>% filter(sum > 32)


library(ggrepel)
library(ggplot2)

p = ggplot(df1, aes(x=n, y=sum, shape=genre)) +
  geom_point(size = 2.5) + theme_bw()  +
  geom_text_repel(
    data = subset(df1, n > 50 | df1$sum >8),
    aes(label = author),size = 3, family = 'Times',
    box.padding   = 0.35,
    point.padding = 0.5,
    nudge_x = .15,
    nudge_y = .5,
    segment.curvature = -1e-20,
    arrow = arrow(length = unit(0.015, "npc"))) +
  labs(x="Number of works", y = "Total number of editions", shape = "Genre:")  + 
  scale_y_sqrt() +
  scale_x_sqrt() + 
  theme(panel.background = element_blank(), 
        text = element_text(family = "Times", size = 14), 
        legend.position = 'bottom') 

ggsave('output/figures/final/figure_7.pdf', plot = p, width = 10, height = 6)
