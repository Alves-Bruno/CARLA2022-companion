options(crayon.enabled=FALSE)
library(tidyverse) 
library(patchwork)

read_csv("../data/comb.csv", progress=FALSE, show_col_types = FALSE, col_types="iiiiii") -> comb

tibble(
  freq.name=c(0.125, 0.250, 0.500, 0.750,  1.00,  1.25)
) %>% 
  mutate(freq=row_number()) %>% 
  mutate(freq.name=paste(sprintf("%.3f",freq.name), "Hz")) -> hz 

comb %>% 
#  print %>% 
  left_join(hz, by = "freq") %>%
  ggplot(aes(x=Group,y=RxTx.pairs, color=as.factor(freq.name)))+
  geom_point(size=2)+
#  geom_line()+
#  geom_smooth(alpha=.1)+
  labs(x="Refinement Groups [id]",y="Rx-Tx Pairs [count]", color="Frequencies")+
  theme_bw(base_size=20) -> p 

#p
#dev.off()
ggsave(
  dpi = 100,
 filename="figure_5.png",
 plot = p,
 #path = "~/Documents/2022/CMP223/artigo_final/evaluation/images",
 path = "../images",
 width = 8,
 height = 5
)

#0.125–1.25
# 0.125Hz 0.350 0.575 0.800 1.025 1.250

#p
