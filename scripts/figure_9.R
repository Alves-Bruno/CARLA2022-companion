options(crayon.enabled=FALSE)
library(tidyverse) 
library(patchwork)

read_csv("../data/mean_trace.csv.gz", progress=FALSE) -> mean_trace
read_csv("../data/comb.csv", progress=FALSE, show_col_types = FALSE, col_types="iiiiii") -> comb

tibble(
  freq_value=c(0.125, 0.250, 0.500, 0.750,  1.00,  1.25)
) %>% 
  mutate(freq=row_number()) %>% 
  mutate(freq_value=paste(sprintf("%.3f",freq_value), "Hz")) -> hz 


mean_trace %>% 
  filter(!is.na(NodesFrom)) %>%
  group_by(Group, Phase, Iteration) %>%
  summarize(Nodes=sum(NodesFrom), Duration.mean=sum(Duration.mean)) -> temp

temp %>% 
  left_join(comb, by="Group") %>% 
  left_join(hz, by="freq")%>%
  filter(Iteration %in% c(5,6,7,8)) -> temp 

temp %>%
  filter(Phase=="Jacobian") %>%
  ggplot(aes(x=Group,y=Duration.mean, colour=as.factor(freq_value)))+
  geom_smooth(span=.3, alpha=.1)+
  theme_bw(base_size=16) + 
  ylim(0, NA) +
  facet_wrap(~Phase) +
  labs(y="Duration [s]")+ 
  theme(
    legend.position="none",
  ) -> a

temp %>%
  filter(Phase=="Jacobian") %>%
  ggplot(aes(x=Group,y=Nodes/10000, colour=as.factor(freq_value)))+
  geom_smooth(span=.3, alpha=.1)+
  theme_bw(base_size=16) + 
  facet_wrap(~Phase) +
  labs(y="Nodes [10K]")+
  theme(
    legend.position="none"
  ) -> b

temp %>%
  filter(Phase=="Smoothing") %>%
  ggplot(aes(x=Group,y=Duration.mean, colour=as.factor(freq_value)))+
  geom_smooth(span=.3, alpha=.1)+
  theme_bw(base_size=16) + 
  ylim(0, NA) +
  facet_wrap(~Phase)+ 
  labs(colour="Frequencies")+
  theme(
    axis.title.y = element_blank()
  )-> c

temp %>%
  filter(Phase=="Smoothing") %>%
  ggplot(aes(x=Group,y=Nodes/10000, colour=as.factor(freq_value)))+
  geom_smooth(span=.3, alpha=.1)+
  theme_bw(base_size=16) + 
  facet_wrap(~Phase) + 
  labs(colour="Frequencies")+
  theme(
    axis.title.y = element_blank()
  )-> d

p <- a + c + b + d + plot_layout(nrow=2)

#dev.off()
ggsave(
  dpi = 100,
  filename="figure_9.png",
  plot = p,
  #path = "~/Documents/2022/CMP223/artigo_final/evaluation/images",
  path = "../images",
  width = 10,
  height = 6
)

p
