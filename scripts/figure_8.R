options(crayon.enabled=FALSE)
library(tidyverse) 
library(patchwork)

read_csv("../data/traces_raw.csv.gz", progress=FALSE) -> traces.raw
read_csv("../data/mean_trace.csv", progress=FALSE) -> mean_trace

traces.raw %>% 
  filter(Operation == "iteration") %>% 
  mutate(Duration = End - Start) %>%  
  group_by(trace.exec) %>%  
  mutate(Iteration = row_number()) %>%  
  group_by(Iteration) %>%
  summarize(
    Duration.mean=mean(Duration), 
    Duration.se=3*sd(Duration)/sqrt(n())  
  ) %>% 
  print -> iterations.duration

traces.raw %>% 
#  filter(Rank==0) %>% select(Operation) %>% distinct()
  filter(Operation %in% c("jacobianCompute", "smoothingOccam")) %>% 
  mutate(Phase=Operation) %>%
  mutate(Phase=gsub("jacobianCompute", "Jacobian", Phase)) %>% 
  mutate(Phase=gsub("smoothingOccam", "Smoothing", Phase)) %>%
  mutate(Duration = End - Start) %>%  
  group_by(trace.exec, Phase) %>%  
  mutate(Iteration = row_number()) %>%  
  group_by(Iteration, Phase) %>%
  summarize(Duration.mean=mean(Duration),     Duration.se=3*sd(Duration)/sqrt(n())  ) %>%
  print -> iterations.phases.duration
#  select(trace.exec, Duration, Operation)

mean_trace %>% 
  ungroup() %>%
  filter(Operation!="local_refinement") %>% 
  group_by(Iteration, Phase) %>%
  summarize(processed_nodes=sum(NodesFrom)) -> p_nodes
  
mean_trace %>% 
  select(Iteration, Phase, Repetition) %>% 
  distinct() %>% 
  group_by(Iteration, Phase) %>% 
  summarize(Repetition = n()) %>%
  print(n=nrow(.)) -> repetitions

iterations.phases.duration %>% 
  left_join(repetitions, by = c("Iteration", "Phase")) %>%
  ggplot()+
  geom_point(aes(x=Iteration,y=Duration.mean, colour=Phase))+
  geom_line(aes(x=Iteration,y=Duration.mean, colour=Phase))+
  geom_text(data=. %>% filter(Phase=="Smoothing",Iteration!=4), aes(x=Iteration, y=Duration.mean+100, label=Repetition))+
  ## geom_text(
  ##   data=. %>% filter(Iteration==4, Phase=="Jacobian"), 
  ##   aes(x=Iteration, y=Duration.mean-100, label=Repetition)
  ## )+
  geom_text(
    data=. %>% filter(Iteration==4, Phase=="Smoothing"), 
    aes(x=Iteration-.17, y=Duration.mean+100, label=Repetition)
  )+
  geom_errorbar(aes(x=Iteration, y=Duration.mean, ymin=Duration.mean-Duration.se, ymax=Duration.mean+Duration.se), width=.3)+
  ylim(0, NA)+
  theme_bw(base_size=16) +
  labs(x="Iteration", y="Mean Duration [s]")+
  theme(
    legend.margin = margin(0, 0, 0, 0), 
    legend.position="top", 
    plot.margin = margin(0, .5, 0, 0, "cm")
  ) -> a

iterations.phases.duration %>% 
  left_join(p_nodes, by = c("Iteration", "Phase")) %>%
  ggplot(aes(x=processed_nodes/ 1000000,y=Duration.mean, colour=Phase))+
  geom_point()+
  geom_line()+
  geom_rect(aes(xmin=600, xmax=1400, ymin=400, ymax=1000), color="grey40", alpha=0, size=.1)+
  geom_text(data=tibble(), aes(x=900, y=1300, label="Zoom"), color="black")+
  ylim(0, NA)+
  theme_bw(base_size=16) +
#  facet_wrap(~"Smoothing limited axis")+
  labs(x="Iteration", y="Mean \nDuration [s]")+
  theme(legend.position="none", axis.title.x=element_blank()) -> b

iterations.phases.duration %>% 
  left_join(p_nodes, by = c("Iteration", "Phase")) %>%
  ggplot(aes(x=processed_nodes / 1000000,y=Duration.mean, colour=Phase))+
  geom_point()+
  geom_line()+
  ylim(0, NA)+
  coord_cartesian(xlim=c(720,1320), ylim=c(480, 920))+
  facet_wrap(~"Zoom into Jacobian")+
  theme_bw(base_size=16)+
  labs(x="Processed nodes x 1M", y="Mean \nDuration [s]")+
  theme(legend.position="none") -> c


p <- a + (b / c)

ggsave(
  dpi = 100,
  filename="figure_8.png",
  plot = p,
  #path = "~/Documents/2022/CMP223/artigo_final/evaluation/images",
  path = "../images",
  width = 10,
  height = 5.5
)

#p
