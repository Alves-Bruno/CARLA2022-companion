options(crayon.enabled=FALSE)
library(tidyverse) 
library(patchwork)

read_csv("../data/traces_enriched.csv.gz", progress=FALSE) -> traces.enriched

base.size <- 16

op <- c("em2dkx_localRefinement", "lhs_gen", "primal_solve", "error_estimate", "derivs_comp_adj")

traces.enriched %>% 
  select(-Group) %>% 
  rename(Group=RefGroup2) %>%
  filter(trace.exec==1, Iteration==3, Rank==7, Group==7) %>%
  filter(Operation %in% op) -> temp 

tibble(
  start = seq(1, 30, 5),
  end = seq(5, 31, 5)
) %>% 
#  print() %>% 
  mutate(Subset.id = row_number()) %>% 
  pivot_longer(cols=c("start", "end"), values_to="iSubset") %>%  
  select(-name) -> sub.id

temp %>% 
  group_by(Phase) %>%
  summarize(Start=min(Start), End=max(End)) %>%
  mutate(Operation="Group Refinement", Group=7) -> op.group


temp %>%
  left_join(sub.id, by = "iSubset") %>%
  group_by(Subset.id, Phase, Rank) %>%
  summarize(Start=min(Start), End=max(End)) %>%
  filter(!is.na(Subset.id)) %>%
  mutate(Operation="Subset Processing") -> op.subset

tibble(
  Operation=c(op, "Subset Processing", "Group Refinement"), 
  y=c(2, 1, 1, 1, 1, 3, 4)
) -> yvalues

bind_rows(
  temp %>% 
    select(Phase, Start, End, Operation), 
  bind_rows(op.subset %>% select(-Rank), op.group)
) %>% 
  left_join(yvalues) %>% 
  mutate(Operation=gsub("em2dkx_localRefinement", "local_refinement", Operation))-> temp.2


stack_plot <- function(df, phase){

df %>% 
  filter(Phase==phase) %>%
  mutate(Phase=gsub("jacobianCompute", "Jacobian", Phase)) %>% 
  mutate(Phase=gsub("smoothingOccam", "Smoothing", Phase)) -> temp.3

temp.3 %>%
  ggplot()+
  geom_rect(
    aes(
      xmin=Start, xmax=End,
      ymin=y -.4, ymax=y +.4, 
      fill=Operation
    ), color="black", size=.2, alpha=.8
  ) + 
  scale_fill_manual(breaks = c("Group Refinement", "Subset Processing", "local_refinement", "derivs_comp_adj", "error_estimate", "lhs_gen", "primal_solve"), 
                       values=c("#E41A1C","#FF7F00","#377EB8","#F781BF","#4DAF4A","#FFFF33","#A65628","#F781BF")) + 

  geom_text(data= temp.3 %>% filter(Operation=="Group Refinement"), aes(x=Start + (End-Start)/2, y=4, label=Group), size=5.5) + 
  geom_text(data= temp.3 %>% filter(Operation=="Subset Processing"), aes(x=Start + (End-Start)/2, y=3, label=paste((Subset.id-1)*5+1,(Subset.id)*5,sep=" - ")), size=4) + 
  labs(y="Stack Level", x= "Time [seconds]") +
  theme_bw(base_size=base.size) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  facet_wrap(~Phase) 
}

a <- stack_plot(temp.2, "jacobianCompute") + 
theme(
  legend.position="top",
  axis.title.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(), 
  plot.margin = margin(0, .5, 0, .5, "cm"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

b <- stack_plot(temp.2, "smoothingOccam") +
theme(
  legend.position="none",
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(), 
  plot.margin = margin(0, .5, 0, .5, "cm"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

p <- a / b

ggsave(
  dpi = 100,
  filename="figure_6.png",
  plot = p,
  path = "../images",
  width = 10,
  height = 6
)
