options(crayon.enabled=FALSE)
library(tidyverse) 
library(patchwork)

read_csv("../data/traces_raw.csv.gz", progress=FALSE) -> traces.raw
read_csv("../data/traces_enriched.csv.gz", progress=FALSE) -> traces.enriched

# MANAGER OPERATIONS:
#1 mpi_manager_send_subset    
#2 mpi_manager_receive_results
#3 jacobianCompute            
#4 smoothingOccam             
#5 Occam                      
#6 iteration

traces.raw %>% 
  filter(trace.exec==1, Rank==0) %>%
  select(Operation, Start, End, Imbrication) %>% 
#  filter(Operation %in% c("iteration", "jacobianCompute", "smoothingOccam")) %>% 
  filter(Operation %in% c("iteration")) %>% 
  group_by(Operation) %>% 
  mutate(Repeat=row_number()) -> iterations

X.min <- 1700
X.max <- 2450
base.size <- 20
iterations %>% 
  ggplot()+
  geom_rect(
    aes(
      xmin=Start, xmax=End,
      ymin=0, ymax=1, 
#      fill=
    ), color="black", fill="gray"
  ) + 
  geom_text(aes(x=Start + (End-Start)/2, y=0.5, label=Repeat), size=5) + 
#  geom_point()+
#  labs(x="x",y="y")+
#  ylim(0, 1) + 
  coord_cartesian(xlim=c(X.min, X.max), ylim=c(0.04, 0.96)) +
  facet_wrap(~"Iteration") + 
  theme_bw(base_size=base.size) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(),

    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), 
    axis.title.y = element_blank(),

    plot.margin = margin(0, 0, 0.1, 0, "cm")
  ) -> plot.it

plot.it

traces.raw %>% 
  filter(trace.exec==1, Rank==0) %>%
  select(Operation, Start, End, Imbrication) %>% 
  filter(Operation %in% c("jacobianCompute", "smoothingOccam")) %>% 
  mutate(Operation=gsub("jacobianCompute", "Jacobian", Operation)) %>% 
  mutate(Operation=gsub("smoothingOccam", "Smoothing", Operation)) %>% 
  group_by(Operation) %>% 
  mutate(Repeat=row_number()) -> phases

phases %>% 
  filter(Repeat==3) %>%
  ggplot()+
  geom_rect(
    aes(
      xmin=Start, xmax=End,
      ymin=0, ymax=1, 
      fill=Operation
    ), color="black"
  ) + 
  geom_text(aes(x=Start + (End-Start)/2, y=0.5, label=Operation), size=5) + 
  coord_cartesian(xlim=c(X.min, X.max), ylim=c(0.04, 0.96)) + 
  facet_wrap(~"Phases") + 
  theme_bw(base_size=base.size) +
  theme(
    legend.position="none",

    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(),

    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), 
    axis.title.y = element_blank(),

    plot.margin = margin(0, 0, .1, 0, "cm")
  ) -> plot.phase

plot.phase

#1 mpi_worker_run_subset 
#2 em2dkx_localRefinement
#3 em2dkx_iteration      
#4 lhs_gen               
#5 primal_solve          
#6 error_estimate        
#7 derivs_comp_adj       
#x <- seq(0, 1, length.out = 25)
#show_col(seq_gradient_pal()(x))

traces.enriched %>%
  filter(
    trace.exec==1, 
    Operation=="mpi_worker_run_subset", 
    Iteration==3) %>%
  filter(Rank <= 25)-> temp 

temp %>%
  select(Group, everything()) %>%
  print() %>%
  ggplot()+
  geom_rect(
    aes(
      xmin=Start, xmax=End,
      ymin=Rank -.4, ymax=Rank +.4, 
      fill=RefGroup1
    ), color="black", size=.4, alpha=.5
  ) + 
  geom_text(data=temp, aes(x=Start + (End-Start)/2, y=Rank, label=RefGroup1), size=4) + 
  scale_fill_gradientn(colours = heat.colors(3)) +
  coord_cartesian(xlim=c(X.min, X.max)) + #, ylim=c(1,15)) + 
  facet_wrap(~"Refinement Groups") + 
  labs(y="Rank [id]", x= "Time [seconds]") +
  theme_bw(base_size=base.size+1) +
  scale_y_continuous(breaks=seq(1, 25, 2), labels=seq(1, 25, 2)) + 
  theme(
    legend.position="none",

#    axis.text.x = element_blank(),
#    axis.ticks.x = element_blank(), 
#    axis.title.x = element_blank(),

#    axis.text.y = element_blank(),
#    axis.ticks.y = element_blank(), 
#    axis.title.y = element_blank(),

    plot.margin = margin(0, 0, 0, 0, "cm"),

#    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) -> plot.groups

plot.groups

#plot.it + 
plot.phase + plot.groups + plot_layout(height=c(1, 8),ncol=1) -> p 

ggsave(
  dpi = 100,
  filename="figure_4.png",
  plot = p,
  #path = "~/Documents/2022/CMP223/artigo_final/evaluation/images",
  path = "../images",
  width = 12,
  height = 8
)
