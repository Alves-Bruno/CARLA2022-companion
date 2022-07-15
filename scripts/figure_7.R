options(crayon.enabled=FALSE)
library(tidyverse) 
library(patchwork)

read_csv("../data/mean_trace.csv", progress=FALSE) -> mean_trace

n_pairs <- mean_trace %>% pull(RxTx.pairs) %>% max()
div.step=20
tibble(
  pairs.group=rep(1:ceiling(n_pairs/div.step), each=div.step, times=1)
) %>% 
  mutate(RxTx.pairs=row_number()) %>% 
  filter(RxTx.pairs<=n_pairs) -> pairs.groups

pairs.groups %>% 
  group_by(pairs.group) %>% 
  summarize(pairs.min=min(RxTx.pairs), pairs.max=max(RxTx.pairs)) %>% 
  mutate(pairs.name=paste(sprintf("%.2d", pairs.min), pairs.max, sep=" - ")) %>% 
  pull(pairs.name) -> pairs.legend

mean_trace %>% 
  left_join(pairs.groups, by = "RxTx.pairs") -> trace.pairs

trace.pairs %>%
  filter(Operation=="derivs_comp_adj", Phase=="Jacobian") %>% 
  ggplot(aes(x=NodesFrom/1000, y=Duration.mean, colour=as.factor(pairs.group))) + 
  geom_point(alpha=.01) + 
  geom_smooth(alpha=.1, method="lm") + 
  scale_colour_discrete(labels = pairs.legend) + 
  theme_bw(base_size=16) + 
  labs(x="Processed Nodes [K]", y="Mean Duration [s]", colour="Rx-Tx pairs") + 
#  ylim(c(0, 25)) + xlim(c(0, 175)) + 
#  theme(legend.position="node") + 
  facet_wrap(~Operation) -> derivs 

derivs

mean_trace %>% 
  filter(Operation %in% c("error_estimate", "lhs_gen", "primal_solve")) %>% 
  ggplot(aes(x=NodesFrom/1000, y=Duration.mean)) + 
  geom_point(alpha=.01) + 
  geom_smooth(method="lm") + 
  theme_bw(base_size=16) + 
  labs(x="Processed Nodes [K]", y="Mean Duration [s]") +
#  ylim(c(0, 25)) + xlim(c(0, 175)) + 
#  theme(legend.position="node") + 
  facet_wrap(~Operation, scale="free_y") -> others

others

p <- others + derivs + plot_layout(widths=c(3,1))

ggsave(
  dpi = 100,
  filename="figure_7.png",
  plot = p,
  path = "../images/",
  width = 12,
  height = 4
)
