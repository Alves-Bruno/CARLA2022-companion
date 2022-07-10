options(crayon.enabled=FALSE)
library(tidyverse) 
library(patchwork)
library(viridis)

# Load data 
read_csv("../data/tx.csv", progress=FALSE, show_col_types = FALSE) %>%
  as_tibble() %>% 
  mutate(tx=row_number())  -> tx

read_csv("../data/rx.csv", progress=FALSE, show_col_types = FALSE) %>%
  as_tibble() %>% 
  mutate(rx=row_number()) -> rx

read_csv("../data/initial_mesh.csv", progress=FALSE, show_col_types = FALSE) %>%
  as_tibble() -> initial_mesh

rx %>% select(id=rx, pos.x=Inline, pos.y=Z) -> recv
tx %>% select(id=tx, pos.x=Inline, pos.y=Z) -> trans

shapes <- c(20, 17)
names(shapes) <- c("Tx", "Rx")

initial_mesh %>% 
    mutate(rho=log(rho)) %>%
    ggplot() +
    geom_polygon(aes(group = triangles, x = Y, y = -Z), size=.01, fill="white", color="black") +
    # Plot Tx, Rx
    geom_point(data=trans, aes(x=pos.x, y=-pos.y, shape="Tx"), color="blue") + 
    geom_point(data=recv, aes(x=pos.x, y=-pos.y, shape="Rx"), color="red") +  
    scale_shape_manual(values=shapes) +
    labs(
      title="A) Inline 04Tx013a area", 
      y="Height (m)", x="Length (m)"
      ) +
    coord_cartesian(ylim=c(-6e3, 1000), xlim=c(-10e3, 52e3)) + 
    theme_bw(base_size=20) +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.position="none"
    ) -> a

a + 
geom_rect(data=tibble(), aes(xmin=-1e3, xmax=42e3, ymin=-200, ymax=-2e3), alpha=.3, color="black", fill="gray") +
geom_text(data=tibble(), aes(x=39e3, y=500, label="Rx/Tx area"), size=5) -> p.a

initial_mesh %>% 
    ggplot() +
    geom_polygon(aes(group = triangles, x = Y, y = -Z), size=.01, fill="white", color="grey50") +
    # Plot Tx, Rx
    geom_point(data=trans, aes(x=pos.x, y=-pos.y, shape="Tx"), color="blue", size=3) + 
    geom_point(data=recv, aes(x=pos.x, y=-pos.y, shape="Rx"), color="red", size=8) + 
    geom_text(data=recv, aes(x=pos.x, y=-pos.y, label=id), size=4.5) + 
    geom_text(data=trans %>% filter(id %in% c(seq(0,206, 10))), aes(x=pos.x, y=-pos.y+60, label=id), size=5) + 
    guides(shape = guide_legend(override.aes = list(colour = "black"))) + 
    scale_shape_manual(values=shapes) +
    labs(title="B) Zoom into the Rx/Tx area", y="Height (m)", x="Length (m)") +
#    xlim(0 - 12e3, 42e3 + 11e3) + ylim(-6e3, 0) + 
    coord_cartesian(
      ylim=c(-1.6e3, -400),
      xlim=c(1.5e3, 39.5e3)
    ) +
    theme_bw(base_size=20) +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.position="none"
    ) -> p.b

p.a + p.b + plot_layout(heights = c(1, 4), ncol=1) -> p 

ggsave(
  dpi = 100,
  filename="figure_2.png",
  plot = p,
  #path = "~/Documents/2022/CMP223/artigo_final/evaluation/images",
  path = "../images",
  width = 15,
  height = 10
)
