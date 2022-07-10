options(crayon.enabled=FALSE)
library(tidyverse) 
library(patchwork)

read_csv("../data/mean_trace.csv.gz", progress=FALSE) -> mean_trace

mean_trace %>% 
  pull(Duration.mean) %>% 
  sum() -> total.time

mean_trace %>% 
  group_by(Operation) %>%  
  summarize(
    instances = n(),
    Op.time = sum(Duration.mean), 
    Op.time.min = min(Duration.mean), 
    Op.time.mean = mean(Duration.mean),
    Op.time.max = max(Duration.mean)
) %>%
  mutate(relative_time=100*(Op.time/total.time)) %>%
  arrange(relative_time) -> table

table %>% 
  ungroup() %>%
  summarize(
    instances=sum(instances),
    relative_time=sum(relative_time)
  ) %>% 
  mutate(
    Operation = "Total", 
    Op.time = 0,
    Op.time.min = 0,
    Op.time.mean = 0,
    Op.time.max = 0
  ) -> table.total

bind_rows(table, table.total) %>%
  arrange(relative_time) %>% 
  mutate(relative_time = sprintf("%.2f%%", relative_time)) %>%
  mutate(instances=sprintf("%.1fK", instances/1000)) %>%
  mutate(Op.time.min=sprintf("%.2fs", Op.time.min)) %>%
  mutate(Op.time.mean=sprintf("%.2fs", Op.time.mean)) %>%
  mutate(Op.time.max=sprintf("%.2fs", Op.time.max)) %>%
  select(Operation, instances, relative_time, Op.time.min, Op.time.mean, Op.time.max) %>% 
  as.data.frame
