library(ggplot2)
library(tidyverse)
library(ggarrow)
library(lubridate)

timeline <- tribble(
  ~event, ~start, ~end,
  'event1', '2016-01-01',  '2018-01-01',
) %>% 
  mutate(start = ymd(start),
         end = ymd(end))

# rect height
rect_height <- 2

## event 1
event_1_start = ymd('2016-01-01')
event_1_end = ymd('2018-01-01')
event_1_label = 'event1'

## event 1
event_1_start = ymd('2016-01-01')
event_1_end = ymd('2018-01-01')
event_1_label = 'event1'

ggplot(uv4plants.tb, aes(x = when, y = 0, label = what)) +
  geom_line() +
  geom_rect(aes(xmin = event_1_start, xmax = event_1_end 
                ymin = 0, ymax = rect_height),
            color = "black", fill = "lightblue", alpha = 0.8) +
  annotate("text", 
           x = mean(c(as.Date('2016-01-01'), as.Date('2018-01-01'))), 
           y = mean(c(0, 2)), 
           label = "Chart title",
           color = "navy") +
  scale_y_continuous(limits = c(0,6)) +
  theme_minimal()










project_data <- tibble(
  Project_Name = c("Warehouse"),
  Proposed_Start = c("05-01-2022"),
  Proposed_Finish = c("12-01-2022"),
  Actual_Start = c("07-01-2022"),
  Actual_Finish = c("12-31-2022")
) %>% 
  pivot_longer(-1, names_sep = "_", names_to = c("Type", ".value"))


project_data %>% 
  ggplot(aes(xmin = mdy(Start), xmax = mdy(Finish), color = Type,
             y = 0)) +
  geom_linerange(size = 70, position = position_dodge(width = 4)) +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  scale_colour_manual(values = c("orange", "deepskyblue4")) +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )


  
 
