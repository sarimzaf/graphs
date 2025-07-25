## 0. Setup  ------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, tidyverse, maps, showtext, glue)

setwd("C:/Users/sarim/Documents/Github/graphs/gov spending/education")

font_add_google("Roboto slab", family = "roboto-slab")
font_add_google("Montserrat", family = "montserrat")

showtext_auto()
showtext_opts(dpi = 300)


## 1. Data  --------------------------------------------------------------------

#### 1.1: Read  -----------------------------------------------------------------
raw <- read.csv("uis_data.csv", 
                header = TRUE)   # source: https://databrowser.uis.unesco.org/browser/EDUCATION/UIS-SDG4Monitoring

world <- st_read("world-administrative-boundaries/world-administrative-boundaries.shp")

#### 1.2: Prepare  -------------------------------------------------------------

edu_spending <- raw %>% 
  arrange(geoUnit, year) %>% 
  group_by(geoUnit) %>% 
  summarise_all(last) %>% 
  filter(value < 10 | is.na(value))

max <- ceiling(max(edu_spending$value) / 10)*10
  
edu_spending <- edu_spending %>% 
  mutate(group = case_when(
    value > 0*max & value <= 0.05*max ~ 1,
    value > 0.05*max & value <= 0.1*max ~ 2,
    value > 0.1*max & value <= 0.3*max ~ 3,
    value > 0.3*max & value <= 0.5*max ~ 4,
    value > 0.5*max & value <= 0.7*max ~ 5,
    value > 0.8*max & value <= max ~ 6,
  )) 

#### 1.3: Merge  ---------------------------------------------------------------
edu_map <- world %>%
  left_join(edu_spending, by = c("iso3" = "geoUnit"))

## 2. Map  ---------------------------------------------------------------------


#### 2.1: Parameters  ----------------------------------------------------------

tenth <- quantile(edu_spending$value, probs = seq(0.1, 0.9, by = 0.05))[1]
ninetieth <- quantile(edu_spending$value, probs = seq(0.1, 0.9, by = 0.05))[17]

red_to_green <- c("#FF0F0F", "#F04F05", "#D38909", "#C5BB0D", "#80A90F", "#458E10", 
                  "#1A7510", "#0D4F1B")

red_to_purple <- c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", 
                   "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")

blueish <- c("#F6EFF7", "#D0D1E6", "#A6BDDB", "#67A9CF", "#1C9099", "#016C59")

greens <- c("#DEEDCF", "#BFE1B0", "#99D492", "#74C67A", "#56B870", "#39A96B",
            "#1D9A6C", "#188977", "#137177", "#0E4D64", "#0A2F51")

legend_title <- "Share of GDP"

#### 2.2: Final plot  ----------------------------------------------------------

ggplot(edu_map) +
  geom_sf(aes(fill = group), color = "gray35") +
  scale_fill_gradientn(
    colours = blueish,
    limits = c(tenth, ninetieth),
    na.value = "grey",
    breaks = c(2,3,4,5,6),  # optional for better ticks
    # Optional: Customize the guide (the legend bar itself)
    guide = guide_legend(
      title = legend_title,
      title.hjust = 0.5,          # Horizontal justification of the title
      label.position = "bottom",   # Position of the labels (right, left, top, bottom)
      keyheight = unit(0.3, "cm"), # Width of the color bar
      keywidth = unit(1.5, "cm"),  # Height of the color bar
      # Use override.aes to specifically add an outline to the legend keys
      override.aes = list(color = "black", linewidth = 0.6)
    )
  ) +
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme_minimal(base_size = 12, base_family = "montserrat") +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major =  element_blank(),
    # legend
    legend.title = element_text(size = 8, hjust = 0.5),
    legend.position = "bottom",
    legend.title.position = "top",
    # legend.key.width = unit(2,"cm"),
    legend.spacing = unit(0.1, 'cm'),
    legend.text = element_text(size = 6),
    # axes
    axis.text.x = element_blank()
  )



