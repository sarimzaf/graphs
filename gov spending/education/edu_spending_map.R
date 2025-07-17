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
  summarise_all(last)

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

legend_title <- "Share of GDP"

#### 2.2: Final plot  ----------------------------------------------------------

ggplot(edu_map) +
  geom_sf(aes(fill = value), color = "white") +
  scale_fill_gradientn(
    colours = red_to_purple,
    limits = c(tenth, ninetieth),
    na.value = "grey",
    guide = guide_colorbar(title = legend_title),
    breaks = seq(round(min(edu_spending$value), 0.1), round(max(edu_spending$value), 0.1), by = 1),  # optional for better ticks
    oob = scales::squish
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
    legend.key.width = unit(2,"cm"),
    # axes
    axis.text.x = element_blank()
  )



