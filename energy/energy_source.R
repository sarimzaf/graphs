# --- 0. Setup ----------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, showtext, stringr, lubridate, magick, ggimage, 
               extrafont, janitor, magrittr, scales, ggrepel, ggpattern, 
               RColorBrewer, usmap, maps, patchwork, ggh4x)

sysfonts::font_add_google("Libre Franklin", "franklin")
myfont <- "franklin"
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto()

# --- 1. Data Setup -------------------------------------------------------

file <- "C:/Users/SZafar/Documents/Github/graphs/energy/generation_monthly.xlsx"

data_2025 <- read_excel(file, sheet = "2025_Preliminary", skip = 3) %>% 
  clean_names() %>% 
  group_by(year, month, state, energy_source) %>% 
  summarise(supply = sum(generation_megawatthours),
            .groups = "drop")

data_2024 <- read_excel(file, sheet = "2024_Preliminary", skip = 3) %>% 
  clean_names() %>% 
  group_by(year, month, state, energy_source) %>% 
  summarise(supply = sum(generation_megawatthours),
            .groups = "drop")


final_years <- seq(from = 2012, to = 2023, by = 1)

for (year in final_years) {
  print(year)
  assign(paste0("data_", year), 
         read_excel(file, sheet = paste0(year, "_Final"), skip = 3) %>%
           clean_names() %>% 
           group_by(year, month, state, energy_source) %>% 
           summarise(supply = sum(generation_megawatthours),
                     .groups = "drop")
  )
}

older_years <- c(2010, 2008, 2005, 2003, 2001)

for (year in older_years) {
  print(year)
  if (year == 2005) {
    sheetname = paste0(year, "-", year+2, "_FINAL")
  }
  else if (year < 2005) {
    sheetname = paste0(year, "_", year+1, "_FINAL")
  }
  else {
    sheetname = paste0(year, "-", year+1, "_FINAL")
  }
  print(sheetname)
  assign(paste0("data_", year), 
         read_excel(file, sheet = sheetname) %>%
           clean_names() %>% 
           group_by(year, month, state, energy_source) %>% 
           summarise(supply = sum(generation_megawatthours),
                     .groups = "drop")
  )
}


object_names <- ls()
data_files <- object_names[grepl("data_20",
                                   object_names)]
data_dfs <- mget(data_files, envir = .GlobalEnv)
# Bind
final <- bind_rows(data_dfs) %>% 
  mutate(energy_source = case_when(
    grepl("Hydroelectric", energy_source, ignore.case = TRUE) ~ "Hydroelectric",
    TRUE ~ energy_source
  ))


# --- 2. Map data  -------------------------------------------------------
states_sf <- us_map(regions = "states")

us <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# --- 2. Cleaning  -------------------------------------------------------
most_used_map <- final %>%
  filter(energy_source != "Total" & str_length(state) < 3) %>% 
  group_by(year, state) %>% 
  filter(supply == max(supply)) %>%
  filter(year == 2001 | year == 2017) %>% 
  left_join(states_sf, by = c("state" = "abbr")) %>% 
  mutate(geometry = geom) 



# --- 3. Plot Map -------------------------------------------------------

### 3.1: Set parameters  ------------------------
colors <- c("Coal" = "#615C46",
            "Natural Gas" = "#E39424", 
            "Nuclear" = "#9E71C2", 
            "Hydroelectric" = "#81E3E9",
            "Petroleum" = "#D366A8",
            "Other Biomass" = "#E2E2E2",
            "Wind" = "#719BC2",
            "Solar" = "#F7D930"
)

### 3.2: Plot  ----------------------------------

##### Till 2017
p1 <- plot_usmap(
  data = most_used_map %>% 
    filter(year == 2001 | year == 2017),
  values = "energy_source",
  region = "states",
  color = "white"
) +
  scale_fill_manual(
    values = colors
  ) +
  labs(
    title = "Top Source of Electricity Generation In Every State"
  ) +
  facet_wrap(vars(year), ncol = 2) +
  theme_minimal(base_size = 12, base_family = myfont) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    # title
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold",
                              margin = margin(t = 1, r = 0, l = 0, b = -2)), 
    # facet
    strip.text = element_text(size = 12, hjust = 0.5, face = "bold",
                              lineheight = 1, margin = margin(t = 2, r = 0, l = 0, b = -1)),
    strip.background = element_blank(),
    # legend
    legend.position = "top",
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    legend.key.size = unit(0.6,"line")
  )

ggsave("C:\\Users\\SZafar\\Documents\\Github\\graphs\\energy\\map_2001_2017.png", 
       plot = p1, width = 10, height = 6, dpi = 400, bg = "white")

##### Till 2025
p2 <- plot_usmap(
  data = most_used_0125_map,
  values = "energy_source",
  region = "states",
  color = "white"
) +
  scale_fill_manual(
    values = colors
  ) +
  labs(
    title = "Top Source of Electricity Generation In Every State"
  ) +
  facet_wrap(vars(year), ncol = 2) +
  theme_minimal(base_size = 12, base_family = myfont) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    # title
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold",
                              margin = margin(t = 1, r = 0, l = 0, b = -2)), 
    # facet
    strip.text = element_text(size = 12, hjust = 0.5, face = "bold",
                              lineheight = 1, margin = margin(t = 2, r = 0, l = 0, b = -1)),
    strip.background = element_blank(),
    # legend
    legend.position = "top",
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    legend.key.size = unit(0.6,"line")
  )

ggsave("C:\\Users\\SZafar\\Documents\\Github\\graphs\\energy\\map_2001_2025.png", 
       plot = p2, width = 10, height = 6, dpi = 400, bg = "white")



##### Till 2025 (incl 2017)
p3 <- plot_usmap(
  data = most_used_011725_map,
  values = "energy_source",
  region = "states",
  color = "white"
) +
  scale_fill_manual(
    values = colors
  ) +
  labs(
    title = "Top Source of Electricity Generation In Every State"
  ) +
  facet_wrap(vars(year), ncol = 3) +
  theme_minimal(base_size = 12, base_family = myfont) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    panel.spacing = unit(-1, "cm", data=NULL),
    # title
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold",
                              margin = margin(t = 1, r = 0, l = 0, b = -2)), 
    # facet
    strip.text = element_text(size = 12, hjust = 0.5, face = "bold",
                              lineheight = 1, margin = margin(t = 2, r = 0, l = 0, b = -1)),
    strip.background = element_blank(),
    # legend
    legend.position = "top",
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    legend.key.size = unit(0.6,"line")
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave("C:\\Users\\SZafar\\Documents\\Github\\graphs\\energy\\map_2001_2017_2025.png", 
       plot = p3, width = 9, height = 5, dpi = 400, bg = "white")


# --- 3. Plot Stacked Area Ordered -------------------------------------------------------

st = "AK"
# Step 2: Normalize to percentages
df_stacked <- final %>%
  filter(state == st & energy_source != "Total") %>%
  group_by(year, month) %>%
  mutate(total = sum(supply),
         percent = supply/total,
         date = my(paste0(month, "-", year))
         ) 


ggplot(df_stacked, aes(x = date, y = percent, fill = energy_source)) +
  geom_area(color = "white", size = 0.2) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_fill_manual(values = colors)   
labs(
    title = "Dynamic Ordered 100% Stacked Area Chart",
    x = "Year", y = "Share", fill = "Category"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())




p <- ggplot(df_stacked, aes(x = year, y = percent, fill= fct_reorder(category, percent, .desc = TRUE)))
p + geom_ribbon(aes(ymin =0 , ymax= 1) 


ggplot(df_stacked, aes(x = year, y = percent, fill = category)) +
  geom_area(color = "white", size = 0.2, stat = "smooth", method = "loess") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "100% Stacked Area Chart (Ordered Top-Down by Rank at Each Time)",
    x = "Year", y = "Share", fill = "Category"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())





