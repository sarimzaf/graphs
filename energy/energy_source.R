# --- 0. Setup ----------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, showtext, stringr, lubridate, magick, ggimage, 
               extrafont, janitor, magrittr, scales, ggrepel, ggpattern, here,
               RColorBrewer, readxl, usmap, maps, patchwork, ggforce, gganimate,
               directlabels)

sysfonts::font_add_google("Libre Franklin", "franklin")
myfont <- "franklin"
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto()

cd <- getwd()
x <- substr(cd, 10, 15)

here <- here()
if (x == "SZafar") {
  file <- paste0("C:/Users/", x, "/Documents/Github/graphs/energy/generation_monthly.xlsx")
} else {
  file <- paste0("C:/Users/sarim/Documents/Github/graphs/energy/generation_monthly.xlsx")
}

# --- 1. Data Setup -------------------------------------------------------

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
  )) %>% 
  filter(str_length(state) < 3 & energy_source != "Total" & supply > 0)


# --- 2. Map data  -------------------------------------------------------
states_sf <- us_map(regions = "states")

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
            "Other Biomass" = "#6B8E23",
            "Wind" = "#719BC2",
            "Solar" = "#F7D930"
)


states_labels <- c(
  "AL" = "Alabama", "AK" = "Alaska",
  "AZ" = "Arizona", "AR" = "Arkansas",
  "CA" = "California", "CO" = "Colorado",
  "CT" = "Connecticut", "DC" = "Washington DC", "DE" = "Delaware",
  "FL" = "Florida", "GA" = "Georgia",
  "HI" = "Hawaii", "ID" = "Idaho",
  "IL" = "Illinois", "IN" = "Indiana",
  "IA" = "Iowa", "KS" = "Kansas",
  "KY" = "Kentucky", "LA" = "Louisiana",
  "ME" = "Maine", "MD" = "Maryland", 
  "MA" = "Massachusetts", "MI" = "Michigan",
  "MN" = "Minnesota", "MS" = "Mississippi",
  "MO" = "Missouri", "MT" = "Montana",
  "NE" = "Nebraska", "NV" = "Nevada",
  "NH" = "New Hampshire", "NJ" = "New Jersey",
  "NM" = "New Mexico", "NY" = "New York",
  "NC" = "North Carolina", "ND" = "North Dakota",
  "OH" = "Ohio", "OK" = "Oklahoma",
  "OR" = "Oregon", "PA" = "Pennsylvania",
  "RI" = "Rhode Island", "SC" = "South Carolina",
  "SD" = "South Dakota", "TN" = "Tennessee",
  "TX" = "Texas", "UT" = "Utah",
  "VT" = "Vermont", "VA" = "Virginia",
  "WA" = "Washington", "WV" = "West Virginia",
  "WI" = "Wisconsin", "WY" = "Wyoming"
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


# --- 4. Plot Stacked Area Ordered -------------------------------------------------------

nums <- seq(4, 52, by = 4)
count = 1
for (i in nums) {
  j = i-3
  x = paste0("states_", count)
  assign(x, pull(final %>% 
                   group_by(state) %>% summarise(count = n()) %>% slice(j:i) %>% select(state))
  )
  count = count+1
}

multi_state_map <- function(state_list) {
  df_stacked <- final %>%
    mutate(quarter = case_when(
      month > 0 & month <= 3 ~ 1,
      month > 3 & month <= 6 ~ 2,
      month > 6 & month <= 9 ~ 3,
      month > 9 & month <= 12 ~ 4,
      TRUE ~ NA
    )) %>% 
    filter(state %in% state_list) %>%
    mutate(energy_source = ifelse(energy_source %in% names(colors), 
                                  energy_source, "Other")) %>% 
    group_by(year, quarter, state, energy_source) %>%
    summarise(supply = sum(supply),
              month = month[1],
              .groups = "drop") %>% 
    group_by(year, quarter, state) %>%
    mutate(total = sum(supply),
           month = month[1]) %>% 
    rowwise() %>% 
    mutate(percent = supply/total,
           date = my(paste0(month, "-", year))
    ) 
  
  colors <- c(colors, "Other" = "#BEBEBE") 
  
  p <- ggplot(df_stacked, aes(x = date, y = percent, fill = energy_source)) +
    geom_area(color = "black", size = 0.2, na.rm=TRUE, 
              position = "fill") +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    scale_fill_manual(values = colors)  +  
    labs(
      title = NULL,
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 12, base_family = myfont) +
    theme(
      panel.grid = element_blank(),
      plot.background = element_blank(),
      legend.title = element_blank(),
      axis.title = element_blank(),
      legend.position = "top",
      plot.title = element_blank(),
      legend.text = element_text(size = 13, family = myfont),
      legend.key.size = unit(1, 'line'),
      axis.text = element_text(size = 10, family = myfont),
      # facet
      strip.text = element_text(size = 14, hjust = 0.5, face = "bold",
                                lineheight = 1, margin = margin(t = -1, r = 0, l = 0, b = 3))
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    coord_cartesian(expand = FALSE, clip = "off") +
    facet_wrap(~ state, ncol = 2, labeller = as_labeller(states_labels))
}


plot_list = list()

for (i in 1:13) {  ####
  # Construct the state list variable name dynamically
  state_list_name <- paste0("states_", i)
  current_states <- get(state_list_name)
  plot_list[[i]] <- multi_state_map(state_list = current_states) ### 
  
  if (i > 1) {
    plot_final <- plot_list[[i]] + theme(legend.position = "none")
  } else {
    plot_final <- plot_list[[i]]
  }
  
  filename = paste0("stacked_area_", i)
  ggsave(paste0("C:\\Users\\SZafar\\Documents\\Github\\graphs\\energy\\", filename, ".png"), 
         plot = plot_final, width = 10, height = 9, dpi = 400, bg = "white")
}


# --- 5. Ribbon -------------------------------------------------------

# Yearly:
df_ribbon <- final %>%
  mutate(date = my(paste0(month, "-", year)))  %>% 
  filter(state == 'AR') %>% 
  group_by(year, state, energy_source) %>%
  summarise(yr_supply = sum(supply),
            .groups = "drop") %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(share = yr_supply / sum(yr_supply)) %>% 
  arrange(year, yr_supply) %>%
  mutate(
    ymin = c(0, head(cumsum(share), -1)),
    ymax = cumsum(share)
  ) %>%
  ungroup() %>% 
  mutate(energy_source = ifelse(energy_source == "Wood and Wood Derived Fuels",
                                "Wood", energy_source))

max_shares <- df_ribbon %>% 
  group_by(energy_source) %>% 
  mutate(highest_share = max(share)) %>% 
  ungroup() %>% 
  filter(year == 2004) %>% 
  arrange(-share) %>%
  filter(share > 0.05) %>% 
  mutate(running_share = 1-cumsum(share)) %>% 
  mutate(y = running_share+(running_share/2)) %>% 
  mutate()

text_colors <- c("Coal" = "white",
                "Natural Gas" = "black", 
                "Nuclear" = "black", 
                "Hydroelectric" = "black",
                "Petroleum" = "black",
                "Other Biomass" = "black",
                "Wind" = "black",
                "Solar" = "black",
                "Other" = "black"
)

# Monthly:
# df_ribbon <- final %>%
#   mutate(date = my(paste0(month, "-", year)))  %>%
#   filter(state == 'AR') %>%
#   group_by(date) %>%
#   mutate(share = supply / sum(supply)) %>%
#   arrange(date, supply) %>%
#   mutate(
#     ymin = c(0, head(cumsum(share), -1)),
#     ymax = cumsum(share)
#   ) %>%
#   ungroup()

colors <- c(colors, "Other" = "#BEBEBE") 


p <- ggplot(df_ribbon, aes(x = year, y = share, group = energy_source)) +
  geom_ribbon(aes(ymin=ymin,
                  ymax=ymax,
                  fill = factor(energy_source)),
              color = "black",
              size = 0.25, alpha = 1) +
  geom_text(data = max_shares,
            aes(x = 2003.5, y = y, label = energy_source, color = energy_source),
            family = myfont, size = 3.4, fontface = "bold") +
  scale_color_manual(values = text_colors) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0, 1),
                     breaks = seq(0.2, 1, by = 0.2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(min(df_ribbon$year), max(df_ribbon$year)+0.45),
                     breaks = seq(min(df_ribbon$year), max(df_ribbon$year) , by = 4),
                     expand = c(0, 0)) +
  theme_minimal(base_size = 12, base_family = myfont) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    # axes
    axis.title = element_blank(),
    # legend
    legend.position = "none"
  )


ggsave(paste0(here, "/energy/output/ribbon_AR.png"), 
       plot = p, width = 10, height = 8, dpi = 200, bg = "white")

  
# --- 5. National: Line -------------------------------------------------------

df_line <- final %>%
  mutate(date = my(paste0(month, "-", year)),
         energy_source = ifelse(energy_source %in% names(colors), 
                                energy_source, "Other")) %>% 
  group_by(year, energy_source) %>%
  summarise(us_supply = sum(supply),
            .groups = "drop") %>%
  group_by(year) %>% 
  mutate(share = percent(us_supply / sum(us_supply))) %>% 
  ungroup()


p <- ggplot(df_line %>% filter(year < 2025), 
       aes(x = year, y = us_supply, color = energy_source)) +
  geom_line(size = 1.1) +
  geom_dl(aes(label = energy_source), method = list(dl.trans(x = x + .2), "last.points",
                                                    cex = 0.7)) +
  scale_color_manual(values = colors) +
  scale_x_continuous(limits = c(min(df_line$year), max(df_line$year)),
                     breaks = seq(min(df_line$year), max(df_line$year) , by = 2)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  # scale_x_date(limits = c(min(df_line$date), max(df_line$date)),
  #              date_breaks  = '2 years',
  #              labels = date_format("%Y")) +
  theme_void(base_size = 10, base_family = myfont) +
  theme(
    axis.text = element_text(size = 6,
                             family = myfont),
    legend.position = "none" 
  ) +
  coord_cartesian(xlim = c(min(df_line$year), max(df_line$year)+1.5),
                  expand = FALSE, clip = 'off')

ggsave(paste0(here, "/energy/line.png"), 
       plot = p, width = 10, height = 8, dpi = 200, bg = "white")


######## animated
p <- p +
  transition_reveal(year)

animate(p, 
        height = 1000, width = 1000,
        res = 200, nframes = 90, fps = 10)

filename <- paste0(here, "/energy/line_anim.gif")
anim_save(filename)

