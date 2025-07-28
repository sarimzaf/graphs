# --- 0. Setup ----------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, showtext, rvest, stringr, lubridate, glue, magick, 
               RCurl, memoise, ggimage, extrafont, janitor, magrittr, urltools, 
               scales, ggrepel, RColorBrewer, ggtext, here, forcats)

sysfonts::font_add_google("Libre Franklin", "franklin")
myfont <- "franklin"
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto()

x <- here()
file <- paste0(x, "/oecd_education/educational_attainment_parents.csv")

# --- 1. Data Setup -------------------------------------------------------

oecd_education <- read_csv(file, col_names = TRUE) %>% 
  clean_names() %>% 
  rename("parent_non_edu_non" = 3,
         "parent_non_edu_sec" = 4,
         "parent_non_edu_ter" = 5,
         "parent_sec_edu_non" = 6,
         "parent_sec_edu_sec" = 7,
         "parent_sec_edu_ter" = 8,
         "parent_ter_edu_non" = 9,
         "parent_ter_edu_sec" = 10,
         "parent_ter_edu_ter" = 11)

# --- 2. Cleaning  ----------------------------------------------------------
education <- oecd_education %>%
  mutate(country = gsub("<fc>", "u", country),
         country = gsub("Slovak Republic", "Slovakia", country)) %>% 
  rowwise() %>% 
  mutate(prop_higher = sum(parent_non_edu_sec, parent_non_edu_ter, parent_sec_edu_ter, 
                           na.rm=TRUE)/300,
         prop_same   = sum(parent_non_edu_non, parent_sec_edu_sec, parent_ter_edu_ter,
                           na.rm = TRUE)/300,
         prop_less   = sum(parent_sec_edu_non, parent_ter_edu_non, parent_ter_edu_sec,
                           na.rm = TRUE)/300) %>% 
  arrange(prop_higher) %>% 
  mutate(country = factor(country, levels = country))
  # mutate(country = fct_reorder(country, prop_higher, .desc = TRUE))

education_long <- education %>% 
  select(country, country_type, prop_higher, prop_same, prop_less) %>%
  pivot_longer(cols = starts_with("prop_"),
               names_to = "change", 
               values_to = "percent"
               ) %>% 
  mutate(change = gsub("prop_", "", change),
         change = factor(change, levels = c("less", "same", "higher"))
         ) %>% 
  mutate(percent = round(percent*100))
  

# --- 3. Plot  -----------------------------------------------------------------

### --- 3.1: Plot Setup ----------------------------

colors <- c(
  "higher" =  "#5E4C5F",
  "same" = "#B8BDD0",
  "less" = "#FFC96F"
)

text_colors <- c(
  "higher" =  "white",
  "same" = "black",
  "less" = "black"
)


num_rows <- length(unique(education_long$country))

top_country <- education_long %>% 
  filter(change == "higher") %>%
  filter(percent == max(percent)) 
top_country <-  as.vector(top_country$country)

annot_x2 <- education_long %>% 
  filter(country == top_country & change == "higher") 
annot_x2 <- as.vector(annot_x2$percent)

annot_x3 <- education_long %>% 
  filter(country == top_country & change == "same") 
annot_x3 <- as.vector(annot_x3$percent)

### --- 3.2: Final plot ----------------------------

ggplot(education_long,
       aes(x = percent, y = country, 
           fill = change)) +
  geom_col(width = 0.85) +
  geom_text(aes(label = percent, color = change),
            size = 3.8,
            position = position_stack(vjust = 0.13),
            family = myfont, 
            show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = text_colors) +
  # lines for top category
  annotate(geom = 'segment',
           x = c(0+4, annot_x2+4, annot_x2+annot_x3+2.5),
           xend = c(0+4, annot_x2+4, annot_x2+annot_x3+2.5),
           y = c(num_rows+0.4, num_rows+0.4, num_rows+0.4),
           yend = c(num_rows+1.4, num_rows+1.4, num_rows+1.4),
           color = "gray60"
  ) +
  # # text for lines for top category
  annotate(geom = 'text',
           x = c(0+4, annot_x2+4, annot_x2+annot_x3+1.3),
           y = c(num_rows+2.5, num_rows+2.5, num_rows+2.5),
           label = c("**MORE**\n(upwardly\nmobile)", "**SAME**\n(status\nquo)",
                     "**LESS**\n(downwardly\nmobile)"),
           size = 3.7,
           family = myfont
  ) +
  # coord
  coord_cartesian(ylim = c(0, num_rows+4),
                  xlim = c(-0.7,100),
                  expand = FALSE) +
  labs(
    x = NULL,
    title = "DOING BETTER THAN PARENTS?",
    subtitle = "Percentage of people who got more, the same or less education than their parents, as of 2021.",
    caption = "By Sarim Zafar | Source: Organization for Economic Cooperation and Development: Education at a Glance, 2024 | \n*Some totals do not add up to 100 because of rounding."
  ) +
  theme_minimal(base_size = 12, base_family = myfont) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    # grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend
    legend.position = "none",
    # axis
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 14, family = myfont,
                               hjust = 0),
    # titles
    plot.title = element_text(size = 15,
                              family = myfont, face = "bold",
                              hjust = 0,
                              margin = margin(t=0, r=0, b=3, l=-100)),
    plot.subtitle = element_text(size = 15,
                                 family = myfont,
                                 hjust = 0,
                                 margin = margin(t=1, r=0, b=3, l=-100)),
    plot.caption = element_text(size = 11,
                                family = myfont,
                                hjust = 0,
                                margin = margin(t=2, r=0, b=0, l=-100))
  )

ggsave(paste0(x, "/oecd_education/education_parents_hbar.png"), 
       width = 10, height = 10,
       bg = "white") 

