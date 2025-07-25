if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, showtext, rvest, stringr, lubridate, glue, magick, 
               RCurl, memoise, ggimage, extrafont, janitor, magrittr, urltools, 
               scales, ggrepel, ggpattern, RColorBrewer, ggtext, ggarrow)

sysfonts::font_add_google("Libre Franklin", "franklin")
myfont <- "franklin"
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto()

cd <- getwd()
x <- str_extract(cd, paste0(".*(?=", "Github", ")"))
newdir <- paste0(x, "Github/graphs/monopolization")
setwd(newdir)

# --- 1. Load Data  ------------------------------------------------------------

markets <- tribble(
  ~industry,        ~share, ~year,    ~period,
  "Hardware stores",    42,  2000,  "Early 2000s",
  "Hardware stores",    80,  2020,  "Now",
  "Shipbuilding",       22,  2000,  "Early 2000s",
  "Shipbuilding",       62,  2020,  "Now",
  "Private prisons",    19,  2000,  "Early 2000s",
  "Private prisons",    58,  2020,  "Now",
  "Tobacco",       51.5,  2000,  "Early 2000s",
  "Tobacco",       82,  2020,  "Now",
  "Pharmacies",       30,  2000,  "Early 2000s",
  "Pharmacies",       60,  2020,  "Now",
  "Airlines",       22,  2000,  "Early 2000s",
  "Airlines",       41,  2020,  "Now",  
  "Car rental",       36,  2000,  "Early 2000s",
  "Car rental",       51,  2020,  "Now",   
  "Industrial laundry",  37,  2000,  "Early 2000s",
  "Industrial laundry",  49,  2020,  "Now",
  "Investment banking",  35,  2000,  "Early 2000s",
  "Investment banking",  23,  2020,  "Now",   
  "Tire mfg.",       69,  2000,  "Early 2000s",
  "Tire mfg.",       45,  2020,  "Now",     
)

# --- 2. Cleaning  -------------------------------------------------------------
final <- markets %>% 
  pivot_wider(id_cols = industry, names_from = year, values_from = c(share, period)) %>% 
  mutate(change = (share_2020 - share_2000),
         movement = ifelse(change > 0, "positive", "negative"),
         industry = fct_reorder(industry, change))

# --- 3. Plot setup  -----------------------------------------------------------
colors <- c("negative" = "grey25",
            "positive" = "#bd1d08"
)

num_rows <- length(unique(final$industry))

annot_x_start <- final %>% 
  filter(change == max(change)) %>% 
  arrange(industry) %>% 
  slice(1) %>% pull(share_2000)

annot_x_end <- final %>% 
  filter(change == max(change)) %>% 
  arrange(industry) %>% 
  slice(1) %>% pull(share_2020)

# --- 4. Final Plot  -----------------------------------------------------------

ggplot(data = final, 
       aes(x = share_2000, xend = share_2020, y = industry)) +
  # median line
  geom_vline(xintercept = 50, 
             linewidth = 1, color = "grey40") +
  # arrow
  geom_arrow_segment(aes(color = movement),
                     linewidth = 2.6,
                     length_head = 1.9,
                     length_fins = 0,
                     show.legend = FALSE) +
  # lines for top category
  annotate(geom = 'segment',
           x = c(annot_x_start, annot_x_end),
           xend = c(annot_x_start, annot_x_end),
           y = c(num_rows+0.3, num_rows+0.3),
           yend = c(num_rows+0.7, num_rows+0.7)
           ) +

  # text for lines for top category
  annotate(geom = 'text',
           x = c(annot_x_start, annot_x_end),
           y = c(num_rows+1, num_rows+1),
           label = c("Early 2000s", "Now"),
           size = 3.3
  ) +  
  # assign colors
  scale_color_manual(values = colors) +
  # x axis
  scale_x_continuous(limits = c(10,105),
                     breaks = seq(10, 100, by = 10),
                     position = "top") +
  # coord
  coord_cartesian(ylim = c(0, num_rows+1.7), expand = FALSE, clip = "off") +
  theme_minimal(base_size = 12, base_family = myfont) +
  labs(
    x = "MARKET\n SHARE:",
    title = "Dominance of Corporate Behemoths",
    subtitle = "The combined market share of the two largest companies in many industries\n has grown in recent years, often because of mergers."
  ) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_blank(),
    # grids
    panel.grid.major.y = element_line(lineend = 100,
                                      linetype = "dotted",
                                      color = "gray60"),
    panel.grid.minor = element_blank(),
    # titles
    axis.title.x = element_text(size = 10,
                                family = myfont,
                                hjust = -0.12, vjust = -0.6),
    plot.title = element_text(size = 16,
                              family = myfont, face = "bold",
                              hjust = -0.4,
                              margin = margin(t=0, r=0, b=2, l=0)),
    plot.subtitle = element_text(size = 13,
                              family = myfont,
                              hjust = -0.6,
                              margin = margin(t=2, r=0, b=4, l=-0.3))
  )

ggsave("monopolization_arrows.png", 
       width = 8, height = 6,
       bg = "white") 
