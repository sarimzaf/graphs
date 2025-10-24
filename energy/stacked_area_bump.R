library(tidyr)

df <- data.frame(
  year = rep(2015:2020, each = 3),
  country = rep(c("A", "B", "C"), times = 6),
  value = c(10, 30, 60, 20, 25, 55, 15, 40, 45, 30, 50, 20, 25, 35, 40, 35, 30, 35)
)

# Interpolate between years for smooth curves
smooth_df <- df %>%
  group_by(country) %>%
  complete(year = seq(min(year), max(year), by = 0.1)) %>%   # densify
  arrange(year) %>%
  mutate(
    value = approx(df$year[df$country == country],
                   df$value[df$country == country],
                   xout = year)$y
  ) %>%
  group_by(year) %>%
  arrange(desc(value)) %>%
  mutate(
    prop = value / sum(value),
    ymin = cumsum(prop) - prop,
    ymax = cumsum(prop),
    mid  = (ymin + ymax) / 2
  ) %>%
  ungroup()

# Now plot
p <- ggplot(smooth_df, aes(x = year)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = country), alpha = 0.8) +
  geom_line(aes(y = mid, color = country), size = 1.2) +
  # scale_y_continuous() +
  theme_minimal(base_size = 12) +
  scale_y_reverse(labels = scales::percent) 


animation <- p +
  transition_time(year)

anim1 <- animate(animation, 
                 fps = 10, 
                 # res = 300,
                 width = 900, height = 600,
                 device = "ragg_png")

anim_save(animation = anim1,
          "C:/Users/SZafar/Documents/Github/graphs/energy/animated_stacked_bump.gif")

animate()















library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)

# Example data
df <- data.frame(
  year = rep(2015:2020, each = 3),
  country = rep(c("A", "B", "C"), times = 6),
  value = c(10, 30, 60,
            20, 25, 55,
            15, 40, 45,
            30, 50, 20,
            25, 35, 40,
            35, 30, 35)
)

# Dense year sequence
years_dense <- seq(min(df$year), max(df$year), by = 0.1)

# Interpolate per country
interp_df <- df %>%
  group_by(country) %>%
  do({
    approx_df <- approx(x = .$year, y = .$value, xout = years_dense, ties = "ordered")
    data.frame(year = approx_df$x, value = approx_df$y, country = unique(.$country))
  }) %>%
  ungroup()

# Compute stacked areas at each dense year
smooth_df <- interp_df %>%
  group_by(year) %>%
  arrange(desc(value), .by_group = TRUE) %>%
  mutate(
    total = sum(value, na.rm = TRUE),
    prop  = value / total,
    ymin  = cumsum(prop) - prop,
    ymax  = cumsum(prop),
    mid   = (ymin + ymax) / 2
  ) %>%
  ungroup()

# Animated plot
p <- ggplot(smooth_df, aes(x = year)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = country), alpha = 0.8) +
  geom_line(aes(y = mid, color = country, group = country), size = 1.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14) +
  labs(title = "Stacked Bump Chart | Year: {round(frame_along,1)}") 

anim <- p +
  transition_reveal(year, transition_length = 2)   # <-- key change

# Render
animate(anim, nframes = 100, fps = 10, width = 800, height = 500)





