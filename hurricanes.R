require(dplyr)
require(ggplot2)
library(gganimate)

hurdat <- readr::read_csv("https://raw.githubusercontent.com/ResidentMario/hurdat-example-repo/master/data/atlantic.csv") %>%
  filter(longitude > -180)

world <- map_data("world")

options(gganimate.dev_args = list(width = 6.61, height = 8, res = 500, units = "in"))

p <- ggplot() +
  geom_polygon(
    data = world, aes(long, lat, group = group),
    colour = "#525252", fill = "#525252", size = 0.005
  ) +
  geom_point(
    data = hurdat %>% group_by(id) %>%
      mutate(time = round(as.numeric(date - min(date)) / (3600 * 6)) * 6) %>%
      filter(maximum_sustained_wind_knots > 0),
    aes(longitude, latitude,
      group = id,
      alpha = maximum_sustained_wind_knots * 1.852,
      size = maximum_sustained_wind_knots * 1.852,
      color = maximum_sustained_wind_knots * 1.852
    ),
    stroke = 0, alpha = 0.3
  ) +
  scale_size_continuous(range = c(0.0001, 3), guide = guide_legend(title = "Wind speed km/h:")) +
  scale_color_gradient2(low = "#ffffd9", mid = "#c7e9b4", high = "#41b6c4", guide = guide_legend(title = "Wind speed km/h:")) +
  coord_map("ortho", orientation = c(35, -54, -10)) +
  hrbrthemes::theme_ipsum(base_family = "Lato") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(), legend.position = "bottom",
    plot.background = element_rect(fill = "#deebf7", colour = "#deebf7"),
    plot.title = element_text(colour = "black"),
    legend.text = element_text(colour = "black"),
    legend.title = element_text(colour = "black"),
    plot.caption = element_text(colour = "black")
  ) +
  labs(
    title = "Atlantic hurricanes' tracks\nDays from start: {round(frame_time%/%24)}", x = "", y = "",
    caption = "Data: Atlantic hurricane database (HURDAT2) 1851-2018"
  ) +
  transition_time(time = time) +
  shadow_wake(wake_length = 0.3)

g <- animate(p, nframes = 600, end_pause = 1, rewind = F, fps = 35)

anim_save("images/hurricanes_green.gif", animation = g)