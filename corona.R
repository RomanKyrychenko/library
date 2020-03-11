corona <- readr::read_csv("https://raw.githubusercontent.com/beoutbreakprepared/nCoV2019/master/ncov_outside_hubei.csv")

corona_travel <- tmaptools::geocode_OSM(stringr::str_remove_all(unique(corona$travel_history_location), "#"), as.data.frame = T)

tr <- corona %>% 
  select(date_onset_symptoms, longitude, latitude, travel_history_location) %>% 
  left_join(corona_travel, by = c("travel_history_location" = "query")) %>% 
  filter(!is.na(lat) & !is.na(travel_history_location))

world_df = corona %>% 
  filter(!is.na(date_confirmation)) %>% 
  mutate(date_confirmation = lubridate::dmy(date_confirmation)) %>% 
  filter(!is.na(travel_history_location) & !is.na(country)  & !(travel_history_location %in% c("none", "no"))) %>% 
  group_by(country, travel_history_location, date_confirmation) %>% count() %>% 
  ungroup() %>% 
  arrange(date_confirmation) %>% 
  group_by(country) %>% slice(1) %>% ungroup() %>% 
  mutate(travel_history_location = case_when(
    travel_history_location == "Qom, Iran" ~ "Iran",
    travel_history_location == "Milan" ~ "Italy",
    travel_history_location == "Wuhan" ~ "China",
    travel_history_location == "Wuchang" ~ "China",
    travel_history_location == "from Wuhan , traveled to Johor from Singapore" ~ "China",
    travel_history_location == "Madrid, Spain" ~ "Spain",
    travel_history_location == "Riga" ~ "Latvia",
    travel_history_location == "Diamond Princess, Japan" ~ "Japan",
    travel_history_location == "Iran and transit through Bahrain" ~ "Iran",
    travel_history_location == "Iran; returned to Iran" ~ "Iran",
    travel_history_location == "Mashhad, Iran" ~ "Iran",
    travel_history_location == "French citizen traveled to Senegal" ~ "France",
    travel_history_location == "Bavaria, Germany" ~ "Germany",
    travel_history_location == "Hubei" ~ "China",
    travel_history_location == "Milan, Italy" ~ "Italy",
    travel_history_location == "Andalo, Northern Italy" ~ "Italy",
    travel_history_location == "Bergamo" ~ "Italy",
    travel_history_location == "Lombardy, Italy" ~ "Italy",
    travel_history_location == "Northern Italy via Spain" ~ "Italy",
    travel_history_location == "Verona, Italy" ~ "Italy",
    travel_history_location == "Italy via Romania" ~ "Italy",
    T ~ travel_history_location
  )) %>% 
  select(-n) %>% 
  bind_rows(
    tribble(
      ~country, ~travel_history_location, ~date_confirmation,
      "Mongolia", "France", NA,
      "Peru", "France", NA,
      "Monaco", "France", NA,
      "Senegal", "France", NA,
      "Cameroon", "France", NA,
      "Togo", "France", NA,
      "Burkina Faso", "France", NA,
      "USA", "China", NA,
      "Poland", "Germany", NA,
      "UK","China",NA,
      "Iran","China",NA,
      "Egypt","China",NA,
      "South Africa","Italy",NA,
      "Latvia","Italy",NA,
      "Albania","Italy",NA,
      "Moldova","Italy",NA,
      "Slovakia","Italy",NA,
      "Denmark","Italy",NA,
      "Malta","Italy",NA,
      "Cyprus","Italy",NA,
      "Colombia","Italy",NA,
      "Austria","Italy",NA,
      "Slovenia","Italy",NA,
      "Bosnia and Herzegovina","Italy",NA,
      "Macedonia","Italy",NA,
      "Belarus","Iran",NA,
      "Hungary","Iran",NA,
      "Armenia","Iran",NA,
      "Azerbaijan","Iran",NA,
      "Indonesia","Japan",NA,
      "Panama","Spain",NA,
      "Paraguay","Spain",NA,
      "Costa Rica","USA",NA,
    )
  ) %>% 
  right_join(world, by = c("country" = "region")) %>% 
  mutate(travel_history_location = ifelse(is.na(travel_history_location), "no information", travel_history_location),
         travel_history_location = factor(travel_history_location, levels = c(
           "China", "Italy", "Iran", "France", "Spain", "Germany",
           "Japan", "Singapore", "USA", "Switzerland", "Latvia",
           "no information"
         )))

ggplot(corona %>% mutate(date = lubridate::dmy(date_onset_symptoms)) %>% 
         arrange(date) %>% 
         group_by(longitude, latitude) %>% count() %>% 
         filter(!is.na(date)), aes(longitude, latitude)) +
  geom_polygon(data = world_df, aes(long, lat, group = group, fill = travel_history_location), color = "black", size = 0.1) +
  geom_segment(data = tr, aes(xend = longitude, yend = latitude, x = lon, y = lat), 
             size = 0.03,arrow = arrow(length = unit(0.05, "inches")),
             curvature = -0.2, color = "#ffeda0") +
  geom_point(alpha = 0.5,aes(size = n), stride = 0) + 
  xlab("") +
  ylab("") +
  labs(title = "Coronavirus: traveling the world",
       subtitle = "From which country the first case of covid 19 was exported?") + 
  scale_fill_manual(values = c(
    "#fb8072",
    "#b3de69",
    "#80b1d3",
    "#bebada",
    "#ffffb3",
    "#8dd3c7",
    "#fdb462",
    "#fccde5",
    "#bc80bd",
    "#ccebc5",
    "#ffed6f",
    "#d9d9d9"
  ), guide = guide_legend(title = "transmitter country", title.position = "top")) +
  scale_size_continuous(range = c(0.1, 6), guide = guide_legend(title = "cases", title.position = "top", nrow = 3)) +
  #scale_color_date(low = "#ffffcc", high = "#800026", guide = guide_legend(title = "Рік")) +
  #scale_fill_date(low = "#ffffcc", high = "#800026", guide = guide_legend(title = "Рік")) +
  coord_map(projection = "gilbert", ylim = c(85, -50), xlim = c(180, -180)) +
  hrbrthemes::theme_ipsum(base_family = "Lato") +
  theme(
    plot.background = element_rect(fill = "#023858", colour="#023858"), 
    panel.background = element_rect(fill = "#023858", colour="#023858"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 35, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5, color = "white"),
    legend.text = element_text(size = 12, color = "white"),
    legend.title = element_text(size = 12, face = "bold", color = "white"),
    legend.position = "bottom"
  )

ggsave("images/corona.png", width = 12, height = 12, dpi = 500)
