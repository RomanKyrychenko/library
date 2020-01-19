require(rvest)
require(ggplot2)
require(ggtext)
require(cowplot)

p <- read_html("https://en.wikipedia.org/w/index.php?title=Special:Search&limit=1000&offset=0&ns0=1&search=intitle%3A%22World+Tour%22+concert&advancedSearch-current={}")

wt <- p %>% 
  html_nodes(".mw-search-result-heading") %>% 
  purrr::map(html_node, "a") %>% 
  purrr::map_chr(html_attr, "href")

wt <- paste0("https://en.wikipedia.org", wt)

wt <- wt[stringr::str_detect(wt, "World_Tour")]

get_tours <- function(x) {
  p <- read_html(x)
  e <- html_table(p, fill = TRUE)
  a <- html_text(html_node(p, ".attendee"))
  cat(a, "\n")
  as_tibble(e[purrr::map_lgl(purrr::map(e, colnames), ~any(stringr::str_detect(., "Date")))]
                  [[1]]
                  [, c("Date", "City", "Country")]) %>% 
    mutate(
      Date = lubridate::mdy(Date),
      Subject = a
    ) %>% 
    filter(!is.na(Date))
}

get_tours <- purrr::safely(get_tours)

result <- purrr::map(wt, get_tours)

result_df <- purrr::map_dfr(result, ~.$result)

city_sum <- result_df %>% 
  mutate(City = stringr::str_remove_all(stringr::str_remove_all(stringr::str_remove_all(City, "[:digit:]"), "\\["), "\\]")) %>% 
  group_by(City) %>% 
  count() %>% 
  arrange(desc(n)) 

country_sum <- result_df %>% 
  group_by(Country) %>% 
  count() %>% 
  arrange(desc(n)) 

city_locations <- tmaptools::geocode_OSM(stringr::str_remove_all(city_sum$City, "#"), as.data.frame = T)

top_plot <- ggplot(slice(ungroup(city_sum), 1:20), aes(reorder(City, n), n)) +
  geom_hline(yintercept = c(0, 100, 200, 300, 400, 500, 600), linetype = "dotted", color = "white", size = 0.1) +
  geom_col(fill = "#fff7bc") + coord_flip() +
  xlab("") +
  ylab("") +
  labs(title = 'Top-20 most visited cities') +
  scale_y_continuous(expand = c(0, 0, 0, 0)) +
  scale_x_discrete(expand = c(0, 0, 0, 0)) +
  hrbrthemes::theme_ipsum(base_family = "Lato", base_size = 6) +
  theme(
    plot.background = element_rect(fill = "#023858", colour = "#023858"), 
    panel.background = element_rect(fill = "#023858", colour="#023858"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(colour = "white"),
    legend.text = element_text(colour = "white"),
    plot.caption = element_text(colour = "white"),
    axis.text = element_text(colour = "white"),
    plot.title = element_text(size = 7, face = "bold", colour = "white", vjust = -1)
  )

world <- map_data("world")

po <- city_sum %>% 
  inner_join(
    city_locations, by = c("City" = "query")
  ) %>% 
  group_by(lon, lat) %>% 
  summarise(n = sum(n)) %>% 
  ggplot(aes(lon, lat, size = n)) +
  geom_polygon(data = world, aes(long, lat, group = group), color = "black", fill = "#3690c0", size = 0.1) +
  geom_point(alpha = 0.5, color = "#fff7bc") +
  scale_size(range = c(0.1, 5), guide = guide_legend(title = "Concert count:")) +
  ggalt::coord_proj("+proj=wintri", ylim = c(85, -50), xlim = c(180, -180)) +
  xlab("") +
  ylab("") +
  labs(title = 'What does "World tour" means?', 
      subtitle = '
Sometimes a music band or artist announces a "World tour." Where do they go in reality?
I’ve analyzed 355 world tours of more than 200 bands and artists described on Wikipedia 
and found out that they have rather specific apprehension of the world.',
       caption = "Data: Wikipedia") +
  hrbrthemes::theme_ipsum(base_family = "Lato") +
  theme(
    plot.background = element_rect(fill = "#023858", colour="#023858"), 
    panel.background = element_rect(fill = "#023858", colour="#023858"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(colour = "white"),
    legend.text = element_text(colour = "white"),
    plot.caption = element_text(colour = "white"),
    axis.text = element_blank(),
    plot.title = element_text(size = 35, face = "bold", colour = "white", vjust = -1),
    plot.subtitle = element_text(size = 18, vjust = -1, lineheight = 1.1, colour = "white")
  ) 

ggdraw(po) + draw_grob(ggplotGrob(top_plot), x = 0, y = 0.04, 0.28, 0.41)

ggsave("images/world_tours.png", width = 11, height = 8.635, dpi = 500)
