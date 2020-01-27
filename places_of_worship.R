#'
#' This script contains code to download data from OpenStreetMap and create map of places of worship in Europe
#'

suppressPackageStartupMessages({
  require(dplyr)
  require(osmdata)
  require(ggplot2)
})

#'
#' to download data from osm
#'

osm_download <- function(type, start = -10, end = 50) {
  b <- combn(start:end, 2) %>% t() 
  b <- b[!duplicated(b[,1]),]
  cw <- opq(bbox = c(b[1,1], 35, b[1,2], 70), 
            timeout = 125, memsize = 100000000) %>%
    add_osm_feature(key = 'building', value = 'mosque') %>%
    osmdata_sp()
  
  xdf <- cw$osm_points@coords %>% as_tibble()
  for(i in 2:nrow(b)) {
    cw <- opq(bbox = c(b[i,1], 35, b[i,2], 70),
              timeout = 125*5, memsize = 100000000) %>%
      add_osm_feature(key = 'building', value = type) %>%
      osmdata_sp()
    xdf <- rbind(xdf, as_tibble(cw$osm_points@coords))
    cat(paste0(round(i/nrow(b),2)*100,"% \n"))
  }
  xdf %>% mutate(type)
} 

places_of_worship <- c('mosque', 'synagogue', 'cathedral', 'church', 'chapel', 'temple', 'shrine')

have_file <- file.exists("data/worship.fst")

if(!have_file) {
  
  worship <- purrr::map_dfr(places_of_worship, osm_download)
  
  fst::write_fst(worship, "data/worship.fst")
  
} else {
  
  worship <- fst::read_fst("data/worship.fst")
  
}

p <- worship %>% 
  ggplot(aes(lon, lat, color = type)) +
  borders("world", xlim = c(-10, 50),  ylim = c(25, 70), size= 0.01, 
          fill = "black", colour = "white") +
  geom_point(size = 0.15, stroke = 0.05) +
  xlab("") +
  ylab("") +
  labs(
    title = "Places of worship in Europe",
    caption = glue::glue("Data: OSM")
  ) +
  scale_x_continuous(expand = c(0,0,0,0)) +
  scale_y_continuous(expand = c(0,0,0,0)) +
  scale_color_brewer(type = "qual", palette = "Set3", guide = guide_legend(
    title = "Type: ", override.aes = list(size=10), ncol = 7
  )) +
  coord_map("gilbert", 
            xlim = c(-10, 45), ylim = c(35, 71)) +
  hrbrthemes::theme_ipsum(base_family = "Lato") +
  theme(
    plot.background = element_rect(fill = "#252525", colour = "#252525"),
    legend.position = "bottom",
    plot.title = element_text(size = 55, face = "bold", colour = "white", vjust = -1, family = "Lato"),
    plot.caption = element_text(colour = "white", size = 20),
    legend.text = element_text(colour = "white", family = "Lato", size = 30),
    legend.title = element_text(colour = "white", family = "Lato", size = 30),
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

ggsave("images/places_of_worship.png", p, width = 20, height = 19.7, dpi = 1000)

