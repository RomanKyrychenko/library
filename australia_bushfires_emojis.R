#'
#'
#'
#'

suppressPackageStartupMessages({
  library(ore)
  library(dplyr)
  library(rgeos)
  library(rworldmap)
  library(ggplot2)
  library(ggtext)
})

emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    read_html() %>%
    html_nodes("tr td a") %>%
    .[1] %>%
    html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    read_html() %>%
    html_node('div[class="vendor-image"] img') %>%
    html_attr("src")
}

link_to_img <- function(x, size = 25) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

emoji_src <- "https://raw.githubusercontent.com/laurenancona/twimoji/gh-pages/twitterEmojiProject/emoticon_conversion_noGraphic.csv"
emoji_fil <- basename(emoji_src)
if (!file.exists(emoji_fil)) download.file(emoji_src, emoji_fil)

emoji <- readr::read_csv(emoji_fil, col_names = FALSE)
emoji_regex <- sprintf("(%s)", paste0(emoji$X2, collapse = "|"))
compiled <- ore(emoji_regex)

aus_det <- readr::read_csv("aus_det.csv")

where <- which(grepl(emoji_regex, aus_det$text, useBytes = TRUE))

chat_emoji_lines <- aus_det$text[where]

found_emoji <- ore.search(compiled, chat_emoji_lines, all=TRUE)
emoji_matches <- ore::matches(found_emoji)

aus_emo <- aus_det[where, ] %>% 
  mutate(
    emoji = emoji_matches
  ) %>% 
  tidyr::unnest(emoji)

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
df <- as.data.frame(centroids, row.names = F)

top_emo <- aus_emo %>% 
  group_by(country, emoji) %>% 
  dplyr::count() %>% 
  group_by(country) %>% 
  top_n(1, wt = n) %>% 
  dplyr::arrange(desc(n)) %>% 
  left_join(as_tibble(df, rownames = "country")) %>%
  mutate(url = purrr::map_chr(emoji, purrr::slowly(~emoji_to_link(.x), purrr::rate_delay(1))),
         label = link_to_img(url)) %>% 
  group_by(country) %>% slice(1)

world <- map_data("world")

p <- ggplot() +
  geom_polygon(data = world, aes(long,lat,group=group), color = "black", fill = "lightgray", size = 0.1) +
  geom_richtext(
    data = top_emo %>% ungroup() %>% 
      mutate(label = stringr::str_replace_all(label, "'25'", paste0("'",round(log1p(top_emo$n)*3), "'"))), 
    aes(x, y, label = label), fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  ggalt::coord_proj("+proj=wintri", ylim = c(85, -50), xlim = c(180, -180)) +
  xlab("") +
  ylab("") +
  labs(
    title = "<img src='https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/apple/237/flag-for-australia_1f1e6-1f1fa.png' width='35'/> Australia bushfires in emojis",
    subtitle = "<br/>Emoji is basically like another language: it has its own rules, it can cover anything that comes to one's mind, <br/>
you can build whole sentences using only those tiny faces and other symbols. While people were praying <br/>
for Australia on Twitter they used plenty of emojis as well, but only few of them were the most common ones.<br/>

The map below shows the emojis used most frequently by country in tweets with hashtags <span style='color:blue'>#prayforaustralia</span>, <br/>
<span style='color:blue'>#australiaonfire</span>, <span style='color:blue'>#australiafires</span>, <span style='color:blue'>#australia</span>, <span style='color:blue'>#australianbushfire</span>, <span style='color:blue'>#australianfires</span>, <span style='color:blue'>#australiaburning</span>, <span style='color:blue'>#australiaburns</span>, <br/> <span style='color:blue'>#pray4australia</span>, <span style='color:blue'>#australiabushfires</span>, <span style='color:blue'>#prayforrain</span><br>", 
    caption = "Data: twitter.com, 181 624 tweets with marked location"
  ) +
  hrbrthemes::theme_ipsum(base_family = "Lato") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.title = element_markdown(size = 35, face = "bold", colour = "black", vjust = -1),
    plot.subtitle = element_markdown(size = 18, vjust= -1, lineheight = 1.1),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold", colour = "black", vjust = 0.5, hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

ggsave(filename = "bushfires_emo.png", plot = p, width = 14, height = 11, dpi = 500)

#top_emo %>% group_by(emoji) %>% dplyr::count() %>% arrange(desc(n)) %>% group_by(n) %>% 
#  dplyr::summarise(emoji = paste(emoji, collapse = "")) %>% arrange(desc(n))

#aus_emo %>% group_by(emoji) %>% dplyr::count() %>% arrange(desc(n)) %>% head(10) %>% pull(1) %>% cat()

