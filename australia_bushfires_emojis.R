#'
#' needed packages
#'

suppressPackageStartupMessages({
  library(ore)
  library(dplyr)
  library(ggplot2)
  library(ggtext)
  library(rvest)
})

#'
#' needed functions
#'

emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/", x) %>%
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

#'
#' emoji extractor
#'

emoji <- readr::read_csv(
  "https://raw.githubusercontent.com/laurenancona/twimoji/gh-pages/twitterEmojiProject/emoticon_conversion_noGraphic.csv",
  col_names = F
) %>% slice(-1)

emoji_regex <- sprintf("(%s)", paste0(emoji$X2, collapse = "|"))
compiled <- ore(emoji_regex)

extract_emojis <- function(text_vector) {
  res <- vector(mode = "list", length = length(text_vector))

  where <- which(grepl(emoji_regex, text_vector, useBytes = TRUE))
  cat("detected items with emojis\n")
  chat_emoji_lines <- aus_det$text[where]

  found_emoji <- ore.search(compiled, chat_emoji_lines, all = TRUE)
  res[where] <- ore::matches(found_emoji)
  cat("created list with emojis\n")
  res
}

#'
#' tweets dataset
#'

aus_det <- readr::read_csv("aus_det.csv")

aus_emo <- aus_det %>%
  mutate(
    emoji = extract_emojis(text)
  ) %>%
  filter(!sapply(emoji, is.null)) %>%
  tidyr::unnest(emoji)

centroids_df <- rworldmap::getMap(resolution = "high") %>%
  rgeos::gCentroid(byid = TRUE) %>%
  as.data.frame(row.names = F) %>%
  as_tibble(rownames = "country")

top_emo <- aus_emo %>%
  group_by(country, emoji) %>%
  dplyr::count() %>%
  group_by(country) %>%
  top_n(1, wt = n) %>%
  dplyr::arrange(desc(n)) %>%
  left_join(centroids_df, by = "country") %>%
  group_by(country) %>%
  slice(1) %>%
  ungroup()

emo <- top_emo %>%
  distinct(emoji) %>%
  mutate(
    url = purrr::map_chr(emoji, purrr::slowly(~ emoji_to_link(.x), purrr::rate_delay(1))),
    label = link_to_img(paste0("emoji/", basename(unique(url))))
  )

top_emo <- top_emo %>% left_join(emo, by = "emoji")

if (!dir.exists("emoji")) dir.create("emoji")

download.file(emo$url, paste0("emoji/", basename(emo$url)))

world <- map_data("world")

p <- ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), color = "black", fill = "lightgray", size = 0.1) +
  geom_richtext(
    data = top_emo %>% ungroup() %>%
      mutate(label = stringr::str_replace_all(label, "'25'", paste0("'", round(log1p(top_emo$n) * 3), "'"))),
    aes(x, y, label = label), fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  ggalt::coord_proj("+proj=wintri", ylim = c(85, -50), xlim = c(180, -180)) +
  xlab("") +
  ylab("") +
  labs(
    title = "<img src='https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/apple/237/flag-for-australia_1f1e6-1f1fa.png' width='35'/> 
Australia bushfires in emojis",
    subtitle = "<br/>Emoji is basically like another language: it has its own rules, it can cover anything that comes to one's mind, <br/>
you can build whole sentences using only those tiny faces and other symbols. While people were praying <br/>
for Australia on Twitter they used plenty of emojis as well, but only few of them were the most common ones.<br/>

The map below shows the emojis used most frequently by country in tweets with hashtags <span style='color:blue'>#prayforaustralia</span>, <br/>
<span style='color:blue'>#australiaonfire</span>, <span style='color:blue'>#australiafires</span>, 
<span style='color:blue'>#australia</span>, <span style='color:blue'>#australianbushfire</span>, 
<span style='color:blue'>#australianfires</span>, <span style='color:blue'>#australiaburning</span>, 
<span style='color:blue'>#australiaburns</span>, <br/> <span style='color:blue'>#pray4australia</span>, 
<span style='color:blue'>#australiabushfires</span>, <span style='color:blue'>#prayforrain</span><br>",
    caption = glue::glue("Data: twitter.com, {format(n_distinct(aus_emo, 'status_id'), big.mark = ' ')} tweets with marked location")
  ) +
  hrbrthemes::theme_ipsum(base_family = "Lato") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.title = element_markdown(size = 35, face = "bold", colour = "black", vjust = -1),
    plot.subtitle = element_markdown(size = 18, vjust = -1, lineheight = 1.1),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold", colour = "black", vjust = 0.5, hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

ggsave(filename = "bushfires_emo.png", plot = p, width = 14, height = 11, dpi = 500)
