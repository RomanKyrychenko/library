require(rvest)
require(ggplot2)
require(dplyr)

l <- read_html("http://stateoftheunion.onetwothree.net/texts/index.html") %>%
  html_nodes(xpath = '//*[@id="text"]/ul/li/a') %>%
  purrr::map_chr(html_attr, "href")

l <- paste0("http://stateoftheunion.onetwothree.net/texts/", l)

textes <- purrr::map_chr(l, function(x) {
  read_html(x) %>%
    html_node(xpath = '//*[@id="text"]') %>%
    html_text()
})

textes_clean <- textes %>%
  stringr::str_replace_all("\t", " ") %>%
  stringr::str_replace_all("\n", " ") %>%
  stringr::str_squish() %>%
  stringr::str_replace_all("< Previous Next >", " ") %>%
  stringr::str_replace_all("^ Return to top", " ") %>%
  stringr::str_squish()

readability <- quanteda::textstat_readability(textes_clean)

readability$date <- l %>%
  stringr::str_remove_all("http://stateoftheunion.onetwothree.net/texts/") %>%
  stringr::str_remove_all(".html") %>%
  lubridate::ymd()

readability$president <- stringr::word(textes_clean, start = 5, end = 9) %>%
  stringr::str_remove_all(c("December|January|Congress|February|Address|of|November|September|October|\\(Budget |Message\\)")) %>%
  stringr::str_remove_all(pattern = c("0|1|2|3|4|5|6|7|8|9|,")) %>%
  stringr::str_squish() %>%
  stringr::word(start = -1)

readability$term <- c(0, cumsum(na.omit(!(readability$president == lag(readability$president)))))

ggplot(readability, aes(date, -Flesch)) +
  geom_rect(data = tibble(
    date = lubridate::ymd("1790-01-01"),
    xmax = lubridate::ymd("2020-02-01"),
    Flesch = -c(0, 30, 50, 60),
    ymax = -c(30, 50, 60, 70),
    group = factor(c("Very Difficult", "Difficult", "Fairly Difficult", "Plain English"), levels = c("Very Difficult", "Difficult", "Fairly Difficult", "Plain English"))
  ), aes(xmin = date, xmax = xmax, ymin = Flesch, ymax = ymax, fill = group), alpha = 0.5) +
  geom_path(size = 0.5) +
  geom_point() +
  geom_text(
    data = readability %>%
      group_by(president, term) %>%
      summarise(date = mean(date)), aes(date, -70, label = president),
    size = 2.5, alpha = 0.5, family = "Lato",
    nudge_y = 0.05,
    hjust = 0,
    angle = 90
  ) +
  annotate("text", x = lubridate::ymd("1792-01-01"), y = -43, label = "more difficult →", angle = 90, family = "Lato", alpha = 0.5) +
  annotate("text", x = lubridate::ymd("1792-01-01"), y = -55, label = "← simpler", angle = 90, family = "Lato", alpha = 0.5) +
  scale_x_date(expand = c(0, 0, 0, 0)) +
  scale_y_continuous(
    expand = c(0, 0, 0, 0),
    breaks = -c(0, 30, 50, 60, 70),
    limits = c(-70, 0),
    labels = c(0, 30, 50, 60, 70)
  ) +
  scale_fill_manual(values = rev(c(
    "#feebe2",
    "#fbb4b9",
    "#f768a1",
    "#ae017e"
  )), guide = guide_legend(title = "Readability:")) +
  labs(
    title = "As simple as that: how US presidents speak",
    subtitle = "**State of the Union readability analysis shows that modern presidents speak simpler than their nineteenth-century counterparts.**

The State of the Union Address is an annual message delivered by the President of the United States to a joint session of the United States<br/>
Congress at the beginning of each calendar year in office. The message typically includes a budget message and an economic report of the nation,<br/>
and also allows the President to propose a legislative agenda and national priorities.",
    caption = "Data: http://stateoftheunion.onetwothree.net/"
  ) +
  ylab("Flesch readability score") +
  xlab("year") +
  hrbrthemes::theme_ipsum(base_family = "Lato", axis_title_size = 10, axis_title_face = "bold") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 23),
    plot.subtitle = ggtext::element_markdown(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dotted")
  )

ggsave("images/state_of_the_union_readability.png", width = 10, height = 10, dpi = 800)
