require(dplyr)
require(ggplot2)

corruption <- readr::read_csv("data/worlds_corruption.csv")
religious <- readr::read_csv("data/words_religiuos_rate.csv")

S_sqrt <- function(x) ifelse(x == 0, 0, x^(1/4))
IS_sqrt <- function(x) x^4
S_sqrt_trans <- function() scales::trans_new("S_sqrt",S_sqrt,IS_sqrt)

inner_join(corruption %>% select(name, corruptionScore), religious, by = "name") %>% 
  mutate(atheism = unaffiliated / (jews + other + folkReligions + buddhists + hindus + unaffiliated + muslims + chistians),
         confession = case_when(
           jews > other & jews > folkReligions & jews > buddhists & jews > hindus & jews > muslims & jews > chistians ~ "Judaism",
           other > jews & other > folkReligions & other > buddhists & other > hindus & other > muslims & other > chistians ~ "other",
           folkReligions > other & folkReligions > jews & folkReligions > buddhists & folkReligions > hindus & folkReligions > muslims & folkReligions > chistians ~ "folk Religion",
           buddhists > other & buddhists > jews & buddhists > folkReligions & buddhists > hindus & buddhists > muslims & buddhists > chistians ~ "Buddhism",
           hindus > other & hindus > jews & hindus > folkReligions & hindus > buddhists & hindus > muslims & hindus > chistians ~ "Hinduism",
           muslims > other & muslims > jews & muslims > hindus & muslims > buddhists & muslims > hindus & muslims > chistians ~ "Islam",
           chistians > other & chistians > jews & chistians > hindus & chistians > muslims & chistians > hindus & chistians > muslims ~ "Christianity",
           T ~ "other"
         )) %>%
  ggplot(aes(atheism, corruptionScore, label = name)) +
  geom_smooth(method = "loess", se = F, method.args = list(family = "symmetric"), span = 3, color = "#a6bddb") +
  geom_point(aes(color = confession), stroke = 0, size = 6, alpha = 0.6) +
  geom_curve(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = tibble(
      x = c(.5),
      y = c(8),
      xend = c(.7),
      yend = c(13),
      name = "Others"
    ),
    curvature = .1, arrow = arrow(length = unit(2, "mm"))
  ) +
  ggrepel::geom_text_repel() +
  scale_x_continuous(trans = "S_sqrt", 
                     labels = c("0.01%", "0.16%", "0.8%", "2.6%", "6%", "13%", "24%", "41%", "66%", "100%"), 
                     breaks = c(0.0001, .0016, 0.008, 0.026, 0.06, 0.13, 0.24, 0.41, 0.66, 1),
                     limits = c(0.00001, 0.8), expand = c(0, 0, 0, 0)) +
  scale_y_continuous(limits = c(0, 90), expand = c(0, 0, 0, 0), breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c(
    "#ffff33",
    "#e41a1c",
    "#984ea3",
    "#ff7f00",
    "#4daf4a",
    "#377eb8",
    "#a65628"), guide = guide_legend(title = "prevailing religion: ", ncol = 7, title.theme = element_text(face = "bold"))) +
  labs(title = "God bless corruption!", 
       subtitle = "The more religious the country is, the more it is corrupt", 
       caption = "Data: Transparency International, Pew Research Center") +
  annotate("text", x = .5, y = 7, label = 'bold("Kim is more popular than God here")', family = "Lato", parse = T) +
  annotate("text", x = .00003, y = 82, label = "less corrupt →", angle = 90, family = "Lato", alpha = 0.5) +
  annotate("text", x = .00003, y = 9, label = "← more corrupt", angle = 90, family = "Lato", alpha = 0.5) +
  xlab("% of atheists in population\nNote: scale is squared!") +
  ylab("Transperancy International corruption score") +
  hrbrthemes::theme_ipsum(base_family = "Lato", axis_title_size = 10, axis_title_face = "bold") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dotted", colour = "black")
  )

ggsave("images/atheism_corruption.png", width = 14, height = 14, dpi = 400)
