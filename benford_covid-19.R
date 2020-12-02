require(dplyr)
require(benford.analysis)
require(ggplot2)
require(ggtext)
require(patchwork)

covid <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv") %>% 
  filter(new_cases >= 0)

cnt <- unique(covid$location[covid$new_cases>1000])

bfd.cp <- purrr::map_dfr(cnt, function(x) {
  benford(covid %>% filter(location == x) %>% pull(new_cases), number.of.digits = 1)$bfd %>% 
  select(digits, data.dist, benford.dist) %>% mutate(country = x)
  }) %>% group_by(country) %>% 
  mutate(mae = mean(abs(data.dist - benford.dist)))

bfd.cp$country <- factor(bfd.cp$country, levels = bfd.cp %>% arrange(mae) %>% pull(country) %>% unique())

bn <- bind_rows(
  bfd.cp %>% group_by(country) %>% 
    summarise(mae = mean(abs(data.dist - benford.dist))) %>% arrange(mae) %>% 
    tail(10),
  bfd.cp %>% group_by(country) %>% 
    summarise(mae = mean(abs(data.dist - benford.dist))) %>% arrange(mae) %>% 
    head(10)
)

p1 <- bfd.cp %>% 
  filter(country %in% bn$country[11:20]) %>% 
  ggplot(aes(digits, data.dist)) +
  geom_col() +
  geom_path(aes(y = benford.dist)) +
  facet_wrap(~country, nrow = 2) +
  scale_y_continuous(expand = c(0, 0, 0, 0), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expand = c(0, 0, 0, 0), labels = c(1:9), breaks = 1:9) +
  xlab("") +
  ylab("percent from daily new COVID-19 cases") +
  labs(
    title = "COVID-19 truth-tellers and liers",
    subtitle = "
<br/>
What countries give falsified data about the new COVID-19 cases? Benford law can provide an answer.
Benford's law is an observation about <br/> the frequency distribution of leading digits in many real-life sets of numerical data.
The law states that in many naturally occurring collections <br/> of numbers, the leading digit is likely to be small.
In sets that obey the law, the number 1 appears as the leading significant digit about 30% of <br/> the time, while 9 appears as the leading significant digit less than 5% of the time.
If the digits were distributed uniformly, they would each occur <br/> about 11.1% of the time. We have calculated the Benford distribution of reported COVID-19 new cases in each country 
with more than 1000 <br/> cases and defined the top 10  truth-tellers and liers (calculating a mean absolute error between a officially stated distribution and Benford distribution). </p>

<b>top 10 truth-tellers:</b>"
  ) +
  hrbrthemes::theme_ipsum(base_family = "Lato") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    plot.title = element_text(size=35, face = "bold", colour = "black", vjust = -1),
    plot.subtitle = element_markdown(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold", colour = "black", vjust = 0.5, hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(linetype = "dotted"))

p2 <-  bfd.cp %>% 
  filter(country %in% bn$country[1:10]) %>% 
  ggplot(aes(digits, data.dist)) +
  geom_col() +
  geom_path(aes(y = benford.dist)) +
  facet_wrap(~country, nrow = 2) +
  scale_y_continuous(expand = c(0, 0, 0, 0), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expand = c(0, 0, 0, 0), labels = c(1:9), breaks = 1:9) +
  xlab("first-digit in reported daily COVID-19 new cases") +
  ylab("") +
  labs(
    subtitle = "<b>top 10 liers:</b>",
    caption = "Data: Johns Hopkins University"
  ) +
  hrbrthemes::theme_ipsum(base_family = "Lato") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    plot.title = element_text(size=35, face = "bold", colour = "black", vjust = -1),
    plot.subtitle = element_markdown(size = 14, vjust= -1, lineheight = 1.1),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold", colour = "black", vjust = 0.5, hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(linetype = "dotted")
    )

(p1/p2)

ggsave("covid-liers.png", width = 14, height = 14, dpi = 500)

