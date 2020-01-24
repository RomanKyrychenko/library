suppressPackageStartupMessages({
  require(dplyr)
  require(ggplot2)
  require(grid)
  require(gridExtra)
})

DATA <- tibble(
  state = c(
    "Макоекономічне середовище", "Інновації", "Вища освіта та професійна підготовка",
    "Технологічна готовність", "Інституції", "Розмір ринку", "Ефективність ринку товарів",
    "Інфраструктура", "Рівень розвитку бізнесу", "Охорона здоров'я та початкова освіта",
    "Рівень розвитку фінансового ринку", "Ефективність ринку праці"
  ),
  "Позитивна динаміка" = c(6, 2, 1, 1, 1, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001, 0.00000001, 0),
  "Негативна динаміка" = c(0, 0, 0, 0, 0, 2, 2, 6, 7, 9, 9, 17)
)

set.seed(1)
# DATA$sales <- DATA$sales_staff * 50 + (runif(nrow(DATA)) * 1000)

# Order the state factor by number of sales staff so that it is plotted in that order
# DATA$state <- factor(DATA$state, levels = DATA[order(DATA$sales_staff),"state"])

my_theme <- hrbrthemes::theme_ipsum() +
  theme(
  axis.text.y = element_blank(),
  axis.line = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank()
)

g.mid <- ggplot(DATA, aes(x = 1, y = reorder(state, `Позитивна динаміка`))) + geom_text(aes(label = state)) +
  ggtitle("") +
  ylab(NULL) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.94, 1.065)) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(color = NA),
    axis.ticks = element_blank(),
    plot.margin = unit(c(1, -1, 1, -1), "mm")
  )

g1 <- ggplot(data = DATA, aes(x = reorder(state, `Позитивна динаміка`), y = `Негативна динаміка`)) +
  geom_bar(stat = "identity") + ggtitle("Негативна динаміка") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(1, -1, 1, 0), "mm")
  ) +
  scale_y_reverse() + coord_flip() +
  my_theme

g2 <- ggplot(data = DATA, aes(x = reorder(state, `Позитивна динаміка`), y = `Позитивна динаміка`)) + xlab(NULL) +
  geom_bar(stat = "identity") + ggtitle("Позитивна динаміка") +
  theme(
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    plot.margin = unit(c(1, 0, 1, -1), "mm")
  ) +
  coord_flip() +
  my_theme

gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1, gg.mid, gg2, ncol = 3, widths = c(3 / 9, 3 / 9, 3 / 9))
