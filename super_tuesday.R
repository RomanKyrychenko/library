library(catchpole) # hrbrmstr/catchpole

plot(delegates_map()[1])
plot(delegates_map()[2])

delegates_map()

library(sf)
library(hrbrthemes)
library(tidyverse)

p <- gg_catchpole() +
  theme_ft_rc(grid="", base_family = "Lato") +
  theme(legend.position = "bottom")

p$data

jsonlite::fromJSON(
  url("https://asset.wsj.net/wsjnewsgraphics/election/2020/delegates.json"),
  simplifyDataFrame = FALSE
) -> del

ple_del <- purrr::map_dfr(del$data, ~bind_cols(.$delCount)) %>% 
  mutate(state = names(del$data)) %>% 
  select(state, Sanders, Biden, Buttigieg, Warren, Klobuchar) %>% 
  filter(state != "US") %>% 
  tidyr::gather(candidate, delegates, -state) %>% 
  filter(delegates != 0) %>% arrange(state, desc(delegates)) %>% 
  group_by(state) %>% 
  mutate(start = lag(cumsum(delegates)),
         start = ifelse(is.na(start), 0, start)+1,
         end = delegates + start-1) %>% 
  ungroup() 

ple_del$number <- mapply(seq, ple_del$start, ple_del$end)

ple_del %>% tidyr::unnest(number) %>% select(state, candidate, number)

super <- c("CA", "TX", "NC", "VA", "MA", "MN", "CO", "TN", "AL", "OK", "AR", "UT", "ME", "VT", "DA", "AS")

c(
  "Biden" = "#5ac4c2",
  "Sanders" = "#63bc51",
  "Warren" = "#9574ae",
  "Buttigieg" = "#007bb1",
  "Klobuchar" = "#af973a"
) -> dcols




df <- as(delegates_map()[1]$geom, 'Spatial') %>% as_tibble() %>% 
  mutate(state = delegates_map()[1]$state) %>% 
  group_by(state) %>% mutate(number = 1:n()) %>% ungroup() %>% 
  left_join(ple_del %>% tidyr::unnest(number) %>% select(state, candidate, number)) %>% 
  mutate(candidate = ifelse(is.na(candidate) & state %in% super, "Super Tuesday states", candidate),
         candidate = ifelse(is.na(candidate), "Not allocated", candidate),
         candidate = factor(candidate, levels = c("Sanders", "Biden", "Buttigieg",
                                                  "Warren", "Klobuchar", "Super Tuesday states",
                                                  "Not allocated"))) 

p <- ggplot() + #fill = state,
  geom_tile(data = df, aes(coords.x1, coords.x2, fill = candidate, 
                    group = state), color = "white") +
  geom_text(data = df %>% group_by(state) %>% summarise(x1 = mean(coords.x1), x2 = mean(coords.x2)),
            aes(x1, x2, label = state), size = 5, color = "black", family = "Lato") +
  scale_fill_manual(values = c("#63bc51", "#5ac4c2", "#007bb1", "#9574ae", "#af973a",
                               "#f0f0f0", "#bdbdbd"), guide = guide_legend(ncol = 7)) +
  labs(title = "Super Tuesday. Democratic Delegate Count",
       subtitle = "Today is Super Tuesday. Super Tuesday is the election day early in a United States presidential primary season (February or March) when the highest 
number of U.S. states hold primary elections and caucuses. More delegates to the presidential nominating conventions can be won on Super Tuesday 
than on any other day, amounting to approximately a third of all delegates. It is, therefore, a reliable indicator of the likely eventual nominee.
       
This year's presidential race is flamboyant. In democratic nomination there were left few: democratic socialist and front-runner Bernie Sanders, 
moderate ex-vice president Joe Biden, ex-mayor of New York and billionaire Michael Blumberg, progressive senator Elizabeth Warren and 
Tulsi Gabbard (no idea how she managed to get so far in the race). The intrigue before Super Tuesday is big because, according to the opinion 
polls Sanders has the  best chances. Still, yesterday two other candidates Pete Buttigieg and Amy Klobuchar endorsed Biden for the nominee 
to stop Bernie Sanders.") +
  hrbrthemes::theme_ipsum(grid="", base_family = "Lato") +
  theme(
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
    )

bar <- ggplot(ple_del %>% group_by(candidate) %>% summarise(delegates = sum(delegates)) %>% 
         mutate(candidate = factor(candidate, levels = rev(c("Sanders", "Biden", "Buttigieg", "Warren", "Klobuchar")))), 
       aes(candidate, delegates, fill = candidate)) +
  geom_hline(yintercept = c(20, 40, 60), linetype = "dotted") +
  geom_col(color = NA) +
  scale_fill_manual(values = rev(c("#63bc51", "#5ac4c2", "#007bb1", "#9574ae", "#af973a"))) +
  labs(title = "Delegate count by candidate") +
  coord_flip() +
  hrbrthemes::theme_ipsum(grid="", base_family = "Lato") +
  theme(
    #axis.text.x = element_blank(), 
    #axis.text.y = element_blank(), 
    axis.title.y = element_blank(),
    plot.title = element_text(size = 12),
    #axis.title.y = element_blank(),
    #legend.title = element_blank(),
    legend.position = "none"
  )

require(cowplot)

ggdraw(p) + draw_grob(ggplotGrob(bar), x = 0, y = 0.04, 0.48, 0.27)

ggsave("images/super_tuesday.png", width = 12, height = 12, dpi = 500)
