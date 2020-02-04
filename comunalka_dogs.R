require(rvest)
require(dplyr)
require(ggplot2)

dog_breeds <- read_html("https://www.olx.ua/zhivotnye/sobaki/q-%D1%81%D0%BE%D0%B1%D0%B0%D0%BA%D0%B0/") %>% 
  html_node(".toplinks.x-normal") %>% 
  html_nodes(".topLink.tdnone.parameter") %>%
  purrr::map_chr(html_attr, "href") %>% 
  stringr::str_split(pattern = "/") %>% 
  purrr::map_chr(~.[6])

dog_count <- read_html("https://www.olx.ua/zhivotnye/sobaki/q-%D1%81%D0%BE%D0%B1%D0%B0%D0%BA%D0%B0/") %>% 
  html_node(".toplinks.x-normal") %>% 
  html_nodes(".counter.nowrap") %>%
  purrr::map_chr(html_text) %>% 
  stringr::str_remove_all(" ") %>% 
  as.numeric()

dog_name <- read_html("https://www.olx.ua/zhivotnye/sobaki/q-%D1%81%D0%BE%D0%B1%D0%B0%D0%BA%D0%B0/") %>% 
  html_node(".toplinks.x-normal") %>% 
  html_nodes(".link") %>%
  purrr::map_chr(html_text)

dog_name <- dog_name[-84]

dog_raw <- tibble(dog_breeds, dog_name, dog_count, 
                  dog_page = ceiling(dog_count / 44),
                  pages = purrr::map2(dog_page > 10, dog_page, function(x, y) if(x) return(1:10) else return(1:y)))

get_dog <- function(dog, page) {
  box <- read_html(glue::glue("https://www.olx.ua/zhivotnye/sobaki/{dog}/q-%D1%81%D0%BE%D0%B1%D0%B0%D0%BA%D0%B0/?page={page}")) %>% 
    html_nodes(".wrap")
  cat(dog, page, "\n")
  tibble(
    breed = dog,
    price = box %>% 
      purrr::map(html_node, ".price") %>% 
      purrr::map_chr(html_text) %>% 
      stringr::str_remove_all("\n") %>% 
      stringr::str_squish(),
    #box %>% 
    #  purrr::map(html_node, ".marginright5.link.linkWithHash.detailsLink") %>% 
    #  purrr::map_chr(html_text) %>% 
    #  stringr::str_remove_all("\n") %>% 
    #  stringr::str_squish()
    title = box %>% 
      purrr::map(html_node, ".title-cell") %>% 
      purrr::map(html_node, ".lheight22.margintop5") %>% 
      purrr::map_chr(html_text) %>% 
      stringr::str_remove_all("\n") %>% 
      stringr::str_remove_all("\t") %>% 
      stringr::str_squish(),
    location = box %>% 
      purrr::map(html_node, ".bottom-cell") %>% 
      purrr::map(html_node, ".lheight16") %>% 
      purrr::map(html_node, ".breadcrumb.x-normal") %>%
      purrr::map_chr(html_text) %>% 
      stringr::str_remove_all("\n") %>% 
      stringr::str_remove_all("\t") %>% 
      stringr::str_squish(),
    date = box %>% 
      purrr::map(html_node, ".bottom-cell") %>% 
      purrr::map(html_node, ".lheight16") %>% 
      purrr::map(html_nodes, ".breadcrumb.x-normal") %>%
      purrr::map(~.[[2]]) %>% 
      purrr::map_chr(html_text) %>% 
      stringr::str_remove_all("\n") %>% 
      stringr::str_remove_all("\t") %>% 
      stringr::str_squish()
  )
}

dog_raw <- dog_raw %>% tidyr::unnest("pages") %>% select(dog_breeds, dog_name, pages)

dog_list <- purrr::map2(dog_raw$dog_breeds, dog_raw$pages, purrr::safely(get_dog)) 

dog_df <- purrr::map_dfr(dog_list, ~.$result) %>% distinct() %>% 
  filter(!stringr::str_detect(title, "вяз(ки|ка|ок)")) %>% 
  filter(!stringr::str_detect(title, "осплей"))

dog_df$price <- dog_df$price %>% stringr::str_remove_all(" грн.") %>% stringr::str_remove_all(" ") %>% as.numeric() #%>% hist()

dog_df <- dog_df %>% 
  filter(!stringr::str_detect(title, "т(э|е)дди")) %>% 
  filter(!stringr::str_detect(title, "teddy")) %>%
  mutate(title = stringr::str_to_lower(title),
         breed = ifelse(stringr::str_detect(title, "мальтипу|тэдди|maltipoo"), "maltipoo", breed),
         breed = ifelse(stringr::str_detect(title, "той пуд"), "toy poodel", breed))

dog_df %>% 
  left_join(dog_raw %>% select(dog_breeds, dog_name), by = c("breed" = "dog_breeds")) %>% 
  mutate(
    dog_name = stringr::str_to_lower(dog_name),
    dog_name = case_when(
      breed == "maltipoo" ~ "мальтіпу",
      dog_name == "другая" ~ "двірняга",
      dog_name == "японский хин" ~ "японський хін",
      dog_name == "бишон фризе" ~ "бішон фрізе",
      dog_name == "шпиц" ~ "шпіц",
      dog_name == "московская сторожевая" ~ "московська сторожова",
      dog_name == "восточно-европейская овчарка" ~ "східно-європейська вівчарка",
      dog_name == " самоедская собака" ~ "самоєд",
      dog_name == "цвергпинчер" ~ "цвергпінчер",
      dog_name == "кавказская овчарка" ~ "кавказька вівчарка",
      dog_name == "аляскинский маламут" ~ "аляскинський маламут",
      dog_name == "французский бульдог" ~ "французький бульдог",
      dog_name == "карликовый пинчер" ~ "карликовий пінчер",
      dog_name == "бельгийская овчарка" ~ "бельгійська вівчарка",
      dog_name == "пекинес" ~ "пекінес",
      dog_name == "колли" ~ "коллі",
      dog_name == "той-терьер" ~ "той-тер'єр",
      dog_name == "среднеазиатская овчарка" ~ "середньоазіатська вівчарка",
      dog_name == "мексиканская голая собака" ~ "мексиканська гола собака",
      dog_name == "сибирский хаски" ~ "сибірський хаскі",
      dog_name == "пит-бультерьер" ~ "піт-бультер'єр",
      dog_name == "стаффордширский бультерьер" ~ "стаффордширський бультер'єр",
      dog_name == "китайская хохлатая" ~ "китайська хохлата",
      dog_name == "спаниель" ~ "спаніель",
      dog_name == "померанский шпиц" ~ "померанський шпіц",
      dog_name == "вест хайленд уайт терьер" ~ "вест айленд уайт тер'єр",
      dog_name == "гриффон" ~ "грифон",
      dog_name == "бультерьер" ~ "бультер'єр",
      dog_name == "мальтийская болонка" ~ "мальтійська болонка",
      dog_name == "папийон" ~ "папійон",
      dog_name == "английский бульдог" ~ "англійський бульдог",
      dog_name == "русский черный терьер" ~ "російський чорний тер'єр",
      dog_name == "бордоский дог" ~ "бордоський дог",
      dog_name == "бернский зенненхунд" ~ "бернський зенненхунд",
      dog_name == "акита" ~ "акіта",
      dog_name == "немецкий дог" ~ "німецький дог",
      dog_name == "золотистый ретривер" ~ "золотистий ретривер",
      dog_name == "американский бульдог" ~ "американський бульдог",
      dog_name == "стаффордширский терьер" ~ "стаффордширський тер'єр",
      dog_name == "немецкая овчарка" ~ "німецька вівчарка",
      dog_name == "йоркширський терьер" ~ "йоркширский тер'єр",
      #breed == "burbul" ~ "бурбуль",
      #breed == "mops" ~ "мопс",
      #breed == "taksa" ~ "такса",
      breed == "toy poodel" ~ "той пудель",
      T ~ dog_name
    )
  ) %>% 
  group_by(dog_name) %>% 
  summarise(std = sd(price, na.rm = T), price = median(price, na.rm = T), n = n(), months = price/2212) %>% arrange(desc(price)) %>% 
  filter(n >= 70) %>% 
  ggplot(aes(reorder(dog_name, months), months)) +
  geom_hline(yintercept = c(0, 4, 8, 12, 16), linetype = "dotted") +
  geom_col(fill = "#1f78b4") +
  geom_text(aes(y = months-0.2, label = round(months, 1)), color = "white", size = 3) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0, 0, 0)) +
  scale_y_continuous(expand = c(0, 0, 0, 0)) +
  labs(title = "На скільки місяців оплаченої комуналки вистачить вашого песика?", 
       subtitle = "За даними держстату середній розмір платіжки українця у грудні становив 2 212 гривень.
       
Ми проаналізували оголошення про продаж собак з сайту OLX і виявили, наскільки в дійсності вистачить 
грошей в середньому з продажу улюбленця в залежності від його породи, виходячи з такого розміру платіжки", 
       caption = "Дані про вартість собак - OLX, дані про розмір платіжки - Держстат") +
  #xlab("Порода собаки") +
  xlab("") +
  ylab("місяців сплаченої комуналки") +
  hrbrthemes::theme_ipsum(base_family = "Lato", axis_title_size = 10, axis_title_face = "bold") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 23),
    plot.subtitle = element_text(size = 15),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

ggsave("images/comunalka.png", width = 14, height = 12, dpi = 400)

#get_dog("mops", 1)

