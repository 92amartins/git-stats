library(readtext)
library(tidytext)
library(tidyverse)
library(ggthemes)
library(jsonlite)

commits <- readtext("data/stewie-commits.txt")
emojis <- read_json("https://raw.githubusercontent.com/carloscuesta/gitmoji/master/src/data/gitmojis.json",
                    simplifyVector = TRUE)$gitmojis


tidy_commits <- commits %>%
  as.data.frame() %>%
  unnest_tokens(word, text, token = "regex") %>%
  select(word)

emoji_counts <- tidy_commits %>%
  inner_join(emojis, by = c("word" = "code")) %>%
  count(emoji, sort = TRUE) %>%
  mutate(percent = paste0(round((n / sum(n)) * 100, 2), "%"))

emoji_counts %>%
  mutate(emoji = reorder(emoji, n)) %>%
  ggplot() +
  geom_text(aes(emoji,n, label = percent)) +
  geom_text(aes(emoji,n, label = emoji), nudge_y = 8) +
  labs(x="Emoji", y="# commits",
       title = "Distribuição de commits no Stewie",
       subtitle = "Carga de bugs de 23%",
       caption="Fonte: Cucaracha Holdings") +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
