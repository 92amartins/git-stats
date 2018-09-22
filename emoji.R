library(readtext)
library(tidytext)
library(tidyverse)
library(ggthemes)

commits <- readtext("data/stewie_commits.txt")
emojis <- read.csv("data/emojis.csv", stringsAsFactors = FALSE)


tidy_commits <- commits %>%
  as.data.frame() %>%
  unnest_tokens(word, text, token = "regex") %>%
  select(word)

emoji_counts <- tidy_commits %>%
  inner_join(emojis, by = c("word" = "name")) %>%
  count(word, sort = TRUE) %>%
  mutate(percent = paste0(round((n / sum(n)) * 100, 2), "%"))

emoji_counts %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_text(aes(word,n, label = percent)) +
  geom_text(aes(word,n, label = word), nudge_y = 5) +
  labs(x="Emoji", y="# commits",
       title = "Distribuição de commits no Stewie",
       subtitle = "Carga de bugs de 23%",
       caption="Fonte: Cucaracha Holdings") +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
