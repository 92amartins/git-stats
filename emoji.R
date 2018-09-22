library(readtext)
library(tidytext)
library(tidyverse)
library(emoGG)

commits <- readtext("data/stewie_commits.txt")
emojis <- read.csv("data/emojis.csv", stringsAsFactors = FALSE)


tidy_commits <- commits %>%
  as.data.frame() %>%
  unnest_tokens(word, text, token = "regex") %>%
  select(word)

emoji_counts <- tidy_commits %>%
  inner_join(emojis, by = c("word" = "name")) %>%
  count(emoji, sort = TRUE) %>%
  mutate(percent = round(n / sum(n), 3))

emoji_counts
  
emoji_counts %>%
  mutate(emoji = reorder(emoji, n)) %>%
  ggplot() +
  geom_text(aes(emoji,n, label = n)) +
  geom_text(aes(emoji,n, label = emoji), nudge_y = 2) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
