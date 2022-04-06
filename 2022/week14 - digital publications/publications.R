library(tidyverse)
library(tidytext)

news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')


undesirable_words <- c("the", "an", "and", "on")


news_tidy <- news_orgs |> 
  select(publication_name, year_founded, tax_status_current, summary) |> 
  unnest_tokens(word, summary) |> 
  filter(!word %in% undesirable_words) |> 
  filter(!nchar(word) < 3) |> 
  anti_join(stop_words)


news_sentiments <- news_tidy |>
  left_join(sentiments) |> 
  mutate(value = case_when(
    sentiment == "positive" ~ 1,
    sentiment == "negative" ~ -1
  ))


green_color <- "#15cf00"
red_color <- "#cf1500"

positivity_labels <- tibble(
  y = c(0.75, -0.125), x = c(8.25,1.7),
  label = c("Positive", "Negative"),
  color = c(green_color, red_color)
)


plot_data <- news_sentiments |> 
  group_by(tax_status_current) |> 
  summarize(av_sent = mean(value, na.rm = TRUE), n = n()) |> 
  drop_na(tax_status_current) |> 
  mutate(
    sent_color = if_else(av_sent > 0, green_color, red_color),
    hjust = if_else(av_sent > 0, 0, 1)
    )

plot_data |> 
  ggplot(aes(x = reorder(tax_status_current, av_sent), y = av_sent)) +
  geom_col(aes(fill = sent_color), alpha = 0.9) +
  ggtext::geom_richtext(
    aes(label = n, color = sent_color, hjust = hjust),
    fill = "#f0f0f0", label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
    fontface = "bold",
    size = 6
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_flip() +
  scale_y_continuous(limits = c(-0.4, 1.2)) +
  ggthemes::theme_fivethirtyeight() +
  labs(
    title = glue::glue("How <span style = 'color:{green_color}'>**Positive**</span> or <span style = 'color:{red_color}'>**Negative**</span> are News Organizations?"),
    subtitle = "Sentiment analysis for news organizations' summaries grouped by current tax status<br>#: number of organizations that fall into category",
    x = "Current Tax Status",
    y = "Average Sentiment",
    caption = "Data: Tidy Tuesday"
  ) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(),
    #panel.grid.major.y = element_blank()
  )

ggsave("./2022/week14 - digital publications/publications.png", dpi = "retina")
