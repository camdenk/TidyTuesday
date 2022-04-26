library(tidyverse)
library(tidytext)

hidden_gems <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-26/hidden_gems.csv')


# hidden_gems_cleaned <- hidden_gems |> 
#   mutate(
#     has_twitter = if_else(is.na(author_twitter), 0, 1),
#     has_linkedin = if_else(is.na(author_linkedin), 0, 1),
#     has_both = if_else(has_twitter + has_linkedin == 2, 1, 0)
#   )
# 
# hidden_gems_cleaned |> 
#   group_by(vol) |> 
#   summarize(
#     num_twitter = sum(has_twitter, na.rm = TRUE),
#     num_linkedin = sum(has_linkedin, na.rm = TRUE),
#     num_both = sum(has_both, na.rm = TRUE)
#     ) |> 
#   ggplot(aes(x = vol, y = num_linkedin)) +
#   geom_line()


undesirable_words <- c("the", "an", "and", "on")


hidden_gems_tidy_review <- hidden_gems |> 
  select(vol, date, author_kaggle, title, review, notes) |> 
  unnest_tokens(review_words, review) |> 
  filter(
    !review_words %in% undesirable_words,
    nchar(review_words) >= 3
    ) |> 
  anti_join(stop_words, by = c("review_words" = "word")) |> 
  left_join(sentiments, by = c("review_words" = "word")) |> 
  mutate(
    value = case_when(
      sentiment == "positive" ~ 1,
      sentiment == "negative" ~ -1,
      TRUE ~ 0
    )
  )

hidden_gems_tidy_title <- hidden_gems |> 
  select(vol, date, author_kaggle, title, review, notes) |> 
  unnest_tokens(title_words, title) |> 
  filter(
    !title_words %in% undesirable_words,
    nchar(title_words) >= 3
  ) |> 
  anti_join(stop_words, by = c("title_words" = "word")) |> 
  left_join(sentiments, by = c("title_words" = "word")) |> 
  mutate(
    value = case_when(
      sentiment == "positive" ~ 1,
      sentiment == "negative" ~ -1,
      TRUE ~ 0
    )
  )


grouped_review <- hidden_gems_tidy_review |> 
  group_by(vol, date, author_kaggle) |> 
  summarize(
    review_sent = mean(value, na.rm = TRUE)
  )

grouped_title <- hidden_gems_tidy_title |> 
  group_by(vol, date, author_kaggle) |> 
  summarize(
    title_sent = mean(value, na.rm = TRUE)
  )

combined <- left_join(grouped_title, grouped_review)


green_color <- "#15cf00"
red_color <- "#cf1500"

combined |> 
  ggplot(aes(x = title_sent, y = review_sent)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#20beff") +
  ggpubr::stat_cor() +
  tRead::theme_tread_538() +
  labs(
    x = "Title Sentiment",
    y = "Review Sentiment",
    title = "How Does the Title of a Kaggle Notebook Relate to the Review it Receives?",
    subtitle = glue::glue("<span style = 'color:{green_color}'>**Positive**</span> words given a value of 1 and <span style = 'color:{red_color}'>**Negative**</span> words given a value of -1"),
    caption = "Data: Tidy Tuesday"
  ) +
  theme(
    plot.subtitle = ggtext::element_markdown()
  ) +
  coord_equal()

ggsave("./2022/week17 - kaggle/plots/final.png", height = 10, width = 14, dpi = "retina")
  

df_name |> 
  group_by(vol) |> 
  summarize(
    num_tutorials = sum(column_name, na.rm = TRUE)
  )
  

