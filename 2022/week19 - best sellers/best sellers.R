library(tidyverse)
library(shadowtext)

nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')


nyt_titles |> 
  group_by(author) |> 
  summarize(total_weeks = sum(total_weeks),
            n_books = n()) |> 
  arrange(-total_weeks, -n_books)

nyt_titles |> 
  group_by(author) |> 
  summarize(total_weeks = sum(total_weeks),
            n_books = n()) |> 
  mutate(weeks_per_book = total_weeks/n_books) |> 
  arrange(-weeks_per_book)


one_hit_wonders <- nyt_titles |> 
  group_by(author) |> 
  filter(n() == 1) |> 
  ungroup() |> 
  arrange(-total_weeks)

double_ups <- c("edited by Nicholas Meyer", "Eugene Burdick and Harvey Wheeler",
                "William J. Lederer and Eugene Burdick")

one_hit_wonders |> 
  filter(total_weeks >= 35,
         !author %in% double_ups,
         author != "General Sir John Hackett and Other Top-ranking NATO Generals and Advisors") |> 
  mutate(author = if_else(author == "written and illustrated by Chris Van Allsburg", "Chris Van Allsburg", author),
         book_author = paste(title, author, sep = " by "),
         century = as.factor(floor(year/100)*100),) |> 
  ggplot(aes(y = reorder(author, total_weeks), x = total_weeks)) +
  geom_col(aes(fill = century, color = century), alpha = 0.5) + 
  geom_shadowtext(aes(label=title), size=3, x = 2, hjust = 0, color = "black", bg.color = "white") +
  #coord_flip() +
  scale_x_continuous(expand = c(0, 0.1)) +
  tRead::theme_tread_538() +
  theme(panel.grid.major.y = element_blank()) +
  labs(x = "Total Weeks on Bestseller List",
       y = "",
       fill = "Debut Century",
       color = "Debut Century",
       title = "Top One-Hit Wonders",
       subtitle = "Authors with only one book on the NYT Bestseller List | Minimum 35 weeks")

ggsave("./2022/week19 - best sellers/Final.png", width = 12, height = 10, dpi = "retina")



one_hit_wonders |> 
  filter(total_weeks >= 35,
         !author %in% double_ups,
         author != "General Sir John Hackett and Other Top-ranking NATO Generals and Advisors",
         year >= 2005) |> 
  mutate(debut_date_val = as.numeric(first_week),
         fall_off_date_val = debut_date_val+7*total_weeks,
         debut_date_val = as.Date.numeric(debut_date_val, origin = "1970-01-01"),
         fall_off_date_val = as.Date.numeric(fall_off_date_val, origin = "1970-01-01"),
         number_one = if_else(best_rank == 1, "#D3B36E", "#0b0b0b")) |> 
  ggplot(aes(y = reorder(title, debut_date_val), color = number_one)) +
  geom_point(aes(x = debut_date_val), size = 5) +
  geom_point(aes(x = fall_off_date_val), size = 5) +
  geom_segment(aes(x = debut_date_val, xend = fall_off_date_val, yend = reorder(title, debut_date_val)), size = 2) +
  scale_color_identity(guide = "legend",
                       name = "Reached Number One?",
                       labels = c("No", "Yes")) +
  labs(x = "",
       y = "",
       title = "Time Spent on NYT Bestseller List",
       subtitle = "One Hit Wonders | Minimum 35 Weeks | Assumed Continuous Time on List") +
  tRead::theme_tread_538() +
  NULL


ggsave("./2022/week19 - best sellers/Emilio's Idea Final.png", width = 12, height = 10, dpi = "retina")




  