library(tidyverse)

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

sports$classification_name |> unique()


D1 <- sports |> 
  filter(grepl("NCAA Division I-", classification_name)) |> 
  mutate(net_dollars = total_rev_menwomen - total_exp_menwomen)

D1 |> 
  group_by(sports) |> 
  summarize(average_net = mean(net_dollars, na.rm = TRUE)) |> 
  ggplot(aes(x = reorder(sports, average_net), y = average_net)) +
  geom_col() +
  #scale_y_log10() +
  coord_flip() +
  tRead::theme_tread_538()


# Used this to manually find FSU and LIU's spending
Spending_2019 <- D1 |> 
  filter(year == 2019) |> 
  group_by(institution_name) |> 
  summarize(
    n = n(), 
    total_men_exp = sum(exp_men, na.rm = TRUE),
    total_women_exp = sum(exp_women, na.rm = TRUE),
    total_profit = sum(net_dollars, na.rm = TRUE)
  ) |> 
  mutate(diff = total_women_exp - total_men_exp)


D1 |> 
  filter(year == 2019) |> 
  group_by(institution_name) |> 
  summarize(
    n = n(), 
    total_men_exp = sum(exp_men, na.rm = TRUE),
    total_women_exp = sum(exp_women, na.rm = TRUE),
    total_profit = sum(net_dollars, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = total_men_exp, y = total_women_exp)) +
  geom_point(aes(color = total_profit)) +
  geom_abline(slope = 1, intercept = 0) + 
  viridis::scale_color_viridis(labels = c("$0 M", "$30 M", "$60 M"), breaks = c(0, 30000000, 60000000)) +
  tRead::theme_tread_538() +
  scale_x_continuous(labels = c("$0 M", "$25 M", "$50 M", "$75 M"), breaks = c(0, 25000000, 50000000, 75000000)) +
  scale_y_continuous(labels = c("$0 M", "$10 M", "$20 M"), breaks = c(0, 10000000, 20000000)) +
  theme(
    legend.position = "right",
    legend.direction = "vertical"
    ) +
  labs(
    color = "Total Profit",
    x = "Men's Expenditures",
    y = "Women's Expenditures",
    title = "2019 NCAA Collegiate Athletics Spending"
    ) +
  annotate("curve",
           y = 25000000, x = 19000000, yend = 12590435, xend = 10386301,
           curvature = 0.25, arrow = arrow(type = "closed", length = unit(0.15, "inches"))
  ) +
  annotate("label", 
    x = 20000000, y = 25000000, 
    label = "Long Island University\nspent over $2 Million\nmore on women's athletics\ncompared to men's",
    size = 3
  )  +
  annotate("curve",
           y = 5000000, x = 65000000, yend = 21248011, xend = 88672517,
           curvature = 0.25, arrow = arrow(type = "closed", length = unit(0.15, "inches"))
  ) +
  annotate("label", 
           x = 65000000, y = 5000000, 
           label = "Florida State University\nspent over $67 Million\nmore on men's athletics\ncompared to women's",
           size = 3
  )

ggsave("/Users/camdenkay/Desktop/2022 TT Week 13.png", dpi = "retina")


