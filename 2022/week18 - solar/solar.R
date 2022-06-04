library(tidyverse)

wind_color <- "#75aaff"
solar_color <- "#ffff91"

capacity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv') |> 
  mutate(type = "wind", color = wind_color) |> 
  rename(mwh = wind_mwh, capacity = wind_capacity)
solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv') |> 
  mutate(type = "solar", color = solar_color) |> 
  rename(mwh = solar_mwh, capacity = solar_capacity)
average_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')


# average_cost |> 
#   pivot_longer(!year, names_to = "type", values_to = "count") |> 
#   ggplot(aes(x = year, y = count)) +
#   geom_line(aes(color = type))
# 
# 
# solar |> 
#   ggplot(aes(x = mwh, y = capacity)) +
#   geom_point(aes(color = lubridate::year(as.Date(date))))


combined <- solar |> 
  bind_rows(wind) |> 
  mutate(year = lubridate::year(as.Date(date)))

combined |> 
  filter(year != 2021) |> 
  ggplot(aes(x = capacity, y = mwh)) +
  geom_point(aes(color = color), alpha = 0.9) +
  scale_color_identity() +
  facet_wrap(~year) +
  labs(
    x = "Capacity (GW)",
    y = "Projected Price ($/MWh)",
    title = "Evolution of Wind and Solar Energy",
    subtitle = glue::glue("<span style = 'color:{solar_color}'>**Solar Energy**</span> has caught up with  <span style = 'color:{wind_color}'>**Wind Energy**</span> with lower prices since 2009"),
    caption = "Data: Tidy Tuesday"
  ) +
  tRead::theme_tread_538() +
  theme(
    plot.subtitle = ggtext::element_markdown()
  )

ggsave("./2022/week18 - solar/Final.png", dpi = "retina")
