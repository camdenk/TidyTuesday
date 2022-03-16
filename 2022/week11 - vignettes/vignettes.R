library(tidyverse)
library(lubridate)

bioc <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv')
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')


cran_versions <- cran |> 
  mutate(version = str_replace(version, "-", ".")) |> 
  distinct() |> 
  mutate(
    datetime = as_datetime(date),
    date = as.Date(datetime),
    dow = factor(
      weekdays(date), ordered = TRUE,
      levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    )
    ) |> 
  mutate(
    weekday = case_when(
      #is.na(dow) ~ NA_integer_,
      dow %in% c("Sunday", "Saturday") ~ 0,
      TRUE ~ 1
    )
  )


cran_v1 <- cran_versions |> 
  filter(version %in% c("1.0", "1", "1.0.0")) |> 
  mutate(
    color = case_when(
      #is.na(dow) ~ NA_integer_,
      dow %in% c("Sunday", "Saturday") ~ "blue",
      TRUE ~ "red"
    )
  )

cran_v1 |> 
  ggplot(aes(x = dow, fill = color)) +
  geom_bar() +
  scale_fill_identity() +
  labs(
    title = "Initial Package Releases by Date"
  ) +
  tRead::theme_tread_538() +
  theme(
    panel.grid.major.x = element_blank()
  )


cran_versions |> 
  ggplot(aes(x = dow)) +
  geom_bar() +
  labs(
    title = "Package Releases/Updates by Date"
  )
  

