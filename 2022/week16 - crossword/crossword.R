library(tidyverse)
library(stringr)

big_dave <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv')
times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')

count_letters <- function(dataframe){
  df <- dataframe |> 
    mutate(
      answer = tolower(answer),
      a = str_count(answer, "a"),
      b = str_count(answer, "b"),
      c = str_count(answer, "c"),
      d = str_count(answer, "d"),
      e = str_count(answer, "e"),
      f = str_count(answer, "f"),
      g = str_count(answer, "g"),
      h = str_count(answer, "h"),
      i = str_count(answer, "i"),
      j = str_count(answer, "j"),
      k = str_count(answer, "k"),
      l = str_count(answer, "l"),
      m = str_count(answer, "m"),
      n = str_count(answer, "n"),
      o = str_count(answer, "o"),
      p = str_count(answer, "p"),
      q = str_count(answer, "q"),
      r = str_count(answer, "r"),
      s = str_count(answer, "s"),
      t = str_count(answer, "t"),
      u = str_count(answer, "u"),
      v = str_count(answer, "v"),
      w = str_count(answer, "w"),
      x = str_count(answer, "x"),
      y = str_count(answer, "y"),
      z = str_count(answer, "z"),
      spaces = str_count(answer, " "),
      characters = str_length(answer),
      letter_length = characters-spaces
    ) |> 
    group_by(source) |> 
    summarize(
      tot_a = sum(a, na.rm = TRUE),
      tot_b = sum(b, na.rm = TRUE),
      tot_c = sum(c, na.rm = TRUE),
      tot_d = sum(d, na.rm = TRUE),
      tot_e = sum(e, na.rm = TRUE),
      tot_f = sum(f, na.rm = TRUE),
      tot_g = sum(g, na.rm = TRUE),
      tot_h = sum(h, na.rm = TRUE),
      tot_i = sum(i, na.rm = TRUE),
      tot_j = sum(j, na.rm = TRUE),
      tot_k = sum(k, na.rm = TRUE),
      tot_l = sum(l, na.rm = TRUE),
      tot_m = sum(m, na.rm = TRUE),
      tot_n = sum(n, na.rm = TRUE),
      tot_o = sum(o, na.rm = TRUE),
      tot_p = sum(p, na.rm = TRUE),
      tot_q = sum(q, na.rm = TRUE),
      tot_r = sum(r, na.rm = TRUE),
      tot_s = sum(s, na.rm = TRUE),
      tot_t = sum(t, na.rm = TRUE),
      tot_u = sum(u, na.rm = TRUE),
      tot_v = sum(v, na.rm = TRUE),
      tot_w = sum(w, na.rm = TRUE),
      tot_x = sum(x, na.rm = TRUE),
      tot_y = sum(y, na.rm = TRUE),
      tot_z = sum(z, na.rm = TRUE),
      total_spaces = sum(spaces, na.rm = TRUE),
      total_characters = sum(characters, na.rm = TRUE),
      total_letters = sum(letter_length, na.rm = TRUE)
    )
    
}

big_dave_cleaned <- big_dave |> 
  count_letters() |> 
  mutate(
    across(tot_a:tot_z, ~ .x/total_letters)
  )

times_cleaned <- times |> 
  count_letters() |> 
  mutate(
    across(tot_a:tot_z, ~ .x/total_letters)
  )

combined <- bind_rows(big_dave_cleaned, times_cleaned) |> 
  select(-c(total_spaces, total_characters, total_letters)) |> 
  pivot_longer(!source, names_to = "letter", values_to = "percentage") |> 
  mutate(
    letter = toupper(substr(letter, 5,5)),
    source = if_else(source == "times_xwd_times", "New York Times", "Daily Telegraph")
  )


combined |> 
  ggplot(aes(x = letter, y = percentage, color = source, fill = source)) +
  geom_col(position = "dodge") +
  tRead::theme_tread_538() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "Relative Frequency",
    x = "",
    fill = "Crossword Source",
    color = "Crossword Source",
    title = "Letter Usage is Remarkably Similar in Crossword Puzzles",
    caption = "Data: Tidy Tuesday"
  ) +
  theme(
    panel.grid.major.x = element_blank()
  )

dir_path <- "~/Desktop/TidyTuesday/2022/week16 - crossword/plots/"

ggsave(paste0(dir_path, "Final Plot.png"), width = 14, height = 10, dpi = "retina")



