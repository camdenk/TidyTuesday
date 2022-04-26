library(tidyverse)
library(gganimate)
library(RcppRoll)

indoor_pollution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv')
death_fuel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_fuel.csv')
death_source <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_source.csv')
death_timeseries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_timeseries.csv')
fuel_access <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/fuel_access.csv')
fuel_gdp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/fuel_gdp.csv')


country_region <- death_fuel |> 
  drop_na(Continent) |> 
  select(Entity, Code, Continent)


combined <- indoor_pollution |> 
  left_join(country_region) |> 
  rename("death_perc" = "Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Percent)")

yoy_change <- combined |> 
  mutate(
    last_year = lag(death_perc),
    improvement = (death_perc - last_year)/last_year
    ) |> 
  drop_na(Continent) |> 
  group_by(Continent, Year) |> 
  summarize(av.perc.change = mean(improvement, na.rm = TRUE)) |> 
  mutate(rolling_mean_change = roll_mean(av.perc.change, n = 3, align = "right", fill = NA))



plot <- yoy_change |> 
  filter(Year >= 1993) |> 
  ggplot(aes(x = Year, y = rolling_mean_change, color = Continent)) +
  geom_line(size = 2, alpha = 0.80) +
  scale_y_continuous(limits = c(-0.1, 0), labels = scales::percent) +
  scale_x_continuous(breaks = seq(1993, 2019, 2)) +
  labs(
    y = "% Change Year Over Year",
    title = "How Did Household Air Pollution Death Rates Change By Continent?",
    subtitle = "Three Year Rolling Mean",
    caption = "Data: Tidy Tuesday"
  ) +
  theme(
    text = element_text(color = "white"),
    title = element_text(color = "white"),
    
    axis.text = element_text(color = "white"),
    
    axis.title.x = element_blank(),
    
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    
    plot.background = element_rect(fill = "black"),
    
    legend.position = "bottom",
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  scale_color_brewer(palette = "Accent")


dir_path <- "~/Desktop/TidyTuesday/2022/week15 - pollution/plots/"

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE) 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

file.copy(from=plots.png.paths, to=dir_path)

plots.png.details <- file.info(plots.png.paths)
plots.png.details <- plots.png.details[order(plots.png.details$mtime),]
sorted.png.names <- gsub(plots.dir.path, dir_path, row.names(plots.png.details), fixed=TRUE)
numbered.png.names <- paste0(dir_path, 1:length(sorted.png.names), ".png")

# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
file.rename(from=sorted.png.names, to=numbered.png.names)



graph.animation = plot +
  transition_reveal(Year) +
  view_follow(fixed_y = TRUE)


animate(graph.animation, height = 500, width = 800, fps = 20, duration = 10, end_pause = 60, res = 100)
anim_save(paste0(dir_path, "TT final graph.gif"))



