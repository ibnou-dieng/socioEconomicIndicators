library(tidyverse)
library(readxl)
library(highcharter)
library(webshot2)

################################################################################
################################################################################
######### Africa Infrastructure Development Index AIDI Index
################################################################################
################################################################################

## get the column names
aidiColumnNames <- as.character(
  read_excel(
    "data/Africa Infrastructure Development Index AIDI Index.xlsx",
    sheet = "AIDI",
    n_max = 1,
    range = "R7C1:R7C21",
    col_names = FALSE
  )
)

## read the data
aidi.2022 <-
  read_excel(
    "data/Africa Infrastructure Development Index AIDI Index.xlsx",
    sheet = "AIDI",
    range = cell_cols("A:U"),
    col_names = aidiColumnNames
  ) %>%
  slice(7:n()) %>%
  rename(Y2022 = `2022`) |>
  select(Country, ISO, Y2022) |>
  mutate(Y2022 = as.numeric(Y2022))

hcmap(
  map = "custom/africa",
  data = aidi.2022,
  name = "Gross national income per capita",
  value = "Y2022",
  borderWidth = 0,
  nullColor = "#d3d3d3",
  joinBy = c("iso-a2", "ISO")
) |>
  hc_colorAxis(stops = color_stops(colors = viridisLite::viridis(
    10, begin = 0, direction = -1
  )))

################################################################################
################################################################################
######### Education: illiterate population
################################################################################
################################################################################

## read the data
education <- read_csv("data/NATMON_DS_22022024030656751.csv") |>
  filter(Country %in% c("Northern Africa", "Sub-Saharan Africa")) |>
  select(Indicator, Country, Time, Value) |>
  unite(concatenation, c(Indicator, Country, Time), remove = FALSE) |>
  distinct(concatenation, .keep_all = TRUE) |>
  select(-concatenation) |>
  mutate(Time = as.factor(Time))

## define the color palette
mycolors <- inlmisc::GetColors(n = 10)
mycolors <- c(mycolors[6], mycolors[8])

## Generate the legend
plot.legend <- education |>
  ggplot() +
  aes(x = Time, y = Value, fill = Indicator) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "",
       y = "") +
  scale_fill_manual(values = mycolors, name = "") +
  theme_bw() +
  theme(legend.position = "top")

legend <- cowplot::get_legend(plot.legend)

## plot the data
education.plots <- education |>
  nest_by(Country) |>
  mutate(plots = list(
    data |>
      ggplot() +
      aes(x = Time, y = Value, fill = Indicator) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "",
           y = "",
           subtitle = Country) +
      scale_fill_manual(values = mycolors, name = "") +
      theme_bw() +
      theme(
        legend.position = "none",
        legend.key.size = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm')
      )
  ))

## Display the plots
gridExtra::grid.arrange(
  legend,
  education.plots$plots[[1]],
  education.plots$plots[[2]],
  ncol = 1,
  heights = c(1, 4, 4),
  bottom = "Year"
)
