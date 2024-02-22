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
  read_excel("data/Africa Infrastructure Development Index AIDI Index.xlsx",
             sheet = "AIDI",
             n_max = 1,
             range = "R7C1:R7C21",
             col_names = FALSE)
)

## read the data
aidi.2022 <- read_excel("data/Africa Infrastructure Development Index AIDI Index.xlsx",
                        sheet = "AIDI",
                        range = cell_cols("A:U"),
                        col_names = aidiColumnNames) %>%
  slice(7:n()) %>%
  rename(Y2022=`2022`) |> 
  select(Country,ISO,Y2022) |> 
  mutate(Y2022=as.numeric(Y2022))

hcmap(
  map="custom/africa", 
  data = aidi.2022,
  name = "Gross national income per capita",
  value = "Y2022",
  borderWidth = 0,
  nullColor = "#d3d3d3",
  joinBy = c("iso-a2", "ISO")
) |>
  hc_colorAxis(
    stops = color_stops(colors = viridisLite::viridis(10, begin = 0, direction = -1))
  )