library(shiny)
library(tidyverse)
library(glue)
library(tidygeocoder)
library(leaflet)

gourds <- read.csv('./data/gourds.csv')

#F = "Field Pumpkin", P = "Giant Pumpkin", S = "Giant Squash", W = "Giant Watermelon", L = "Long Gourd", T = Tomato
gourd_colors <- c(
  `Field Pumpkin` = "yellow",
  `Giant Pumpkin` = "orange",
  `Giant Squash` = "green",
  `Giant Watermelon` = "blue",
  `Long Gourd` = "purple",
  `Tomato` = "red"
)

weight_func <- function(n, A = 20, k = 0.1) {
  A * exp(-k * n)
}

sunshine_func <- function(latitude, start_day = 0, end_day = 365) {
  total <- 0
  
  for (day in start_day:end_day) {
    #P: solar declination angle
    P <- asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(0.00860 * (day - 186)))))
    #I: hour angle of the sun
    I <- (sin(0.8333 * pi / 180) + sin(latitude * pi / 180) * sin(P)) / 
      (cos(latitude * pi / 180) * cos(P))
    #D: daylight duration
    D <- 24 - ((24 / pi) * acos(I))
    total <- total + D
  }
  
  return(total)
}

gourds <- gourds |> 
  mutate(Gourd_Type = case_when(type_bk == "F"~"Field Pumpkin",
                                type_bk == "P"~"Giant Pumpkin",
                                type_bk == "S"~"Giant Squash",
                                type_bk == "W"~"Giant Watermelon",
                                type_bk == "L"~"Long Gourd",
                                type_bk == "T"~"Tomato"),
         city = as.character(city),
         location = paste(city, state_prov, country, sep = ", "),
         color = unname(gourd_colors[Gourd_Type]))

#geo_US_gourds <- gourds |> filter(country == "United States") |> drop_na(city, state_prov, country) |> geocode(city = city, state = state_prov, country = country, method = "osm")

#write.csv(geo_US_gourds, "./data/geo_US_gourds.csv", row.names = FALSE)

geo_US_gourds <- read_csv("./data/geo_US_gourds.csv") |> mutate(place_weight = weight_func(as.numeric(place))) |> drop_na(place) |> arrange(desc(place))

geo_US_gourds <- geo_US_gourds |> mutate(annual_sunlight_hours = sunshine_func(lat), summer_sunlight_hours = sunshine_func(lat, start_day=120, end_day=249)) #May 1st - Oct 1st

gourds_summary <- gourds |> 
  group_by(Gourd_Type, year_bk) |> 
  summarize(max_weight = max(weight_lbs))
