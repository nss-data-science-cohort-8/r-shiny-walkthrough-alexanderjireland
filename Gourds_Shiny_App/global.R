library(shiny)
library(tidyverse)
library(glue)
library(tidygeocoder)
library(leaflet)

gourds <- read.csv('./data/gourds.csv')

#F = "Field Pumpkin", P = "Giant Pumpkin", S = "Giant Squash", W = "Giant Watermelon", L = "Long Gourd", T = Tomato

gourds <- gourds |> 
  mutate(Gourd_Type = case_when(type_bk == "F"~"Field Pumpkin",
                                type_bk == "P"~"Giant Pumpkin",
                                type_bk == "S"~"Giant Squash",
                                type_bk == "W"~"Giant Watermelon",
                                type_bk == "L"~"Long Gourd",
                                type_bk == "T"~"Tomato"),
         city = as.character(city)) |> 
  geocode(city, method = "osm")

gourds_summary <- gourds |> 
  group_by(Gourd_Type, year_bk) |> 
  summarize(max_weight = max(weight_lbs))