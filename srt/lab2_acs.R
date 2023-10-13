
library(tidycensus)
library(tigris)
library(sf)
library(tidyverse)

census_api_key("936c96236b979ae522c6cf67edb51923cd391fb3")

vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012",
  Totpop = "B03002_001"
)

cook_race_wide <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = vars,
  year = 2020,
  output = "wide", 
  geometry = TRUE) 

chi_place <- places(state = "IL", year = 2020) |> 
  filter(NAME == "Chicago")

library(here)
regions <- st_read(here("data", "regions", "regions.shp"))

# Convert the projection
regions <- regions |> st_transform(4269)

# Perform the intersection
chi_race_region <- st_intersection(cook_race_wide, regions)

chi_race_region <- cook_race_wide |>
  st_intersection(regions) |> 
  select(GEOID, ends_with("E"), region_nam) |> 
  rename(White = WhiteE,
         Black = BlackE,
         Native = NativeE,
         Asian = AsianE,
         HIPI = HIPIE,
         Hispanic = HispanicE,
         Totpop = TotpopE,
         Region = region_nam)

grouped <- chi_race_region |>  
  st_drop_geometry()|> 
  group_by(Region) |>  
  summarise(
    sum_White = sum(White, na.rm = TRUE),
    sum_Black = sum(Black, na.rm = TRUE),
    sum_Native = sum(Native, na.rm = TRUE),
    sum_Asian = sum(Asian, na.rm = TRUE),
    sum_HIPI = sum(HIPI, na.rm = TRUE),
    sum_Hispanic = sum(Hispanic, na.rm = TRUE),
    sum_Totpop = sum(Totpop, na.rm = TRUE),
  )

chi_race_region <- 
  left_join(chi_race_region, grouped, by= "Region") |> 
  st_drop_geometry()

rm(list=ls()[! ls() %in% c("chi_race_region")])
