library(tidyverse)
library(tidycensus)
library(tigris)


#1.Compile ACS data------------
census_api_key("936c96236b979ae522c6cf67edb51923cd391fb3")
options(tigris_use_cache=TRUE)
#acs_2020_vars<-load_variables(year = 2020, "dpgu")

DL_Year <- 2020
survey <- "acs5"
state = "IL"
county = "Cook"


#B17001: Poverty ------------
B17001_Vars<-c("B17001_001",
               "B17001_002")

B17001 <- get_acs(geography = "tract", state = state, county = county,
                  variables = B17001_Vars, survey = survey, year = DL_Year, output = "wide", geometry = TRUE)

B17001 <-B17001 |> 
  mutate(p_poverty = B17001_002E / B17001_001E) |> 
  mutate(p_poverty = ifelse(is.nan(p_poverty), NA, p_poverty)) |>
  select(GEOID, p_poverty)


#B25106: Cost burden (housing costs 30 percent or more of household income)--------------
B25106_Vars <- c("B25106_001",
                 "B25106_006",
                 "B25106_010",
                 "B25106_014", 
                 "B25106_018", 
                 "B25106_022", 
                 "B25106_028", 
                 "B25106_032",
                 "B25106_036",
                 "B25106_040",
                 "B25106_044")

B25106 <- get_acs(geography = "tract", state = state, county = county, 
                  variables = B25106_Vars, survey = survey, year = DL_Year, output = "wide")

B25106$p_cost_burden<-(B25106$B25106_006E+
                         B25106$B25106_010E+
                         B25106$B25106_014E+
                         B25106$B25106_018E+
                         B25106$B25106_022E+
                         B25106$B25106_028E+
                         B25106$B25106_032E+
                         B25106$B25106_036E+
                         B25106$B25106_040E+
                         B25106$B25106_044E)/B25106$B25106_001E
B25106$p_cost_burden[B25106$p_cost_burden == "NaN"]<-NA

B25106 <- B25106 |>  select(GEOID, p_cost_burden)


#B22007: Receive foodstamps-------
B22007_Vars<-c("B22007_001",
               "B22007_002")
B22007 <- get_acs(geography = "tract", state = state, county = county,
                  variables = B22007_Vars, survey = survey, year = DL_Year, output = "wide")

B22007 <-B22007 |> 
  mutate(p_food_assist = B22007_002E / B22007_001E) |> 
  mutate(p_food_assist = ifelse(is.nan(p_food_assist), NA, p_food_assist)) |>
  select(GEOID, p_food_assist)


#B11003 Single Families with Children-------
B11003_Vars<-c("B11003_001",
               "B11003_010",
               "B11003_016")
B11003 <- get_acs(geography = "tract", state = state, county = county,
                  variables = B11003_Vars, survey = survey, year = DL_Year, output = "wide")
B11003 <- B11003 |> 
  mutate(p_children = (B11003_010E + B11003_016E)/B11003_001E) |> 
  mutate(p_children = ifelse(is.nan(p_children), NA, p_children)) |>
  select(GEOID, p_children)
  
#DP03 Unemployment-------
DP03_vars <- load_variables(2020, "acs5/profile", cache = TRUE) %>%
  filter(str_detect(name, "DP03")) %>%
  pull(name)
DP03 <- get_acs(geography = "tract", state = state, county = county,
                variables = DP03_vars, output = "wide")
DP03 <- DP03 |> 
  mutate(p_unemp = DP03_0005E/DP03_0003E) |> 
  mutate(p_unemp = ifelse(is.nan(p_unemp), NA, p_unemp)) |>
  select(GEOID, p_unemp)


#B08141: Drive to work ----
B08141_Vars<-c("B08141_001",
               "B08141_002")

B08141 <- get_acs(geography = "tract", state = state, county = county,
                  variables = B08141_Vars, survey = survey, year = DL_Year, output = "wide")

B08141<-B08141 |> 
  mutate(p_transit_dep = B08141_002E / B08141_001E) |> 
  mutate(p_transit_dep = ifelse(is.nan(p_transit_dep), NA, p_transit_dep)) |>
  select(GEOID, p_transit_dep)

#B01001
B01001 <- get_acs(geography = "tract", state = state, county = county,
                  variables = "B01001_001", survey = survey, year = DL_Year, output = "wide")
B01001 <-  B01001 |> select(GEOID, pop = B01001_001E)

rm(B17001_Vars,
   B25106_Vars,
   B22007_Vars, 
   B11003_Vars,
   DP03_vars,
   B08141_Vars
   )

