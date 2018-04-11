library(tidyverse)
library(rvest)
library(twidlr)

world_bank_data <- read_csv("world_bank_data.csv") %>%
  select(country = "Country Name", country_code = "Country Code",
         gdp_pc_ppp = "2016 [YR2016] - GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]",
         export_pct_gdp = "2016 [YR2016] - Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]",
         food_export_pct_merch = "2016 [YR2016] - Food exports (% of merchandise exports) [TX.VAL.FOOD.ZS.UN]",
         fuel_export_pct_merch = "2016 [YR2016] - Fuel exports (% of merchandise exports) [TX.VAL.FUEL.ZS.UN]",
         suicide_rate = "2016 [YR2016] - Suicide mortality rate (per 100,000 population) [SH.STA.SUIC.P5]",
         male_pop_pct = "2016 [YR2016] - Population, male (% of total) [SP.POP.TOTL.MA.ZS]",
         female_pop_pct = "2016 [YR2016] - Population, female (% of total) [SP.POP.TOTL.FE.ZS]",
         population = "2016 [YR2016] - Population, total [SP.POP.TOTL]",
         ghg_emissions = "2016 [YR2016] - Total greenhouse gas emissions (kt of CO2 equivalent) [EN.ATM.GHGT.KT.CE]",
         co2_emissions = "2016 [YR2016] - CO2 emissions (kt) [EN.ATM.CO2E.KT]") %>%
  mutate_at(vars(-country, -country_code), funs(as.numeric)) %>%
  slice(1:217) %>%
  mutate(country = replace(country, which(country == "Antigua and Barbuda"), "Antigua")) %>%
  mutate(country = replace(country, which(country == "Bahamas, The"), "Bahamas")) %>%
  mutate(country = replace(country, which(country == "St. Lucia"), "Saint Lucia")) %>%
  mutate(country = replace(country, which(country == "Sint Maarten (Dutch part)"), "Saint Maarten")) %>%
  mutate(country = replace(country, which(country == "St. Vincent and the Grenadines"), "Saint Vincent And The Grenadines")) %>%
  mutate(country = replace(country, which(country == "Trinidad and Tobago"), "Trinidad And Tobago")) %>%
  mutate(country = replace(country, which(country == "Turks and Caicos Islands"), "Turks And Caicos")) %>%
  mutate(country = replace(country, which(country == "United States"), "USA")) %>%
  mutate(country = replace(country, which(country == "Virgin Islands (U.S.)"), "Us Virgin Islands")) %>%
  mutate(country = replace(country, which(country == "Bosnia and Herzegovina"), "Bosnia And Herzegovina")) %>%
  mutate(country = replace(country, which(country == "Isle of Man"), "Isle Of Man")) %>%
  mutate(country = replace(country, which(country == "Macedonia, FYR"), "Macedonia")) %>%
  mutate(country = replace(country, which(country == "Russian Federation"), "Russia")) %>%
  mutate(country = replace(country, which(country == "Slovak Republic"), "Slovakia")) %>%
  mutate(country = replace(country, which(country == "Brunei Darussalam"), "Brunei")) %>%
  mutate(country = replace(country, which(country == "Iran, Islamic Rep."), "Iran")) %>%
  mutate(country = replace(country, which(country == "Kyrgyz Republic"), "Kyrgyzstan")) %>%
  mutate(country = replace(country, which(country == "Lao PDR"), "Laos")) %>%
  mutate(country = replace(country, which(country == "West Bank and Gaza"), "Palestine")) %>%
  mutate(country = replace(country, which(country == "Korea, Rep."), "South Korea")) %>%
  mutate(country = replace(country, which(country == "Venezuela, RB"), "Venezuela")) %>%
  mutate(country = replace(country, which(country == "Cabo Verde"), "Cape Verde")) %>%
  mutate(country = replace(country, which(country == "Egypt, Arab Rep."), "Egypt")) %>%
  mutate(country = replace(country, which(country == "Cote d'Ivoire"), "Ivory Coast")) %>%
  mutate(country = replace(country, which(country == "Gambia, The"), "The Gambia"))
  
  
url <- c("https://www.happycow.net/north_america/",
         "https://www.happycow.net/europe/",
         "https://www.happycow.net/australia/",
         "https://www.happycow.net/asia/",
         "https://www.happycow.net/south_america/",
         "https://www.happycow.net/africa/")

# different CSS selector for NA and Europe
get_data_na_europe <- function(country_num) { 
  read_html(url[country_num]) %>%
    html_nodes("p+ .panel--region .list__item") %>%
    html_text() %>% 
    str_split("'") %>% 
    unlist() %>% 
    as_tibble() %>%
    mutate(country = str_replace(value, "\\s*\\([^\\)]+\\)", ""), veg_places = str_replace(value, ".*\\((.*)\\).*", "\\1")) %>%
    select(-value)}


# Different for Latter 4
get_data_else <- function(country_num) { 
  read_html(url[country_num]) %>%
    html_nodes(".list--margin .list__item") %>%
    html_text() %>% 
    str_split("'") %>% 
    unlist() %>% 
    as_tibble() %>%
    mutate(country = str_replace(value, "\\s*\\([^\\)]+\\)", ""), veg_places = str_replace(value, ".*\\((.*)\\).*", "\\1")) %>%
    select(-value)}

veg_data <- map_df(1:2, get_data_na_europe) %>% 
  bind_rows(map_df(3:6, get_data_else)) %>%
  mutate(veg_places = as.numeric(veg_places)) %>%
  distinct()

my_data <- veg_data %>%
  inner_join(world_bank_data, by = "country") %>%
  mutate(veg_pc = veg_places / population)

my_data %>% select(country, veg_pc, veg_places, population) %>% arrange(desc(veg_pc))

my_data %>%
  select(-suicide_rate, -ghg_emissions, -co2_emissions) %>%
  drop_na() %>%
  lm(veg_pc ~ gdp_pc_ppp + export_pct_gdp + food_export_pct_merch + fuel_export_pct_merch +
       female_pop_pct + population) %>%
  summary()
