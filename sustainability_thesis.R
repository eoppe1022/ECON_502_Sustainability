library(tidyverse)
library(wbstats)
library(rvest)

world_bank_data <- wb(indicator = c("ST.INT.ARVL", "NY.GDP.PCAP.PP.KD", "EN.ATM.CO2E.KT", "TX.VAL.FOOD.ZS.UN", "SP.POP.TOTL", "PRJ.MYS.25UP.MF", "SP.POP.1564.TO.ZS", "SP.URB.TOTL.IN.ZS", "NV.IND.MANF.ZS"),
   country = "countries_only",
   startdate = "2010",
   enddate = "2010",
   removeNA = FALSE,
   return_wide = TRUE) %>%
  as_tibble() %>%
  rename(co2_emissions = EN.ATM.CO2E.KT, gdp_pc_ppp = NY.GDP.PCAP.PP.KD, tourism = ST.INT.ARVL, food_exports = TX.VAL.FOOD.ZS.UN, population = SP.POP.TOTL, mean_years_schooling = PRJ.MYS.25UP.MF, population_working_age = SP.POP.1564.TO.ZS, urban_population_pct = SP.URB.TOTL.IN.ZS, manufacturing = NV.IND.MANF.ZS) %>%
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
  mutate(country = replace(country, which(country == "Gambia, The"), "The Gambia")) %>%
  mutate(country = replace(country, which(country == "Micronesia, Fed. Sts."), "Micronesia"))

url <- c("https://www.happycow.net/north_america/",
         "https://www.happycow.net/europe/",
         "https://www.happycow.net/australia/",
         "https://www.happycow.net/asia/",
         "https://www.happycow.net/south_america/",
         "https://www.happycow.net/africa/")

get_data <- function(country_num) { 
  read_html(url[country_num]) %>%
    html_nodes(".region--list .list__item") %>%
    html_text() %>% 
    str_split("'") %>% 
    unlist() %>% 
    as_tibble() %>%
    mutate(country = str_replace(value, "\\s*\\([^\\)]+\\)", ""), veg_places = str_replace(value, ".*\\((.*)\\).*", "\\1"), region = str_replace_all(url[country_num], c("https://www.happycow.net/" = "", "/" = ""))) %>%
    select(-value)}

veg_data <- map_df(1:6, get_data) %>%
  mutate(veg_places = as.numeric(veg_places)) %>%
  distinct() %>%
  arrange(desc(veg_places))

mydata <- veg_data %>%
  mutate(country = if_else(country %in% c("New South Wales", "Queensland", "Australian Capital Territory", "Northern Territory", "Western Australia", "South Australia", "Tasmania", "Victoria"), "Australia", country)) %>%
  group_by(country, region) %>%
  summarize(veg_places = sum(veg_places)) %>%
  ungroup() %>%
  inner_join(world_bank_data, by = "country") %>%
  mutate(veg_pc = veg_places / population) %>%
  rename(country_code = iso3c) %>%
  select(-iso2c) %>%
  select(country, country_code, date, veg_pc, veg_places, everything())

mydata %>%
  drop_na() %>%
  twidlr::lm(log(veg_pc) ~ log(gdp_pc_ppp) + log(tourism) + log(mean_years_schooling) + log(urban_population_pct) + log(population_working_age) + region) %>%
  summary()

mydata %>%
  drop_na() %>%
  twidlr::lm(log(co2_emissions) ~ poly(log(gdp_pc_ppp), 2) + log(veg_pc) + log(food_exports) + log(tourism) + log(mean_years_schooling) + log(urban_population_pct) + log(population_working_age) + log(manufacturing) + region) %>%
  summary()
