library(tidyverse)
library(wbstats)
library(rvest)
library(corrr)
library(scales)
library(stargazer)

world_bank_data <- wb(indicator = c("ST.INT.ARVL", "NY.GDP.PCAP.PP.KD", "EN.ATM.CO2E.KT", "TX.VAL.FOOD.ZS.UN", "SP.POP.TOTL", "PRJ.MYS.25UP.MF", "SP.POP.1564.TO.ZS", "SP.URB.TOTL.IN.ZS", "NV.IND.MANF.ZS"),
   country = "countries_only",
   startdate = "2010",
   enddate = "2010",
   removeNA = FALSE,
   return_wide = TRUE) %>%
  as_tibble() %>%
  rename(co2_emissions = EN.ATM.CO2E.KT, gdp_pc_ppp = NY.GDP.PCAP.PP.KD, tourism = ST.INT.ARVL, food_exports = TX.VAL.FOOD.ZS.UN, population = SP.POP.TOTL, schooling_years_mean = PRJ.MYS.25UP.MF, working_age_population = SP.POP.1564.TO.ZS, urban_population_pct = SP.URB.TOTL.IN.ZS, manufacturing = NV.IND.MANF.ZS) %>%
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
  mutate(country = replace(country, which(country == "Timor-Leste"), "East Timor")) %>%
  mutate(country = replace(country, which(country == "The Gambia"), "Gambia")) %>%
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
  mutate(country = if_else(country %in% c("Scotland", "England", "Wales", "Northern Ireland"), "United Kingdom", country)) %>%
  group_by(country, region) %>%
  summarize(veg_places = sum(veg_places)) %>%
  ungroup() %>%
  inner_join(world_bank_data, by = "country") %>%
  mutate(veg_pc = (veg_places / population)) %>%
  rename(country_code = iso3c) %>%
  select(-iso2c) %>%
  select(country, country_code, date, veg_pc, veg_places, everything()) %>%
  drop_na()

# ggplot theme setup and charts
theme_awesome <- theme_minimal() +
  theme(plot.title = element_text(hjust = 0, face = "bold", family = "mono", size = 32, margin = margin(1, 1, 20, 1)),
        axis.title = element_text(face = "bold", family = "mono", size = 25),
        axis.text = element_text(family = "mono", face = "bold", size = 14),
        axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
        plot.margin = margin(25, 25, 25, 25),
        legend.title = element_text(family = "mono", face = "bold", size = 15),
        legend.title.align = 0,
        legend.text = element_text(family = "mono", face = "bold", size = 13, margin = margin(1, 1, 1, 100)),
        legend.key.size = unit(1, "cm"),
        legend.margin = margin(0, 30, 30, 20),
        axis.line = element_blank(),
        axis.ticks.length = unit(0.7, "cm"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        strip.text = element_text(family = "mono", face = "bold", size = 16, margin = margin(0, 0, 10, 0)),
        strip.background = element_rect(fill = "white", color = "black"),
        plot.subtitle = element_text(size = 18, face = "bold", family = "mono", hjust = 0, margin = margin(0, 0, 30, 0)))

mydata %>%
  ggplot(aes(x = veg_pc * 10000)) +
  geom_histogram(fill = "darkmagenta", bins = 15) +
  labs(title = "Histogram of Vegetarian Restaurants per 10,000 people", subtitle = "Without Logarithmic Transformation", x = "# of Vegetarian Restaurants per 10,000 People", y = "Frequency") + 
  theme_awesome +
  theme(panel.grid.major.y = element_line(color = "grey"),
        panel.grid.major.x = element_line(color = "grey"))

mydata %>%
  ggplot(aes(x = log(veg_pc * 10000))) +
  geom_histogram(fill = "maroon", bins = 15) +
  labs(title = "Histogram of Vegetarian Restaurants per 10,000 people", subtitle = "With Natural Logarithmic Transformation", x = "# of Vegetarian Restaurants per 10,000 People (log scale)", y = "Frequency") + 
  theme_awesome +
  theme(panel.grid.major.y = element_line(color = "grey"),
        panel.grid.major.x = element_line(color = "grey"))

mydata %>%
  ggplot(aes(x = (gdp_pc_ppp))) +
  geom_histogram(fill = "darkmagenta", bins = 15) +
  labs(title = "Histogram of GDP (per capita, PPP)", subtitle = "Without Logarithmic Transformation", x = "GDP (per capita, PPP, in 2011 International $)", y = "Frequency") + 
  scale_x_continuous(labels = scales::dollar) +
  theme_awesome +
  theme(panel.grid.major.y = element_line(color = "grey"),
        panel.grid.major.x = element_line(color = "grey"))

mydata %>%
  ggplot(aes(x = log(gdp_pc_ppp))) +
  geom_histogram(fill = "maroon", bins = 15) +
  labs(title = "Histogram of GDP (per capita, PPP)", subtitle = "With Natural Logarithmic Transformation", x = "GDP (per capita, PPP, in 2011 International $, log scale)", y = "Frequency") + 
  theme_awesome +
  theme(panel.grid.major.y = element_line(color = "grey"),
        panel.grid.major.x = element_line(color = "grey"))

mydata %>%
  ggplot(aes(x = (co2_emissions))) +
  geom_histogram(fill = "darkmagenta", bins = 15) +
  labs(title = "Histogram of CO2 Emissions", subtitle = "Without Logarithmic Transformation", x = "CO2 Emissions (kt)", y = "Frequency") + 
  scale_x_continuous(labels = scales::comma) +
  theme_awesome +
  theme(panel.grid.major.y = element_line(color = "grey"),
        panel.grid.major.x = element_line(color = "grey"),
        axis.ticks.length = unit(0.5, "cm"))

mydata %>%
  ggplot(aes(x = log(co2_emissions))) +
  geom_histogram(fill = "maroon", bins = 15) +
  labs(title = "Histogram of CO2 Emissions", subtitle = "With Natural Logarithmic Transformation", x = "CO2 Emissions (kt, log scale)", y = "Frequency") + 
  theme_awesome +
  theme(panel.grid.major.y = element_line(color = "grey"),
        panel.grid.major.x = element_line(color = "grey"))

mydata %>%
  rename_all(funs(str_trunc(names(mydata), width = 13))) %>%  
  select(-c(country, country_code, date, veg_places, region, population)) %>%
  correlate() %>%
  shave() %>%
  rplot(legend = TRUE, colors = c("red", "white", "navy"), shape = 15) +
  theme_awesome +
  theme(axis.line = element_line(color = "grey")) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2), alpha = guide_legend(order = 2)) +
  labs(title = "Correlations Between the Variables", subtitle = "Without Logarithmic Transformation", alpha = "Strength", size = "Strength", color = "Sign")
  
mydata %>%
  rename_all(funs(str_trunc(names(mydata), width = 13))) %>%  
  select(-c(country, country_code, date, veg_places, region, population)) %>%
  mutate_all(funs(log)) %>%
  correlate() %>%
  shave() %>%
  rplot(legend = TRUE, colors = c("red", "white", "navy"), shape = 15) +
  theme_awesome +
  theme(axis.line = element_line(color = "grey")) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2), alpha = guide_legend(order = 2)) +
  labs(title = "Correlations Between the Variables", subtitle = "With Natural Logarithmic Transformation", alpha = "Strength", size = "Strength", color = "Sign")

# Summary statistics and Models
# FYI in all stargazer files, I manually edit HTML file with "cellpadding = 5" in the table header
mydata %>% 
  data.frame() %>%
  stargazer(out = "veg_summary_stats.html",
            title = "Summary Statistics",
            covariate.labels = c("Vegetarian Restaurants (per capita)",
                                 "Vegetarian Restaurants (Total)",
                                 "CO2 Emissions (kt)",
                                 "Manufacturing Value Added (% of GDP)",
                                 "GDP (per capita, PPP)",
                                 "Mean Years of Schooling (Projected 25+)",
                                 "Population Ages 15-64 (% of Total Population)",
                                 "Total Population",
                                 "Urban Population (% of Total Population)",
                                 "Tourism (# of arrivals)",
                                 "Food Exports (% of Merchandise Exports)"))

# Vegetarian Restaurants Model
veg_regression <- lm(log(veg_pc) ~ log(gdp_pc_ppp) + log(tourism) + log(schooling_years_mean) + log(urban_population_pct) + log(working_age_population) + region, data = mydata)

veg_regression %>% 
  stargazer::stargazer(out = "veg_reg.html",
                       title = "Regression Model 1 Results",
                       dep.var.labels = "Vegetarian Restaurants (per capita, natural log.)",
                       covariate.labels = c("GDP (per capita, PPP, natural log.)",
                                            "Tourism (# of arrivals, natural log.)",
                                            "Mean Years of Schooling (Projected 25+, natural log.)",
                                            "Urban Population (% of Total Population, natural log.)",
                                            "Population Ages 15-64 (% of Total Population, natural log.)",
                                            "Asia", 
                                            "Australia",
                                            "Europe",
                                            "North America",
                                            "South America"))

# CO2 Emissions Model
co2_regression <- lm(log(co2_emissions) ~ poly(log(gdp_pc_ppp), 2) + log(veg_pc) + log(food_exports) + log(tourism) + log(schooling_years_mean) + log(urban_population_pct) + log(working_age_population) + log(manufacturing) + region, data = mydata)

co2_regression %>%
  stargazer::stargazer(out = "co2_reg.html",
                       title = "Regression Model 2",
                       dep.var.labels = "CO2 Emissions (kt, natural log.)",
                       covariate.labels = c("GDP (per capita, PPP, natural log.)",
                                            "Squared GDP (per capita, PPP, natural log.)",
                                            "Vegetarian Restaurants (per capita, natural log.)",
                                            "Food Exports (% of Merchandise Exports, natural log.)",
                                            "Tourism (# of arrivals, natural log.)",
                                            "Mean Years of Schooling (Projected 25+, natural log.)",
                                            "Urban Population (% of Total Population, natural log.)",
                                            "Population Ages 15-64 (% of Total Population, natural log.)",
                                            "Manufacturing Value Added (% of GDP, natural log.)",
                                            "Asia", 
                                            "Australia",
                                            "Europe",
                                            "North America",
                                            "South America"))
