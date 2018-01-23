library(dplyr)
library(readr)
library(corrr)
library(stargazer)

mortality <- read_csv("mortality rate infant 2014.csv")
health_exp <- read_csv("health expenditure 2014.csv")
gdp <- read_csv("GDP per capita 2014.csv")

mydata <- mortality %>%
  inner_join(health_exp, by = c("Country Name", "Country Code")) %>%
  inner_join(gdp, by = c("Country Name", "Country Code"))

mydata <- mydata %>%
  select(country_name = `Country Name`, country_code = `Country Code`, mortality_rate = `2014.x`, health_exp = `2014.y`, gdp = `2014`) %>%
  mutate(mortality_log = log(mortality_rate),
         health_exp_log = log(health_exp),
         gdp_log = log(gdp))

plot <- mydata %>%
  select(mortality_rate, health_exp, gdp) %>%
  correlate() %>%
  network_plot()

model_1 <- lm(mortality_rate ~ gdp_log, data = mydata)

model_1 %>% summary()

# exports results table to working directory
stargazer(model_1, title = "Results", align = TRUE, out = "table1.htm")
