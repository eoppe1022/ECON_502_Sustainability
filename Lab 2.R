library(dplyr)
library(haven)
library(tidyr)
library(stringr)
library(stargazer)
library(ggplot2)
library(scales)

gdp_data <- haven::read_dta("GDPpc_1990_2013.dta") %>%
  gather(year, gdp_pc, GDPpc1990:GDPpc2013) %>%
  mutate(year = str_replace(year, "GDPpc", ""))

carbon_data <- haven::read_dta("CO2pc_1990_2013.dta") %>%
  gather(year, CO2_pc, CO2pc1990:CO2pc2013) %>%
  mutate(year = str_replace(year, "CO2pc", ""))

unbalanced_data <- gdp_data %>%
  inner_join(carbon_data, by = c("CountryName", "CountryCode", "year")) %>%
  drop_na()

# need to make sure 24 years of data
balanced_data <- unbalanced_data %>%
  group_by(CountryCode) %>%
  summarize(num_years_data = n()) %>%
  filter(num_years_data == 24) %>%
  inner_join(unbalanced_data, by = "CountryCode") %>%
  select("CountryName", "CountryCode", "year", "gdp_pc", "CO2_pc")

time_average_balanced_data <- balanced_data %>%
  group_by(CountryCode) %>%
  summarize(average_gdp_pc = mean(gdp_pc), 
            average_CO2_pc = mean(CO2_pc))

time_average_unbalanced_data <- unbalanced_data %>%
  group_by(CountryCode) %>%
  summarize(average_gdp_pc = mean(gdp_pc), 
            average_CO2_pc = mean(CO2_pc))


panel_model_balanced <- lm(log(CO2_pc) ~ poly(log(gdp_pc), 2) + year + CountryCode, data = balanced_data)
panel_model_unbalanced <- lm(log(CO2_pc) ~ poly(log(gdp_pc), 2) + year + CountryCode, data = unbalanced_data)
avg_model_balanced <- lm(average_CO2_pc ~ poly(average_gdp_pc, 2), data = time_average_balanced_data)
avg_model_unbalanced <- lm(average_CO2_pc ~ poly(average_gdp_pc, 2), data = time_average_unbalanced_data)

# balanced avg turning point (in $) -- using coefficients from panel
(-45.734423)/(2 * -16.202700)

# unbalanced avg turning point (in $) -- using coefficients from panel
(-52.180275)/(2 * -35.916682)

# balanced panel turning point (in $) -- using coefficients from panel
exp((-67.956399831)/(2 * -9.233505296))

# unbalanced panel turning point (in $) -- using coefficients from panel
exp((-62.89691890)/(2 * -11.53222817))

balanced_summary <- balanced_data %>%
  as.data.frame() %>%
  select("GDP per Capita (U.S. $)" = gdp_pc, "Carbon Dioxide Emissions (metric tons per capita)" = CO2_pc) %>%
  stargazer(out = "balanced_summary.html")

unbalanced_summary <- unbalanced_data %>%
  as.data.frame() %>%
  select("GDP per Capita (U.S. $)" = gdp_pc, "Carbon Dioxide Emissions (metric tons per capita)" = CO2_pc) %>%
  stargazer(out = "unbalanced_summary.html")

time_avg_balanced_summary <- time_average_balanced_data %>%
  as.data.frame() %>%
  select("Average GDP per Capita (U.S. $)" = average_gdp_pc, "Average Carbon Dioxide Emissions (metric tons per capita)" = average_CO2_pc) %>%
  stargazer(out = "time_avg_balanced_summary.html")

time_avg_unbalanced_summary <- time_average_unbalanced_data %>%
  as.data.frame() %>%
  select("Average GDP per Capita (U.S. $)" = average_gdp_pc, "Average Carbon Dioxide Emissions (metric tons per capita)" = average_CO2_pc) %>%
  stargazer(out = "time_avg_unbalanced_summary.html")

balanced_scatter <- ggplot(data = time_average_balanced_data) +
  aes(x = average_gdp_pc, y = average_CO2_pc) + 
  geom_point(color = "darkmagenta") +
  labs(x = "GDP per Capita (U.S. $)", y = "CO2 Emissions per Capita (metric tons)", title = "CO2 Emissions vs. GDP (1990 - 2013)", subtitle = "Balanced Data") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "mono", size = 35, margin = margin(1,1,20,1)),
        axis.title = element_text(face = "bold", family = "mono", size = 25),
        axis.text = element_text(family = "mono", face = "bold", size = 20),
        axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
        plot.margin = margin(25, 25, 25, 25),
        legend.title = element_blank(),
        legend.text = element_text(family = "mono", face = "bold", size = 18, margin = margin(1, 1, 1, 100)),
        legend.key.size = unit(1, "cm"),
        plot.subtitle = element_text(size = 16, face = "bold", family = "mono", hjust = 0.5, margin = margin(0,0,30,0)))


unbalanced_scatter <- ggplot(data = time_average_unbalanced_data) +
  aes(x = average_gdp_pc, y = average_CO2_pc) + 
  geom_point(color = "maroon") +
  labs(x = "GDP per Capita (U.S. $)", y = "CO2 Emissions per Capita (metric tons)", title = "CO2 Emissions vs. GDP (1990 - 2013)", subtitle = "Unbalanced Data") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "mono", size = 35, margin = margin(1,1,20,1)),
        axis.title = element_text(face = "bold", family = "mono", size = 25),
        axis.text = element_text(family = "mono", face = "bold", size = 20),
        axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
        plot.margin = margin(25, 25, 25, 25),
        legend.title = element_blank(),
        legend.text = element_text(family = "mono", face = "bold", size = 18, margin = margin(1, 1, 1, 100)),
        legend.key.size = unit(1, "cm"),
        plot.subtitle = element_text(size = 16, face = "bold", family = "mono", hjust = 0.5, margin = margin(0,0,30,0)))




