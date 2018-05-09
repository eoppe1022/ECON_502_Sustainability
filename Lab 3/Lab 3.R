library(tidyverse)
library(haven)
library(knitr)
library(kableExtra)
library(broom)
library(twidlr) # if not installed, devtools::install_github("drsimonj/twidlr")


mydata <- haven::read_dta("LAB3.dta") %>%
  mutate(natenvir = ifelse(natenvir == 1, 1, 0),
         partyid = as_factor(partyid),
         age = as.numeric(age),
         childs = as.numeric(childs),
         degree = as_factor(degree),
         income = as_factor(income),
         marital = as_factor(marital),
         race = as_factor(race),
         sex = as_factor(sex),
         wrkstat = as_factor(wrkstat))


mydata %>% 
  summary() %>%
  kable(format = "html") %>%
  kable_styling()

table(mydata$partyid, mydata$natenvir) %>%
  kable(format = "html", row.names = TRUE) %>%
  kable_styling()

mydata %>%
  drop_na() %>%
  lm(natenvir ~ poly(age, 2) + degree + marital + partyid + race) %>%
  tidy() %>%
  kable(format = "html") %>%
  kable_styling()

mydata %>%
  drop_na() %>%
  lm(natenvir ~ poly(age, 2) + degree + marital + partyid + race) %>%
  glance() %>%
  kable(format = "html") %>%
  kable_styling()

mydata %>%
  drop_na() %>%
  lm(natenvir ~ poly(age, 2) + degree + marital + partyid + race) %>%
  predict(mydata) %>%
  summary() %>%
  tidy() %>%
  kable(format = "html") %>%
  kable_styling()
  
  
