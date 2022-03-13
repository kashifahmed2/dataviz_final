library(tidyverse)
library(haven)
setwd("~/Documents/GitHub/dataviz_final/data/")


rm(list=ls())

df_raw <- read.csv("worldbank_clean.csv")

vars <- df %>%
  group_by(variable) %>%
  summarise(n=n())
df <- df_raw
df$variable[df$variable=="GDP growth (annual %)"] <- "gdp_growth"
df$variable[df$variable=="GNI per capita, Atlas method (current US$)"] <- "gni_per_cap"
df$variable[df$variable=="Population growth (annual %)"] <- "pop_growth_pct"
df$variable[df$variable=="Population, female (% of total population)"] <- "pop_fem_pct"
df$variable[df$variable=="Population, female"] <- "pop_female"
df$variable[df$variable=="Population ages 0-14, female"] <- "pop0to14_female"
df$variable[df$variable=="Population ages 0-14, male"] <- "pop0to14_male"
df$variable[df$variable=="Population ages 0-14, total"] <- "pop0to14"
df$variable[df$variable=="Population ages 15-64, female"] <- "pop15to64_female"
df$variable[df$variable=="Population ages 15-64, male"] <- "pop15to64_male"
df$variable[df$variable=="Population ages 15-64, total"] <- "pop15to64"
df$variable[df$variable=="Population ages 65 and above, female"] <- "pop65up_female"
df$variable[df$variable=="Population ages 65 and above, male"] <- "pop65up_male"
df$variable[df$variable=="Population ages 65 and above, total"] <- "pop65up"
df$variable[df$variable=="Population, male (% of total population)"] <- "pop_male_pct"
df$variable[df$variable=="Population, male"] <- "pop_male"
df$variable[df$variable=="Population, total"] <- "pop_total"
df$variable[df$variable=="Population ages 15-64 (% of total population)"] <- "pop_working_pct"
df$variable[df$variable=="Total greenhouse gas emissions (% change from 1990)"] <- "gg_emissions_pct_change"
df$variable[df$variable=="Total greenhouse gas emissions (kt of CO2 equivalent)"] <- "gg_emissions_kt"
df$variable[df$variable=="Death rate, crude (per 1,000 people)"] <- "death_rate"
df$variable[df$variable=="Birth rate, crude (per 1,000 people)"] <- "birth_rate"
df$variable[df$variable=="Agriculture, forestry, and fishing, value added (annual % growth)"] <- "agri_growth"
df$variable[df$variable=="Industry (including construction), value added (annual % growth)"] <- "industry_growth"
df$variable[df$variable=="Manufacturing, value added (annual % growth)"] <- "manufacturing_growth"
df$variable[df$variable=="Services, value added (annual % growth)"] <- "services_growth"
df$variable[df$variable=="Exports of goods and services (annual % growth)"] <- "exports_growth"
df$variable[df$variable=="Imports of goods and services (annual % growth)"] <- "imports_growth"

list <- c("gdp_growth", "gni_per_cap", "pop_growth_pct", "pop_fem_pct", "pop_female", "pop_female",
          "pop_male_pct", "pop_male", "pop_total", "pop_working_pct", "gg_emissions_pct_change", 
          "gg_emissions_kt", "death_rate", "birth_rate", "agri_growth", "industry_growth",
          "manufacturing_growth", "services_growth", "exports_growth", "imports_growth",
          "pop0to14_female", "pop0to14_male", "pop0to14", "pop15to64_female", 
          "pop15to64_male", "pop15to64", "pop65up_male", "pop65up_female", 
          "pop65up")

df <- df %>% filter(variable %in% list)


df1 <- df %>%
  filter(variable=="gdp_growth" | 
           variable=="gni_per_cap" | 
           variable=="pop_total" | 
           variable=="imports_growth" | 
           variable=="exports_growth")
df1$value <- as.numeric(df1$value)

library(reshape)
df1 <- cast(df1, year + countrycode ~variable)

df1 <- df1 %>%
  mutate(trade = ifelse(imports_growth<exports_growth, "Faster Exporting Growth", 
                        "Faster Importing Growth"))

df1$pop_total <- df1$pop_total/1000000

regions <- df %>% 
  group_by(countrycode, region) %>%
  summarise(n=n()) %>%
  select(-n)

df1 <- left_join(df1, regions, by = "countrycode")
df1$era[df1$year<2014] <- "2011-2014"
df1$era[df1$year>=2014] <- "2014-2017"
df1$era[df1$year>2018] <- "2018-2020"


ggplot(data = df1, aes(x=gni_per_cap, y=gdp_growth, 
                          size = pop_total, color = factor(era)))+
           geom_point(alpha=0.5) +
  geom_hline(yintercept=0,linetype=3) +
  geom_vline(xintercept=1479,linetype=2) +
  scale_x_continuous(labels=scales::dollar_format()) +
  scale_y_continuous(name="GDP Growth (%)", limits=c(-30, 30)) +
  labs(title = "Sub-Saharan African Growth in the 2010s",
       subtitle = "Comparing Regions",
       caption = "Source: World Bank Data",
       x = "Gross National Income Per Capita",
       y = "GDP Growth %",
       size = "Population in Millions",
       color = "Year") +  
  facet_grid(region~ .) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum()

df_pop <- df %>% 
  filter(grepl('pop', variable)) %>%
  filter(!grepl('pct', variable)) %>%
  filter(grepl('male|female', variable)) %>%
  filter(variable!='pop_female') %>%
  filter(variable!='pop_male')


df_pop$value <- as.numeric(df_pop$value)

df_pop <- df_pop %>% 
  group_by(year, variable) %>%
  summarise(value= sum(value, na.rm = TRUE))


df_pop$year <- as.character(df_pop$year)
df_pop$value <- df_pop$value/1000000

df_pop$variable[df_pop$variable=="pop0to14_male"] <- "0-14 Males"
df_pop$variable[df_pop$variable=="pop0to14_female"] <- "0-14 Females"
df_pop$variable[df_pop$variable=="pop15to64_male"] <- "15-64 Males"
df_pop$variable[df_pop$variable=="pop15to64_female"] <- "15-64 Females"
df_pop$variable[df_pop$variable=="pop65up_female"] <- "65+ Females"
df_pop$variable[df_pop$variable=="pop65up_male"] <- "65+ Males"

ggplot(df_pop, aes(fill=variable, y=value, x=year)) + 
  geom_bar(position="stack", stat="identity", color="black") +
  labs(title = "Population Growth in Sub-Saharan Africa",
       subtitle = "2011-2020",
       caption = "source: World Bank Data",
       x = "Year",
       y = "Population in Millions",
       fill = "Age Group") +
  theme_minimal() + scale_fill_brewer(palette = "Set2")

df_gg <- df %>%
  filter(variable == "gg_emissions_pct_change" | 
          variable == "gg_emissions_kt" |
           variable == "pop_total" | variable == "gni_per_cap")

df_gg$value <- as.numeric(df_gg$value)

df_gg<- cast(df_gg, year + countrycode ~variable)
df_gg$gg_kt_per_cap <- df_gg$gg_emissions_kt/df_gg$pop_total
df_gg$co2ton_pp <- df_gg$gg_kt_per_cap*1000

df_gg <- df_gg %>% arrange(countrycode) %>%
  select(-gg_emissions_pct_change)
df_gg <- df_gg %>% filter(!is.na(gg_kt_per_cap)) 

df_gg <- df_gg %>%
  group_by(countrycode) %>%
  summarise(co2ton_pp = mean(co2ton_pp, na.rm=TRUE),
            gni_per_cap = mean(gni_per_cap, na.rm=TRUE)) 


write_csv(df_gg, "df_gg.csv")
df_ind <- df %>%
  filter(variable == "agri_growth"|
          variable == "industry_growth" | 
          variable == "manufacturing_growth" |
          variable == "services_growth" |
          variable == "exports_growth" |
          variable == "imports_growth") # | variable == "pop_total" |
          # variable == "gni_per_cap")

df_ind$value <- as.numeric(df_ind$value)
df_ind <- df_ind %>% group_by(year, variable, region) %>%
  summarise(value = mean(value, na.rm = TRUE))

df_ind$variable[df_ind$variable=="agri_growth"] <- "Agriculture"
df_ind$variable[df_ind$variable=="exports_growth"] <- "Exports"
df_ind$variable[df_ind$variable=="imports_growth"] <- "Imports"
df_ind$variable[df_ind$variable=="industry_growth"] <- "Industry"
df_ind$variable[df_ind$variable=="manufacturing_growth"] <- "Manufacturing"
df_ind$variable[df_ind$variable=="services_growth"] <- "Services"


ggplot(df_ind, aes(x = year, y = value, color = factor(region))) +
  geom_line() +
  geom_hline(yintercept=0,linetype=3) +
  scale_x_continuous(labels = label_number(accuracy = 1)) +
  scale_y_continuous(limits=c(-20, 20)) +
  labs(title = "Sub-Saharan African Growth in the 2010s",
       subtitle = "Comparing Sectors",
       caption = "Source: World Bank Data",
       x = "Year",
       y = "Annual % Growth",
       color = "Region") +  
  facet_wrap(variable~ .) +
  theme_ipsum()

df_ind <- cast(df_ind, year + countrycode ~variable)

  
