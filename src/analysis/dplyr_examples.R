# working with dpylr using the gapminder dataset
library(tidyverse)
gapminder <- readr::read_csv(here("data/gapminder/raw/gapminder_data.csv"))

mean(gapminder$gdpPercap[gapminder$continent == "Africa"])
mean(gapminder$gdpPercap[gapminder$continent == "Americas"])


year_country_gdp <- select(gapminder, year, country, gdpPercap)

# using pipe instead
year_country_gdp <- gapminder %>%
                    filter(continent == "Europe") %>%
                    select(year, country, gdpPercap, continent)
head(year_country_gdp)                    


### produce dataframe of African life, country, year

subset_africa <- gapminder %>%
                filter(continent == "Africa") %>%
                select(year, lifeExp, country)
head(subset_africa)
dim(subset_africa)


### grouping data 
gapminder %>%
    group_by(continent) %>%
    summarize(mean_val = mean(gdpPercap))

# calc avglifeexp per country

mean_lifeExp <- gapminder %>%
    group_by(country) %>%
    summarize(mean_val = mean(lifeExp))
##Iceland is highest mean life expectancy

mean_lifeExp %>%
    filter(mean_lifeExp == min(mean_lifeExp) | mean_lifeExp == max(mean_lifeExp))


gapminder %>%
  group_by(continent) %>%
  summarize(mean_val = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap))

## can keep building levels with the pipes

# can also pipe directly into a plot

# plotting all of it
ggplot(data = gapminder, aes(x= year, y= lifeExp, color = continent)) + 
  geom_line() + facet_wrap(~country)

# filtering with dpylr


# make a plot of life expectancy in African countries
gapminder %>%
  filter(continent == "Africa") %>%
  ggplot( aes(x= year, y= lifeExp)) + 
  geom_line() + facet_wrap(~country)







