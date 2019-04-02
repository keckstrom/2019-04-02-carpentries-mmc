## tidyr demo
str(gapminder)
skimr::skim(gapminder)

# Create gap_wide
gap_wide <- gapminder %>%
  gather(key = 'key', value = 'value', c('pop', 'lifeExp', 'gdpPercap')) %>%
  mutate(year_var = paste(key, year, sep = '_')) %>%
  select(country, continent, year_var, value) %>%
  spread(key = 'year_var', value = 'value')


gap_long <- gap_wide %>%
  gather(obstype_year, obs_values, starts_with('pop'),
         starts_with('lifeExp'), starts_with('gdpPercap'))
str(gap_long)
skimr::skim(gap_long)

gap_long <- gap_wide %>% gather(obstype_year,obs_values,-continent,-country)
str(gap_long)

gap_long <- gap_long %>%
  separate(obstype_year, into = c('obs_type', 'year'), sep='_') %>%
    mutate(year = as.numeric(year))


gap_summary <- gap_long %>%
  group_by(continent, obs_type) %>%
  summarize(means = mean(obs_values))
head(gap_summary)

new_sum <-gap_long %>% 
  group_by(continent,obs_type) %>%
  summarize(means=mean(obs_values))
head(new_sum)



## spread
gap_normal <- gap_long %>%
              spread(obs_type,obs_values)
head(gap_normal)

#reorder based on gapminder
gap_normal <- gap_normal[,names(gapminder)]


all_equal(gap_normal, gapminder, ignore_col_order = T)

gap_temp <- gap_long %>%
  unite(var_ID, continent, country, sep = '_')
skimr::skim(gap_temp)

head(gap_temp)
# concatenates continent and country to create var_ID
gap_temp <- gap_long %>%
  unite(ID_var,continent,country,sep="_") %>%
  unite(var_names,obs_type,year,sep="_")
str(gap_temp)


gap_wide_new <- gap_long %>%
  unite(ID_var,continent,country,sep="_") %>%
  unite(var_names,obs_type,year,sep="_") %>%
  spread(var_names,obs_values)
str(gap_wide_new)
  
head(gap_wide_new)

## make it even WIDER

gap_super_wide <- gap_long %>%
  unite(var_names,obs_type,year,country,sep="_") %>%
  spread(var_names,obs_values)
head(gap_super_wide)

