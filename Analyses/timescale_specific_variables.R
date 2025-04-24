## In this script, we perform moving windows to quantify timescale-specific 
## measures of growth, precipitation, and temperature. Window lengths were selected to 
## correspond to the middle of each timescale band with the exception of the biennial band;
## biennial = 3 year windows, multiannual = 7 year windows, decadal = 15 year windows, 
## and multidecadal = 25 year windows.

# source cleaned and subsetted datasets; avg_plot_growth, winter_ppt, summer_tmin 
source(here::here("Analyses/data_cleaning_and_subsetting.R"))

#### timescale-specific precipitation ####
# biennial precipitation = 3 year windows
window_length <- 3
start_year <- min(winter_ppt$wateryear)
end_year <- max(winter_ppt$wateryear)

ppt_window_3 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 1
  window_end <- i + 1
  select_years <- window_start:window_end
  print(select_years)
  
  
  ppt_3 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 1, w_end = i + 1, window_lenth = window_length,
               band = "biennial")
  
  ppt_window_3 <- bind_rows(ppt_window_3, ppt_3)
}

# multiannual precipitation = 7 year windows
window_length <- 7
start_year <- min(winter_ppt$wateryear)
end_year <- max(winter_ppt$wateryear)

ppt_window_7 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 3
  window_end <- i + 3
  select_years <- window_start:window_end
  print(select_years)
  
  
  ppt_7 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  ppt_window_7 <- bind_rows(ppt_window_7, ppt_7)
}

# decadal precipiation = 15 year windows 
window_length <- 15
start_year <- min(winter_ppt$wateryear)
end_year <- max(winter_ppt$wateryear)

ppt_window_15 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 7
  window_end <- i + 7
  select_years <- window_start:window_end
  print(select_years)
  
  
  ppt_15 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  ppt_window_15 <- bind_rows(ppt_window_15, ppt_15)
}

# multidecadal precipitation = 25 year windows
window_length <- 25
start_year <- min(winter_ppt$wateryear)
end_year <- max(winter_ppt$wateryear)

ppt_window_25 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 12
  window_end <- i + 12
  select_years <- window_start:window_end
  print(select_years)
  
  
  ppt_25 <- winter_ppt %>% 
    filter(wateryear %in% select_years) %>%
    summarise(window_ppt = mean(winter_ppt)) %>%
    add_column(window_year = i, w_start = i - 12, w_end = i + 12, window_lenth = window_length,
               band = "multidecadal")
  
  ppt_window_25 <- bind_rows(ppt_window_25, ppt_25)
}

# join and average timescale specific precipitation data per year per band
timescale_specific_avg_ppt <- rbind(ppt_window_3, ppt_window_7, ppt_window_15, ppt_window_25) %>%
  group_by(window_year, band, w_start,
           w_end, window_lenth ) %>%
  summarise(window_avg_ppt = mean(window_ppt))

#### timescale-specific temperature ####
# biennial temperature = 3 year windows
window_length <- 3
start_year <- min(summer_tmin$year)
end_year <- max(summer_tmin$year)

tmin_window_3 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 1
  window_end <- i + 1
  select_years <- window_start:window_end
  print(select_years)
  
  
  tmin_3 <- summer_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(summer_tmin)) %>%
    add_column(window_year = i, w_start = i - 1, w_end = i + 1, window_lenth = window_length,
               band = "biennial")
  
  tmin_window_3 <- bind_rows(tmin_window_3, tmin_3)
}

# multiannual temperature = 7 year windows
window_length <- 7
start_year <- min(summer_tmin$year)
end_year <- max(summer_tmin$year)

tmin_window_7 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 3
  window_end <- i + 3
  select_years <- window_start:window_end
  print(select_years)
  
  
  tmin_7 <- summer_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(summer_tmin)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  tmin_window_7 <- bind_rows(tmin_window_7, tmin_7)
}

# decadal temperature = 15 year windows
window_length <- 15
start_year <- min(summer_tmin$year)
end_year <- max(summer_tmin$year)

tmin_window_15 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 7
  window_end <- i + 7
  select_years <- window_start:window_end
  print(select_years)
  
  
  tmin_15 <- summer_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(summer_tmin)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  tmin_window_15 <- bind_rows(tmin_window_15, tmin_15)
}

# multidecadal temperature = 25 year windows
window_length <- 25
start_year <- min(summer_tmin$year) 
end_year <- max(summer_tmin$year)

tmin_window_25 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 12
  window_end <- i + 12
  select_years <- window_start:window_end
  print(select_years)
  
  
  tmin_25 <- summer_tmin %>% 
    filter(year %in% select_years) %>%
    summarise(window_tmin = mean(summer_tmin)) %>%
    add_column(window_year = i, w_start = i - 12, w_end = i + 12, window_lenth = window_length,
               band = "multidecadal")
  
  tmin_window_25 <- bind_rows(tmin_window_25, tmin_25)
}

# join and average timescale specific temperature data per year per band
timescale_specific_avg_tmin <- rbind(tmin_window_3, tmin_window_7, tmin_window_15, tmin_window_25)%>%
  group_by(window_year, band, w_start,
           w_end, window_lenth ) %>%
  summarise(window_avg_tmin = mean(window_tmin))


#### timescale-specific growth ####
# biennial growth = 3 year windows 
window_length <- 3
start_year <- min(avg_plot_growth$year)
end_year <- max(avg_plot_growth$year)

rwi_window_3 <- tibble()

for(i in start_year:end_year){
  window_start <- i - 1
  window_end <- i + 1
  select_years <- window_start:window_end
  print(select_years)
  
  
  rwi_3 <- avg_plot_growth %>% 
    filter(year %in% select_years) %>%
    summarise(window_rwi = mean(avg_growth)) %>%
    add_column(window_year = i, w_start = i - 1, w_end = i + 1, window_lenth = window_length,
               band = "biennial")
  
  rwi_window_3 <- bind_rows(rwi_window_3, rwi_3)
}

# multiannual growth = 7 year windows 
window_length <- 7
start_year <- min(avg_plot_growth$year)
end_year <- max(avg_plot_growth$year)

rwi_window_7 <- tibble()
for(i in start_year:end_year){
  window_start <- i - 3
  window_end <- i + 3
  select_years <- window_start:window_end
  print(select_years)
  
  
  rwi_7 <- avg_plot_growth %>% 
    filter(year %in% select_years) %>%
    summarise(window_rwi = mean(avg_growth)) %>%
    add_column(window_year = i, w_start = i - 3, w_end = i + 3, window_lenth = window_length,
               band = "multiannual")
  
  rwi_window_7 <- bind_rows(rwi_window_7, rwi_7)
}

# decadal growth = 15 year windows
window_length <- 15
start_year <- min(avg_plot_growth$year)
end_year <- max(avg_plot_growth$year)

rwi_window_15 <- tibble()
for(i in start_year:end_year){
  window_start <- i - 7
  window_end <- i + 7
  select_years <- window_start:window_end
  print(select_years)
  
  
  rwi_15 <- avg_plot_growth %>% 
    filter(year %in% select_years) %>%
    summarise(window_rwi = mean(avg_growth)) %>%
    add_column(window_year = i, w_start = i - 7, w_end = i + 7, window_lenth = window_length,
               band = "decadal")
  
  rwi_window_15 <- bind_rows(rwi_window_15, rwi_15)
}

# multidecadal growth = 25 year windows 
window_length <- 25
start_year <- min(avg_plot_growth$year)
end_year <- max(avg_plot_growth$year)

rwi_window_25 <- tibble()
for(i in start_year:end_year){
  window_start <- i - 12
  window_end <- i + 12
  select_years <- window_start:window_end
  print(select_years)
  
  
  rwi_25 <- avg_plot_growth %>% 
    filter(year %in% select_years) %>%
    summarise(window_rwi = mean(avg_growth)) %>%
    add_column(window_year = i, w_start = i - 12, w_end = i + 12, window_lenth = window_length,
               band = "multidecadal")
  
  rwi_window_25 <- bind_rows(rwi_window_25, rwi_25)
}

# join and average timescale specific temperature data per year per band
timescale_specific_avg_rwi <- rbind(rwi_window_3, rwi_window_7, rwi_window_15, rwi_window_25)%>%
  group_by(window_year, band, w_start,
           w_end, window_lenth ) %>%
  summarise(window_avg_rwi = mean(window_rwi))

#### join all timescale-specific data together in one dataframe ####
timescale_specific_data <- left_join(timescale_specific_avg_ppt, timescale_specific_avg_tmin, 
                                         by = c("window_year", "band", "w_start",
                                                "w_end", "window_lenth")) %>%
  left_join(timescale_specific_avg_rwi, by = c("window_year", "band", "w_start",
                                               "w_end", "window_lenth"))



