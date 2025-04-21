#### This script contains... 

# load packages
library("here")
library("dplyr")
library("tidyr")

# load data
rwi_dat <- read.csv(here("Data/rwi_dat.csv"))

# rename plot and data columns
rwi_dat <- rwi_dat %>%
  select(year, plot_id_needle, tree_num, value) %>%
  rename(plot = "plot_id_needle", rwi = "value")

# subset time series to 1900-2018, each row represents an individual tree's data
rwi_00s <- rwi_dat %>%
  filter(year >= 1900)%>% # data collected in 2018, so no cut off specification needed for max year
  pivot_wider(names_from = year, values_from=rwi)

# remove any trees with missing data along the time series 
rwi_00s_tree_filtered<- rwi_00s[rowSums(is.na(rwi_00s))==0,]

# find plots with at least 5 trees with the whole time series
rwi_00s_plot_filtered <- rwi_00s_tree_filtered %>% 
  pivot_longer(3:121, names_to = "year", values_to = "rwi")%>%
  group_by(plot) %>%
  mutate(num_trees = round(n()/118))%>%
  filter(num_trees >= 5)

# create a wide version
rwi_00s_plot_filtered_wide <- rwi_00s_plot_filtered %>%
  pivot_wider(names_from = year, values_from = rwi)

# calculate annual average growth per plot per year
avg_plot_growth <- rwi_00s_plot_filtered %>%
  filter(year >= 1900)%>%
  group_by(plot, year)%>%
  summarize(avg_growth = mean(rwi))

# create a wide version
avg_plot_growth_wide <- avg_plot_growth %>%
  pivot_wider(names_from = "year", values_from = "avg_growth")

# format wide version as a matrix for wavelet analysis
avg_plot_growth_wide <- as.matrix(avg_plot_growth_wide)
colnames(avg_plot_growth_wide) <- NULL
avg_plot_growth_wide <- avg_plot_growth_wide[, c(2:120)] # time series 1900 -2018

# convert character matrix to numeric
avg_plot_growth_wide = as.data.frame(avg_plot_growth_wide, stringsAsFactors = FALSE)
avg_plot_growth_wide = map_df(avg_plot_growth_wide, as.numeric)
avg_plot_growth_mx <- as.matrix(avg_plot_growth_wide)

