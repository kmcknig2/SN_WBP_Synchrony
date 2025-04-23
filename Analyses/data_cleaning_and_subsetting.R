## This script contains code to clean and subset the 
## three core datasets: tree-ring data, precipitation data, 
## and temperature data.

# load packages
library("here")
library("tidyverse")

#### load tree-ring data ####
rwi_dat <- read.csv(here("Data/rwi_dat.csv"))

# rename plot and data columns
rwi_dat <- rwi_dat %>%
  select(year, plot_id_needle, tree_num, value) %>%
  rename(plot = "plot_id_needle", rwi = "value")

# subset time series to 1900-2018, each row represents an individual tree's data
rwi_00s <- rwi_dat %>%
  filter(year >= 1900)%>% # data collected in 2018, so no cut off specification needed for max year
  pivot_wider(names_from = year, values_from=rwi)

# remove rows (trees) with missing data along the time series 
rwi_00s_tree_filtered<- rwi_00s[rowSums(is.na(rwi_00s))==0,]

# find plots with at least 5 trees with growth data for the whole time series
rwi_00s_plot_filtered <- rwi_00s_tree_filtered %>% 
  pivot_longer(3:121, names_to = "year", values_to = "rwi")%>%
  group_by(plot) %>%
  mutate(num_trees = round(n()/118))%>% # divide by 118 years of data
  filter(num_trees >= 5)

# create a wide version of subsetted dataset
rwi_00s_plot_filtered_wide <- rwi_00s_plot_filtered %>%
  pivot_wider(names_from = year, values_from = rwi)

# calculate annual average growth per plot per year
avg_plot_growth <- rwi_00s_plot_filtered %>%
  filter(year >= 1900)%>%
  group_by(plot, year)%>%
  summarize(avg_growth = mean(rwi))

# create a wide version of average growth dataset 
avg_plot_growth_wide <- avg_plot_growth %>%
  pivot_wider(names_from = "year", values_from = "avg_growth")

# format wide version of average growth data as a matrix for wavelet analysis
avg_plot_growth_wide <- as.matrix(avg_plot_growth_wide)
colnames(avg_plot_growth_wide) <- NULL
avg_plot_growth_wide <- avg_plot_growth_wide[, c(2:120)] # time series 1900 -2018

# convert character matrix to numeric
avg_plot_growth_wide = as.data.frame(avg_plot_growth_wide, stringsAsFactors = FALSE)
avg_plot_growth_wide = map_df(avg_plot_growth_wide, as.numeric)
avg_plot_growth_mx <- as.matrix(avg_plot_growth_wide)

#### load precipitation data ####
ppt_dat <- read.csv(here("Data/ppt_dat.csv")) 

# rename plot column
ppt_dat <- ppt_dat %>%
  rename(plot = "plot_id_needle") 

# remove NAs (removes annual measurements from dataset as month = NA)
ppt_dat <- na.omit(ppt_dat)

# match up plots with tree-ring data
plots <- unique(rwi_00s_plot_filtered$plot)
ppt_dat <- ppt_dat %>%
  filter(plot %in% plots)

# produce water-year variable (Oct from previous year to May of current year)
winter_ppt <- ppt_dat %>%
  mutate(wateryear = case_when(month == "10" ~ year+1,
                               month == "11" ~ year+1,
                               month == "12" ~ year+1,
                               TRUE ~ as.numeric(year)))

# filter for winter months (Oct - May)
winter_months <- c(1, 2, 3, 4, 5, 10, 11, 12)
winter_ppt <- winter_ppt %>%
  filter(month %in% winter_months)

# calculate average winter precip per plot per water-year
winter_ppt <- winter_ppt %>%
  group_by(plot, wateryear)%>%
  summarise(winter_ppt = mean(ppt))%>%
  filter(wateryear >= 1900)%>%
  filter(wateryear <= 2018)

# create wide version of average precip data
winter_ppt_wide <- winter_ppt %>%
  pivot_wider(names_from = "wateryear", values_from = "winter_ppt")

# format wide version of average precip data as a matrix for wavelet analysis
winter_ppt_wide <- as.matrix(winter_ppt_wide)
colnames(winter_ppt_wide) <- NULL
winter_ppt_wide <- winter_ppt_wide[, c(2:120)] # time series 1900 -2018

# convert character matrix to numeric
winter_ppt_wide = as.data.frame(winter_ppt_wide, stringsAsFactors = FALSE)
winter_ppt_wide = map_df(winter_ppt_wide, as.numeric)
winter_ppt_mx <- as.matrix(winter_ppt_wide)


#### load temperature data ####
tmin_dat <- read.csv(here("Data/tmin_dat.csv")) 

# rename plot column
tmin_dat <- tmin_dat %>%
  rename(plot = "plot_id_needle") 

# remove NAs (removes annual measurements from dataset as month = NA)
tmin_dat <- na.omit(tmin_dat)

# match plots with tree-ring data
tmin_dat <- tmin_dat %>%
  filter(plot %in% plots)

# filter for summer months (June - Aug)
summer_months <- c(6, 7, 8)
summer_tmin <- tmin_dat %>%
  filter(month %in% summer_months )

# calculate average minimum temperature per plot per year
summer_tmin <- summer_tmin %>%
  group_by(plot, year)%>%
  summarise(summer_tmin = mean(tmin))%>%
  filter(year >= 1900)%>%
  filter(year <= 2018)

# create wide version of average temp data
summer_tmin_wide <- summer_tmin %>%
  pivot_wider(names_from = "year", values_from = "summer_tmin")

# format wide version of average temp data as a matrix for wavelet analysis
summer_tmin_wide <- as.matrix(summer_tmin_wide)
colnames(summer_tmin_wide) <- NULL
summer_tmin_wide <- summer_tmin_wide[, c(2:120)] # time series 1900 -2018

# convert character matrix to numeric
summer_tmin_wide = as.data.frame(summer_tmin_wide, stringsAsFactors = FALSE)
summer_tmin_wide = map_df(summer_tmin_wide, as.numeric)
summer_tmin_mx <- as.matrix(summer_tmin_wide)

## The following cleaned and subsetted datasets 
## are to be used in further analyses:
## tree-ring data = avg_plot_growth,
## precipitation data = winter_ppt,
## temperature data = summer_tmin. 




