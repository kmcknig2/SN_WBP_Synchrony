## In this script, we perform wavelet analyses to calculate the degree of synchrony 
## in growth, precipitation, and temperature patterns over time and across timescales
## to produce wavelet mean fields (wmf) and wavelet phasor mean fields (wpmf).

# load necessary packages
library("wsyn")
library("here")

# source cleaned and subsetted datasets; avg_plot_growth_mx, winter_ppt_mx, summer_tmin_mx 
source(here::here("Analyses/data_cleaning_and_subsetting.R"))
# source psync.by.chance and plotmag_tts functions for plotting
source(here::here("Functions/psync_by_chance.R"))
source(here::here("Functions/plotmag_tts.R"))

#### growth synchrony ####
# standardize data for wmf and wpmf using cleandat function, clev = 5
times <- 1900:2018
avg_plot_growth_mx <- cleandat(avg_plot_growth_mx, times, clev = 5)$cdat

# produce wpmf & wmf objects for growth data
res_growth_wpmf<-wpmf(avg_plot_growth_mx,times,sigmethod="none")
res_growth_wmf<-wmf(avg_plot_growth_mx,times)

# calculate signficance thresholds using function psync.by.chance
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]

# get wpmf data to plot significance contours
wav <- Mod(get_values(res_growth_wpmf))
times <- get_times(res_growth_wpmf)
timescales <- get_timescales(res_growth_wpmf)

# plot growth wmf with adjusted axes to correspond to timescale band limits
plotmag.tts(res_growth_wmf)
axis(1, at = seq(1900, 2020, by = 20), labels = seq(1900, 2020, by = 20))
axis(2, at = log2(c(3, 10, 20, 30)), labels = c("3", "10", "20", "30"), las = 1)

# add contour lines (upper and lower significance thresholds) from wpmf
graphics::contour(x = times, y = log2(timescales), z = wav, levels = upper, 
                  drawlabels = FALSE, lwd = 2, xaxs = "i", xaxt = "n", 
                  xaxp = c(0, 1, 5), las = 1, frame = FALSE, lty = 1, 
                  yaxt = "n", add = TRUE)

#### precipitation synchrony ####
# standardize data for wmf using cleandat function, clev = 5
times <- 1900:2018
winter_ppt_mx <- cleandat(winter_ppt_mx, times, clev = 5)$cdat

# produce wmf object for precipitation data
res_ppt_wmf<-wmf(winter_ppt_mx,times)

# plot precip wmf with adjusted axes to correspond to timescale band limits
plotmag.tts(res_ppt_wmf)
axis(1, at = seq(1900, 2020, by = 20), labels = seq(1900, 2020, by = 20))
axis(2, at = log2(c(3, 10, 20, 30)), labels = c("3", "10", "20", "30"), las = 1)

#### temperature synchrony ####
# standardize data for wmf using cleandat function, clev = 5
times <- 1900:2018
summer_tmin_mx <- cleandat(summer_tmin_mx, times, clev = 5)$cdat

# produce wmf object for temperature data
res_tmin_wmf<-wmf(summer_tmin_mx,times)

# plot temp wmf with adjusted axes to correspond to timescale band limits
plotmag.tts(res_tmin_wmf)
axis(1, at = seq(1900, 2020, by = 20), labels = seq(1900, 2020, by = 20))
axis(2, at = log2(c(3, 10, 20, 30)), labels = c("3", "10", "20", "30"), las = 1)

#### plot precipitation and temperature wmfs using a global range for consistent synchrony color scale ####
wav1 <- Mod(get_values(res_ppt_wmf))
wav2 <- Mod(get_values(res_tmin_wmf))
global_range <- range(c(wav1, wav2), na.rm = TRUE)

plotmag.tts(res_ppt_wmf, zlims=global_range)
axis(1, at = seq(1900, 2020, by = 20), labels = seq(1900, 2020, by = 20))
axis(2, at = log2(c(3, 10, 20, 30)), labels = c("3", "10", "20", "30"), las = 1)

plotmag.tts(res_tmin_wmf, zlims = global_range)
axis(1, at = seq(1900, 2020, by = 20), labels = seq(1900, 2020, by = 20))
axis(2, at = log2(c(3, 10, 20, 30)), labels = c("3", "10", "20", "30"), las = 1)




