## In this script, we calculate wavelet coherence between growth and precipitation
## and between growth and temperature across our timeseries at each timescale band.


# source cleaned and subsetted datasets; avg_plot_growth_mx, winter_ppt_mx, summer_tmin_mx 
source(here::here("Analyses/data_cleaning_and_subsetting.R"))
# source coh_tv function, modified to test coherence in a time varying way
source(here::here("Functions/coh_tv.R"))

library("wsyn")

# clean data for each variable using cleandat function with clev = 5 
times = 1900:2018
x <- cleandat(avg_plot_growth_mx, times, clev = 5)$cdat
y1 <- cleandat(winter_ppt_mx, times, clev = 5)$cdat
y2 <- cleandat(summer_tmin_mx, times, clev = 5)$cdat

# calculate time varying coherence for each variable across whole time series
tv_timeseries_ppt <- coh_tv(dat1 = x, dat2 = y1, times = times, norm = "powall",
                            sigmethod = "fftsurrog1", nrand = 1000)
tv_timeseries_tmin <- coh_tv(dat1 = x, dat2 = y2, times = times, norm = "powall",
                             sigmethod = "fftsurrog1", nrand = 1000)

# calculate average coherence in precipitation across timescale bands for each timestep
# extract the gt values from the significance test (proportion of observed coherence that is greater than surrogate coherence)
coh.ppt <- as.data.frame(tv_timeseries_ppt$signif$gt)

# make timescales column names
colnames(coh.ppt) <- tv_timeseries_ppt$timescales

# add year column and pivot longer
coh.ppt$times <- tv_timeseries_ppt$times
coh.ppt <- coh.ppt %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")

# make timescale column numeric and specify timescale bands
coh.ppt$ts <- as.numeric(coh.ppt$ts)
coh.ppt <- coh.ppt %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

# calculate average 'significant' coherence per year, per band and omit NAs
avg.coh.ppt <- coh.ppt %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.ppt <- na.omit(avg.coh.ppt)  

# calculate average coherence in temperature across timescale bands for each timestep
# extract the gt values from the significance test (proportion of observed coherence that is greater than surrogate coherence)
coh.tmin <- as.data.frame(tv_timeseries_tmin$signif$gt)

# make timescales column names
colnames(coh.tmin) <- tv_timeseries_tmin$timescales

# add year column and pivot longer
coh.tmin$times <- tv_timeseries_tmin$times
coh.tmin <- coh.tmin %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")

# make timescale column numeric and specify timescale bands
coh.tmin$ts <- as.numeric(coh.tmin$ts)
coh.tmin <- coh.tmin %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

# calculate average 'significant' coherence per year, per band and omit NAs
avg.coh.tmin <- coh.tmin %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.coh.tmin <- na.omit(avg.coh.tmin)  

# combine into one data frame for plotting
avg.coh.ppt$driver <- "ppt"
avg.coh.tmin$driver <- "tmin"
avg.tv.coh <- rbind(avg.coh.ppt, avg.coh.tmin)

# make year a character, factor bands in timescale order and drivers 
avg.tv.coh$times <- as.character(avg.tv.coh$times)
avg.tv.coh$band <- factor(avg.tv.coh$band , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))
avg.tv.coh$driver <- factor(avg.tv.coh$driver, levels=c('ppt', 'tmin'))

# plot avg coherence across time per band for each driver
labels <- c(annual = "biennial", interannual = "multiannual", decadal = "decadal", multidecadal = "multidecadal")
ggplot() +
  geom_line(data = avg.tv.coh, aes(x = times, y = avg_coh, group = driver, color = driver)) +
  facet_grid(rows = "band", labeller=labeller(band = c("biennial" = "Biennial", "multiannual" = "Multiannual", "decadal" = "Decadal", "multidecadal"= "Multidecadal")))+
  theme_bw()+
  scale_color_manual(values = c("#377EB8", "#E41A1C"), labels = c("Winter Precipitation", "Summer Temperatures"))+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", size = 16,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  ylab("Average Proportion of Significant Coherence")+
  xlab("Year")
