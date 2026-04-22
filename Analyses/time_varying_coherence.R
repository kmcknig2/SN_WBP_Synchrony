## In this script, we calculate wavelet coherence between growth and precipitation
## and between growth and temperature across our timeseries at each timescale band.

library("wsyn")
# source cleaned and subsetted datasets; avg_plot_growth_mx, winter_ppt_mx, summer_tmin_mx 
source(here::here("Analyses/data_cleaning_and_subsetting.R"))
# source coh_tv function, modified to test coherence in a time varying way
source(here::here("Functions/coh_tv.R"))

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

# calculate average significant coherence in precipitation across timescale bands for each timestep
# extract the gt values from the significance test (proportion of observed coherence that is greater than surrogate coherence)
sig.coh.ppt <- as.data.frame(tv_timeseries_ppt$signif$gt)

# make timescales column names
colnames(sig.coh.ppt) <- tv_timeseries_ppt$timescales

# add year column and pivot longer
sig.coh.ppt$times <- tv_timeseries_ppt$times
sig.coh.ppt <- sig.coh.ppt %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")

# make timescale column numeric and specify timescale bands
sig.coh.ppt$ts <- as.numeric(sig.coh.ppt$ts)
sig.coh.ppt <- sig.coh.ppt %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

# calculate average 'significant' coherence per year, per band and omit NAs
avg.sig.coh.ppt <- sig.coh.ppt %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.sig.coh.ppt <- na.omit(avg.sig.coh.ppt)  

# calculate significant average coherence in temperature across timescale bands for each timestep
# extract the gt values from the significance test (proportion of observed coherence that is greater than surrogate coherence)
sig.coh.tmin <- as.data.frame(tv_timeseries_tmin$signif$gt)

# make timescales column names
colnames(sig.coh.tmin) <- tv_timeseries_tmin$timescales

# add year column and pivot longer
sig.coh.tmin$times <- tv_timeseries_tmin$times
sig.coh.tmin <- sig.coh.tmin %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")

# make timescale column numeric and specify timescale bands
sig.coh.tmin$ts <- as.numeric(sig.coh.tmin$ts)
sig.coh.tmin <- sig.coh.tmin %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

# calculate average 'significant' coherence per year, per band and omit NAs
avg.sig.coh.tmin <- sig.coh.tmin %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.sig.coh.tmin <- na.omit(avg.sig.coh.tmin)  

# combine into one data frame for plotting
avg.sig.coh.ppt$driver <- "ppt"
avg.sig.coh.tmin$driver <- "tmin"
avg.tv.sig.coh <- rbind(avg.sig.coh.ppt, avg.sig.coh.tmin)

# make year a character, factor bands in timescale order and drivers 
avg.tv.sig.coh$times <- as.character(avg.tv.sig.coh$times)
avg.tv.sig.coh$band <- factor(avg.tv.sig.coh$band , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))
avg.tv.sig.coh$driver <- factor(avg.tv.sig.coh$driver, levels=c('ppt', 'tmin'))

# plot avg coherence across time per band for each driver
labels <- c(annual = "biennial", interannual = "multiannual", decadal = "decadal", multidecadal = "multidecadal")
ggplot() +
  geom_line(data = avg.tv.sig.coh, aes(x = times, y = avg_coh, group = driver, color = driver)) +
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


# calculate average magnitude of coherence in precipitation across timescale bands for each timestep
# extract the magnitude values from tv_coh object and take the modulus to remove imaginary numbers
mag.coh.ppt <- as.data.frame(Mod(tv_timeseries_ppt$coher))

# make timescales column names
colnames(mag.coh.ppt) <- tv_timeseries_ppt$timescales

# add year column and pivot longer
mag.coh.ppt$times <- tv_timeseries_ppt$times
mag.coh.ppt <- mag.coh.ppt %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")

# make timescale column numeric and specify timescale bands
mag.coh.ppt$ts <- as.numeric(mag.coh.ppt$ts)
mag.coh.ppt <- mag.coh.ppt %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

# calculate average magnitude coherence per year, per band and omit NAs
avg.mag.coh.ppt <- mag.coh.ppt %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.mag.coh.ppt <- na.omit(avg.mag.coh.ppt)  



# calculate average magnitude of coherence in temperature across timescale bands for each timestep
# extract the magnitude values from tv_coh object and take the modulus to remove imaginary numbers
mag.coh.tmin <- as.data.frame(Mod(tv_timeseries_tmin$coher))

# make timescales column names
colnames(mag.coh.tmin) <- tv_timeseries_tmin$timescales

# add year column and pivot longer
mag.coh.tmin$times <- tv_timeseries_tmin$times
mag.coh.tmin <- mag.coh.tmin %>%
  pivot_longer(1:67, names_to = "ts", values_to = "coh")

# make timescale column numeric and specify timescale bands
mag.coh.tmin$ts <- as.numeric(mag.coh.tmin$ts)
mag.coh.tmin <- mag.coh.tmin %>%
  mutate(band = case_when(ts >= 2 & ts <= 3 ~ "biennial",
                          ts > 3  & ts <= 10 ~ "multiannual",
                          ts > 10 & ts <= 20 ~ "decadal",
                          ts > 20 & ts <= 30 ~ "multidecadal"))

# calculate average magnitude coherence per year, per band and omit NAs
avg.mag.coh.tmin <- mag.coh.tmin %>%
  group_by(times, band) %>%
  summarise(avg_coh = mean(coh))
avg.mag.coh.tmin <- na.omit(avg.mag.coh.tmin)  

# combine into one data frame for plotting
avg.mag.coh.ppt$driver <- "ppt"
avg.mag.coh.tmin$driver <- "tmin"
avg.tv.mag.coh <- rbind(avg.mag.coh.ppt, avg.mag.coh.tmin)

# make year a character, factor bands in timescale order and drivers 
avg.tv.mag.coh$times <- as.character(avg.tv.mag.coh$times)
avg.tv.mag.coh$band <- factor(avg.tv.mag.coh$band , levels=c('biennial', 'multiannual', 'decadal', 'multidecadal'))
avg.tv.mag.coh$driver <- factor(avg.tv.mag.coh$driver, levels=c('ppt', 'tmin'))

# plot avg coherence across time per band for each driver
labels <- c(annual = "biennial", interannual = "multiannual", decadal = "decadal", multidecadal = "multidecadal")
ggplot() +
  geom_line(data = avg.tv.mag.coh, aes(x = times, y = avg_coh, group = driver, color = driver)) +
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
  ylab("Average Magnitude of Coherence")+
  xlab("Year")

