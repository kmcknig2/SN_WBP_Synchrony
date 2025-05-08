## In this script, we quantify timescale specific data quartiles to correlate 
## with synchrony values across years and timescale bands. 

# source timescale specific variables; timescale_specific_avg_ppt, timescale_specific_avg_tmin, timescale_specific_avg_rwi
source(here::here("Analyses/timescale_specific_variables.R"))
# source average synchrony data for each variable: ppt_sync_long, tmin_sync_long, growth_sync_long
source(here::here("Analyses/average_synchrony.R"))

#### calculate average synchrony across full timeseries ####
## calculate average precip synchrony across timeseries ##
avg_ppt_sync_total <- ppt_sync_long %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(values))
avg_ppt_sync_total$year <- as.numeric(avg_ppt_sync_total$year)

## calculate average temperature synchrony across timeseries ##
avg_tmin_sync_total <- tmin_sync_long %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(values))
avg_tmin_sync_total$year <- as.numeric(avg_tmin_sync_total$year)

## calculate average growth synchrony across timeseries ##
avg_growth_sync_total <- growth_sync_long %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(values))
avg_growth_sync_total$year <- as.numeric(avg_growth_sync_total$year)

#### calculate temperature quantiles ####
## calculate biennial temperature quantiles ##
b_tmin_quantiles <- timescale_specific_avg_tmin %>%
  filter(band == "biennial")
b_tmin_q <- quantile(b_tmin_quantiles$window_avg_tmin, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
b_tmin_quantiles <- b_tmin_quantiles %>%
  mutate(
    quantile = case_when(
      window_avg_tmin <= b_tmin_q[[1]] ~ 1,
      window_avg_tmin >  b_tmin_q[[1]] & window_avg_tmin <= b_tmin_q[[2]] ~ 2,
      window_avg_tmin >  b_tmin_q[[2]] & window_avg_tmin <= b_tmin_q[[3]] ~ 3,
      window_avg_tmin >  b_tmin_q[[3]] ~ 4
    )
  )

## calculate multiannual temperature quantiles ##
ma_tmin_quantiles <- timescale_specific_avg_tmin %>%
  filter(band == "multiannual")
ma_tmin_q <- quantile(ma_tmin_quantiles$window_avg_tmin, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
ma_tmin_quantiles <- ma_tmin_quantiles %>%
  mutate(
    quantile = case_when(
      window_avg_tmin <= ma_tmin_q[[1]] ~ 1,
      window_avg_tmin >  ma_tmin_q[[1]] & window_avg_tmin <= ma_tmin_q[[2]] ~ 2,
      window_avg_tmin >  ma_tmin_q[[2]] & window_avg_tmin <= ma_tmin_q[[3]] ~ 3,
      window_avg_tmin >  ma_tmin_q[[3]] ~ 4
    )
  )

## calculate decadal temperature quantiles ##
# subset decadal band
d_tmin_quantiles <- timescale_specific_avg_tmin %>%
  filter(band == "decadal")
d_tmin_q <- quantile(d_tmin_quantiles$window_avg_tmin, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
d_tmin_quantiles <- d_tmin_quantiles %>%
  mutate(
    quantile = case_when(
      window_avg_tmin <= d_tmin_q[[1]] ~ 1,
      window_avg_tmin >  d_tmin_q[[1]] & window_avg_tmin <= d_tmin_q[[2]] ~ 2,
      window_avg_tmin >  d_tmin_q[[2]] & window_avg_tmin <= d_tmin_q[[3]] ~ 3,
      window_avg_tmin >  d_tmin_q[[3]] ~ 4
    )
  )

## calculate multidecadal temperature quantiles ##
# subset multidecadal band
md_tmin_quantiles <- timescale_specific_avg_tmin %>%
  filter(band == "multidecadal")
md_tmin_q <- quantile(md_tmin_quantiles$window_avg_tmin, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
md_tmin_quantiles <- md_tmin_quantiles %>%
  mutate(
    quantile = case_when(
      window_avg_tmin <= md_tmin_q[[1]] ~ 1,
      window_avg_tmin >  md_tmin_q[[1]] & window_avg_tmin <= md_tmin_q[[2]] ~ 2,
      window_avg_tmin >  md_tmin_q[[2]] & window_avg_tmin <= md_tmin_q[[3]] ~ 3,
      window_avg_tmin >  md_tmin_q[[3]] ~ 4
    )
  )

#### plot synchrony across temperature quantiles ####
## join temperature quartiles to average synchrony data ##
temperature_quartiles <- rbind(b_tmin_quantiles, ma_tmin_quantiles, d_tmin_quantiles, md_tmin_quantiles)
tmin_quant_ppt_sync <- inner_join(temperature_quartiles, avg_ppt_sync_total, by = join_by(window_year == year, band))
tmin_quant_tmin_sync <- inner_join(temperature_quartiles, avg_tmin_sync_total, by = join_by(window_year == year, band))
tmin_quant_rwi_sync <- inner_join(temperature_quartiles, avg_growth_sync_total, by = join_by(window_year == year, band))

## calculate mean synchrony and CIs per band, per temperature quantile ##
tmin_quant_ppt_sync_CIs <- tmin_quant_ppt_sync %>%
  group_by(band, quantile)%>%
  summarise(mean.ppt.sync = mean(avg_sync, na.rm = TRUE),
            sd.ppt.sync = sd(avg_sync, na.rm = TRUE),
            n.ppt.sync = n()) %>%
  mutate(se.ppt.sync = sd.ppt.sync / sqrt(n.ppt.sync),
         lower.ci.ppt.sync = mean.ppt.sync - qt(1 - (0.05 / 2), n.ppt.sync - 1) * se.ppt.sync,
         upper.ci.ppt.sync = mean.ppt.sync + qt(1 - (0.05 / 2), n.ppt.sync - 1) * se.ppt.sync)

tmin_quant_tmin_sync_CIs <- tmin_quant_tmin_sync %>%
  group_by(band, quantile)%>%
  summarise(mean.tmin.sync = mean(avg_sync, na.rm = TRUE),
            sd.tmin.sync = sd(avg_sync, na.rm = TRUE),
            n.tmin.sync = n()) %>%
  mutate(se.tmin.sync = sd.tmin.sync / sqrt(n.tmin.sync),
         lower.ci.tmin.sync = mean.tmin.sync - qt(1 - (0.05 / 2), n.tmin.sync - 1) * se.tmin.sync,
         upper.ci.tmin.sync = mean.tmin.sync + qt(1 - (0.05 / 2), n.tmin.sync - 1) * se.tmin.sync)

tmin_quant_rwi_sync_CIs <- tmin_quant_rwi_sync %>%
  group_by(band, quantile)%>%
  summarise(mean.rwi.sync = mean(avg_sync, na.rm = TRUE),
            sd.rwi.sync = sd(avg_sync, na.rm = TRUE),
            n.rwi.sync = n()) %>%
  mutate(se.rwi.sync = sd.rwi.sync / sqrt(n.rwi.sync),
         lower.ci.rwi.sync = mean.rwi.sync - qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync,
         upper.ci.rwi.sync = mean.rwi.sync + qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync)

## plot synchrony across temperature quantiles ##
tmin_quant_ppt_sync_CIs$band <- factor(tmin_quant_ppt_sync_CIs$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))
tmin_quant_tmin_sync_CIs$band <- factor(tmin_quant_tmin_sync_CIs$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))
tmin_quant_rwi_sync_CIs$band <- factor(tmin_quant_rwi_sync_CIs$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))

tmin_quant_ppt_sync_plot <- ggplot(data = tmin_quant_ppt_sync_CIs, aes(x = quantile, y = mean.ppt.sync, col = band)) +
  geom_point(size = 1.5)+
  geom_line(linewidth = 1.5)+
  geom_errorbar(aes(ymin = lower.ci.ppt.sync, ymax = upper.ci.ppt.sync, x = quantile, y=mean.ppt.sync, width = 0.2)) +
  theme_bw()+
  #ylim(0.0, 2.0) +
  scale_color_brewer(name = "Timescale Band", palette="RdYlBu", direction = -1, labels = c("Biennial (2-3 yrs)","Multiannual (3-10 yrs)", "Decadal (10-20 yrs)", "Multidecadal (20-30 yrs)"))+
  ylab("Average Precipitation Synchrony")+
  xlab("Temperature Quartiles")+
  theme(axis.text.x = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 1.0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, 
                                   angle = 0, hjust = .5, 
                                   face = "plain"),
        axis.title.x = element_text(color = "black", size = 14,
                                    angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, 
                                    angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 12,
                                    angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) 

tmin_quant_tmin_sync_plot <- ggplot(data = tmin_quant_tmin_sync_CIs, aes(x = quantile, y = mean.tmin.sync, col = band)) +
  geom_point(size = 1.5)+
  geom_line(linewidth = 1.5)+
  geom_errorbar(aes(ymin = lower.ci.tmin.sync, ymax = upper.ci.tmin.sync, x = quantile, y=mean.tmin.sync, width = 0.2)) +
  theme_bw()+
  #ylim(0.0, 2.0) +
  scale_color_brewer(name = "Timescale Band", palette="RdYlBu", direction = -1, labels = c("Biennial (2-3 yrs)","Multiannual (3-10 yrs)", "Decadal (10-20 yrs)", "Multidecadal (20-30 yrs)"))+
  ylab("Average Temperature Synchrony")+
  xlab("Temperature Quartiles")+
  theme(axis.text.x = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 1.0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, 
                                   angle = 0, hjust = .5, 
                                   face = "plain"),
        axis.title.x = element_text(color = "black", size = 14,
                                    angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, 
                                    angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 12,
                                    angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) 

tmin_quant_rwi_sync_plot <- ggplot(data = tmin_quant_rwi_sync_CIs, aes(x = quantile, y = mean.rwi.sync, col = band)) +
  geom_point(size = 1.5)+
  geom_line(linewidth = 1.5)+
  geom_errorbar(aes(ymin = lower.ci.rwi.sync, ymax = upper.ci.rwi.sync, x = quantile, y=mean.rwi.sync, width = 0.2)) +
  theme_bw()+
  #ylim(0.0, 2.0) +
  scale_color_brewer(name = "Timescale Band", palette="RdYlBu", direction = -1, labels = c("Biennial (2-3 yrs)","Multiannual (3-10 yrs)", "Decadal (10-20 yrs)", "Multidecadal (20-30 yrs)"))+
  ylab("Average Growth Synchrony")+
  xlab("Temperature Quartiles")+
  theme(axis.text.x = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 1.0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, 
                                   angle = 0, hjust = .5, 
                                   face = "plain"),
        axis.title.x = element_text(color = "black", size = 14,
                                    angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, 
                                    angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 12,
                                    angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) 


#### calculate precipitation quantiles ####
## calculate biennial precipitation quantiles ##
b_ppt_quantiles <- timescale_specific_avg_ppt %>%
  filter(band == "biennial")
b_ppt_q <- quantile(b_ppt_quantiles$window_avg_ppt, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
b_ppt_quantiles <- b_ppt_quantiles %>%
  mutate(
    quantile = case_when(
      window_avg_ppt <= b_ppt_q[[1]] ~ 1,
      window_avg_ppt >  b_ppt_q[[1]] & window_avg_ppt <= b_ppt_q[[2]] ~ 2,
      window_avg_ppt >  b_ppt_q[[2]] & window_avg_ppt <= b_ppt_q[[3]] ~ 3,
      window_avg_ppt >  b_ppt_q[[3]] ~ 4
    )
  )

## calculate multiannual precipitation quantiles ##
ma_ppt_quantiles <- timescale_specific_avg_ppt %>%
  filter(band == "multiannual")
ma_ppt_q <- quantile(ma_ppt_quantiles$window_avg_ppt, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
ma_ppt_quantiles <- ma_ppt_quantiles %>%
  mutate(
    quantile = case_when(
      window_avg_ppt <= ma_ppt_q[[1]] ~ 1,
      window_avg_ppt >  ma_ppt_q[[1]] & window_avg_ppt <= ma_ppt_q[[2]] ~ 2,
      window_avg_ppt >  ma_ppt_q[[2]] & window_avg_ppt <= ma_ppt_q[[3]] ~ 3,
      window_avg_ppt >  ma_ppt_q[[3]] ~ 4
    )
  )

## calculate decadal precipitation quantiles ##
# subset decadal band
d_ppt_quantiles <- timescale_specific_avg_ppt %>%
  filter(band == "decadal")
d_ppt_q <- quantile(d_ppt_quantiles$window_avg_ppt, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
d_ppt_quantiles <- d_ppt_quantiles %>%
  mutate(
    quantile = case_when(
      window_avg_ppt <= d_ppt_q[[1]] ~ 1,
      window_avg_ppt >  d_ppt_q[[1]] & window_avg_ppt <= d_ppt_q[[2]] ~ 2,
      window_avg_ppt >  d_ppt_q[[2]] & window_avg_ppt <= d_ppt_q[[3]] ~ 3,
      window_avg_ppt >  d_ppt_q[[3]] ~ 4
    )
  )

## calculate multidecadal precipiation quantiles ##
# subset multidecadal band
md_ppt_quantiles <- timescale_specific_avg_ppt %>%
  filter(band == "multidecadal")
md_ppt_q <- quantile(md_ppt_quantiles$window_avg_ppt, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
md_ppt_quantiles <- md_ppt_quantiles %>%
  mutate(
    quantile = case_when(
      window_avg_ppt <= md_ppt_q[[1]] ~ 1,
      window_avg_ppt >  md_ppt_q[[1]] & window_avg_ppt <= md_ppt_q[[2]] ~ 2,
      window_avg_ppt >  md_ppt_q[[2]] & window_avg_ppt <= md_ppt_q[[3]] ~ 3,
      window_avg_ppt >  md_ppt_q[[3]] ~ 4
    )
  )




#### plot synchrony across precipitation quantiles ####
## join precipitation quartiles to average synchrony data ##
precipitation_quartiles <- rbind(b_ppt_quantiles, ma_ppt_quantiles, d_ppt_quantiles, md_ppt_quantiles)
ppt_quant_ppt_sync <- inner_join(precipitation_quartiles, avg_ppt_sync_total, by = join_by(window_year == year, band))
ppt_quant_tmin_sync <- inner_join(precipitation_quartiles, avg_tmin_sync_total, by = join_by(window_year == year, band))
ppt_quant_rwi_sync <- inner_join(precipitation_quartiles, avg_growth_sync_total, by = join_by(window_year == year, band))

## calculate mean synchrony and CIs per band, per precipitation quantile ##
ppt_quant_ppt_sync_CIs <- ppt_quant_ppt_sync %>%
  group_by(band, quantile)%>%
  summarise(mean.ppt.sync = mean(avg_sync, na.rm = TRUE),
            sd.ppt.sync = sd(avg_sync, na.rm = TRUE),
            n.ppt.sync = n()) %>%
  mutate(se.ppt.sync = sd.ppt.sync / sqrt(n.ppt.sync),
         lower.ci.ppt.sync = mean.ppt.sync - qt(1 - (0.05 / 2), n.ppt.sync - 1) * se.ppt.sync,
         upper.ci.ppt.sync = mean.ppt.sync + qt(1 - (0.05 / 2), n.ppt.sync - 1) * se.ppt.sync)

ppt_quant_tmin_sync_CIs <- ppt_quant_tmin_sync %>%
  group_by(band, quantile)%>%
  summarise(mean.tmin.sync = mean(avg_sync, na.rm = TRUE),
            sd.tmin.sync = sd(avg_sync, na.rm = TRUE),
            n.tmin.sync = n()) %>%
  mutate(se.tmin.sync = sd.tmin.sync / sqrt(n.tmin.sync),
         lower.ci.tmin.sync = mean.tmin.sync - qt(1 - (0.05 / 2), n.tmin.sync - 1) * se.tmin.sync,
         upper.ci.tmin.sync = mean.tmin.sync + qt(1 - (0.05 / 2), n.tmin.sync - 1) * se.tmin.sync)

ppt_quant_rwi_sync_CIs <- ppt_quant_rwi_sync %>%
  group_by(band, quantile)%>%
  summarise(mean.rwi.sync = mean(avg_sync, na.rm = TRUE),
            sd.rwi.sync = sd(avg_sync, na.rm = TRUE),
            n.rwi.sync = n()) %>%
  mutate(se.rwi.sync = sd.rwi.sync / sqrt(n.rwi.sync),
         lower.ci.rwi.sync = mean.rwi.sync - qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync,
         upper.ci.rwi.sync = mean.rwi.sync + qt(1 - (0.05 / 2), n.rwi.sync - 1) * se.rwi.sync)

## plot synchrony across precipitation quantiles ##
ppt_quant_ppt_sync_CIs$band <- factor(ppt_quant_ppt_sync_CIs$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))
ppt_quant_tmin_sync_CIs$band <- factor(ppt_quant_tmin_sync_CIs$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))
ppt_quant_rwi_sync_CIs$band <- factor(ppt_quant_rwi_sync_CIs$band, levels = c("biennial", "multiannual", "decadal", "multidecadal" ))

ppt_quant_ppt_sync_plot <- ggplot(data = ppt_quant_ppt_sync_CIs, aes(x = quantile, y = mean.ppt.sync, col = band)) +
  geom_point(size = 1.5)+
  geom_line(linewidth = 1.5)+
  geom_errorbar(aes(ymin = lower.ci.ppt.sync, ymax = upper.ci.ppt.sync, x = quantile, y=mean.ppt.sync, width = 0.2)) +
  theme_bw()+
  #ylim(0.0, 2.0) +
  scale_color_brewer(name = "Timescale Band", palette="RdYlBu", direction = -1, labels = c("Biennial (2-3 yrs)","Multiannual (3-10 yrs)", "Decadal (10-20 yrs)", "Multidecadal (20-30 yrs)"))+
  ylab("Average Precipitation Synchrony")+
  xlab("Precipitation Quartiles")+
  theme(axis.text.x = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 1.0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, 
                                   angle = 0, hjust = .5, 
                                   face = "plain"),
        axis.title.x = element_text(color = "black", size = 14,
                                    angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, 
                                    angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 12,
                                    angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) 

ppt_quant_tmin_sync_plot <- ggplot(data = ppt_quant_tmin_sync_CIs, aes(x = quantile, y = mean.tmin.sync, col = band)) +
  geom_point(size = 1.5)+
  geom_line(linewidth = 1.5)+
  geom_errorbar(aes(ymin = lower.ci.tmin.sync, ymax = upper.ci.tmin.sync, x = quantile, y=mean.tmin.sync, width = 0.2)) +
  theme_bw()+
  #ylim(0.0, 2.0) +
  scale_color_brewer(name = "Timescale Band", palette="RdYlBu", direction = -1, labels = c("Biennial (2-3 yrs)","Multiannual (3-10 yrs)", "Decadal (10-20 yrs)", "Multidecadal (20-30 yrs)"))+
  ylab("Average Temperature Synchrony")+
  xlab("Precipitation Quartiles")+
  theme(axis.text.x = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 1.0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, 
                                   angle = 0, hjust = .5, 
                                   face = "plain"),
        axis.title.x = element_text(color = "black", size = 14,
                                    angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, 
                                    angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 12,
                                    angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) 

ppt_quant_rwi_sync_plot <- ggplot(data = ppt_quant_rwi_sync_CIs, aes(x = quantile, y = mean.rwi.sync, col = band)) +
  geom_point(size = 1.5)+
  geom_line(linewidth = 1.5)+
  geom_errorbar(aes(ymin = lower.ci.rwi.sync, ymax = upper.ci.rwi.sync, x = quantile, y=mean.rwi.sync, width = 0.2)) +
  theme_bw()+
  #ylim(0.0, 2.0) +
  scale_color_brewer(name = "Timescale Band", palette="RdYlBu", direction = -1, labels = c("Biennial (2-3 yrs)","Multiannual (3-10 yrs)", "Decadal (10-20 yrs)", "Multidecadal (20-30 yrs)"))+
  ylab("Average Growth Synchrony")+
  xlab("Precipitation Quartiles")+
  theme(axis.text.x = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 1.0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, 
                                   angle = 0, hjust = .5, 
                                   face = "plain"),
        axis.title.x = element_text(color = "black", size = 14,
                                    angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, 
                                    angle = 90, hjust = .5, face = "plain"),
        legend.title = element_text(color = "grey20", size = 12,
                                    angle = 0, hjust = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12,
                                   angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) 

#### anova tests across quartiles ####
# correction factor applied for 6 tests per band with 4 quantiles in each band
corr_p_value <- (0.05/6)

## precipitation synchrony across temperature quantiles ##
# make sure quantile is a factor
tmin_quant_ppt_sync$quantile <- factor(tmin_quant_ppt_sync$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
# create an empty list to store the results for each band
t.test_tmin_quant_ppt_sync_results <- list()

# loop through each timescale band
for (xx in 1:length(unique(tmin_quant_ppt_sync$band))) {
  
  current <- unique(tmin_quant_ppt_sync$band)[xx]
  band_data <- tmin_quant_ppt_sync %>%
    filter(band == current)
  
  # manually generate pairs in the desired order (1-2, 1-3, 1-4, etc.)
  pairs <- list()
  # specify the order of quartiles for pair generation (even if the factor levels are reversed)
  quartile_order <- c("1", "2", "3", "4")
  
  for (i in 1:(length(quartile_order) - 1)) {
    for (j in (i + 1):length(quartile_order)) {
      pairs[[length(pairs) + 1]] <- c(quartile_order[i], quartile_order[j])
    }
  }
  
  # ensure the factor levels in the data are still set to 4, 3, 2, 1
  band_data$quantile <- factor(band_data$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
  
  # perform t-tests manually for each pair and extract statistics
  results <- lapply(pairs, function(pair) {
    # subset data for the current pair
    subset_data <- subset(band_data, quantile %in% pair)
    
    # perform t-test
    t_test <- t.test(avg_sync ~ quantile, data = subset_data)
    
    # calculate mean difference between the two groups
    group_means <- aggregate(avg_sync ~ quantile, data = subset_data, mean)
    mean_diff <- diff(group_means$avg_sync)
    
    # extract p-values, confidence intervals, and t-statistics
    data.frame(
      Band = current,
      Group1 = pair[1],
      Group2 = pair[2],
      Mean_Difference = mean_diff,
      P_Value = t_test$p.value,
      Lower_CI = t_test$conf.int[1],
      Upper_CI = t_test$conf.int[2],
      T_Statistic = t_test$statistic,
      DF = t_test$parameter
    )
  })
  # combine the individual paired results into a data frame for the current band
  band_results_df <- do.call(rbind, results)
  
  # apply Bonferroni adjustment for the current band
  band_results_df$Adjusted_P_Value <- p.adjust(band_results_df$P_Value, method = "bonferroni")
  
  # combine the band-specific results
  t.test_tmin_quant_ppt_sync_results [[current]] <- band_results_df
}

# combine results for all bands into a data frame
t.test_tmin_quant_ppt_sync_results  <- do.call(rbind, t.test_tmin_quant_ppt_sync_results) %>%
  mutate(significant = case_when(Adjusted_P_Value <= corr_p_value ~ "yes", 
                                 Adjusted_P_Value > corr_p_value ~ "no"))
# multiply the mean difference by -1 to correct the direction of change from smaller quartiles to larger quartiles
t.test_tmin_quant_ppt_sync_results$Mean_Difference <- t.test_tmin_quant_ppt_sync_results$Mean_Difference * -1


## temperature synchrony across temperature quantiles ##
# make sure quantile is a factor
tmin_quant_tmin_sync$quantile <- factor(tmin_quant_tmin_sync$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
# create an empty list to store the results for each band
t.test_tmin_quant_tmin_sync_results <- list()

# loop through each timescale band
for (xx in 1:length(unique(tmin_quant_tmin_sync$band))) {
  
  current <- unique(tmin_quant_tmin_sync$band)[xx]
  band_data <- tmin_quant_tmin_sync %>%
    filter(band == current)
  
  # manually generate pairs in the desired order (1-2, 1-3, 1-4, etc.)
  pairs <- list()
  # specify the order of quartiles for pair generation (even if the factor levels are reversed)
  quartile_order <- c("1", "2", "3", "4")
  
  for (i in 1:(length(quartile_order) - 1)) {
    for (j in (i + 1):length(quartile_order)) {
      pairs[[length(pairs) + 1]] <- c(quartile_order[i], quartile_order[j])
    }
  }
  
  # ensure the factor levels in the data are still set to 4, 3, 2, 1
  band_data$quantile <- factor(band_data$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
  
  # perform t-tests manually for each pair and extract statistics
  results <- lapply(pairs, function(pair) {
    # subset data for the current pair
    subset_data <- subset(band_data, quantile %in% pair)
    
    # perform t-test
    t_test <- t.test(avg_sync ~ quantile, data = subset_data)
    
    # calculate mean difference between the two groups
    group_means <- aggregate(avg_sync ~ quantile, data = subset_data, mean)
    mean_diff <- diff(group_means$avg_sync)
    
    # extract p-values, confidence intervals, and t-statistics
    data.frame(
      Band = current,
      Group1 = pair[1],
      Group2 = pair[2],
      Mean_Difference = mean_diff,
      P_Value = t_test$p.value,
      Lower_CI = t_test$conf.int[1],
      Upper_CI = t_test$conf.int[2],
      T_Statistic = t_test$statistic,
      DF = t_test$parameter
    )
  })
  # combine the individual paired results into a data frame for the current band
  band_results_df <- do.call(rbind, results)
  
  # apply Bonferroni adjustment for the current band
  band_results_df$Adjusted_P_Value <- p.adjust(band_results_df$P_Value, method = "bonferroni")
  
  # combine the band-specific results
  t.test_tmin_quant_tmin_sync_results [[current]] <- band_results_df
}

# combine results for all bands into a data frame
t.test_tmin_quant_tmin_sync_results  <- do.call(rbind, t.test_tmin_quant_tmin_sync_results) %>%
  mutate(significant = case_when(Adjusted_P_Value <= corr_p_value ~ "yes", 
                                 Adjusted_P_Value > corr_p_value ~ "no"))
# multiply the mean difference by -1 to correct the direction of change from smaller quartiles to larger quartiles
t.test_tmin_quant_tmin_sync_results$Mean_Difference <- t.test_tmin_quant_tmin_sync_results$Mean_Difference * -1


## growth synchrony across temperature quantiles ##
# make sure quantile is a factor
tmin_quant_rwi_sync$quantile <- factor(tmin_quant_rwi_sync$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
# create an empty list to store the results for each band
t.test_tmin_quant_rwi_sync_results <- list()

# loop through each timescale band
for (xx in 1:length(unique(tmin_quant_rwi_sync$band))) {
  
  current <- unique(tmin_quant_rwi_sync$band)[xx]
  band_data <- tmin_quant_rwi_sync %>%
    filter(band == current)
  
  # manually generate pairs in the desired order (1-2, 1-3, 1-4, etc.)
  pairs <- list()
  # specify the order of quartiles for pair generation (even if the factor levels are reversed)
  quartile_order <- c("1", "2", "3", "4")
  
  for (i in 1:(length(quartile_order) - 1)) {
    for (j in (i + 1):length(quartile_order)) {
      pairs[[length(pairs) + 1]] <- c(quartile_order[i], quartile_order[j])
    }
  }
  
  # ensure the factor levels in the data are still set to 4, 3, 2, 1
  band_data$quantile <- factor(band_data$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
  
  # perform t-tests manually for each pair and extract statistics
  results <- lapply(pairs, function(pair) {
    # subset data for the current pair
    subset_data <- subset(band_data, quantile %in% pair)
    
    # perform t-test
    t_test <- t.test(avg_sync ~ quantile, data = subset_data)
    
    # calculate mean difference between the two groups
    group_means <- aggregate(avg_sync ~ quantile, data = subset_data, mean)
    mean_diff <- diff(group_means$avg_sync)
    
    # extract p-values, confidence intervals, and t-statistics
    data.frame(
      Band = current,
      Group1 = pair[1],
      Group2 = pair[2],
      Mean_Difference = mean_diff,
      P_Value = t_test$p.value,
      Lower_CI = t_test$conf.int[1],
      Upper_CI = t_test$conf.int[2],
      T_Statistic = t_test$statistic,
      DF = t_test$parameter
    )
  })
  # combine the individual paired results into a data frame for the current band
  band_results_df <- do.call(rbind, results)
  
  # apply Bonferroni adjustment for the current band
  band_results_df$Adjusted_P_Value <- p.adjust(band_results_df$P_Value, method = "bonferroni")
  
  # combine the band-specific results
  t.test_tmin_quant_rwi_sync_results [[current]] <- band_results_df
}

# combine results for all bands into a data frame
t.test_tmin_quant_rwi_sync_results  <- do.call(rbind, t.test_tmin_quant_rwi_sync_results) %>%
  mutate(significant = case_when(Adjusted_P_Value <= corr_p_value ~ "yes", 
                                 Adjusted_P_Value > corr_p_value ~ "no"))
# multiply the mean difference by -1 to correct the direction of change from smaller quartiles to larger quartiles
t.test_tmin_quant_rwi_sync_results$Mean_Difference <- t.test_tmin_quant_rwi_sync_results$Mean_Difference * -1


## temperature synchrony across temperature quantiles ##
# make sure quantile is a factor
tmin_quant_tmin_sync$quantile <- factor(tmin_quant_tmin_sync$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
# create an empty list to store the results for each band
t.test_tmin_quant_tmin_sync_results <- list()

# loop through each timescale band
for (xx in 1:length(unique(tmin_quant_tmin_sync$band))) {
  
  current <- unique(tmin_quant_tmin_sync$band)[xx]
  band_data <- tmin_quant_tmin_sync %>%
    filter(band == current)
  
  # manually generate pairs in the desired order (1-2, 1-3, 1-4, etc.)
  pairs <- list()
  # specify the order of quartiles for pair generation (even if the factor levels are reversed)
  quartile_order <- c("1", "2", "3", "4")
  
  for (i in 1:(length(quartile_order) - 1)) {
    for (j in (i + 1):length(quartile_order)) {
      pairs[[length(pairs) + 1]] <- c(quartile_order[i], quartile_order[j])
    }
  }
  
  # ensure the factor levels in the data are still set to 4, 3, 2, 1
  band_data$quantile <- factor(band_data$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
  
  # perform t-tests manually for each pair and extract statistics
  results <- lapply(pairs, function(pair) {
    # subset data for the current pair
    subset_data <- subset(band_data, quantile %in% pair)
    
    # perform t-test
    t_test <- t.test(avg_sync ~ quantile, data = subset_data)
    
    # calculate mean difference between the two groups
    group_means <- aggregate(avg_sync ~ quantile, data = subset_data, mean)
    mean_diff <- diff(group_means$avg_sync)
    
    # extract p-values, confidence intervals, and t-statistics
    data.frame(
      Band = current,
      Group1 = pair[1],
      Group2 = pair[2],
      Mean_Difference = mean_diff,
      P_Value = t_test$p.value,
      Lower_CI = t_test$conf.int[1],
      Upper_CI = t_test$conf.int[2],
      T_Statistic = t_test$statistic,
      DF = t_test$parameter
    )
  })
  # combine the individual paired results into a data frame for the current band
  band_results_df <- do.call(rbind, results)
  
  # apply Bonferroni adjustment for the current band
  band_results_df$Adjusted_P_Value <- p.adjust(band_results_df$P_Value, method = "bonferroni")
  
  # combine the band-specific results
  t.test_tmin_quant_tmin_sync_results [[current]] <- band_results_df
}

# combine results for all bands into a data frame
t.test_tmin_quant_tmin_sync_results  <- do.call(rbind, t.test_tmin_quant_tmin_sync_results) %>%
  mutate(significant = case_when(Adjusted_P_Value <= corr_p_value ~ "yes", 
                                 Adjusted_P_Value > corr_p_value ~ "no"))
# multiply the mean difference by -1 to correct the direction of change from smaller quartiles to larger quartiles
t.test_tmin_quant_tmin_sync_results$Mean_Difference <- t.test_tmin_quant_tmin_sync_results$Mean_Difference * -1


## temperature synchrony across precipitation quantiles ##
# make sure quantile is a factor
ppt_quant_tmin_sync$quantile <- factor(ppt_quant_tmin_sync$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
# create an empty list to store the results for each band
t.test_ppt_quant_tmin_sync_results <- list()

# loop through each timescale band
for (xx in 1:length(unique(ppt_quant_tmin_sync$band))) {
  
  current <- unique(ppt_quant_tmin_sync$band)[xx]
  band_data <- ppt_quant_tmin_sync %>%
    filter(band == current)
  
  # manually generate pairs in the desired order (1-2, 1-3, 1-4, etc.)
  pairs <- list()
  # specify the order of quartiles for pair generation (even if the factor levels are reversed)
  quartile_order <- c("1", "2", "3", "4")
  
  for (i in 1:(length(quartile_order) - 1)) {
    for (j in (i + 1):length(quartile_order)) {
      pairs[[length(pairs) + 1]] <- c(quartile_order[i], quartile_order[j])
    }
  }
  
  # ensure the factor levels in the data are still set to 4, 3, 2, 1
  band_data$quantile <- factor(band_data$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
  
  # perform t-tests manually for each pair and extract statistics
  results <- lapply(pairs, function(pair) {
    # subset data for the current pair
    subset_data <- subset(band_data, quantile %in% pair)
    
    # perform t-test
    t_test <- t.test(avg_sync ~ quantile, data = subset_data)
    
    # calculate mean difference between the two groups
    group_means <- aggregate(avg_sync ~ quantile, data = subset_data, mean)
    mean_diff <- diff(group_means$avg_sync)
    
    # extract p-values, confidence intervals, and t-statistics
    data.frame(
      Band = current,
      Group1 = pair[1],
      Group2 = pair[2],
      Mean_Difference = mean_diff,
      P_Value = t_test$p.value,
      Lower_CI = t_test$conf.int[1],
      Upper_CI = t_test$conf.int[2],
      T_Statistic = t_test$statistic,
      DF = t_test$parameter
    )
  })
  # combine the individual paired results into a data frame for the current band
  band_results_df <- do.call(rbind, results)
  
  # apply Bonferroni adjustment for the current band
  band_results_df$Adjusted_P_Value <- p.adjust(band_results_df$P_Value, method = "bonferroni")
  
  # combine the band-specific results
  t.test_ppt_quant_tmin_sync_results [[current]] <- band_results_df
}

# combine results for all bands into a data frame
t.test_ppt_quant_tmin_sync_results  <- do.call(rbind, t.test_ppt_quant_tmin_sync_results) %>%
  mutate(significant = case_when(Adjusted_P_Value <= corr_p_value ~ "yes", 
                                 Adjusted_P_Value > corr_p_value ~ "no"))
# multiply the mean difference by -1 to correct the direction of change from smaller quartiles to larger quartiles
t.test_ppt_quant_tmin_sync_results$Mean_Difference <- t.test_ppt_quant_tmin_sync_results$Mean_Difference * -1


## growth synchrony across precipitation quantiles ##
# make sure quantile is a factor
ppt_quant_rwi_sync$quantile <- factor(ppt_quant_rwi_sync$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
# create an empty list to store the results for each band
t.test_ppt_quant_rwi_sync_results <- list()

# loop through each timescale band
for (xx in 1:length(unique(ppt_quant_rwi_sync$band))) {
  
  current <- unique(ppt_quant_rwi_sync$band)[xx]
  band_data <- ppt_quant_rwi_sync %>%
    filter(band == current)
  
  # manually generate pairs in the desired order (1-2, 1-3, 1-4, etc.)
  pairs <- list()
  # specify the order of quartiles for pair generation (even if the factor levels are reversed)
  quartile_order <- c("1", "2", "3", "4")
  
  for (i in 1:(length(quartile_order) - 1)) {
    for (j in (i + 1):length(quartile_order)) {
      pairs[[length(pairs) + 1]] <- c(quartile_order[i], quartile_order[j])
    }
  }
  
  # ensure the factor levels in the data are still set to 4, 3, 2, 1
  band_data$quantile <- factor(band_data$quantile, levels = c("4", "3", "2", "1"), ordered = TRUE)
  
  # perform t-tests manually for each pair and extract statistics
  results <- lapply(pairs, function(pair) {
    # subset data for the current pair
    subset_data <- subset(band_data, quantile %in% pair)
    
    # perform t-test
    t_test <- t.test(avg_sync ~ quantile, data = subset_data)
    
    # calculate mean difference between the two groups
    group_means <- aggregate(avg_sync ~ quantile, data = subset_data, mean)
    mean_diff <- diff(group_means$avg_sync)
    
    # extract p-values, confidence intervals, and t-statistics
    data.frame(
      Band = current,
      Group1 = pair[1],
      Group2 = pair[2],
      Mean_Difference = mean_diff,
      P_Value = t_test$p.value,
      Lower_CI = t_test$conf.int[1],
      Upper_CI = t_test$conf.int[2],
      T_Statistic = t_test$statistic,
      DF = t_test$parameter
    )
  })
  # combine the individual paired results into a data frame for the current band
  band_results_df <- do.call(rbind, results)
  
  # apply Bonferroni adjustment for the current band
  band_results_df$Adjusted_P_Value <- p.adjust(band_results_df$P_Value, method = "bonferroni")
  
  # combine the band-specific results
  t.test_ppt_quant_rwi_sync_results [[current]] <- band_results_df
}

# combine results for all bands into a data frame
t.test_ppt_quant_rwi_sync_results  <- do.call(rbind, t.test_ppt_quant_rwi_sync_results) %>%
  mutate(significant = case_when(Adjusted_P_Value <= corr_p_value ~ "yes", 
                                 Adjusted_P_Value > corr_p_value ~ "no"))
# multiply the mean difference by -1 to correct the direction of change from smaller quartiles to larger quartiles
t.test_ppt_quant_rwi_sync_results$Mean_Difference <- t.test_ppt_quant_rwi_sync_results$Mean_Difference * -1






