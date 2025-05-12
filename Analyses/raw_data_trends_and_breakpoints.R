## In this script, we analyze trends in raw data for each variable and calculate
## breakpoints to assess significant changes in the mean. 

# load necessary packages
library("segmented")

# source cleaned and subsetted datasets; avg_plot_growth_mx, winter_ppt_mx, summer_tmin_mx 
source(here::here("Analyses/data_cleaning_and_subsetting.R"))

#### trends in growth ####
# make year column names
colnames(avg_plot_growth_wide) <- 1900:2018

# pivot longer and calculate average growth across plots per year
avg_rwi <- avg_plot_growth_wide %>%
  pivot_longer(1:119, names_to = "year", values_to = "rwi")%>%
  group_by(year)%>%
  summarize(trend = mean(rwi))
avg_rwi$plot <- "Trend"

# make year a character for plotting
avg_rwi$year <- as.character(avg_rwi$year)
avg_plot_growth$year <- as.character(avg_plot_growth$year)

# plot growth over time with individual populations in grey scale and overall trend in black
raw_rwi <- ggplot()+
  geom_line(data = avg_plot_growth, aes(x=year, y=avg_growth, group=plot, col=plot))+
  geom_line(data = avg_rwi, mapping = aes(x=year, y=trend, group = plot, col = plot), color = "black", linewidth = 1.25)+
  theme_bw()+
  scale_colour_grey(start = 1, end = 0.5)+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Average Annual Growth \n(Ring Width Index)")

#### trends in precipitation ####
# make year column names
colnames(winter_ppt_wide) <- 1900:2018

# pivot longer and calculate average precipitation across plots per year
winter_ppt_trend <- winter_ppt_wide %>%
  pivot_longer(1:119, names_to = "wateryear", values_to = "ppt")%>%
  group_by(wateryear)%>%
  summarize(trend = mean(ppt))
winter_ppt_trend$plot <- "Trend"

# make year a character for plotting
winter_ppt$wateryear <- as.character(winter_ppt$wateryear)
winter_ppt_trend$wateryear <- as.character(winter_ppt_trend$wateryear)

# plot precipiation over time with individual populations in grey scale and overall trend in black
raw_ppt <- ggplot()+
  geom_line(data = winter_ppt, mapping = aes(x=wateryear, y=winter_ppt, group=plot, col=plot))+
  geom_line(data = winter_ppt_trend, mapping = aes(x=wateryear, y=trend, group = plot, col = plot), color = "black", linewidth = 1.25)+
  theme_bw()+
  scale_colour_grey(start = 1, end = 0.5)+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Winter Precipitation \n(Oct - May, mm)")

#### trends in temperature ####
# make year column names
colnames(summer_tmin_wide) <- 1900:2018

# pivot longer and calculate average temperature across plots per year
summer_tmin_trend <- summer_tmin_wide %>%
  pivot_longer(1:119, names_to = "year", values_to = "tmin")%>%
  group_by(year)%>%
  summarize(trend = mean(tmin))
summer_tmin_trend$plot <- "Trend"

# make year a character for plotting
summer_tmin$year <- as.character(summer_tmin$year)
summer_tmin_trend$year <- as.character(summer_tmin_trend$year)

# plot temperature over time with individual populations in grey scale and overall trend in black
raw_tmin <- ggplot()+
  geom_line(data = summer_tmin, aes(x=year, y=summer_tmin, group=plot, col=plot))+
  geom_line(data = summer_tmin_trend, mapping = aes(x=year, y=trend, group = plot, col = plot), color = "black", linewidth = 1.25)+
  theme_bw()+
  scale_colour_grey(start = 1, end = 0.5)+
  scale_x_discrete(breaks = seq(1900,2018,10))+
  theme(axis.text.x = element_text(color = "grey20", size = 14, angle = 45, hjust = 1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, face = "plain"),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(color = "grey20", size = 10,angle = 0, hjust = 0, face = "plain"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year")+
  ylab("Summer Minimum Temperatures \n(Jun - Aug, C)")


#### breakpoints in growth ####
# convert year to numeric
avg_rwi$year <- as.numeric(avg_rwi$year)

# create initial line plot of growth over time
rwi.p <- ggplot(data = avg_rwi, aes(x=year, y = trend)) + geom_line()

# fit simple linear model with the trend in growth as a function of year
rwi.lm <- lm(trend~year, data = avg_rwi)

# extract coefficients from the linear model
rwi.coeff <- coef(rwi.lm)

# add the linear model to the initial plot
rwi.p.line <- rwi.p + geom_abline(intercept = rwi.coeff[1],
                                  slope = rwi.coeff[2])

# fit a segmented regression to identify breakpoints in the trend,
# using 1974 as an initial guess (segmented will still identify the correct breakpoint but needs an intitial input)
rwi.seg <- segmented(rwi.lm,
                     seg.Z = ~ year,
                     psi = list(year = c(1974)))

# extract the fitted values from the segmented model
rwi.fitted <- fitted(rwi.seg)

# create a dataframe with year and the fitted segmented trend
rwi.model <- data.frame(year = avg_rwi$year, trend = rwi.fitted)

# add the segmented fit line on the plot 
rwi.p <- rwi.p + geom_line(data = rwi.model, aes(x=year, y = trend), color = "tomato")

# extract the estimated breakpoint values from the model
rwi.lines <- rwi.seg$psi[,2]

# add a vertical dashed line to the plot at the estimated breakpoint
rwi.p <- rwi.p + geom_vline(xintercept = rwi.lines, linetype = "dashed", color = "darkgrey") +
  labs(y = "average annual growth")+
  theme_bw()


#### breakpoints in precipitation ####
# convert wateryear to numeric
winter_ppt_trend$wateryear <- as.numeric(winter_ppt_trend$wateryear)

# create initial line plot of precipitation over time
ppt.p <- ggplot(data = winter_ppt_trend, aes(x=wateryear, y = trend)) + geom_line()

# fit simple linear model with the trend in precipitation as a function of wateryear
ppt.lm <- lm(trend~wateryear, data = winter_ppt_trend)

# extract coefficients from the linear model
ppt.coeff <- coef(ppt.lm)

# add the linear model to the initial plot
ppt.p.line <- ppt.p + geom_abline(intercept = rwi.coeff[1],
                                  slope = rwi.coeff[2])

# fit a segmented regression to identify breakpoints in the trend,
# using 1974 as an initial guess (segmented will still identify the correct breakpoint but needs an intitial input)
ppt.seg <- segmented(ppt.lm,
                     seg.Z = ~ wateryear,
                     psi = list(wateryear = c(1974)))

# extract the fitted values from the segmented model
ppt.fitted <- fitted(ppt.seg)

# create a dataframe with year and the fitted segmented trend
ppt.model <- data.frame(year = winter_ppt_trend$wateryear, trend = ppt.fitted)

# add the segmented fit line on the plot 
ppt.p <- ppt.p + geom_line(data = ppt.model, aes(x=year, y = trend), color = "tomato")

# extract the estimated breakpoint values from the model
ppt.lines <- ppt.seg$psi[,2]

# add a vertical dashed line to the plot at the estimated breakpoint
ppt.p <- ppt.p + geom_vline(xintercept = ppt.lines, linetype = "dashed", color = "darkgrey") +
  labs(y = "average annual precipitation")+
  theme_bw()

#### breakpoints in temperature ####
# convert year to numeric
summer_tmin_trend$year <- as.numeric(summer_tmin_trend$year)

# create initial line plot of temperature over time
tmin.p <- ggplot(data = summer_tmin_trend, aes(x=year, y = trend)) + geom_line()

# fit simple linear model with the trend in temperature as a function of year
tmin.lm <- lm(trend~year, data = summer_tmin_trend)

# extract coefficients from the linear model
tmin.coeff <- coef(tmin.lm)

# add the linear model to the initial plot
tmin.p.line <- tmin.p + geom_abline(intercept = rwi.coeff[1],
                                  slope = rwi.coeff[2])

# fit a segmented regression to identify breakpoints in the trend,
# using 1974 as an initial guess (segmented will still identify the correct breakpoint but needs an intitial input)
tmin.seg <- segmented(tmin.lm,
                     seg.Z = ~ year,
                     psi = list(year = c(1974)))

# extract the fitted values from the segmented model
tmin.fitted <- fitted(tmin.seg)

# create a dataframe with year and the fitted segmented trend
tmin.model <- data.frame(year = summer_tmin_trend$year, trend = tmin.fitted)

# add the segmented fit line on the plot 
tmin.p <- tmin.p + geom_line(data = tmin.model, aes(x=year, y = trend), color = "tomato")

# extract the estimated breakpoint values from the model
tmin.lines <- tmin.seg$psi[,2]

# add a vertical dashed line to the plot at the estimated breakpoint
tmin.p <- tmin.p + geom_vline(xintercept = tmin.lines, linetype = "dashed", color = "darkgrey") +
  labs(y = "average annual temperature")+
  theme_bw()
