## In this script, we fit a GLMER model to test sensitivity of tree growth to 
## precipiation across low, mean and high temperatures

# load necessary packages
library(lme4)
library(ggeffects)
library(lmerTest)

# source cleaned and subsetted datasets; avg_plot_growth, winter_ppt, summer_tmin 
source(here::here("Analyses/data_cleaning_and_subsetting.R"))

# define datasets
rwi_data <- avg_plot_growth
ppt_data <- winter_ppt %>%
  rename(year = "wateryear")
tmin_data <- summer_tmin

# make year a character
rwi_data$year <- as.character(rwi_data$year)
ppt_data$year <- as.character(ppt_data$year)
tmin_data$year <- as.character(tmin_data$year)


# join datasets 
sensitivity_data <- left_join(rwi_data, ppt_data) %>%
  left_join(tmin_data) %>%
  rename("ppt" = "winter_ppt", "tmin" = "summer_tmin", "rwi" = "avg_growth")

tmin_q <- quantile(sensitivity_data$tmin, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)

# fit the GLMER model
model <- lmer(
  rwi ~ tmin * ppt + (1 | plot),
  data = sensitivity_data
)

summary(model)

# predict growth patterns across precipitation values for the three tmin quantiles
pred_tmin_quant <- ggpredict(model, terms = c("ppt", "tmin [1.3, 3.67, 6.47]"))

# make the quantiles a factor and give them specific colors to plot
pred_tmin_quant$group <- factor(
  pred_tmin_quant$group,
  levels = c("1.3", "3.67", "6.47"),
  labels = c("Low: 1.3\u00B0C", "Mean: 3.67\u00B0C", "High: 6.47\u00B0C")
)

cols <- c(
  "Low: 1.3\u00B0C" = "#440154FF",
  "Mean: 3.67\u00B0C" = "#21908CFF",
  "High: 6.47\u00B0C" = "#FDE725FF"
)

# plot predicted RWI across precipiation values for each temperature condition
ggplot() +
  geom_line(data = pred_tmin_quant, aes(x = x, y = predicted, group = group, color = group), linewidth = 1.2) +
  geom_ribbon(data = pred_tmin_quant, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = group, fill=group),
              alpha = 0.08, show.legend=FALSE) +
  labs(x = "Precipitation (mm)", y = "Predicted RWI", color = "Temperature") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols)+
  theme_bw()
