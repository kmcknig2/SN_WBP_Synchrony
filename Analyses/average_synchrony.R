## In this script, we calculate average synchrony per year across four timescale bands:
## biennial (2–3 years), multiannual (3–10 years), decadal (10–20 years), and
## multidecadal (20–30 years). We then test null, linear, and quadratic models
## to evaluate trends in average synchrony over time across bands. Finally, we measure
## contrasts between the first and last years of synchrony data to test for significant 
## changes overtime in growth synchrony only.

# load necessary packages
library("tidyverse")
library("glmmTMB")
library("ggeffects")
library("here")
library("purrr")
library("emmeans")
library("broom")

# source outputs from wavelet script: res_growth_wmf, res_ppt_wmf, res_tmin_wmf
source(here::here("Analyses/wavelets.R"))
# source AIC function to test model fit
source(here::here("Functions/AIC.R"))


#### average growth synchrony ####
# extract wavelet modulus values for growth synchrony, make timescales the column names
growth_sync_values <- as.data.frame(res_growth_wmf$values)
colnames(growth_sync_values) <- res_growth_wmf$timescales

# remove imaginary components
growth_sync_values <- abs(growth_sync_values) 

# add year column and reshape to long format
growth_sync_values$year <- 1900:2018
growth_sync_long <- growth_sync_values %>%
  pivot_longer(cols = 1:67, names_to = "ts", values_to = "values") %>%
  drop_na()

# convert timescale column to numeric and classify timescale band intervals
growth_sync_long$ts <- as.numeric(growth_sync_long$ts)
growth_sync_long <- growth_sync_long %>%
  mutate(band = case_when(
    ts >= 2 & ts <= 3 ~ "biennial",
    ts > 3  & ts <= 10 ~ "multiannual",
    ts > 10 & ts <= 20 ~ "decadal",
    ts > 20 & ts <= 30 ~ "multidecadal"
  ))

# identify how many timescales occur per band and year
growth_ts_coverage <- growth_sync_long %>%
  group_by(year, band) %>%
  summarise(num_ts = n())

# determine how many unique timescales are in each band
growth_ts_counts <- growth_sync_long %>%
  group_by(band) %>%
  summarise(ts_num = n_distinct(ts))

# filter for years with full timescale coverage for each band
growth_band_coverage <- growth_ts_coverage %>%
  left_join(growth_ts_counts, by = "band") %>%
  filter(num_ts == ts_num) %>%
  unite("uID", year, band, remove = FALSE)

# recombine and filter full dataset to only include those complete time-band combos
growth_sync_long <- growth_sync_long %>%
  select(year, band, ts, values) %>%
  unite("uID", year, band, remove = FALSE)
growth_filtered_timeseries <- growth_sync_long %>%
  filter(uID %in% growth_band_coverage$uID)

# calculate average synchrony per year and band
avg_growth_sync <- growth_filtered_timeseries %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(values))

# scale year term for models
avg_growth_sync$scaled_year <- scale(avg_growth_sync$year)

# round scaled year term to join original dataset with predicted dataset by x (aka. scaled_year or year)
avg_growth_sync$x <- as.numeric(round(avg_growth_sync$scaled_year,2))

# separate out the biennial band
avg_growth_sync_biennial <- avg_growth_sync %>%
  filter(band == "biennial")

# test null, linear, and quadratic models
growth_b_null_model <- glmmTMB(avg_sync~1, data = avg_growth_sync_biennial)
growth_b_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_growth_sync_biennial)
growth_b_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_growth_sync_biennial)

# AIC comparison
growth_b_aic <- AIC(growth_b_null_model, growth_b_linear_model, growth_b_quad_model)
# quadratic model fit best

# repeat for multiannual band
avg_growth_sync_multiannual <- avg_growth_sync%>%
  filter(band == "multiannual")
growth_ma_null_model <- glmmTMB(avg_sync~1, data = avg_growth_sync_multiannual)
growth_ma_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_growth_sync_multiannual)
growth_ma_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_growth_sync_multiannual)
growth_ma_aic <- AIC(growth_ma_null_model, growth_ma_linear_model, growth_ma_quad_model)
# quadratic model fit best

# repeat for decadal band
avg_growth_sync_decadal <- avg_growth_sync%>%
  filter(band == "decadal")
growth_d_null_model <- glmmTMB(avg_sync~1, data = avg_growth_sync_decadal)
growth_d_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_growth_sync_decadal)
growth_d_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_growth_sync_decadal)
growth_d_aic <- AIC(growth_d_null_model, growth_d_linear_model, growth_d_quad_model)
# quadratic model fit best

# repeat for multidecadal band
avg_growth_sync_multidecadal <- avg_growth_sync%>%
  filter(band == "multidecadal")
growth_md_null_model <- glmmTMB(avg_sync~1, data = avg_growth_sync_multidecadal)
growth_md_linear_model <- glmmTMB(avg_sync ~ scaled_year, data= avg_growth_sync_multidecadal)
growth_md_quad_model <- glmmTMB(avg_sync~ poly(scaled_year, 2, raw=TRUE), data= avg_growth_sync_multidecadal)
growth_md_aic <- AIC(growth_md_null_model, growth_md_linear_model, growth_md_quad_model)
# quadratic model fit best

# predict growth synchrony using quadratic models (best fit for all bands)
growth_b_vis_prod <- ggpredict(growth_b_quad_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci_level = .95)
growth_b_vis_prod$band <- "biennial"

growth_ma_vis_prod <- ggpredict(growth_ma_quad_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci_level = .95)
growth_ma_vis_prod$band <- "multiannual"

growth_d_vis_prod <- ggpredict(growth_d_quad_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci_level = .95)
growth_d_vis_prod$band <- "decadal"

growth_md_vis_prod <- ggpredict(growth_md_quad_model, 
                       terms = c("scaled_year[all]"), 
                       type = "fe", 
                       ci_level = .95)
growth_md_vis_prod$band <- "multidecadal"

# bind all predicted outputs
predicted_avg_growth_sync <- rbind(growth_b_vis_prod, growth_ma_vis_prod, growth_d_vis_prod, growth_md_vis_prod)

# join raw and predicted 
final_avg_growth_sync <- inner_join(avg_growth_sync, predicted_avg_growth_sync, by=join_by(band, x))

# make year a character for plotting
final_avg_growth_sync$year <- as.character(final_avg_growth_sync$year)

# factorize bands in timescale order: biennial, multiannual, decadal, multidecadal
final_avg_growth_sync$band <- factor(final_avg_growth_sync$band, 
                               levels = c("biennial", "multiannual", "decadal", "multidecadal"))

# plot average growth synchrony, both raw and predicted values
avg_growth_sync_plot <- ggplot() +
  geom_point(data = final_avg_growth_sync, aes(x=year, y=avg_sync, col=band), alpha = 0.4) +
  geom_line(data = final_avg_growth_sync, aes(x=year, y=predicted, group=band, col=band), linewidth = 1) +
  geom_ribbon(data = final_avg_growth_sync, aes(
    x = year,
    y = predicted,
    group = band,
    fill = band,
    alpha = 0.4,
    ymin = conf.low,
    ymax = conf.high),
    show.legend = FALSE) +
  theme_bw() +
  scale_x_discrete(breaks = seq(1920, 2020, 20)) +
  scale_color_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    ),
    labels = c(
      "biennial" = "Biennial (2-3 yrs)",
      "multiannual" = "Multiannual (3-10 yrs)",
      "decadal" = "Decadal (10-20 yrs)",
      "multidecadal" = "Multidecadal (20-30 yrs)"
    )
  ) +
  scale_fill_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    )
  ) +
  theme(
    axis.text.x = element_text(color = "grey20", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    legend.title = element_blank(),
    legend.text = element_text(color = "grey20", size = 10),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  ylab("Average Growth Synchrony") +
  xlab("Year")

# use emmeans to compare min and max years of growth synchrony data
# extract the min and max years and corresponding scaled_year values for each band
growth_extremes <- final_avg_growth_sync %>%
  group_by(band) %>%
  filter(year == max(year) | year == min(year)) %>%
  arrange(band, year) %>%
  select(band, year, scaled_year)

# create named list of best fit models per band (all quadratic)
band_models <- list(
  biennial = growth_b_quad_model,
  multiannual = growth_ma_quad_model,
  decadal = growth_d_quad_model,
  multidecadal = growth_md_quad_model
)

# loop and return contrasts for each band
contrast_summary <- map_dfr(names(band_models), function(band_name) {
  
  # subset the extremes for each band and ensure chronological order
  band_extremes <- growth_extremes %>%
    filter(band == band_name) %>%
    arrange(year)
  
  year_min <- band_extremes$year[1]
  year_max <- band_extremes$year[2]
  
  scaled_min <- band_extremes$scaled_year[1]
  scaled_max <- band_extremes$scaled_year[2]
  
  # get the appropriate model
  model <- band_models[[band_name]]
  
  # estimate contrast between both time points and return outputs as a dataframe
  em_res <- emmeans(model, ~ scaled_year, at = list(scaled_year = c(scaled_min, scaled_max)))
  
  contrast <- contrast(em_res, method = list("end - start" = c(-1, 1))) %>%
    tidy() %>%
    mutate(
      band = band_name,
      year_min = year_min,
      year_max = year_max,
      direction = case_when(
        estimate > 0 ~ "increasing",
        estimate < 0 ~ "decreasing",
        TRUE ~ "no change"
      )
    ) %>%
    select(band, year_min, year_max, estimate, std.error, statistic, p.value, direction)
  
  return(contrast)
})

#### average precipitation synchrony ####
# extract wavelet modulus values for precipitation synchrony, make timescales the column names
ppt_sync_values <- as.data.frame(res_ppt_wmf$values)
colnames(ppt_sync_values) <- res_ppt_wmf$timescales

# remove imaginary components
ppt_sync_values <- abs(ppt_sync_values)

# add year column and reshape to long format
ppt_sync_values$year <- 1900:2018
ppt_sync_long <- ppt_sync_values %>%
  pivot_longer(cols = 1:67, names_to = "ts", values_to = "values") %>%
  drop_na()

# convert timescale column to numeric and classify timescale band intervals
ppt_sync_long$ts <- as.numeric(ppt_sync_long$ts)
ppt_sync_long <- ppt_sync_long %>%
  mutate(band = case_when(
    ts >= 2 & ts <= 3 ~ "biennial",
    ts > 3  & ts <= 10 ~ "multiannual",
    ts > 10 & ts <= 20 ~ "decadal",
    ts > 20 & ts <= 30 ~ "multidecadal"
  ))

# identify how many timescales occur per band and year
ppt_ts_coverage <- ppt_sync_long %>%
  group_by(year, band) %>%
  summarise(num_ts = n())

# determine how many unique timescales are in each band
ppt_ts_counts <- ppt_sync_long %>%
  group_by(band) %>%
  summarise(ts_num = n_distinct(ts))

# filter for years with full timescale coverage for each band
ppt_band_coverage <- ppt_ts_coverage %>%
  left_join(ppt_ts_counts, by = "band") %>%
  filter(num_ts == ts_num) %>%
  unite("uID", year, band, remove = FALSE)

# recombine and filter full dataset to only include those complete time-band combos
ppt_sync_long <- ppt_sync_long %>%
  select(year, band, ts, values) %>%
  unite("uID", year, band, remove = FALSE)
ppt_filtered_timeseries <- ppt_sync_long %>%
  filter(uID %in% ppt_band_coverage$uID)

# calculate average synchrony per year and band
avg_ppt_sync <- ppt_filtered_timeseries %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(values))

# scale year term for models
avg_ppt_sync$scaled_year <- scale(avg_ppt_sync$year)

# round scaled year term to join original dataset with predicted dataset by x (aka. scaled_year or year)
avg_ppt_sync$x <- as.numeric(round(avg_ppt_sync$scaled_year,2))

# separate out the biennial band to test null, linear, and quadratic models
avg_ppt_sync_biennial <- avg_ppt_sync %>%
  filter(band == "biennial")

# test null, linear, and quadratic models for biennial
ppt_b_null_model <- glmmTMB(avg_sync~1, data = avg_ppt_sync_biennial)
ppt_b_linear_model <- glmmTMB(avg_sync ~ scaled_year, data = avg_ppt_sync_biennial)
ppt_b_quad_model <- glmmTMB(avg_sync ~ poly(scaled_year, 2, raw=TRUE), data = avg_ppt_sync_biennial)

# AIC comparison
ppt_b_aic <- AIC(ppt_b_null_model, ppt_b_linear_model, ppt_b_quad_model)
# linear model fit best

# repeat for multiannual band
avg_ppt_sync_multiannual <- avg_ppt_sync %>%
  filter(band == "multiannual")
ppt_ma_null_model <- glmmTMB(avg_sync~1, data = avg_ppt_sync_multiannual)
ppt_ma_linear_model <- glmmTMB(avg_sync ~ scaled_year, data = avg_ppt_sync_multiannual)
ppt_ma_quad_model <- glmmTMB(avg_sync ~ poly(scaled_year, 2, raw=TRUE), data = avg_ppt_sync_multiannual)
ppt_ma_aic <- AIC(ppt_ma_null_model, ppt_ma_linear_model, ppt_ma_quad_model)
# null model fit best

# repeat for decadal band
avg_ppt_sync_decadal <- avg_ppt_sync %>%
  filter(band == "decadal")
ppt_d_null_model <- glmmTMB(avg_sync~1, data = avg_ppt_sync_decadal)
ppt_d_linear_model <- glmmTMB(avg_sync ~ scaled_year, data = avg_ppt_sync_decadal)
ppt_d_quad_model <- glmmTMB(avg_sync ~ poly(scaled_year, 2, raw=TRUE), data = avg_ppt_sync_decadal)
ppt_d_aic <- AIC(ppt_d_null_model, ppt_d_linear_model, ppt_d_quad_model)
# quadratic model fit best 

# repeat for multidecadal band
avg_ppt_sync_multidecadal <- avg_ppt_sync %>%
  filter(band == "multidecadal")
ppt_md_null_model <- glmmTMB(avg_sync~1, data = avg_ppt_sync_multidecadal)
ppt_md_linear_model <- glmmTMB(avg_sync ~ scaled_year, data = avg_ppt_sync_multidecadal)
ppt_md_quad_model <- glmmTMB(avg_sync ~ poly(scaled_year, 2, raw=TRUE), data = avg_ppt_sync_multidecadal)
ppt_md_aic <- AIC(ppt_md_null_model, ppt_md_linear_model, ppt_md_quad_model)
# quadratic model fit best

# predict precipitation synchrony using best fit models
ppt_b_vis_prod <- ggpredict(ppt_b_linear_model, terms = c("scaled_year[all]"), type = "fe", ci_level = 0.95)
ppt_b_vis_prod$band <- "biennial"

# null models can not be predicted using ggpredict, create predicted output from null model intercept
ppt_ma_null_intercept <- fixef(ppt_ma_null_model)$cond["(Intercept)"]
ppt_ma_null_vcov_matrix <- vcov(ppt_ma_null_model)$cond
ppt_ma_null_se_intercept <- sqrt(ppt_ma_null_vcov_matrix["(Intercept)", "(Intercept)"])
ppt_ma_null_ci_low <- ppt_ma_null_intercept - 1.96 * ppt_ma_null_se_intercept
ppt_ma_null_ci_high <- ppt_ma_null_intercept + 1.96 * ppt_ma_null_se_intercept
ppt_ma_vis_prod <- data.frame(
  x = avg_ppt_sync_multiannual$x, 
  predicted = ppt_ma_null_intercept,
  std.error = ppt_ma_null_se_intercept,
  conf.low = ppt_ma_null_ci_low,
  conf.high = ppt_ma_null_ci_high,
  group = "1",
  band = "multiannual"
)

ppt_d_vis_prod <- ggpredict(ppt_d_quad_model, terms = c("scaled_year[all]"), type = "fe", ci_level = 0.95)
ppt_d_vis_prod$band <- "decadal"

ppt_md_vis_prod <- ggpredict(ppt_md_quad_model, terms = c("scaled_year[all]"), type = "fe", ci_level = 0.95)
ppt_md_vis_prod$band <- "multidecadal"

# bind all predicted outputs
predicted_avg_ppt_sync <- rbind(ppt_b_vis_prod, ppt_ma_vis_prod, ppt_d_vis_prod, ppt_md_vis_prod)

# join raw and predicted
final_avg_ppt_sync <- inner_join(avg_ppt_sync, predicted_avg_ppt_sync, by = join_by(band, x))

# make year a character for plotting
final_avg_ppt_sync$year <- as.character(final_avg_ppt_sync$year)

# factorize band order
final_avg_ppt_sync$band <- factor(final_avg_ppt_sync$band, levels = c("biennial", "multiannual", "decadal", "multidecadal"))

# plot average ppt synchrony
avg_ppt_sync_plot <- ggplot() +
  geom_point(data = final_avg_ppt_sync, aes(x = year, y = avg_sync, col = band), alpha = 0.4) +
  geom_line(data = final_avg_ppt_sync, aes(x = year, y = predicted, group = band, col = band), linewidth = 1) +
  geom_ribbon(data = final_avg_ppt_sync, aes(
    x = year,
    y = predicted,
    group = band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.4,
    show.legend = FALSE
  ) +
  theme_bw() +
  scale_x_discrete(breaks = seq(1920, 2020, 20)) +
  scale_color_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    )
  ) +
  scale_fill_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    )
  ) +
  theme(
    axis.text.x = element_text(color = "grey20", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    legend.title = element_blank(),
    legend.text = element_text(color = "grey20", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  ylab("Average Precipitation Synchrony") +
  xlab("Year")

#### average temperature synchrony ####
# extract wavelet modulus values for temperature synchrony, make timescales the column names
tmin_sync_values <- as.data.frame(res_tmin_wmf$values)
colnames(tmin_sync_values) <- res_tmin_wmf$timescales

# remove imaginary components
tmin_sync_values <- abs(tmin_sync_values)

# add year column and reshape to long format
tmin_sync_values$year <- 1900:2018
tmin_sync_long <- tmin_sync_values %>%
  pivot_longer(cols = 1:67, names_to = "ts", values_to = "values") %>%
  drop_na()

# convert timescale column to numeric and classify timescale band intervals
tmin_sync_long$ts <- as.numeric(tmin_sync_long$ts)
tmin_sync_long <- tmin_sync_long %>%
  mutate(band = case_when(
    ts >= 2 & ts <= 3 ~ "biennial",
    ts > 3  & ts <= 10 ~ "multiannual",
    ts > 10 & ts <= 20 ~ "decadal",
    ts > 20 & ts <= 30 ~ "multidecadal"
  ))

# identify how many timescales occur per band and year
tmin_ts_coverage <- tmin_sync_long %>%
  group_by(year, band) %>%
  summarise(num_ts = n())

# determine how many unique timescales are in each band
tmin_ts_counts <- tmin_sync_long %>%
  group_by(band) %>%
  summarise(ts_num = n_distinct(ts))

# filter for years with full timescale coverage for each band
tmin_band_coverage <- tmin_ts_coverage %>%
  left_join(tmin_ts_counts, by = "band") %>%
  filter(num_ts == ts_num) %>%
  unite("uID", year, band, remove = FALSE)

# recombine and filter full dataset to only include those complete time-band combos
tmin_sync_long <- tmin_sync_long %>%
  select(year, band, ts, values) %>%
  unite("uID", year, band, remove = FALSE)
tmin_filtered_timeseries <- tmin_sync_long %>%
  filter(uID %in% tmin_band_coverage$uID)

# calculate average synchrony per year and band
avg_tmin_sync <- tmin_filtered_timeseries %>%
  group_by(year, band) %>%
  summarise(avg_sync = mean(values))

# scale year term for models
avg_tmin_sync$scaled_year <- scale(avg_tmin_sync$year)

# round scaled year term to join original dataset with predicted dataset by x (aka. scaled_year or year)
avg_tmin_sync$x <- as.numeric(round(avg_tmin_sync$scaled_year,2))

# separate out the biennial band to test null, linear, and quadratic models
avg_tmin_sync_biennial <- avg_tmin_sync %>%
  filter(band == "biennial")

# test null, linear, and quadratic models for biennial
tmin_b_null_model <- glmmTMB(avg_sync~1, data = avg_tmin_sync_biennial)
tmin_b_linear_model <- glmmTMB(avg_sync ~ scaled_year, data = avg_tmin_sync_biennial)
tmin_b_quad_model <- glmmTMB(avg_sync ~ poly(scaled_year, 2, raw=TRUE), data = avg_tmin_sync_biennial)

# AIC comparison
tmin_b_aic <- AIC(tmin_b_null_model, tmin_b_linear_model, tmin_b_quad_model)
# null model fit best

# repeat for multiannual band
avg_tmin_sync_multiannual <- avg_tmin_sync %>%
  filter(band == "multiannual")
tmin_ma_null_model <- glmmTMB(avg_sync~1, data = avg_tmin_sync_multiannual)
tmin_ma_linear_model <- glmmTMB(avg_sync ~ scaled_year, data = avg_tmin_sync_multiannual)
tmin_ma_quad_model <- glmmTMB(avg_sync ~ poly(scaled_year, 2, raw=TRUE), data = avg_tmin_sync_multiannual)
tmin_ma_aic <- AIC(tmin_ma_null_model, tmin_ma_linear_model, tmin_ma_quad_model)
# linear model fit best

# repeat for decadal band
avg_tmin_sync_decadal <- avg_tmin_sync %>%
  filter(band == "decadal")
tmin_d_null_model <- glmmTMB(avg_sync~1, data = avg_tmin_sync_decadal)
tmin_d_linear_model <- glmmTMB(avg_sync ~ scaled_year, data = avg_tmin_sync_decadal)
tmin_d_quad_model <- glmmTMB(avg_sync ~ poly(scaled_year, 2, raw=TRUE), data = avg_tmin_sync_decadal)
tmin_d_aic <- AIC(tmin_d_null_model, tmin_d_linear_model, tmin_d_quad_model)
# quadratic model fit best 

# repeat for multidecadal band
avg_tmin_sync_multidecadal <- avg_tmin_sync %>%
  filter(band == "multidecadal")
tmin_md_null_model <- glmmTMB(avg_sync~1, data = avg_tmin_sync_multidecadal)
tmin_md_linear_model <- glmmTMB(avg_sync ~ scaled_year, data = avg_tmin_sync_multidecadal)
tmin_md_quad_model <- glmmTMB(avg_sync ~ poly(scaled_year, 2, raw=TRUE), data = avg_tmin_sync_multidecadal)
tmin_md_aic <- AIC(tmin_md_null_model, tmin_md_linear_model, tmin_md_quad_model)
# quadratic model fit best

# predict temperature synchrony using best fit models
# null models can not be predicted using ggpredict, create predicted output from null model intercept
tmin_b_null_intercept <- fixef(tmin_b_null_model)$cond["(Intercept)"]
tmin_b_null_vcov_btrix <- vcov(tmin_b_null_model)$cond
tmin_b_null_se_intercept <- sqrt(tmin_b_null_vcov_btrix["(Intercept)", "(Intercept)"])
tmin_b_null_ci_low <- tmin_b_null_intercept - 1.96 * tmin_b_null_se_intercept
tmin_b_null_ci_high <- tmin_b_null_intercept + 1.96 * tmin_b_null_se_intercept
tmin_b_vis_prod <- data.frame(
  x = avg_tmin_sync_multiannual$x,
  predicted = tmin_b_null_intercept,
  std.error = tmin_b_null_se_intercept,
  conf.low = tmin_b_null_ci_low,
  conf.high = tmin_b_null_ci_high,
  group = "1",
  band = "biennial"
)

tmin_ma_vis_prod <- ggpredict(tmin_d_linear_model, terms = c("scaled_year[all]"), type = "fe", ci_level = 0.95)
tmin_ma_vis_prod$band <- "multiannual"

tmin_d_vis_prod <- ggpredict(tmin_d_quad_model, terms = c("scaled_year[all]"), type = "fe", ci_level = 0.95)
tmin_d_vis_prod$band <- "decadal"

tmin_md_vis_prod <- ggpredict(tmin_md_quad_model, terms = c("scaled_year[all]"), type = "fe", ci_level = 0.95)
tmin_md_vis_prod$band <- "multidecadal"

# bind all predicted outputs
predicted_avg_tmin_sync <- rbind(tmin_b_vis_prod, tmin_ma_vis_prod, tmin_d_vis_prod, tmin_md_vis_prod)

# join raw and predicted
final_avg_tmin_sync <- inner_join(avg_tmin_sync, predicted_avg_tmin_sync, by = join_by(band, x))

# make year a character for plotting
final_avg_tmin_sync$year <- as.character(final_avg_tmin_sync$year)

# factorize band order
final_avg_tmin_sync$band <- factor(final_avg_tmin_sync$band, levels = c("biennial", "multiannual", "decadal", "multidecadal"))

# plot average tmin synchrony
avg_tmin_sync_plot <- ggplot() +
  geom_point(data = final_avg_tmin_sync, aes(x = year, y = avg_sync, col = band), alpha = 0.4) +
  geom_line(data = final_avg_tmin_sync, aes(x = year, y = predicted, group = band, col = band), linewidth = 1) +
  geom_ribbon(data = final_avg_tmin_sync, aes(
    x = year,
    y = predicted,
    group = band,
    fill = band,
    ymin = conf.low,
    ymax = conf.high),
    alpha = 0.4,
    show.legend = FALSE
  ) +
  theme_bw() +
  scale_x_discrete(breaks = seq(1920, 2020, 20)) +
  scale_color_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    )
  ) +
  scale_fill_manual(
    values = c(
      "multidecadal" = "#EE5A36",
      "decadal" = "#F5AB54",
      "multiannual" = "#9FC4E8",
      "biennial" = "darkslateblue"
    )
  ) +
  theme(
    axis.text.x = element_text(color = "grey20", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    legend.title = element_blank(),
    legend.text = element_text(color = "grey20", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  ylab("Average temperature Synchrony") +
  xlab("Year")

