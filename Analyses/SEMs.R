## In this script, we perform structural equation modelling to test direct and 
## indirect effects of mean temperatures, mean precipitation, temperature synchrony,
## and precipitation synchrony on growth synchrony

# load necessary packages
library(lavaan)
library(psych)
library(semPlot)
library(piecewiseSEM)

# source wavelet mean field objects; res_growth_wmf, res_ppt_wmf, res_tmin_wmf
source(here::here("Analyses/wavelets.R"))

# extract synchrony values , make timescales column names, remove imaginary components as well as nas, and pivot longer
rwi_sync_values <- as.data.frame(res_growth_wmf$values)
colnames(rwi_sync_values) <- res_growth_wmf$timescales
rwi_sync_values <- abs(rwi_sync_values)
rwi_sync_values$year <- 1900:2018
rwi_sync_long <- rwi_sync_values %>%
  pivot_longer(cols = 1:67, names_to = "ts", values_to = "values") %>%
  drop_na()


ppt_sync_values <- as.data.frame(res_ppt_wmf$values)
colnames(ppt_sync_values) <- res_ppt_wmf$timescales
ppt_sync_values <- abs(ppt_sync_values)
ppt_sync_values$year <- 1900:2018
ppt_sync_long <- ppt_sync_values %>%
  pivot_longer(cols = 1:67, names_to = "ts", values_to = "values") %>%
  drop_na()

tmin_sync_values <- as.data.frame(res_tmin_wmf$values)
colnames(tmin_sync_values) <- res_tmin_wmf$timescales
tmin_sync_values <- abs(tmin_sync_values)
tmin_sync_values$year <- 1900:2018
tmin_sync_long <- tmin_sync_values %>%
  pivot_longer(cols = 1:67, names_to = "ts", values_to = "values") %>%
  drop_na()

# convert timescale column to numeric and classify timescale band intervals
rwi_sync_long$ts <- as.numeric(rwi_sync_long$ts)
rwi_sync_long <- rwi_sync_long %>%
  mutate(band = case_when(
    ts >= 2 & ts <= 3 ~ "biennial",
    ts > 3  & ts <= 10 ~ "multiannual",
    ts > 10 & ts <= 20 ~ "decadal",
    ts > 20 & ts <= 30 ~ "multidecadal"
  ))

ppt_sync_long$ts <- as.numeric(ppt_sync_long$ts)
ppt_sync_long <- ppt_sync_long %>%
  mutate(band = case_when(
    ts >= 2 & ts <= 3 ~ "biennial",
    ts > 3  & ts <= 10 ~ "multiannual",
    ts > 10 & ts <= 20 ~ "decadal",
    ts > 20 & ts <= 30 ~ "multidecadal"
  ))

tmin_sync_long$ts <- as.numeric(tmin_sync_long$ts)
tmin_sync_long <- tmin_sync_long %>%
  mutate(band = case_when(
    ts >= 2 & ts <= 3 ~ "biennial",
    ts > 3  & ts <= 10 ~ "multiannual",
    ts > 10 & ts <= 20 ~ "decadal",
    ts > 20 & ts <= 30 ~ "multidecadal"
  ))

# calculate average synchrony across full timeseries
avg_rwi_sync_total <- rwi_sync_long %>%
  group_by(year, band) %>%
  summarise(rwi_sync = mean(values))
avg_rwi_sync_total$year <- as.numeric(avg_rwi_sync_total$year)

avg_ppt_sync_total <- ppt_sync_long %>%
  group_by(year, band) %>%
  summarise(ppt_sync = mean(values))
avg_ppt_sync_total$year <- as.numeric(avg_ppt_sync_total$year)

avg_tmin_sync_total <- tmin_sync_long %>%
  group_by(year, band) %>%
  summarise(tmin_sync = mean(values))
avg_tmin_sync_total$year <- as.numeric(avg_tmin_sync_total$year)

# combine all into average synchrony df
avg_total_sync <- left_join(avg_rwi_sync_total, avg_ppt_sync_total) %>%
  left_join(avg_tmin_sync_total)


# source timescale specific environmental variables and their quartile values; temperatures_quartiles, precipiation_quartiles
source(here::here("Analyses/env_quantile_sync_cor.R"))

# clean up quantile dataframes and rename columns to only include, year, band, timescale specific values, and temperature quantiles
ts_tmin_quant <- temperature_quartiles %>%
  ungroup() %>%
  select(window_year, band, window_avg_tmin, quantile) %>%
  rename("year" = window_year) %>%
  rename(ts_tmin = window_avg_tmin) %>%
  rename(tmin_quartile = quantile)


ts_ppt_quant <- precipitation_quartiles %>%
  ungroup() %>%
  select(window_year, band, window_avg_ppt) %>%
  rename("year" = window_year) %>%
  rename(ts_ppt = window_avg_ppt)

# join all data to create a dataframe that includes all variables going into the SEM;
# rwi_sync, ppt_sync, tmin_sync, ts_ppt, ts_tmin, tmin_quartile
model_df <- left_join(avg_total_sync, ts_tmin_quant) %>%
  left_join(ts_ppt_quant)


# create a grouping variable for SEMs to estimate pathways under cool conditions (temp quartiles 1 & 2) and warm conditions (temp quartiles 3 & 4)
model_df <- model_df %>% 
  mutate(group = case_when(tmin_quartile == 1 ~ "cool",
                           tmin_quartile == 2 ~ "cool", 
                           tmin_quartile == 3 ~ "warm",
                           tmin_quartile == 4 ~ "warm")) %>%
  filter(!is.na(group))

# make group a factor with levels = cool, warm 
model_df$group <- factor(model_df$group, levels = c("cool","warm"))

# remove NAs
model_df <- na.omit(model_df)

## Across all timescales 
# Saturated model (without covariance between endogenous variables)
SEM_sat <- '
  ts_ppt ~~ ts_tmin
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync  ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

fit_sat <- sem(
  SEM_sat,
  data = model_df,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR"
)

summary(fit_sat, standardized = TRUE, fit.measures = TRUE)

# group-trimmed model to include only significant pathways for each group
SEM_trim <- '
  ts_ppt ~~ c(0,NA)*ts_tmin
  ppt_sync ~ c(0,NA)*ts_ppt + c(NA,NA)*ts_tmin
  tmin_sync ~ c(NA,0)*ts_tmin + c(0,0)*ts_ppt
  rwi_sync ~ ppt_sync +
             c(0,0)*tmin_sync +
             c(0,NA)*ts_ppt +
             c(NA,0)*ts_tmin
'

fit_trim <- sem(
  SEM_trim,
  data = model_df,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR"
)

summary(fit_trim, standardized=TRUE, fit.measures=TRUE)

# symmetric-trim model to remove only non-sig pathways across both groups
SEM_sym <- '
  ts_ppt ~~ ts_tmin
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_ppt + ts_tmin
  rwi_sync  ~ ppt_sync + ts_tmin +ts_ppt
'

fit_sym <- sem(
  SEM_sym,
  data = model_df,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR"
)

summary(fit_sym, standardized=TRUE, fit.measures=TRUE)

# grouped-trimmed model fits best for describing how the system behaves under each temp condition
# compared symmetric model with equal regressions across groups
fit_equal_sym <- sem(
  SEM_sym,
  data = model_df,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR",
  group.equal = "regressions"
)

summary(fit_equal_sym, standardized = TRUE, fit.measures = TRUE)

# compare model fits between symmetric free and symmetric equal 
lavTestLRT(fit_sym,fit_equal_sym)


## Break up SEMs into timescale bands
model_df_b <- model_df %>%
  filter(band == "biennial") 
model_df_b$group <- factor(model_df_b$group, levels = c("cool","warm"))

model_df_ma <- model_df %>%
  filter(band == "multiannual") 
model_df_ma$group <- factor(model_df_ma$group, levels = c("cool","warm"))

model_df_d <- model_df %>%
  filter(band == "decadal") 
model_df_d$group <- factor(model_df_d$group, levels = c("cool","warm"))

model_df_md <- model_df %>%
  filter(band == "multidecadal") 
model_df_md$group <- factor(model_df_md$group, levels = c("cool","warm"))

# Saturated models (without covariance between endogenous variables)
SEM_sat_b <- '
  ts_ppt ~~ ts_tmin
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync  ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

fit_sat_b <- sem(
  SEM_sat_b,
  data = model_df_b,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR"
)

summary(fit_sat_b, standardized = TRUE, fit.measures = TRUE)

SEM_sat_ma <- '
  ts_ppt ~~ ts_tmin
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync  ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

fit_sat_ma <- sem(
  SEM_sat_ma,
  data = model_df_ma,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR"
)

summary(fit_sat_ma, standardized = TRUE, fit.measures = TRUE)


SEM_sat_d <- '
  ts_ppt ~~ ts_tmin
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync  ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

fit_sat_d <- sem(
  SEM_sat_d,
  data = model_df_d,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR"
)

summary(fit_sat_d, standardized = TRUE, fit.measures = TRUE)

SEM_sat_md <- '
  ts_ppt ~~ ts_tmin
  ppt_sync  ~ ts_ppt + ts_tmin
  tmin_sync ~ ts_tmin + ts_ppt
  rwi_sync  ~ ppt_sync + tmin_sync + ts_ppt + ts_tmin
'

fit_sat_md <- sem(
  SEM_sat_md,
  data = model_df_md,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR"
)

summary(fit_sat_md, standardized = TRUE, fit.measures = TRUE)

# group-trimmed model to include only significant pathways for each group
SEM_trim_b <- '
  ts_ppt ~~ c(0,NA)*ts_tmin
  ppt_sync  ~ c(0,NA)*ts_ppt + c(NA,0)*ts_tmin
  tmin_sync ~ c(0,NA)*ts_tmin + c(0,0)*ts_ppt
  rwi_sync  ~ c(0,0)*ppt_sync + c(0,0)*tmin_sync + c(0,0)*ts_ppt + c(NA,NA)*ts_tmin
'

fit_trim_b <- sem(
  SEM_trim_b,
  data = model_df_b,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR"
)

summary(fit_trim_b, standardized = TRUE, fit.measures = TRUE)


SEM_trim_ma <- '
  ts_ppt ~~ c(NA,0)*ts_tmin
  ppt_sync  ~ c(0,0)*ts_ppt + c(0,NA)*ts_tmin
  tmin_sync ~ c(NA,NA)*ts_tmin + c(NA,0)*ts_ppt
  rwi_sync  ~ c(0,0)*ppt_sync + c(NA,0)*tmin_sync + c(NA,NA)*ts_ppt + c(NA,NA)*ts_tmin
'

fit_trim_ma <- sem(
  SEM_trim_ma,
  data = model_df_ma,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR"
)

summary(fit_trim_ma, standardized = TRUE, fit.measures = TRUE)


SEM_trim_d <- '
  ts_ppt ~~ c(0,0)*ts_tmin
  ppt_sync  ~ c(NA,NA)*ts_ppt + c(NA,NA)*ts_tmin
  tmin_sync ~ c(NA,0)*ts_tmin + c(NA,NA)*ts_ppt
  rwi_sync  ~ c(NA,NA)*ppt_sync + c(NA,NA)*tmin_sync + c(NA,NA)*ts_ppt + c(0,NA)*ts_tmin
'

fit_trim_d <- sem(
  SEM_trim_d,
  data = model_df_d,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR"
)

summary(fit_trim_d, standardized = TRUE, fit.measures = TRUE)


SEM_trim_md <- '
  ts_ppt ~~ c(NA,NA)*ts_tmin
  ppt_sync  ~ c(NA,NA)*ts_ppt + c(0,NA)*ts_tmin
  tmin_sync ~ c(NA,NA)*ts_tmin + c(0,NA)*ts_ppt
  rwi_sync  ~ c(0,NA)*ppt_sync + c(NA,NA)*tmin_sync + c(0,0)*ts_ppt + c(NA,NA)*ts_tmin
'

fit_trim_md <- sem(
  SEM_trim_md,
  data = model_df_md,
  group = "group",
  group.label = c("cool", "warm"),
  estimator = "MLR"
)

summary(fit_trim_md, standardized = TRUE, fit.measures = TRUE)


