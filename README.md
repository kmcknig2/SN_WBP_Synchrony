# SN_WBP_Synchrony

Code for the paper "Precipitation pulls the strings, but temperature 
sets the stage: Changing climate conditions increase whitebark pine synchrony"

Contains: 1. raw tree-ring and environmental data (/Data), 2. cleaned and subsetted
dataframes, wavelet analyses, models, and visualization code for the main text and 
supplemental analyses (/Analyses), and 3. functions used to compare models, 
test time-varying coherence, and plot wavelets (/Functions).
See below for additional detail.

1.  **Data:**

    *rwi_dat.csv* - contains ring width index data for 27 whitebark pine populations
    across the southern Sierra Nevada.

    *ppt_dat.csv* - contains montly total precipiation data extracted from PRISM 
    to match the same 27 populations from 1895-2018

    *tmin_dat.csv* - contains montly minimum temperature data extracted from PRISM 
    to match the same 27 populations from 1895-2018
    

2.  **Analyses**

    *data_cleaning_and_subsetting.R* - script contains data cleaning and subsetting
    procedures used to ensure wavelet analyses only include data from sites with at least
    five trees alive from 1900-2018

    *wavelets.R* - script contains analyses used to create wavelet mean fields (wmf)
    and wavelet phasor mean fields (wpmf) for growth and environmental variables. 
    
    *raw_data_trends_and_breakpoints.R* - script contains analyses used to assess trends
    and breakpoints in raw data.
    
    *average_synchrony.R* - script contains analyses used to model average synchrony across
    time and timescale bands in all variables.
    
    *time_varying_coherence.R* - script contains analyses used to calculate coherence
    between the environmental variables and growth patterns across time and timescale band.
    
    *timescale_specific_variables.R* - script used to create timescale-specific measurements
    of the environmental and growth data.
    
    *env_quantil_sync_cor.R* - script used to assess correlations between timescale-specific
    environmnetal quartiles and synchrony across variables.


3.  **Functions**

    *AIC.R* - supporting script with function for AIC comparisons.
    
    *coh_tv.R* - supporting script with modified wsyn function for calculating coherence 
    in a time-varying manner.
    
    *plotmag_tts.R* - supporting script with modified wsyn function for plotting wmfs
    without fixed axes.
    
    *psync_by_chance* - supporting script with function to calculate signficant synchrony 
    depending on number of sites. 

