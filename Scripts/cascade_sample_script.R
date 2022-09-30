# PROJECT: Demo Cascade
# PURPOSE: Show how cascade works
# AUTHOR: Tim Essam | SI
# REF ID:   c3274783
# LICENSE: MIT
# DATE: 2022-09-20
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

# Libraries
  library(gagglr)
  library(tidyverse)
  library(cascade)
  library(scales)
  library(extrafont)


# SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  site_path <- return_latest(folderpath = merdata,
                             pattern = "Site.*_FY20-23.*Zambia.txt")
  file_path <- return_latest(folderpath = merdata, pattern = "PSNU.*_FY20-23.*Zambia")
  
# Load required metadata into Glob Env.
  get_file_metadata(file_path)
  


# LOAD MSD ----------------------------------------------------------------

  msd_df <- read_msd(file_path)  
  
# CASCADE demo ------------------------------------------------------------

  # What plots are available?
  plot_name
  
  # What if I just want to preview the cascade df?
  return_cascade(msd_df, cscd_num = 4) %>% 
    prinf()

  # Say I want to make this into a graphic?
  return_cascade_plot(msd_df, export = F,)
  
  # JUST GIVE ME ALL THE CASCADE PLOTS
  batch_cascade_plot(msd_df, imgtype = ".png")
  
  # What if I want this just for a partner or SNU1?
  # Subset the data, then pass it through
  ptr_msd_df <- msd_df %>% filter(mech_name == "SAFE", 
                                  snu1 == "Copperbelt Province")
  
  return_cascade_plot(ptr_msd_df)
  