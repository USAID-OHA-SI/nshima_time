
#Object containing all the options for the cascade
plot_name <- c("Standard", "Standard Female", "Standard Male", 
               "Pediatric", "Pediatric Female", "Pediatric Male", 
               "AYP (15-24 years old)", "AYP Female", "AYP Male", "KP") 

usethis::use_data(plot_name, overwrite = TRUE)


# List of indicators to keep
keep_ind <- c("Linkage", "NNT", "VLS", "VLC")
usethis::use_data(keep_ind, overwrite = TRUE)


# List of disags needed for peds
disag_peds <- c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
usethis::use_data(disag_peds, overwrite = TRUE)

# list of disaggs needed for p
disag_kp <-  c("KeyPop/Result", "KeyPop/HIVStatus", "KeyPop/Indication/HIVStatus")
usethis::use_data(disag_kp, overwrite = TRUE)


