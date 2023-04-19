library(OECD)
options(timeout = 300)
gov <- OECD::get_dataset("GOV")
unique(gov$IND)
gov_meta <- OECD::get_data_structure("GOV")

debug(OECD::get_data_structure)

dataset <- get_dataset("DUR_D")

# library(rqog)

# rqog::run_app()
# qog_oecd <- rqog::read_qog("oecd")
# qog_std <- rqog::read_qog("standard")
