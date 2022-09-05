source("_targets_packages.R")

targets::tar_load(ec_counts)
targets::tar_load(ec_pay)

skimr::skim(ec_counts)
skimr::skim(ec_pay)
