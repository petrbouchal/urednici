library(targets)
library(tarchetypes)
library(future)
library(conflicted)


# Config ------------------------------------------------------------------

options(conflicts.policy = list(warn = FALSE),
        clustermq.scheduler = "LOCAL",
        timeout = 120, # for download.file when things take long to download
        crayon.enabled = TRUE,
        scipen = 100,
        czso.dest_dir = "data-input/czso_data",
        yaml.eval.expr = TRUE) 

source("_targets_packages.R")

conflict_prefer("filter", "dplyr")

# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.
future::plan(multisession)

cnf <- config::get(config = "default")
names(cnf) <- paste0("c_", names(cnf))
list2env(cnf, envir = .GlobalEnv)

lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)