# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint
conflicted::conflict_prefer("filter", "dplyr")

options(scipen = 100)

# Set target options:
tar_option_set(
  packages = c("tibble", "ispv", "dplyr", "stringr", "forcats", "czso", "ptrr",
               "ggplot2", "purrr", "curl", "readr", "tidyr", "eurostat",
               "wbstats", "quarto", "arrow", "broom", "janitor",
               "modelr", "ggiraph"),
  format = "rds" # default storage format
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
l_wgi <- list(
  tar_target(wbi_wagepremiums, wbi_get_wagepremiums()),
  tar_target(wbi_plot_line, wbi_make_plot_line(wbi_wagepremiums)),
  tar_target(wbi_plot_bar, wbi_make_plot_bar(wbi_wagepremiums))
)
l_ec <- list(
  tar_target(ec_counts, get_eurostat("prc_rem_nr")),
  tar_target(ec_pay, get_eurostat("prc_rem_avg")),
  tar_target(ec_plot_multi, ec_make_plot_multi(ec_pay)),
  tar_target(ec_plot_bar, ec_make_plot_bar(ec_pay)),
  tar_target(ec_plot_count_bar, ec_make_plot_count_bar(ec_counts))
)

l_systemizace <- list(
  tar_file_read(syst_pocty_long,
                "data-input/systemizace/systemizace_pocty_long.parquet",
                arrow::read_parquet(!!.x)),
  tar_file_read(syst_all,
                "data-input/systemizace/systemizace_all.parquet",
                arrow::read_parquet(!!.x)),
  tar_target(syst_all_fixed, syst_fix_data(syst_all)),
  tar_target(syst_platy, syst_make_platy(syst_all_fixed)),
  tar_target(syst_platy_uo, syst_make_platy_uo(syst_all_fixed)),
  tar_target(syst_pocty_long_uo, lengthen_syst_data(syst_all)),
  tar_target(syst_tridy, syst_make_tridy(syst_pocty_long)),
  tar_target(syst_tridy_uo, syst_make_tridy_uo(syst_pocty_long_uo)),
  tar_target(syst_platy_for_model, syst_make_platy_for_model(syst_platy, syst_tridy)),
  tar_target(syst_model_annual, syst_run_model_annual(syst_platy_for_model)),
  tar_target(syst_model, syst_run_model(syst_platy_for_model)),
  tar_target(syst_plot_tridy, syst_make_plot_tridy(syst_tridy_uo)),
  tar_target(syst_plot_platy, syst_make_plot_platy(syst_platy_uo)),

  tar_target(syst_model_tidy, tidy(syst_model)),
  tar_target(syst_model_predictions, syst_make_predictions(syst_model, syst_platy_for_model)),
  tar_target(syst_plot_residyr, syst_make_plot_residyr(syst_model_annual)),
  tar_target(syst_plot_coefyr, syst_make_plot_coefyr(syst_model_annual)),
  tar_target(syst_plot_model, syst_make_plot_model(syst_model_tidy)),
  tar_target(syst_plot_model_predictions_all,
             syst_make_plot_model_predictions_all(syst_model_predictions)),
  tar_target(syst_plot_model_by_grp,
             syst_make_plot_model_by_grp(syst_platy_for_model)),
  tar_target(syst_plot_model_resid,
             syst_make_plot_model_resid(syst_model_predictions)),
  tar_target(syst_plot_model_resid_uo,
             syst_make_plot_model_resid_uo(syst_model_predictions))

)

l_quarto <- list(
  tar_quarto(quarto_project)
)

l_ispv <- list(
  tar_target(pv_reg_list, pv_list_reg(2021)),
  tar_target(pv_cr_list, pv_list_cr(2021)),
  tar_target(pv_pg_list_q4, pv_reg_list |> filter(str_detect(name, "Pra_214"))),
  tar_target(pv_cr_list_q4, pv_cr_list |> filter(str_detect(name, "214_(MZ|PL)S\\."))),
  tar_url(pv_cr_urls, pv_cr_list_q4[["url"]]),
  tar_url(pv_pg_urls, pv_pg_list_q4[["url"]]),
  tar_target(pv_cr_filenames, file.path("data-input/ispv/", pv_cr_list_q4[["name"]])),
  tar_target(pv_pg_filenames, file.path("data-input/ispv/", pv_pg_list_q4[["name"]])),
  tar_file(pv_cr_files,
           curl::curl_download(pv_cr_urls, pv_cr_filenames),
           pattern = map(pv_cr_urls, pv_cr_filenames)),
  tar_file(pv_pg_files,
           curl::curl_download(pv_pg_urls, pv_pg_filenames),
           pattern = map(pv_pg_urls, pv_pg_filenames)),
  tar_target(pv_isco_cr, pv_cr_monthlypay_isco(pv_cr_files, sheet = 7) |> isco_recode()),
  tar_target(pv_isco_pg, pv_reg_monthlypay_isco4(pv_pg_files) |> isco_recode()),
  tar_target(pv_edu_cr, pv_cr_monthlypay_education(pv_cr_files) |> isco_recode()),
  tar_target(pv_edu_pg, pv_reg_monthlypay_education(pv_pg_files) |> isco_recode()),
  tar_target(pv_genderage_cr, pv_cr_monthlypay_age_gender(pv_cr_files) |> isco_recode()),
  tar_target(pv_genderage_pg, pv_reg_monthlypay_age_gender(pv_pg_files) |> isco_recode()),

  tar_target(pv_isco_cr_long, pv_isco_lengthen(pv_isco_cr)),
  tar_target(pv_isco_pg_long, pv_isco_lengthen(pv_isco_pg)),
  tar_target(pv_isco_long, pv_isco_bind(pv_isco_cr_long,
                                        pv_isco_pg_long)),
  tar_target(pv_isco, pv_isco_bind(pv_isco_cr, pv_isco_pg))
)

l_utils <- list()

list(l_wgi, l_ec, l_utils, l_ispv, l_quarto, l_systemizace)