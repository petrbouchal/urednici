# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint
conflicted::conflict_prefer("filter", "dplyr")

options(scipen = 100)
options(crayon.enabled = TRUE)

# Set target options:
tar_option_set(
  packages = c("tibble", "ispv", "dplyr", "stringr", "forcats", "czso", "ptrr",
               "scales", "ggbump", "countrycode",
               "ggplot2", "purrr", "curl", "readr", "tidyr", "eurostat",
               "wbstats", "quarto", "arrow", "broom", "janitor", "readxl",
               "modelr", "ggiraph", "lubridate"),
  format = "rds" # default storage format
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# International data ------------------------------------------------------


## WB WGI ------------------------------------------------------------------

l_wgi <- list(
  tar_target(wbi_wagepremiums, wbi_get_wagepremiums()),
  tar_target(wbi_plot_line, wbi_make_plot_line(wbi_wagepremiums)),
  tar_target(wbi_plot_bar, wbi_make_plot_bar(wbi_wagepremiums))
)

## EC Officials pay  ------------------------------------------------------------------

l_ec <- list(
  tar_target(ec_counts, get_eurostat("prc_rem_nr")),
  tar_target(ec_pay, get_eurostat("prc_rem_avg")),
  tar_target(ec_plot_multi, ec_make_plot_multi(ec_pay)),
  tar_target(ec_plot_bar, ec_make_plot_bar(ec_pay)),
  tar_target(ec_plot_count_bar, ec_make_plot_count_bar(ec_counts)),


  tar_target(earn_ses, get_eurostat("earn_ses_monthly",
                                    filters = list(isco08 = "TOTAL", age = "TOTAL",
                                                   sex = "T", worktime = "TOTAL",
                                                   # time = "2018-01-01",
                                                   indic_se = "MED_E_EUR",
                                                   nace_r2 = "B-S_X_O"))),
  tar_target(earn_oecd, OECD::get_dataset("AV_AN_WAGE")),
  tar_target(earn_oecd_sub, process_oecd_wages(earn_oecd)),
  tar_target(ec_plot_earnoecd, ec_make_plot_earnoecd(ec_pay, earn_oecd_sub)),
  tar_target(ec_plot_earnses, ec_make_plot_earnses(ec_pay, earn_ses))
)


# National comparative ----------------------------------------------------


## SZU ---------------------------------------------------------------------

l_szu <- list(
  tar_file_read(szu_sections,
                # copied from `studie-urednici` repo at https://github.com/idea-cergeei/studie-urednici
                "data-input/szu/data_all.parquet",
                arrow::read_parquet(!!.x))
)

l_szu_plots <- list(
  tar_target(szu_plot_bump,
             make_szu_plot_bump(szu_sections)),
  tar_target(szu_plot_prum,
             plot_mini_line(szu_sections, prumerny_plat,
                            title = "Průměrné platy na ministerstvech, 2003-2022",
                            subtitle = "Služební i pracovní místa; jen ústřední orgány",
                            girafe = TRUE)),

  tar_target(szu_plot_prum_2022price,
             plot_mini_line(szu_sections, prumerny_plat_c2022,
                            title = "Průměrné platy na ministerstvech, 2003-2022, v cenách roku 2022",
                            subtitle = "Služební i pracovní místa; jen ústřední orgány")),

  tar_target(szu_plot_vucinh,
             plot_mini_line(szu_sections, prumerny_plat_vucinh,
                            title = "Průměrné platy na ministerstvech ve srovnání s průměrným platem v Praze, 2003-2022",
                            subtitle = "Služební i pracovní místa; jen ústřední orgány",
                            girafe = TRUE,
                            zdroj = "ISP/Státní závěrečný účet\nvlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady")),

  tar_target(szu_plot_mezirocne,
             plot_mini_line(szu_sections, prumerny_plat_vucinh_mezirocne,
                            fn = function(...) geom_point(alpha = .6, ...),
                            title = "Změna reálných platů na ministerstvech, 2004-2022",
                            subtitle = "Služební i pracovní místa; jen ústřední orgány",
                            zdroj = "ISP/Státní závěrečný účet\nvlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady a 010022 Indexy spotř. cen)",
                            girafe = TRUE))
)

## Systemizace  ---------------------------------------------------------------------

l_systemizace <- list(
  tar_file_read(syst_all,
                # copied from `systemizace` repo at https://github.com/petrbouchal/systemizace
                # raw files downloaded via https://github.com/petrbouchal/statnisluzba-downloader
                "data-input/systemizace/systemizace_all.parquet",
                arrow::read_parquet(!!.x)),
  tar_file_read(syst_pocty_long,
                # copied from `systemizace` repo at https://github.com/petrbouchal/systemizace
                # raw files downloaded via https://github.com/petrbouchal/statnisluzba-downloader
                "data-input/systemizace/systemizace_pocty_long.parquet",
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
  tar_target(syst_plot_predstaveni, syst_make_plot_predstaveni(syst_pocty_long_uo)),

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
             syst_make_plot_model_resid_uo(syst_model_predictions)),
  tar_target(comp_plot_compsal, make_plot_paycomp_adjusted(pv_edu_pg, syst_pocty_long_uo, szu_sections))

)

l_quarto <- list(
  tar_quarto(quarto_project)
)


# ISPV --------------------------------------------------------------------

l_ispv <- list(


  ## List, download files ----------------------------------------------------

  tar_target(pv_reg_list, pv_list_reg()),
  tar_target(pv_cr_list, pv_list_cr()),
  tar_target(pv_pg_list_q4, pv_reg_list |> filter(str_detect(name, "Pra_224"))),
  tar_target(pv_cr_list_q4, pv_cr_list |> filter(str_detect(name, "224_(MZ|PL)S\\."))),
  tar_target(pv_cr_urls, pv_cr_list_q4[["url"]]),
  tar_target(pv_pg_urls, pv_pg_list_q4[["url"]]),
  tar_target(pv_cr_filenames, file.path("data-input/ispv", pv_cr_list_q4[["name"]])),
  tar_target(pv_pg_filenames, file.path("data-input/ispv", pv_pg_list_q4[["name"]])),
  tar_file(pv_cr_files,
           curl::curl_download(pv_cr_urls, pv_cr_filenames),
           pattern = map(pv_cr_urls, pv_cr_filenames)),
  tar_file(pv_pg_files,
           curl::curl_download(pv_pg_urls, pv_pg_filenames),
           pattern = map(pv_pg_urls, pv_pg_filenames)),

  ## ISCO --------------------------------------------------------------------


  tar_target(pv_isco_cr, pv_cr_monthlypay_isco(pv_cr_files, sheet = 7) |> isco_recode()),
  tar_target(pv_isco_pg, pv_reg_monthlypay_isco4(pv_pg_files) |> isco_recode()),
  tar_target(pv_isco_cr_long, pv_isco_lengthen(pv_isco_cr)),
  tar_target(pv_isco_pg_long, pv_isco_lengthen(pv_isco_pg)),
  tar_target(pv_isco_long, pv_isco_bind(pv_isco_cr_long,
                                        pv_isco_pg_long)),
  tar_target(pv_isco, pv_isco_bind(pv_isco_cr, pv_isco_pg)),

  ## Education, gender -------------------------------------------------------

  tar_target(pv_edu_cr, pv_cr_monthlypay_education(pv_cr_files) |> pv_fix_totals()),
  tar_target(pv_edu_pg, pv_reg_monthlypay_education(pv_pg_files) |> pv_fix_totals()),
  tar_target(pv_genderage_cr, pv_cr_monthlypay_age_gender(pv_cr_files) |> isco_recode()),
  tar_target(pv_genderage_pg, pv_reg_monthlypay_age_gender(pv_pg_files) |> isco_recode()),

  ## Plots -------------------------------------------------------------------

  tar_target(pv_plot_isco_dist, pv_make_plot_isco_dist(pv_isco,
                                                       c("4110", "3343", "2422",
                                                         "2619", "3342", "1219",
                                                         "3511", "4312", "2431"))),
  tar_target(pv_plot_isco_percentiles,
             pv_make_plot_isco_percentiles(pv_isco_long,
                                           c("4110", "3343", "2422",
                                             "2619", "3342", "1219",
                                             "3511", "4312", "2431"))),
  tar_target(pv_plot_edu, pv_make_plot_edu(pv_edu_pg, pv_edu_cr)),
  tar_target(pv_plot_ga_pg, pv_make_plot_ga_pg(pv_genderage_pg)),
  tar_target(pv_plot_ga_cr, pv_make_plot_ga_cr(pv_genderage_cr))
)


# CZSO --------------------------------------------------------------------

l_pmz <- list(
  tar_target(czso_pmz_nace, czso_get_table("110079", force_redownload = TRUE)),
  tar_target(czso_infl, czso_get_table("010022", force_redownload = TRUE)),
  tar_target(czso_infl_q, czso_infl_process_quarterly(czso_infl)),
  tar_target(czso_pmz_nace_clean_q, czso_nace_process_quarterly(czso_pmz_nace, czso_infl_q)),
  tar_target(czso_plot_nace_q, czso_make_plot_nace_quarterly(czso_pmz_nace_clean_q)),

  tar_target(czso_infl_annual, czso_infl_process_annual(czso_infl)),
  tar_target(czso_pmz_nace_clean_annual, czso_nace_process_annual(czso_pmz_nace, czso_infl_annual)),
  tar_target(czso_plot_nace_annual, czso_make_plot_nace_annual(czso_pmz_nace_clean_annual, add_years = 7))
)


# OVM ---------------------------------------------------------------------

l_ovm <- list(
  tar_download(ovm_json,
               "https://rpp-opendata.egon.gov.cz/odrpp/datovasada/ovm.json",
               "data-input/ovm.json"),
  tar_target(ovm, load_ovm(ovm_json))
)


# Stosestka ---------------------------------------------------------------

l_stosestka <- list(
  tar_file_read(isp_raw, "data-input/isp/Příloha - Údaje z ISOP.xlsx", read_excel(!!.x)),
  tar_target(isp, process_isp(isp_raw, ovm))
)

l_utils <- list()

list(l_wgi, l_ec, l_utils, l_ispv, l_quarto, l_systemizace, l_pmz, l_ovm,
     l_szu_plots,
     l_stosestka, l_szu)
