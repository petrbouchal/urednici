library(dplyr)
library(ispv)
library(ggplot2)
library(ptrr)
library(writexl)

crlist <- ispv::pv_list_cr(2022)

isco222file <- download.file(crlist$url[c(1, 3)],
                                   c("data-input/ispv/CR_222_MZS.xlsx",
                                     "data-input/ispv/CR_222_PLS.xlsx"))
isco222 <- ispv::pv_cr_monthlypay_isco(c("data-input/ispv/CR_222_MZS.xlsx",
                                         "data-input/ispv/CR_222_PLS.xlsx"), 7)

isco222 |>
  filter(isco_id == 2422) |>
  write_xlsx("data-export/ispv-2022-2422-cr-pro-sv.xlsx")

targets::tar_load(pv_isco)

pv_isco |>
  filter(isco_id == 2422) |>
  write_xlsx("data-export/ispv-2021-2422-reg-pro-sv.xlsx")

isco222 |>
  filter(isco_id == 2422) |>
  filter(isco_digits == 4 | is.na(isco_digits)) |>
  arrange(sfera) |>
  ggplot(aes(colour = sfera, y = sfera)) +
  geom_pointrange(aes(x = pay_median, xmin = pay_q1, xmax = pay_q3),
                  position = position_dodge2(width = .5)) +
  geom_point(aes(x = pay_d1), position = position_dodge2(width = .5), shape = 20) +
  geom_point(aes(x = pay_d9), position = position_dodge2(width = .5), shape = 20) +
  facet_wrap(~ isco_full, ncol = 1, scales = "free_x") +
  scale_x_number_cz(limits = c(18e3, 135e3),
                    breaks = seq(from = 30e3, to = 135e3, by = 15e3), scale = 1/1000) +
  scale_color_manual(values = c("grey40", "darkblue"), guide = "none") +
  scale_y_discrete(labels = c(PLS = "Platová sféra", MZS = "Mzdová sféra"), breaks = c("PLS", "MZS")) +
  theme_ptrr(multiplot = T, gridlines = "x", legend.position = "top") +
  labs(colour = "Sféra")
