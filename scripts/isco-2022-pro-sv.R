source("_targets_packages.R")
library(targets)

tar_source()

targets::tar_load(pv_isco)

pv_isco |>
  filter(isco_id %in% c(2422, 1219)) |>
  write_xlsx("data-export/ispv-2022-2422-reg-pro-sv.xlsx")

pv_isco |>
  filter(isco_id %in% c(2422, 1219)) |>
  filter(isco_digits == 4 | is.na(isco_digits)) |>
  mutate(isco_label = ifelse(isco_id == 1219,
                        paste0(isco_id, ": Ostatní řídící pracovníci správy podniku a pod. činností"),
                        paste0(isco_id, ": ",isco_name))) |>
  arrange(sfera) |>
  ggplot(aes(colour = sfera, y = geo)) +
  geom_pointrange(aes(x = pay_median, xmin = pay_q1, xmax = pay_q3),
                  position = position_dodge2(width = .5), key_glyph = draw_key_pointrange_h) +
  geom_point(aes(x = pay_d1), position = position_dodge2(width = .5), shape = 20) +
  geom_point(aes(x = pay_d9), position = position_dodge2(width = .5), shape = 20) +
  facet_wrap(~ isco_label, ncol = 1, scales = "free_x") +
  scale_x_number_cz(limits = c(18e3, 210e3),
                    breaks = seq(from = 30e3, to = 210e3, by = 15e3), scale = 1/1000) +
  scale_color_manual(values = c("grey40", "darkblue")) +
  # scale_y_discrete(labels = c(PLS = "Platová sféra", MZS = "Mzdová sféra"), breaks = c("PLS", "MZS")) +
  theme_ptrr(multiplot = T, gridlines = "x", legend.position = "top") +
  labs(colour = "Sféra", title = "Platy podle ISCO - Praha a ČR, mzdová a platová sféra",
       subtitle = "malé tečky: 1. a 9. decil, rozpětí: 1. a 3. kvartil; velká tečka: medián")
