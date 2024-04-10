make_plot_paycomp_adjusted <- function(pv_edu_pg, syst_pocty_long_uo, dta_uo_all, rok = NULL) {

  pv_edu_pg_all <- pv_edu_pg |>
    filter(!is.na(code), year == rok) |>
    group_by(category) |>
    summarise(pay_mean_ispv = weighted.mean(pay_mean, fte_thous),
              fte_thous = sum(fte_thous)) |>
    ungroup() |>
    mutate(podil_ispv = fte_thous/sum(fte_thous))

  pv_pg_all <- pv_edu_pg |>
    filter(year == rok, category == "Celkem") |>
    summarise(pay_mean_ispv = weighted.mean(pay_mean, fte_thous))

  syst_pocty <- syst_pocty_long_uo |>
    filter(rok == rok, ustredni_organ) |>
    mutate(category =
             case_match(trida,
                        # https://ppropo.mpsv.cz/XX5Platovatrida
                        c("01", "02") ~ "Základní a nedokončené",
                        c("03", "04", "05", "06") ~ "Střední bez maturity",
                        c("07", "08", "09") ~ "Střední s maturitou",
                        c("10", "11", "12") ~ "Vyšší odborné a bakalářské",
                        c("13", "14", "15", "16") ~ "Vysokoškolské")
    ) |>
    group_by(kapitola_zkr, category, kap_kod = kapitola_kod) |>
    mutate(rok = rok) |>
    summarise(pocet = sum(pocet, na.rm = TRUE), .groups = "drop") |>
    group_by(kapitola_zkr) |>
    mutate(podil_mini = pocet/sum(pocet)) |>
    ungroup() |>
    drop_na(category)

  rok0 <- rok

  platy_szu <- dta_uo_all |>
    drop_na(kap_zkr) |>
    filter(faze_rozpoctu == "SKUT", rok == rok0,
           !kap_zkr %in% c("MV", "MO", "ÚSTR")) |>
    mutate(prumerny_plat_vucinh = prumerny_plat/phasal_all) |>
    select(kap_kod, prumerny_plat, prumerny_plat_vucinh, kap_mini) |>
    mutate(kap_kod = as.character(kap_kod))

  platy_equiv <- syst_pocty |>
    left_join(pv_edu_pg_all, by = "category") |>
    left_join(platy_szu, by = "kap_kod") |>
    mutate(podil_rel = podil_mini / podil_ispv) |>
    # select(kapitola_zkr, category, starts_with("podil")) |>
    group_by(kapitola_zkr, kap_kod, prumerny_plat, kap_mini) |>
    # vážený
    summarise(pay_nh_equiv = weighted.mean(pay_mean_ispv, podil_rel),
              # niže jen ponechat plat - pro každé mini je ve zdroji jen jedno unikátní číslo
              prumerny_plat_vucinh = mean(prumerny_plat_vucinh), .groups = "drop") |>
    mutate(pay_mean_ispv = pv_pg_all$pay_mean_ispv,
           prumerny_plat_vucinh_equiv = prumerny_plat/pay_nh_equiv,
           kapitola_zkr = as.factor(kapitola_zkr) |>
             fct_reorder(prumerny_plat_vucinh_equiv) |>
             fct_drop()) |>
    drop_na(prumerny_plat_vucinh)

  platy_equiv_long <- platy_equiv |>
    pivot_longer(cols = c(prumerny_plat_vucinh_equiv, prumerny_plat_vucinh))

  ggplot(platy_equiv, aes(y = kapitola_zkr)) +
    geom_vline(aes(xintercept = 1), colour = "darkgrey") +
    geom_col(aes(prumerny_plat_vucinh_equiv, fill = kap_mini)) +
    geom_text(aes(label = label_percent_cz(1, suffix = "")(prumerny_plat_vucinh_equiv),
                  x = prumerny_plat_vucinh_equiv), nudge_x = -.02,
              colour = "white", size = 2.6, family = "IBM Plex Sans Condensed") +
    geom_text(aes(label = label_percent_cz(1, suffix = "")(prumerny_plat_vucinh),
                  x = prumerny_plat_vucinh), nudge_x = .03,
              colour = "darkblue", size = 2.6, family = "IBM Plex Sans Condensed") +
    geom_point(aes(prumerny_plat_vucinh)) +
    scale_y_discrete(drop = TRUE) +
    scale_fill_manual(values = c("grey20", "darkblue"),
                      name = "Typ organizace",
                      labels = c("Ostatní", "Ministerstva")) +
    theme_ptrr("both",
               legend.position = "top",
               legend.key.size = unit(8, "pt"),
               panel.grid.major.x = element_line(linewidth = .3),
               panel.grid.minor.x = element_line(linewidth = .1)) +
    scale_x_percent_cz(limits = c(0, 1.4), n.breaks = 8, expand = expansion(add = c(0, 0.05))) +
    labs(title = "Průměrné platy ministerstev ve srovnání s pracovní sílou Prahy (2023)",
         caption = "Zdroj: systemizace 2024 (pouze ústřední orgán, služební i pracovní místa), SZÚ 2023 a ISPV.\nVzdělanostní struktura odvozena od zastoupení platových tříd na každém úřadu.")

  # ggplot(platy_equiv_long, aes(y = kapitola_zkr)) +
  #   geom_vline(aes(xintercept = 1), colour = "darkgrey") +
  #   geom_linerange(data = platy_equiv, aes(xmin = prumerny_plat_vucinh_equiv,
  #                                          xmax = prumerny_plat_vucinh),
  #                  colour = "darkblue") +
  #   geom_point(aes(value, colour = name)) +
  #   # geom_text(aes(label = ptrr::label_percent_cz(accuracy = 1)(value),
  #   #           x = value)) +
  #   theme_ptrr("both",
  #              panel.grid.major.x = element_line(linewidth = .3),
  #              panel.grid.minor.x = element_line(linewidth = .1)) +
  #   scale_x_percent_cz(limits = c(.7, 1.2)) +
  #   scale_color_manual(values = c("darkgrey", "darkblue"))

}

pv_bind <- function(data_cr, data_pg) {
  data_cr |>
    mutate(geo = "ČR") |>
    bind_rows(data_pg |>
                mutate(geo = "Praha"))
}

pv_fix_totals <- function(data) {
  data |>
    mutate(category = if_else(str_detect(category, "CELKEM"), "Celkem", category))
}


