library(dplyr)
library(ggplot2)
library(ggbump)
library(forcats)
library(magrittr)

sm <- readRDS("data-interim/sections.rds")
sm_mini <- sm |>
  filter(typ_rozpoctu == "SKUT",
         # rok == 2022,
         kap_name != "MI",
         kategorie_2014 == "Ministerstva") |>
  mutate(kap_name = as.factor(kap_name) |> fct_relevel("MŠMT") |> fct_rev())

plot_mini_line <- function(data, variable, title, subtitle,
                           zdroj =  "ISP/Státní závěrečný účet",
                           fn = geom_line) {
  sm_mini |>
    ggplot(aes(rok, {{variable}}, group = kap_name)) +
    # facet_wrap(~kap_name) +
    fn(aes(color = kap_name == "MŠMT",
                  linewidth = kap_name == "MŠMT")) +
    scale_color_manual(values = c("grey", "darkblue"), guide = "none") +
    scale_linewidth_manual(values = c(0.5, 1), guide = "none") +
    ptrr::scale_y_number_cz() +
    ptrr::theme_ptrr("both") +
    scale_x_continuous(limits = c(2003, 2022), breaks = seq(2004, 2022, by = 3)) +
    labs(title = title,
         subtitle = subtitle,
         caption = zdroj)
}

plot_mini_line(sm_mini, prumerny_plat,
               title = "Průměrné platy na ministerstvech, 2003-2022",
               subtitle = "Zvýrazněno MŠMT\nSlužební i pracovní místa; jen ústřední orgány")

plot_mini_line(sm_mini, wage_to_general,
               title = "Průměrné platy na ministerstvech ve srovnání s průměrným platem v Praze, 2003-2022",
               subtitle = "Zvýrazněno MŠMT\nSlužební i pracovní místa; jen ústřední orgány",
               zdroj = "ISP/Státní závěrečný účet\nvlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady") +
  ptrr::scale_y_percent_cz(n.breaks = 10) +
  geom_hline(yintercept = 1)

plot_mini_line(sm_mini, wage_in_2022_change, fn = function(...) geom_point(alpha = .6, ...),
               title = "Změna reálných platů na ministerstvech, 2004-2022",
               subtitle = "Zvýrazněno MŠMT\nSlužební i pracovní místa; jen ústřední orgány",
               zdroj = "ISP/Státní závěrečný účet\nvlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady a 010022 Indexy spotř. cen)") +
  ptrr::scale_y_percent_cz(n.breaks = 10) +
  geom_hline(yintercept = 0)

sm_mini |>
  ggplot(aes(rok, prumerny_plat, group = kap_name)) +
  # facet_wrap(~kap_name) +
  geom_line(aes(color = kap_name == "MŠMT",
                linewidth = kap_name == "MŠMT")) +
  scale_color_manual(values = c("grey", "darkblue"), guide = "none") +
  scale_linewidth_manual(values = c(0.5, 1), guide = "none") +
  ptrr::scale_y_number_cz() +
  ptrr::theme_ptrr("both") +
  scale_x_continuous(limits = c(2003, 2022), breaks = seq(2004, 2022, by = 3)) +
  labs(title = "Průměrné platy na ministerstvech, 2003-2022",
       subtitle = "Zvýrazněno MŠMT\nSlužební i pracovní místa; jen ústřední orgány",
       zdroj = "ISP/Státní závěrečný účet")

sm_mini |>
  group_by(rok) |>
  mutate(rnk = rank(prumerny_plat)) |>
  ungroup() |>
  mutate(kap_name = as.factor(kap_name) |> fct_relevel("MŠMT") |> fct_rev()) |>
  ggplot(aes(rok, rnk, group = kap_name)) +
  geom_bump(se = FALSE, method = "lm", formula = y ~ poly(x, 9),
            lineend = "round", linejoin = "round", alpha = .6, smooth = 5,
            # position = position_jitter(),
            mapping = aes(color = kap_name == "MŠMT",
                          linewidth = kap_name == "MŠMT")) +
  scale_color_manual(values = c("grey60", "darkblue")) +
  scale_linewidth_manual(values = c(0.5, 1), guide = "none") +
  geom_text(data = . %>% filter(rok == max(rok)),
            aes(label = kap_name, color = kap_name == "MŠMT"),
            hjust = 0, nudge_x = 0.5) +
  geom_point(data = . %>% filter(rok == max(rok)),
             aes(color = kap_name == "MŠMT")) +
  expand_limits(x = 2024) +
  scale_x_continuous(breaks = seq(2004, 2022, by = 3), expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(n.breaks = 15) +
  guides(color = "none", size = "none") +
  ptrr::theme_ptrr(gridlines = "none") +
  labs(title = "Pořadí - průměrné platy na ministerstvech, 2003-2022",
       subtitle = c("Služební i pracovní místa; jen ústřední orgány"),
       caption = "ISP/Státní závěrečný účet")
