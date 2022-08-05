library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(forcats)

source("theme.R")

zm <- czso::czso_get_table("110079") # mzdy podle NACE
infl <- czso::czso_get_table("010022") # indexy spotř. cen

zm |> count(odvetvi_txt)

infl_qonq <- infl |>
  filter(casz_txt == "stejné období předchozího roku",
         is.na(ucel_txt)) |> # všechny druhy zboží/služeb
  mutate(ctvrtletí = quarter(obdobiod)) |>
  group_by(rok, ctvrtletí) |>
  summarise(inflace_qonq = (mean(hodnota) - 100)/100, .groups = "drop")

1zm_plt_dt <- zm |>
  filter(stapro_txt == "Průměrná hrubá mzda na zaměstnance",
         typosoby_txt == "přepočtený") |>
  mutate(ctvrtletí = as.numeric(ctvrtletí),
         tm = make_date(rok, ctvrtletí * 3),
         clr = case_when(odvetvi_kod == "O" ~ "státní správa",
                         # odvetvi_kod %in% c("P") ~ "vzdělávání",
                         odvetvi_kod %in% c("M") ~ "profesní",
                         odvetvi_kod %in% c("J") ~ "ICT",
                         TRUE ~ "ostatní") |>
           as_factor() |> fct_relevel("ostatní", "profesní", "ICT", "státní správa"),
         public = odvetvi_kod %in% c("O")) |>
  arrange(tm, odvetvi_kod) |>
  group_by(odvetvi_kod) |>
  mutate(zmena_qonq = (hodnota - lag(hodnota, n = 4))/lag(hodnota, n = 4)) |>
  left_join(infl_qonq) |>
  select(tm, zmena_qonq, clr, hodnota, public, inflace_qonq) |>
  ungroup() |>
  arrange(clr)

zm_plt <- ggplot(zm_plt_dt,
                 aes(tm - days(15), zmena_qonq - inflace_qonq, colour = clr, size = public)) +
  guides(colour = guide_legend(reverse = T, title = "Skupina NACE"), size = "none") +
  scale_size_manual(values = c(1, 2), expand = expansion(0, 0)) +
  ptrr::scale_y_percent_cz(limits = c(-.2, .2), expand = expansion(0, 0)) +
  labs(title = "Reálná změna platů podle NACE skupin, 2003 - 1Q 2022",
       x = "Rok", y = "Reálná meziroční změna (očištěno o inflaci)",
       caption = "Zdroj: vlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady a 010022 Indexy spotř. cen)") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y",
               expand = expansion(0, 0),
               limits = c(as.Date("2003-01-01"), as.Date("2022-07-01"))) +
  geom_hline(yintercept = 0, colour = "grey10", linetype = "solid") +
  scale_color_manual(values = c("grey40", "blue3", "goldenrod", "red3")) +
  geom_point(alpha = .6) +
  theme_urednici +
  theme(panel.grid.major = element_line(color = "white"))

zm_plt

ggsave(plot = zm_plt, "plt_twitter.png", bg = "white", scale = 3, dpi = 300, device = ragg::agg_png,
       width = 1200, height = 675, limitsize = FALSE, units = "px")
ggsave(plot = zm_plt, "plt_facebook.png", bg = "white", scale = 3, dpi = 300, device = ragg::agg_png,
       width = 1200, height = 630, limitsize = FALSE, units = "px")


