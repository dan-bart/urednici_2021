library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(forcats)
library(tidyr)

source("theme.R")

infl <- czso::czso_get_table("010022", force_redownload = TRUE) # indexy spotř. cen
max(infl$obdobido)

zm <- czso::czso_get_table("110079", force_redownload = TRUE) # mzdy podle NACE
count(zm, rok, ctvrtletí) |> arrange(desc(rok)) |> head()
czso::czso_get_dataset_metadata("110079")
zm <- czso:::read_czso_csv("https://www.czso.cz/documents/62353418/171454104/110079-22data090722.csv/18c4739a-bbcf-49e3-96f0-6749ad50c3e6?version=1.1")
count(zm, rok, ctvrtletí) |> arrange(desc(rok)) |> head()

zm |> count(odvetvi_txt)

# Q on Q ------------------------------------------------------------------

infl_qonq <- infl |>
  filter(casz_txt == "stejné období předchozího roku",
         is.na(ucel_txt)) |> # všechny druhy zboží/služeb
  mutate(ctvrtletí = quarter(obdobiod)) |>
  group_by(rok, ctvrtletí) |>
  summarise(inflace_qonq = (mean(hodnota) - 100)/100, .groups = "drop")

zm_plt_dt_q <- zm |>
  filter(stapro_txt == "Průměrná hrubá mzda na zaměstnance",
         typosoby_txt == "přepočtený") |>
  mutate(ctvrtletí = as.numeric(ctvrtletí),
         tm = make_date(rok, ctvrtletí * 3),
         clr = case_when(odvetvi_kod == "O" ~ "veřejná správa",
                         # odvetvi_kod %in% c("P") ~ "vzdělávání",
                         is.na(odvetvi_kod) ~ "celá ekonomika",
                         odvetvi_kod %in% c("M") ~ "profesní",
                         odvetvi_kod %in% c("J") ~ "ICT",
                         TRUE ~ "ostatní") |>
           as_factor() |> fct_relevel("ostatní", "profesní", "ICT", "veřejná správa"),
         public = odvetvi_kod %in% c("O")) |>
  arrange(tm, odvetvi_kod) |>
  group_by(odvetvi_kod) |>
  mutate(zmena_qonq = (hodnota - lag(hodnota, n = 4))/lag(hodnota, n = 4)) |>
  left_join(infl_qonq) |>
  select(tm, zmena = zmena_qonq, clr, hodnota, public, inflace = inflace_qonq) |>
  ungroup() |>
  arrange(clr)

infl_yony <- infl |>
  filter(casz_txt == "stejné období předchozího roku",
         is.na(ucel_txt)) |> # všechny druhy zboží/služeb
  group_by(rok) |>
  summarise(inflace_yony = (mean(hodnota) - 100)/100, .groups = "drop")

zm_plt_dt_y <- zm |>
  filter(stapro_txt == "Průměrná hrubá mzda na zaměstnance",
         typosoby_txt == "přepočtený") |>
  group_by(rok, odvetvi_kod, odvetvi_txt) |>
  summarise(hodnota = mean(hodnota)) |>
  mutate(tm = make_date(rok, 01, 01),
         clr = case_when(odvetvi_kod == "O" ~ "veřejná správa",
                         # odvetvi_kod %in% c("P") ~ "vzdělávání",
                         is.na(odvetvi_kod) ~ "celá ekonomika",
                         odvetvi_kod %in% c("M") ~ "profesní",
                         odvetvi_kod %in% c("J") ~ "ICT",
                         TRUE ~ "ostatní") |>
           as_factor() |> fct_relevel("ostatní", "profesní", "ICT", "veřejná správa"),
         public = odvetvi_kod %in% c("O")) |>
  arrange(tm, odvetvi_kod) |>
  group_by(odvetvi_kod) |>
  mutate(zmena_yony = (hodnota - lag(hodnota, n = 1))/lag(hodnota, n = 1)) |>
  left_join(infl_yony) |>
  select(tm, zmena = zmena_yony, clr, hodnota, public, inflace = inflace_yony) |>
  mutate(realna_zmena = zmena - inflace) |>
  ungroup() |>
  arrange(clr)

zm_plt_dt_y |>
  filter(tm == "2021-01-01", is.na(odvetvi_kod)) |>
  mutate(realna_zmena = zmena - inflace)

make_nace_plot <- function(data) {
  zm_plt <- ggplot(data |>
                       # filter(month(tm) == 12) |> mutate(tm = floor_date(tm, "year")) |>
                       filter(),
                     aes(tm, realna_zmena, colour = clr, size = clr != "ostatní",
                         group = clr)) +
    guides(colour = guide_legend(reverse = T, title = "Skupina NACE (odvětví)"), size = "none") +
    scale_size_manual(values = c(1, 2), expand = expansion(0, 0), guide = "none") +
    ptrr::scale_y_percent_cz(expand = expansion(0, 0),
                             # limits = c(-.2, .2),
                             breaks = seq(-.2, .2, .05)) +
    labs(title = "Meziroční reálná změna platů podle NACE skupin (odvětví)",
         x = "Rok", y = "Reálná meziroční změna (očištěno o inflaci)",
         caption = "Zdroj: vlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady a 010022 Indexy spotř. cen)") +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y",
                 expand = expansion(0, 0),
                 limits = c(as.Date("2000-09-01"), as.Date("2022-03-31"))) +
    geom_hline(yintercept = 0, colour = "grey10", linetype = "solid") +
    geom_point(aes(alpha = clr != "ostatní"), fill = "white") +
    scale_alpha_discrete(range = c(.6, 1), guide = "none") +
    geom_line(data = ~subset(., clr == "celá ekonomika")) +
    geom_point(data = ~subset(., clr == "celá ekonomika"), size = 2) +
    geom_point(data = ~subset(., clr == "celá ekonomika"), colour = "white", size = 1.2) +
    geom_line(data = ~subset(., public == TRUE)) +
    geom_point(data = ~subset(., public == TRUE), size = 2) +
    geom_point(data = ~subset(., public == TRUE), colour = "white", size = 1.2) +
    scale_color_manual(values = c(ostatní = "grey40", profesní = "blue3",
                                  `celá ekonomika` = "grey20",
                                  `ICT` = "goldenrod", `veřejná správa` = "red3")) +
    theme_urednici +
    theme(panel.grid.major = element_line(color = "white")) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  zm_plt
}

zm_plt_y <- make_nace_plot(zm_plt_dt_y)
zm_plt_y
zm_plt_y2 <- make_nace_plot(zm_plt_dt_y2)
zm_plt_y

zm_plt_q <- make_nace_plot(zm_plt_dt_q)
zm_plt_q4 <- make_nace_plot(zm_plt_dt_q |> filter(month(tm) == 12) |> mutate(tm = floor_date(tm, "year")))
zm_plt_y_2021 <- make_nace_plot(zm_plt_dt_y |> filter(tm < "2022-01-01"))

ggsave(plot = zm_plt_y, "plt_twitter.png", bg = "white", scale = 3, dpi = 300, device = ragg::agg_png,
       width = 1200, height = 675, limitsize = FALSE, units = "px")
ggsave(plot = zm_plt_y, "plt_facebook.png", bg = "white", scale = 3, dpi = 300, device = ragg::agg_png,
       width = 1200, height = 630, limitsize = FALSE, units = "px")


