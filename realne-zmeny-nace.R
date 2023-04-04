library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(forcats)
library(tidyr)

source("theme.R")
options(czso.dest_dir = "data-input/czso")

infl <- czso::czso_get_table("010022", force_redownload = TRUE) # indexy spotř. cen
czso::czso_get_dataset_metadata("010022")
max(infl$obdobido)

infl |> count(casz_kod, casz_txt)

infl |>
  filter(mesic == 12, casz_kod == "K", is.na(ucel_txt)) |>
  arrange(desc(rok)) |>
  select(rok, hodnota)

infl |>
  filter(mesic == 12, casz_kod == "Z", is.na(ucel_txt)) |>
  arrange(desc(rok)) |>
  select(rok, hodnota)

zm <- czso::czso_get_table("110079", force_redownload = TRUE) # mzdy podle NACE
count(zm, rok, ctvrtleti) |> arrange(desc(rok)) |> head()
count(zm, rok, ctvrtleti) |> filter(rok == 2022)

zm |> count(odvetvi_txt)
zm |> count(odvetvi_kod)
zm |> count(odvetvi_kod, odvetvi_txt)
zm |> count(stapro_txt)
zm |> count()


# FN ----------------------------------------------------------------------

make_nace_plot <- function(data, add_years = 5) {

  data <- data |>
    drop_na(realna_zmena) |>
    group_by(tm) |>
    mutate(is_minmax = realna_zmena == max(realna_zmena) | realna_zmena == min(realna_zmena)) |>
    ungroup() |>
    mutate(is_last_period = tm == max(tm),
           needs_label = is_last_period & (is_minmax | clr != "Ostatní"),
           name_for_label = ifelse(is_minmax, odvetvi_txt, as.character(clr)) |> str_wrap(30))

  new_labels <- c(seq(year(min(data$tm)), year(max(data$tm))), rep(" ", times = add_years))
  print(new_labels)

  new_breaks <- make_date(seq(year(min(data$tm)), year(max(data$tm))))
  print(new_breaks)

  format_pct_change <- scales::label_number(.1, 100, suffix = " %", decimal.mark = ",",
                                            style_positive = "plus", style_negative = "minus")
  format_pct_change_axis <- scales::label_number(1, 100, suffix = " %", decimal.mark = ",",
                                                 style_positive = "plus", style_negative = "minus")

  zm_plt <- ggplot(data |>
                     # filter(month(tm) == 12) |> mutate(tm = floor_date(tm, "year")) |>
                     filter(),
                   aes(tm, realna_zmena, colour = clr, size = clr != "Ostatní",
                       group = clr)) +
    geom_hline(yintercept = 0, colour = "grey10", linetype = "solid") +
    geom_point(aes(alpha = clr != "Ostatní"), fill = "white") +
    scale_alpha_discrete(range = c(.6, 1), guide = "none") +
    geom_line(data = ~subset(., clr == "Celá ekonomika")) +
    geom_point(data = ~subset(., clr == "Celá ekonomika"), size = 2) +
    geom_point(data = ~subset(., clr == "Celá ekonomika"), colour = "white", size = 1.2) +
    geom_line(data = ~subset(., public == TRUE)) +
    geom_point(data = ~subset(., public == TRUE), size = 2) +
    geom_point(data = ~subset(., public == TRUE), colour = "white", size = 1.2) +
    geom_label(data = ~subset(., needs_label),
               aes(label = paste(format_pct_change(realna_zmena), name_for_label), fill = clr),
               label.padding = unit(0.2, "lines"),
               hjust = 0, nudge_x = 40, color = "white", family = "Arial", size = 3, fontface = "bold") +
    scale_color_manual(values = c(Ostatní = "grey40", Profesní = "blue3",
                                  `Celá ekonomika` = "grey20",
                                  `ICT` = "goldenrod", `Veřejná správa` = "red3")) +
    scale_fill_manual(values = c(Ostatní = "grey40", Profesní = "blue3",
                                  `Celá ekonomika` = "grey20",
                                  `ICT` = "goldenrod", `Veřejná správa` = "red3")) +
    scale_size_manual(values = c(1, 2), guide = "none") +
    scale_x_date(date_breaks = "1 years",
                 date_labels = "%Y",
                 breaks = new_breaks,
                 expand = expansion(add = c(0, 365 * add_years)),
                 limits = c(as.Date("2000-09-01"), as.Date("2022-03-31")),
                 ) +
    scale_y_continuous(expand = expansion(add = c(.02, 0.001)),
                             # limits = c(-.2, .2),
                       labels = format_pct_change_axis,
                       breaks = seq(-.3, .3, .05)) +
    guides(colour = guide_legend(reverse = T, title = "Skupina NACE (odvětví)"), size = "none", fill = "none") +
    labs(title = "Meziroční změny průměrných reálných mezd (v odvětvích dle NACE), v %",
         x = "Rok", y = "Reálná meziroční změna (očištěno o inflaci)",
         caption = "Zdroj: vlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady a 010022 Indexy spotř. cen)") +
    theme_urednici +
    theme(panel.grid.major = element_line(color = "white")) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  zm_plt
}

# Q on Q ------------------------------------------------------------------

infl_qonq <- infl |>
  filter(casz_txt == "stejné období předchozího roku",
         is.na(ucel_txt)) |> # všechny druhy zboží/služeb
  mutate(ctvrtleti = quarter(obdobiod)) |>
  group_by(rok, ctvrtleti) |>
  summarise(inflace_qonq = (mean(hodnota) - 100)/100, .groups = "drop")

zm_plt_dt_q <- zm |>
  filter(stapro_txt == "Průměrná hrubá mzda na zaměstnance",
         typosoby_txt == "přepočtený") |>
  mutate(ctvrtleti = as.numeric(ctvrtleti),
         tm = make_date(rok, ctvrtleti * 3),
         clr = case_when(odvetvi_kod == "O" ~ "veřejná správa",
                         # odvetvi_kod %in% c("P") ~ "vzdělávání",
                         is.na(odvetvi_kod) ~ "celá ekonomika",
                         odvetvi_kod %in% c("M") ~ "profesní",
                         odvetvi_kod %in% c("J") ~ "ICT",
                         TRUE ~ "ostatní") |>
           as_factor() |> fct_relevel("ostatní", "profesní", "ICT", "veřejná správa"),
         public = odvetvi_kod %in% c("O")) |>
  ungroup() |>
  left_join(infl_qonq, by = c("rok", "ctvrtleti")) |>
  arrange(odvetvi_kod, tm) |>
  group_by(odvetvi_kod) |>
  mutate(realna_mzda = hodnota/(1 + inflace_qonq),
         realna_zmena = realna_mzda/lag(realna_mzda, 4) - 1,
         rozdil_nominalni = hodnota - lag(hodnota, 4),
         rozdil_nominalni_perc = rozdil_nominalni/lag(hodnota, 4),
         realna_zmena_2 = (1 + rozdil_nominalni_perc)/(1 + inflace_qonq) - 1) |>
  ungroup() |>
  select(tm, realna_mzda, realna_zmena, clr, hodnota, public,
         rozdil_nominalni, rozdil_nominalni_perc,
         inflace = inflace_qonq, realna_zmena_2) |>
  ungroup() |>
  arrange(clr)

infl_yony <- infl |>
  filter(casz_txt == "stejných 12 měsíců předchozího roku", mesic == 12,
         is.na(ucel_txt)) |> # všechny druhy zboží/služeb
  group_by(rok) |>
  select(inflace_yony = hodnota, rok)

infl_index <- infl |>
  filter(casz_txt == "průměr bazického roku",
         is.na(ucel_txt)) |> # všechny druhy zboží/služeb
  group_by(rok) |>
  summarise(inflace_index = mean(hodnota))

zm_plt_dt_y <- zm |>
  filter(stapro_txt == "Průměrná hrubá mzda na zaměstnance",
         typosoby_txt == "přepočtený") |>
  group_by(rok, odvetvi_kod, odvetvi_txt) |>
  summarise(hodnota = mean(hodnota), .groups = "drop") |>
  mutate(tm = make_date(rok, 01, 01),
         clr = case_when(odvetvi_kod == "O" ~ "Veřejná správa",
                         # odvetvi_kod %in% c("P") ~ "vzdělávání",
                         is.na(odvetvi_kod) ~ "Celá ekonomika",
                         odvetvi_kod %in% c("M") ~ "Profesní",
                         odvetvi_kod %in% c("J") ~ "ICT",
                         TRUE ~ "Ostatní") |>
           as_factor() |> fct_relevel("Ostatní", "Profesní", "ICT", "Veřejná správa"),
         public = odvetvi_kod %in% c("O")) |>
  ungroup() |>
  left_join(infl_yony, by = "rok") |>
  select(tm, clr, hodnota, public, inflace = inflace_yony, odvetvi_kod, odvetvi_txt) |>
  arrange(odvetvi_kod, tm) |>
  group_by(odvetvi_kod) |>
  mutate(nominalni_zmena = hodnota/lag(hodnota, 1),
         realna_zmena = nominalni_zmena/inflace*100 - 1) |>
  ungroup() |>
  # select(tm, mzda = hodnota, realna_mzda, realna_zmena, clr, hodnota, public, realna_zmena_2) |>
  ungroup() |>
  arrange(clr)

make_nace_plot(zm_plt_dt_y)

zm_plt_dt_y2 <- zm |>
  filter(stapro_txt == "Průměrná hrubá mzda na zaměstnance",
         typosoby_txt == "přepočtený") |>
  group_by(rok, odvetvi_kod, odvetvi_txt) |>
  summarise(hodnota = mean(hodnota), .groups = "drop") |>
  mutate(tm = make_date(rok, 01, 01),
         clr = case_when(odvetvi_kod == "O" ~ "Veřejná správa",
                         # odvetvi_kod %in% c("P") ~ "vzdělávání",
                         is.na(odvetvi_kod) ~ "Celá ekonomika",
                         odvetvi_kod %in% c("M") ~ "Profesní",
                         odvetvi_kod %in% c("J") ~ "ICT",
                         TRUE ~ "Ostatní") |>
           as_factor() |> fct_relevel("Ostatní", "Profesní", "ICT", "Veřejná správa"),
         public = odvetvi_kod %in% c("O")) |>
  ungroup() |>
  left_join(infl_index, by = "rok") |>
  select(tm, clr, hodnota, public, inflace_index, odvetvi_kod, odvetvi_txt) |>
  arrange(odvetvi_kod, tm) |>
  group_by(odvetvi_kod) |>
  mutate(realna_mzda = hodnota/inflace_index*100,
         nominalni_rozdil = hodnota - lag(hodnota, 1),
         realna_zmena = realna_mzda/lag(realna_mzda, 1) - 1) |>
  ungroup() |>
  # select(tm, mzda = hodnota, realna_mzda, realna_zmena, clr, hodnota, public, realna_zmena_2) |>
  ungroup() |>
  arrange(clr)

make_nace_plot(zm_plt_dt_y)
make_nace_plot(zm_plt_dt_y2)

# zm_plt_y2 <- make_nace_plot(zm_plt_dt_y2)
# zm_plt_y

zm_plt_q <- make_nace_plot(zm_plt_dt_q)
zm_plt_q
zm_plt_q4 <- make_nace_plot(zm_plt_dt_q |> filter(month(tm) == 12) |> mutate(tm = floor_date(tm, "year")))
zm_plt_y_2021 <- make_nace_plot(zm_plt_dt_y |> filter(tm < "2022-01-01"))
zm_plt_y_2021

# Plot and export ---------------------------------------------------------

zm_plt_y <- make_nace_plot(zm_plt_dt_y)
ggsave(plot = zm_plt_y, "plt_twitter.png", bg = "white", scale = 3, dpi = 300, device = ragg::agg_png,
       width = 1200, height = 675, limitsize = FALSE, units = "px")
ggsave(plot = zm_plt_y, "plt_facebook.png", bg = "white", scale = 3, dpi = 300, device = ragg::agg_png,
       width = 1200, height = 630, limitsize = FALSE, units = "px")


zm_plt_dt_y |> filter(tm > "2021-12-31", is.na(odvetvi_kod))
zm_plt_dt_y2 |> filter(tm > "2021-12-31", is.na(odvetvi_kod))
make_nace_plot(zm_plt_dt_y2)
