library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(ptrr)

infl <- read_rds("data-interim/inflace.rds")
rocenky_lng_ltst <- read_rds("data-input/csu-rocenky-zam-sektory.rds")

rocenky_lng_ltst |>
  filter(variable == "plat", Sektory2 %in% c("Soukromý sektor celkem",
                                             "Veřejný sektor celkem")) |>
  ggplot(aes(rok, value, colour = Sektory2)) +
  geom_line()

dta <- rocenky_lng_ltst |>
  filter(
    variable == "plat",
    Sektory2 %in% c(
      "místní", "ústřední",
      "nefinanční podniky"
    )
  ) |>
  left_join(infl)

dta2 <- dta |>
  mutate(Sektory2 = case_match(Sektory2,
                               "ústřední" ~ "Ústřední vláda",
                               "místní" ~ "Samospráva",
                               "nefinanční podniky" ~ "Nefinanční podniky",
                               .default = Sektory2
  ))

ggplot(dta2 |> filter(!Sektory2 %in% c("finanční instituce",
                                       "nefinanční soukromé národní",
                                       "nefinanční soukromé pod zahraniční kontrolou")),
       aes(rok, value, colour = Sektory2, group = Sektory2)) +
  geom_line() +
  scale_color_manual(values = c(`finanční instituce` = "grey10",
                                `Nefinanční podniky` = "grey40",
                                `Ústřední vláda` = "darkred",
                                `Samospráva` = "goldenrod"), guide = "none") +
  geom_text_repel(data = dta2 |> filter(rok == max(rok)),
                  aes(x = rok, label = Sektory2),
                  hjust = "outward", nudge_x = .3, min.segment.length = 1) +
  geom_point(data = dta2 |> filter(rok == max(rok))) +
  ptrr::theme_ptrr("both") +
  scale_y_number_cz() +
  scale_x_continuous(expand = expansion(add = c(0, 6)),
                     breaks = seq(from = 2005, to = 2023, by = 3)) +
  labs(title = "Průměrná měsíční mzda podle sektorů",
       subtitle = "Vybrané sektory",
       caption = "Zdroj: ČSÚ (VDB, statistické ročenky ČR)")

ggplot(dta2 |> filter(!Sektory2 %in% c("finanční instituce",
                                       "nefinanční soukromé národní",
                                       "nefinanční soukromé pod zahraniční kontrolou")),
       aes(rok, value * ceny_deflator_2023, colour = Sektory2, group = Sektory2)) +
  geom_line() +
  scale_color_manual(values = c(`finanční instituce` = "grey10",
                                `Nefinanční podniky` = "grey40",
                                `Ústřední vláda` = "darkred",
                                `Samospráva` = "goldenrod"), guide = "none") +
  geom_text_repel(data = dta2 |> filter(rok == max(rok)),
                  aes(x = rok, label = Sektory2),
                  hjust = "outward", nudge_x = .3, min.segment.length = 1) +
  geom_point(data = dta2 |> filter(rok == max(rok))) +
  theme_urednici +
  scale_y_number_cz() +
  scale_x_continuous(expand = expansion(add = c(0, 6)),
                     breaks = seq(from = 2005, to = 2023, by = 3)) +
  labs(title = "Průměrná měsíční mzda podle sektorů",
       subtitle = "V cenách roku 2023; vybrané sektory",
       caption = "Zdroj: ČSÚ (VDB, statistické ročenky ČR)")

# This is a more detailed version of Graf A15

rocenky_lng_ltst |>
  filter(
    variable == "pocet",
    rok < 2022,
    Sektory2 %in% c(
      "místní",
      # "nefinanční soukromé pod zahraniční kontrolou",
      # "nefinanční soukromé národní", "finanční instituce",
      "nefinanční veřejné",
      "ústřední"
    )
  ) |>
  mutate(Sektory2 = as.factor(Sektory2) |>
           fct_relevel(
             "finanční instituce", "nefinanční soukromé národní",
             "nefinanční soukromé pod zahraniční kontrolou"
           )) |>
  ggplot(aes(rok, value, fill = Sektory2, group = Sektory2)) +
  scale_x_continuous(n.breaks = 20) +
  geom_area()


