library(dplyr)
library(ggplot2)
library(ggokabeito)
library(lubridate)

dta <- readRDS("./data-interim/sections.rds")

dta |>
  filter(typ_rozpoctu == "SKUT", !name %in% c("ROPO", "OSS"),
         !kategorie_2014_cz %in% c("Státní správa", "Státní úředníci")) |>
  mutate(kategorie = case_when(kategorie_2014_cz == "Příspěvkové organizace" & kap_name == "MŠMT" ~ "PO MŠMT - hlavně školy",
                               .default = kategorie_2014_cz)) |>
  summarise(pocet = sum(pocet_zamestnancu), .by = c(rok, kategorie)) |>
  group_by(kategorie) |>
  arrange(kategorie, rok) |>
  mutate(delta = pocet - lag(pocet), datum = make_date(rok)) |>
  filter(rok > 2003) |>
  ggplot(aes(datum, delta, fill = kategorie, alpha = rok != 2012)) +
  geom_col(colour = "white", linewidth = .5) +
  geom_hline(yintercept = 0, colour = "grey92") +
  scale_fill_okabe_ito(name = "Skupina") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = c(c(.01, 0), c(0.01, 0))) +
  # scale_x_continuous(n.breaks = 20, expansion(add = c(0, 0)), limits = c(2003, 2024)) +
  scale_alpha_discrete(range = c(0.4, 1), guide = "none") +
  guides(fill = guide_legend(keywidth = unit(6, "pt"), keyheight = unit(12, "pt"),
                             override.aes = list(colour = NA))) +
  ptrr::scale_y_number_cz(n.breaks = 12, expand = expansion(add = c(2000, 2000)),
                          labels = scales::number_format(style_positive = "plus", style_negative = "minus")) +
  # ptrr::theme_ptrr("both") +
  labs(title = "Změny počtu zaměstnanců ve sféře rozpočtové regulace, podle kategorií organizací",
       subtitle = "Meziroční změna skutečného počtu zaměstnanců podle Státního závěrečného účtu",
       caption = "Změna v roce 2012 způsobena změnou klasifikace některých zaměstnanců MV a MZV") +
  theme(panel.grid = element_line(colour = "grey95"),
        text = element_text(family = "Arial"),
        plot.subtitle = element_text(margin = margin(6, 6, 18, 6, "pt")),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect("white"),
        plot.background = element_rect(fill = "grey95"))
