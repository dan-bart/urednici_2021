library(dplyr)
library(plotly )
library(ggplot2)
library(data.table)
library(stringr)
library(tidyr)
library(forcats)
library(RColorBrewer)
library(gplots)
library(bslib)
library(readr)
library(readxl)
library(tibble)
library(tidyr)
library(here)
library(stringr)
library(janitor)
library(czso)
library(kableExtra)
library(htmlwidgets)
library(coloratio)
library(ggokabeito)
unloadNamespace("plyr")

library(arrow)
options(scipen = 100, digits = 8)

dta <- readRDS("./data-interim/sections.rds")
# write_parquet(dta, "dashboard/dta.parquet")
dta_sum <- readRDS("./data-interim/summary.rds")

dta$kap_name[dta$kap_name == "Ksen"] <- "KSen"
dta$kap_name[dta$kap_name == "Kparl"] <- "KSněm"
dta$kap_name[dta$kap_name == "Mzdr"] <- "MZd"
dta$kap_name[dta$kap_name == "Mspr"] <- "MSp"

## ----plotly-utils------------------------------------------------------------------------------------------------

btnrm <- c("zoomIn2d", "zoomOut2d", "pan2d", "lasso2d", "select2d", "autoScale2d")
grdclr <- "grey"
cap_col <- "mediumblue"
wrap_len <- 135

js <- "
function(el){
el.on('plotly_restyle', function() {
placeLegendAnnot();
});
el.on('plotly_relayout', function() {
placeLegendAnnot();
});
}"

cap_size <- 25
pozn_size <- 17
pozn_long_size <- 14
lbl_size <- 17
kat_tick_size <- 14
num_tick_size <- 17
axis_size <- 20
lgnd_size <- 17
hover_size <- 15
mrk_min_size <- 10
mrk_maj_size <- 13

mrg <- list(l = 0, r = 0, b = 0, t = 0, pad = 0, autoexpand = FALSE)
mrg2 <- list(t = 50,b=100, autoexpand = TRUE)
mrg3 <- list(t = 50, autoexpand = TRUE)
mrg4 <- list(t = 50,b=120, autoexpand = TRUE)
mrg5 <- list(t = 80,b=120, autoexpand = TRUE)
mrg6 <- list(t = 70,b=100, autoexpand = TRUE)
mrg7 <- list(t = 70, autoexpand = TRUE)
mrg8 <- list(t = 50,b=30, l=10, autoexpand = TRUE)

uni_font <- "Arial"

title_font <- list(color = cap_col,size=cap_size,
                   # family="Georgia,Times,Times New Roman,serif")
                   family=uni_font)
pozn_font <- list(size = pozn_size)
pozn_font_small <- list(size = pozn_long_size)
axis_font <- list(color = "#000000",size=axis_size,
                  family=uni_font)
legend_below = list(x = 0, y = 0,orientation = "h",xanchor = "left",xref='paper',yref="paper",font=list(size=lgnd_size,family=uni_font))
kat_ticks<-list(tickfont=list(size=kat_tick_size,family=uni_font),showticklabels = T,tickangle = 0,tickmode = "array")
kat_ticks_rotated<-list(tickfont=list(size=kat_tick_size,family=uni_font),showticklabels = T,tickangle = -90,tickmode = "array")
num_ticks <- list(tickfont=list(size=num_tick_size,family=uni_font))
num_tilt_ticks <- list(tickfont=list(size=num_tick_size,family=uni_font),tickangle = -45)
frame_y<-list(mirror=T,linewidth = 2,ticks='outside',showline=T,gridcolor = grdclr)
frame_x<-list(mirror=T,linewidth = 2,ticks='outside',showline=T,dtick=5)
annot_below<-list(                       align='left',
                                         xref='paper',
                                         yref="paper",
                                         x=0,
                                         y=0,
                                         font = pozn_font_small,
                                         bordercolor = 'rgba(0,0,0,0)',
                                         borderwidth=1,
                                         showarrow = FALSE)

kat_order_ss <- c("Ministerstva", "Ostatní ústřední", "Neústřední st. správa")
kat_order_all <- c("Ministerstva", "Ostatní ústřední", "Neústřední st. správa",
                   "Státní úředníci",
                   "Ostatní vč. armády", "Sbory", "Příspěvkové organizace")

color_map <- c("Ministerstva" =             "#221669",
               "Ostatní ústřední" =         "#1C00C9",
               "Neústřední st. správa" =    "#0069B4",
               "Ostatní vč. armády" =       "#3CB450",
               "Příspěvkové organizace" =   "#C3C7C4",
               "Sbory" =                    "#BB133E",
               "Ústřední orgány" =          "#EB96D8",
               "Státní úředníci" =          "#C666BC",
               "Státní správa" =            "#9BC9E9",
               "Organizační složky státu" = "#C4DFF2")


lbls <- names(color_map)

color_map <- c(ggokabeito::palette_okabe_ito(1:4),
               "#CCCCCC",
               ggokabeito::palette_okabe_ito(5:9))
names(color_map) <- lbls


text_color_map <- coloratio::cr_choose_bw(color_map)
names(text_color_map) <- names(color_map)

kaps <- unique(dta$kap_name)
color_map_kap <- ifelse(substr(kaps,1,1) == "M", "dimgray", "cornflowerblue")
names(color_map_kap) <- kaps
cols_df <- tibble(labels = names(color_map), color = unname(color_map)) |>
  mutate(color_text = cr_choose_bw(color_map))
colorspace::swatchplot(cols_df$color)

# Theme gg ----------------------------------------------------------------

source("theme.R")

## ----tree_prep---------------------------------------------------------------------------------------------------
aux <- dta %>%
  filter(!is.na(kategorie_2014_cz), typ_rozpoctu == "SKUT",
         !kategorie_2014 %in% c("Statni sprava", "Statni urednici"),
         rok == 2023) %>%
  group_by(kategorie_2014_cz) %>%
  summarise(cost = sum(prostredky_na_platy),
            count = sum(pocet_zamestnancu)) %>%
  rename(labels = kategorie_2014_cz) %>%
  mutate(parents = case_match(labels,
                              "Příspěvkové organizace" ~ "",
                              "Sbory" ~ "Státní správa",
                              "Ostatní vč. armády" ~ "Organizační složky státu",
                              "Neústřední st. správa" ~ "Státní úředníci",
                              "Ostatní ústřední" ~ "Ústřední orgány",
                              "Ministerstva" ~ "Ústřední orgány"

  ))

aux_sum <- aux |>
  summarise(across(c(cost, count), sum), .by = parents)

to_append = tribble(
  ~labels,               ~parents,
  "Ústřední orgány", "Státní úředníci",
  "Státní úředníci", "Státní správa",
  "Státní správa", "Organizační složky státu",
  "Organizační složky státu", "") |>
  mutate(
  cost = c(
    aux$cost[aux$labels == "Ministerstva"] +
      aux$cost[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"],
    aux$cost[aux$labels == "Ministerstva"] +
      aux$cost[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"]+
      aux$cost[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"],
    aux$cost[aux$labels == "Ministerstva"] +
      aux$cost[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"] +
      aux$cost[aux$labels == "Sbory"] +
      aux$cost[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"],
    aux$cost[aux$labels == "Ministerstva"] +
      aux$cost[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"] +
      aux$cost[aux$labels == "Sbory"] +
      aux$cost[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"] +
      aux$cost[aux$labels == "Ostatn\u00ED v\u010D. arm\u00E1dy"]
  ),
  count = c(
    aux$count[aux$labels == "Ministerstva"] +
      aux$count[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"],
    aux$count[aux$labels == "Ministerstva"] +
      aux$count[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"]+
      aux$count[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"],
    aux$count[aux$labels == "Ministerstva"] +
      aux$count[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"] +
      aux$count[aux$labels == "Sbory"] +
      aux$count[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"],
    aux$count[aux$labels == "Ministerstva"] +
      aux$count[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"] +
      aux$count[aux$labels == "Sbory"] +
      aux$count[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"] +
      aux$count[aux$labels == "Ostatn\u00ED v\u010D. arm\u00E1dy"]
  ))

tree_data <- bind_rows(aux, to_append)

tree_data <- tree_data %>% mutate("cost_perc" = cost/sum(cost[which(tree_data$parents == "")]),
                                  "count_perc" = count/sum(count[which(tree_data$parents  == "")]),
                                  labels = as.character(labels))

## ----priprava_zbytek------------------------------------------------------------------------------------------------
dta <- dta %>%
  # mutate(kategorie_2014_cz = ifelse(kategorie_2014_cz == "Státní správa",
  #                                   "Státní úředníci celkem",kategorie_2014_cz)) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel(kat_order_all) %>% fct_rev())

abs_metrics <- dta %>%
  filter(rok == 2023, typ_rozpoctu == "SCHV") %>%
  filter(!is.na(as.numeric(kap_num))) %>%
  filter(!name %in% c("ROPO", "SS", "OSS")) %>%
  group_by(kap_name) %>%
  summarise(
    pocet_zamestnancu = sum(pocet_zamestnancu),
    prostredky_na_platy = sum(prostredky_na_platy),
    prumerny_plat = sum(prumerny_plat)
  )

bar_dt <- dta %>%
  filter(rok == 2023, typ_rozpoctu == "SKUT") %>%
  filter(!is.na(kategorie_2014_cz)) %>%
  filter(!is.na(as.numeric(kap_num))) %>%
  filter(!name %in% c("ROPO", "SS", "OSS"),
         kategorie_2014_cz != "Státní úředníci") %>%
  group_by(kap_name)%>%
  mutate(bar_name_count = paste(kap_name, " Celkem: ",sum(pocet_zamestnancu))) %>%
  mutate(bar_name_cost = paste(kap_name, " Celkem: ",sum(prostredky_na_platy))) %>%
  mutate(pocet_zamestnancu_agg = sum(pocet_zamestnancu),
         prostredky_na_platy_agg = sum(prostredky_na_platy),
         prumerny_plat_agg = sum(prumerny_plat))
lty <- c(schvaleny = "dash", skutecnost = "solid")

## ----tree_plot---------------------------------------------------------------------------------------------------
root_label <- "Celek"
root_color <- "#f0f0f0"

graf_A1 <- tree_data %>%
  left_join(cols_df, by = "labels")

graf_A1$labels_tree <- graf_A1$labels
graf_A1$labels_tree[graf_A1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")] <-
  paste0(graf_A1$labels[graf_A1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")],"<br><sup>",
         gsub("^\\s+","",format(round(graf_A1$cost[graf_A1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")]/10^9,1))),
         " mld. Kč</sup>")
graf_A1$parents[graf_A1$parents==""] <- root_label
graf_A1 <- rbind(graf_A1,data.frame(labels = root_label,
                                    labels_tree = root_label,
                                    cost = sum(graf_A1$cost[graf_A1$parents==root_label]),
                                    count = sum(graf_A1$count[graf_A1$parents==root_label]),
                                    cost_perc = sum(graf_A1$cost_perc[graf_A1$parents==root_label]),
                                    count_perc = sum(graf_A1$count_perc[graf_A1$parents==root_label]),
                                    color = root_color,
                                    color_text = "black",
                                    parents = ""))

graf_A1 <- graf_A1 |>
  plot_ly(
    type = "treemap",
    branchvalues = "total",
    labels = ~labels_tree,
    parents = ~parents,
    marker = list(colors = ~color),
    pathbar = list(side = "bottom", thickness = 30),
    values = ~cost,
    textfont = list(family = uni_font,size = 18,color = ~color_text),
    hovertemplate = ~ paste("<extra></extra>", " Kategorie: ",
                            labels, "<br>", " Rozpo\u010Det:",
                            format(cost, big.mark = " "), "K\u010D", "<br>",
                            " Pod\u00EDl na celku:", round(cost_perc * 100, 1), "%"),
    hoverlabel = list(font = list(size = hover_size, color = ~color_text)),
    domain = list(column = 0)
  ) %>%
  layout(title = list(font=title_font,
                      text = paste0("<b>Graf A1. Výdaje na platy státních zaměstnanců dle regulace zaměstnanosti (2023)</b>",
                                    "<br>","<sup>","Velikost obdélníků je úměrná podílu dané skupiny na celkových výdajích","</sup>"),
                      y = 0.97),
         margin = mrg8) %>%
  # layout(annotations = list(text = "<i>Pozn.: Pro bližší detail lze kategorie rozkliknout.</i>",
  #                           x = 1, y = -0.05, showarrow = FALSE, font = pozn_font_small)) %>%
  layout(uniformtext = list(minsize = lbl_size, mode = 'show')) %>%
  config(displaylogo = FALSE, modeBarButtonsToRemove = btnrm) %>%
  onRender(js)

tree_data %>%
  left_join(cols_df, by = "labels") |>
  count(color)

graf_A1

graf_1 <- tree_data %>%
  left_join(cols_df, by = "labels")
graf_1$labels_tree <- graf_1$labels
graf_1$labels_tree[graf_1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")] <-
  paste0(graf_1$labels[graf_1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")],"<br><sup>",
        gsub("^\\s+","",format(graf_1$count[graf_1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")],big.mark = " ")),
        " zaměst.</sup>")
graf_1$parents[graf_1$parents==""] <- root_label
graf_1 <- rbind(graf_1,data.frame(labels = root_label,
                                  labels_tree = root_label,
                                  cost = sum(graf_1$cost[graf_1$parents==root_label]),
                                  count = sum(graf_1$count[graf_1$parents==root_label]),
                                  cost_perc = sum(graf_1$cost_perc[graf_1$parents==root_label]),
                                  count_perc = sum(graf_1$count_perc[graf_1$parents==root_label]),
                                  color = root_color,
                                  color_text = "black",
                                  parents = ""))

graf_1 <- graf_1 |>
  plot_ly(
    type = "treemap",
    branchvalues = "total",
    labels = ~labels_tree,
    parents = ~parents,
    marker = list(colors = ~color),
    pathbar = list(side = "bottom", thickness = 30),
    values = ~count,
    textfont = list(family=uni_font, size = 18,color = ~color_text),
    hovertemplate = ~ paste("<extra></extra>", " Kategorie: ", labels, "<br>",
                            " Po\u010Det zam\u011Bstnanc\u016F:",
                            format(count, big.mark = " "), "<br>", " Pod\u00EDl na celku:",
                            round(count_perc * 100, 1), "%"),
    hoverlabel = list(font = list(size = hover_size,color = text_color_map)),
    domain = list(column = 0)
  ) %>%
  layout(title = list(font = title_font,
                      text = paste0("<b>Graf 1. Počet státních zaměstnanců dle regulace zaměstnanosti (2023)</b>",
                                    "<br>","<sup>","Velikost obdélníků je úměrná podílu dané skupiny na celkovém počtu státních zaměstnanců","</sup>"),
                      y = 0.97),
         margin = mrg8) %>%
  # layout(annotations = list(text = "<i>Pozn.: Pro bližší detail lze kategorie rozkliknout.</i>", x = 1,
  #                           y = -0.05, showarrow = FALSE, font = pozn_font_small)) %>%
  layout(uniformtext = list(minsize=lbl_size, mode = 'show')) %>%
  config(displaylogo = FALSE, modeBarButtonsToRemove = btnrm) %>%
  onRender(js)

graf_1


## ----counts------------------------------------------------------------------------------------------------------

graf_2 <- bar_dt %>% group_by(kategorie_2014_cz)%>%
  plot_ly(
    x = ~kap_name, y = ~ pocet_zamestnancu/1000, type="bar",
    color = ~kategorie_2014_cz, colors = color_map,
    #marker=list(size=10, colors=color_map[bar_dt$kategorie_2014_cz]),
    hovertemplate = ~ ifelse(pocet_zamestnancu > 0, paste(
      "<extra></extra>", " Zdroj:", name, "<br>", "Kapitola:", cz_kap_name, "<br>",
      "Po\u010Det zam\u011Bstnanc\u016F:",
      format(pocet_zamestnancu, big.mark = " "), "<br>", "Celkem za kapitolu: ",
      format(pocet_zamestnancu_agg, big.mark = " ")
    ), ""), hoverinfo = "text",hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
  layout(
    # hovermode = "x",
    legend = legend_below,
    # annotations = c(list(text = str_wrap("<i>Pozn.: Ministerstvo školství, mládeže a tělovýchovy zkresluje graf vzhledem k zahrnutí učitelů v kategorii “Příspěvkové organizace”. Lze odflitrovat v legendě nebo v grafu.</i>",wrap_len),
    #                      font = pozn_font_small),
    #                 annot_below),
    title = list(font=title_font,
                 text = "<b>Graf 2. Počet st\u00E1tn\u00EDch zam\u011Bstnanc\u016F dle rozpo\u010Dtov\u00FDch kapitol (2023)</b>", y = 0.97),
    xaxis = c(kat_ticks_rotated,frame_x,
              list(title = "<b>Kapitoly</b>",categoryorder = "array",categoryarray = arrange(bar_dt, desc(pocet_zamestnancu_agg))$kap_name,
                   titlefont = axis_font)),
    yaxis = c(num_ticks,frame_y,list(title = "<b>Po\u010Det zam\u011Bstnanc\u016F (v tisících)</b>",
                                     titlefont = axis_font)),
    barmode = "stack",
    margin = mrg4) %>%config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)
graf_2

## ----costs, include = F------------------------------------------------------------------------------------------

annot_below_A2 <- list(align='left',
                       xref='paper',
                       yref="paper",
                       x=0,
                       y=-0,
                       bordercolor = 'rgba(0,0,0,0)',
                       borderwidth=1,
                       showarrow = FALSE)
legend_below_A2 = list(x = 0, y = 0,orientation = "h",xanchor = "left",xref='paper',yref="paper",font=list(size=lgnd_size,family=uni_font))

graf_A2 <- plot_ly(bar_dt,
                   x = ~kap_name, y = ~ prostredky_na_platy / 1e9,
                   color = ~kategorie_2014_cz, colors = color_map,
                   hovertemplate = ~ ifelse(prostredky_na_platy > 0,
                                            paste("<extra></extra>", " Zdroj:",
                                                  name, "<br>", "Kapitola:",
                                                  cz_kap_name, "<br>", "Platy celkem:",
                                                  format(prostredky_na_platy, big.mark = " "),
                                                  "K\u010D", "<br>", "Celkem za kapitolu: ",
                                                  format(prostredky_na_platy_agg, big.mark = " "),
                                                  "K\u010D"),
                                            ""),
                   hoverlabel = list(font=list(size=hover_size,family=uni_font)),
                   hoverinfo = "text") %>%
  add_bars() %>%
  layout(
    legend = legend_below_A2,
    # hovermode = "x",
    # annotations = c(list(text = str_wrap("<i>Pozn.: Ministerstvo školství, mládeže a tělovýchovy zkresluje graf vzhledem k zahrnutí učitelů v kategorii “Příspěvkové organizace”. Lze odflitrovat v legendě nebo v grafu.</i>",wrap_len),
    #                      font = pozn_font_small),
    #                 annot_below_A2),
    title = list(font=title_font,
                 text = str_wrap("<b>Graf A2. Výdaje na platy státních zaměstnanců dle rozpo\u010Dtov\u00FDch kapitol (rok 2023, mld. K\u010D)</b>",70), y = 0.96),
    xaxis = c(kat_ticks_rotated,frame_x, list(title="<b>Kapitoly</b>",titlefont = axis_font,categoryorder = "array", categoryarray = arrange(bar_dt, desc(prostredky_na_platy_agg))$kap_name)),
    yaxis = c(num_ticks,frame_y,list(title = "<b>Platy (mld. Kč)</b>",titlefont = axis_font)),
    barmode = "stack",
    margin = mrg5) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)


## ----mean_costs_ALL----------------------------------------------------------------------------------------------

kat_means <- bar_dt %>% filter(kategorie_2014_cz != "Státní úředníci") %>% group_by(kategorie_2014_cz)%>%
  summarise(prumerny_plat_mean = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)/12)/1e3)

vline <- function(x = 0, color = "gray",width=1) {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color,width=1)
  )
}

hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color,width=2)
  )
}

kat_order_graf3 <- c("Ministerstva", "Ostatní ústřední", "Neústřední st. správa",
                     "Ostatní vč. armády", "Sbory", "Příspěvkové organizace")

dt_mean_salary_all <- dta %>%
  filter(typ_rozpoctu == "SKUT",
         !kategorie_2014 %in% c("Statni sprava", "Statni urednici"), rok == 2023) %>%
  summarise(prumerny_plat = round(sum(prostredky_na_platy)/sum(pocet_zamestnancu)/12/1e3)) %>%
  pull(prumerny_plat)

graf_3 <- bar_dt %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel(kat_order_graf3) %>% fct_rev())%>%
  filter(kategorie_2014_cz != "Státní úředníci") %>%
  filter(prumerny_plat>25000) %>% left_join(kat_means,by="kategorie_2014_cz")%>%
  mutate(prumerny_plat = prumerny_plat/1e3) %>%
  plot_ly(line = list(color='#D3D3D3',width=2),
          x = ~prumerny_plat, y = ~ kategorie_2014_cz, color = ~kategorie_2014_cz,
          colors = color_map,marker=list(size=mrk_min_size),
          opacity = 0.7,
          type = "scatter" , mode = "line+markers",
          hovertemplate = ~ ifelse(prostredky_na_platy > 0,
                                   paste("<extra></extra>", " Zdroj:", name, "<br>",
                                         "Kapitola:", cz_kap_name, "<br>", "Pr\u016Fm\u011Brn\u00FD plat:",
                                         format(prumerny_plat, big.mark = " "), "K\u010D"),
                                   ""),
          hoverlabel = list(font=list(size=hover_size,family=uni_font)),
          hoverinfo = "text"
  ) %>%
  add_trace(type="scatter",mode="markers",
            opacity=1,
            hovertemplate = ~ ifelse(prostredky_na_platy > 0,
                                     paste("<extra></extra>", "Pr\u016Fm\u011Br za kategorii:", "<br>",
                                           format(round(prumerny_plat_mean*1e3,0), big.mark = " "),
                                           "K\u010D"),
                                     ""),
            hoverinfo = "text",
            marker=list(size=mrk_maj_size,color="black"),
            x = ~prumerny_plat_mean, y=~kategorie_2014_cz) %>%
  layout(hovermode = "closest",
         title = list(font=title_font,
                      text = "<b>Graf 3. Pr\u016Fm\u011Brn\u00E9 platy st\u00E1tn\u00EDch zam\u011Bstnanc\u016F (2023)</b>", y = 1.1),
         # annotations = list(align='left',
         #                    xref='paper',
         #                    yref="paper",
         #                    x=0,
         #                    y=-0.1,
         #                    font = pozn_font_small,
         #                    bordercolor = 'rgba(0,0,0,0)',
         #                    borderwidth=1,
         #                    showarrow = FALSE,
         #                    text = str_wrap("<i>Pozn.: Průměrné platy se liší dle typu organizací i napříč jednotlivými organizacemi.</i>",wrap_len)),
         xaxis = c(num_ticks,frame_y,list(dtick = 5,title = "<b>Průměrný hrubý měsíční plat (v tisících Kč)</b>",
                                          titlefont = axis_font)),
         yaxis = c(num_ticks,frame_y,list(title = "",titlefont = axis_font)),showlegend = F,
         margin = mrg2) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)
graf_3

bar_dt$width<-0.8
## ----mean_costs_v2-----------------------------------------------------------------------------------------------
graf_A3 <- bar_dt %>%
  group_by(kategorie_2014_cz) %>%
  filter(kategorie_2014 != "Prispevkove organizace") %>%
  filter(kategorie_2014_cz != "Sbory") %>%
  mutate(wage_to_general = (ifelse(kategorie_2014 %in% c("Ministerstva", "Ostatni ustredni"),
                                   prumerny_plat / phasal_all - 1,
                                   prumerny_plat / czsal_all-1))) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa")) %>%
  group_map(~ plot_ly(
    data = .,
    hovertemplate = ~  paste(
      "<extra></extra>", "Zdroj:", name, "<br>", "Kapitola:", cz_kap_name, "<br>",
      "Pr\u016Fm\u011Brn\u00FD plat:",
      format(prumerny_plat, big.mark = " "), "K\u010D", "<br>",
      "Rozd\u00EDl k pr\u016Fm\u011Brn\u00E9 mzd\u011B:",
      round(wage_to_general*100,1), "%"),
    hoverlabel = list(font=list(size=hover_size,family=uni_font)),
    hoverinfo = "text"
  ) %>% add_bars(x = ~kap_name, y = ~ (wage_to_general) * 100,
                 type = "bar", width=~0.8,color = ~kategorie_2014_cz, colors = color_map)%>%
    add_annotations(
      text = ~paste("<b>",unique(kategorie_2014_cz),"</b>"),
      x = 0.5,
      y = 1.25,
      yref = "paper",
      xref = "paper",
      font = list(family = "verdana", size = axis_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE
    ) %>%
    layout(margin = list(t = 150,b=100,l=20),
           yaxis = c(kat_ticks,frame_y,list(title = list(text="<b>Procenta</b>"),
                                            titlefont = axis_font,range=c(-65,65),
                                            ticktext = lapply(seq(-50,50,50), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                            tickvals = seq(-50,50,50),
                                            tickmode = "array",
                                            ticksuffix="%")),
           xaxis = c(kat_ticks_rotated,frame_x,list("categoryorder" = "total ascending"),
                     titlefont = axis_font),
           title = list(font=title_font,
                        text = str_wrap("<b>Graf A3. Pr\u016Fm\u011Brn\u00E9 platy st\u00E1tn\u00EDch zam\u011Bstnanc\u016F ve vztahu k pr\u016Fm\u011Brn\u00E9 mzd\u011B v n\u00E1rodn\u00EDm hospod\u00E1\u0159stv\u00ED (2023)</b>",70),
                        x = 50, y = 0.95), legend = list(x = 50, y = 0.5),
           showlegend = F
    ), keep = TRUE) %>%
  subplot(nrows = 2, shareY = F, margin = c(0.07,0.07,0.15,0.15),titleY =T) %>%
  config(displaylogo = FALSE, modeBarButtonsToRemove = btnrm) %>%
  onRender(js)
graf_A3

## ----count_2014--------------------------------------------------------------------------------------------------
#kapitoly s velkým nárůstem zaměstnanců v období 2011-2012
zk <- dta %>% filter(rok %in% c(2011,2012)) %>%
  filter(typ_rozpoctu == "SKUT", kategorie_2014 %in% c("Ministerstva")) %>%
  select(rok,full_kap_name, kap_name, pocet_zamestnancu) %>%
  pivot_wider(names_from = c("rok"),names_prefix = "rok_",
              values_from = c("pocet_zamestnancu")) %>%
  mutate(perc_change = round((rok_2012-rok_2011)/rok_2011*100,1)) %>%
  arrange(desc(perc_change))


graf_4 <- dta %>%
  filter(typ_rozpoctu == "SKUT",
         kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni")) %>%
  filter(!kap_num %in% c(314, 306)) %>%
  group_by(kategorie_2014_cz, rok) %>%
  summarise(pocet_zamestnancu = sum(pocet_zamestnancu)) %>%
  group_by(rok) %>%
  mutate(pocet_zamestnancu_agg = sum(pocet_zamestnancu)) %>%
  plot_ly(
    x = ~rok, y = ~ pocet_zamestnancu / 1000, color = ~kategorie_2014_cz,
    colors = color_map,
    hovertemplate = ~ paste(
      "<extra></extra>", "Rok:", rok, "<br>", "Kategorie:", kategorie_2014_cz, "<br>",
      "Zam\u011Bstnanc\u016F:", format(pocet_zamestnancu, big.mark = " "), "<br>",
      "Celkem za rok: ", format(pocet_zamestnancu_agg, big.mark = " ")
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font)),
    hoverinfo = "text"
  ) %>%
  add_bars()%>%
  layout(barmode="stack",bargap=0.5,
         title = list(font=title_font,
                      text = "<b>Graf 4. Počet státních úředníků (2003–2023)</b>",
                      y = 0.98),
         # annotations = c(annot_below,list(text = str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie. Graf A14 s kapitolami ministerstev vnitra a zahraničních věcí je v příloze.</i>",wrap_len),
         #                                  font = pozn_font_small)),
         xaxis = c(num_ticks,frame_x,list(title = list(text="<b>Rok</b>",
                                                       standoff=10),
                                          dtick=2,
                                          titlefont = axis_font),
                   list(tickvals = seq(2003,2023,2))),
         yaxis = c(num_ticks,frame_y,list(title = "<b>Počet státních úředníků (v tisících)</b>",
                                          titlefont = axis_font)),
         legend = legend_below,
         margin = mrg2 ) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)

graf_4


## ----cost_2014---------------------------------------------------------------------------------------------------
vyvoj_bar <- dta %>%
  filter(typ_rozpoctu == "SKUT",
         kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni")) %>%
  filter(!kap_num %in% c(314, 306)) %>%
  group_by(kategorie_2014_cz, rok) %>%
  summarise(
    prostredky_na_platy_nom = sum(prostredky_na_platy),
    prostredky_na_platy_real = sum(prostredky_na_platy * base_2023)
  ) %>%
  group_by(rok) %>%
  mutate(prostredky_na_platy_nom_agg = sum(prostredky_na_platy_nom),
         prostredky_na_platy_real_agg = sum(prostredky_na_platy_real),
         width_bar=0.8)

graf_A4 <- vyvoj_bar %>%
  plot_ly(
    x = ~as.character(rok), y = ~ prostredky_na_platy_nom / 1e9, type = "bar",
    color = ~kategorie_2014_cz, colors = color_map,
    hovertemplate = ~ paste(
      "<extra></extra>", "Rok:", rok, "<br>", "Kategorie:", kategorie_2014_cz, "<br>",
      "Rozpo\u010Det:", format(prostredky_na_platy_nom, big.mark = " "), "K\u010D",
      "<br>",
      "Celkem za rok: ", format(prostredky_na_platy_nom_agg, big.mark = " "), "K\u010D"
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font)),
    hoverinfo = "text"
  ) %>%
  layout(barmode='stack',bargap=0.5,
         title = list(font=title_font,
                      text = "<b>Graf A4. Výdaje na platy státních úředníků (2003-2023)</b>",
                      y = 0.98),
         # annotations = c(list(text =str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie. </i>",wrap_len),
         #                      font = pozn_font_small),
         #                 annot_below),
         xaxis = c(num_ticks,frame_x,list(title = list(text="<b>Rok</b>",standoff=10),
                                          titlefont = axis_font,
                                          xaxis = list(categoryarray = seq(2003, 2023),
                                                       categoryorder = "array"))),
         yaxis = c(num_ticks,frame_y,list(title = "<b>Výdaje na platy (v mld. Kč)</b>",titlefont = axis_font,
                                          dtick = 5)),
         legend = legend_below, margin = mrg2
  ) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)

graf_A5 <- vyvoj_bar %>%
  plot_ly(
    x = ~rok, y = ~prostredky_na_platy_real / 1e9,
    color = ~kategorie_2014_cz, colors = color_map,
    hovertemplate = ~ paste(
      "<extra></extra>", "Rok:", rok, "<br>", "Kategorie:",
      kategorie_2014_cz, "<br>",
      "Rozpo\u010Det:", format(prostredky_na_platy_real, big.mark = " "),
      "K\u010D",
      "<br>",
      "Celkem za rok: ", format(prostredky_na_platy_real_agg, big.mark = " "),
      "K\u010D"
    ),
    hoverlabel = list(font = list(size=hover_size)),
    hoverinfo = "text"
  ) %>%
  add_bars() %>%
  layout(barmode='stack',bargap=0.5,
         title = list(font=title_font,
                      text = "<b>Graf A5. Reálné výdaje na platy státních úředníků (2003-2023)</b>",
                      y = 0.98),
         # annotations = c(list(text = str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie. </i>",wrap_len),
         #                      font = pozn_font_small),
         #                 annot_below),
         xaxis = c(num_ticks,frame_x,list(title = list(text="<b>Rok</b>",standoff=10),titlefont = axis_font),
                                          # xaxis = list(categoryarray = seq(2003,2023), categoryorder = "array"),
                   list(tickvals = seq(2003,2023,5))),
         yaxis = c(num_ticks,frame_y,list(title = "<b>Reálné výdaje na platy (v mld. Kč, ceny roku 2023)</b>",titlefont = axis_font)),
         legend = legend_below, margin = mrg2
  ) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)


## ----cost_cumsum_2014--------------------------------------------------------------------------------------------
aux2 <- dta %>%
  filter(kategorie_2014_cz %in% c("Státní úředníci", "Ministerstva",
                                  "Ostatní ústřední", "Neústřední st. správa"),
         typ_rozpoctu == "SKUT") %>%
  filter(!kap_num %in% c(314, 306)) %>%
  # not included in previous
  group_by(kategorie_2014_cz, rok) %>%
  summarise(
    prostredky_na_platy = sum(prostredky_na_platy),
    base_2003 = mean(base_2003),
    pocet_kapitol = length(kap_num),
    platy_weighted = prostredky_na_platy / pocet_kapitol
  ) %>%
  mutate(plat_base = prostredky_na_platy[1]) %>%
  mutate(plat_base_weighted = platy_weighted[1]) %>%
  mutate(output = (prostredky_na_platy / (base_2003 * plat_base) - 1)) %>%
  mutate(output_weighted = (prostredky_na_platy / base_2003 - plat_base) / plat_base) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa", "Státní úředníci"))

graf_A6 <- aux2 %>%
  plot_ly(
    x = ~rok, y = ~ output * 100, type = "scatter", color = ~kategorie_2014_cz,
    colors = color_map,
    mode = "line", line = list(width = 7),
    marker = list(size=5,symbol="circle-dot",line = list(color="Black",width=3)),
    text = ~ paste(
      " Rok:", rok, "<br>", "Kategorie:", kategorie_2014_cz, "<br>",
      "Hodnota:", round(output, 4) * 100, "%"
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font)),
    hoverinfo = "text",
    legendgroup = ~kategorie_2014_cz
  ) %>%
  layout(
    title = list(font=title_font,
                 text = str_wrap("<b>Graf A6. Změna reálných výdajů na platy státních úředníků (k základně roku 2003)</b>",60)),
    # annotations = c(annot_below,list(text = str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie.</i>",200),
    #                                  font = pozn_font_small)),
    xaxis = c(num_ticks,frame_y,list(title = list(text="<b>Rok</b>",standoff=10),
                                     titlefont = axis_font),
              list(tickvals = seq(2003,2023,5))),
    yaxis = c(num_ticks,frame_y,list(title = str_wrap("<b>Změna reálných výdajů na platy oproti roku 2003 (v %, ceny roku 2023)</b>",50),
                                     ticksuffix = "%",titlefont = axis_font,
                                     ticktext = lapply(seq(-10,70,10), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                     tickvals = seq(-10,70,10),
                                     tickmode = "array")),
    margin = mrg5,
    legend=legend_below) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)


## ----mean_wage_2023----------------------------------------------------------------------------------------------

graf_5_dt <- dta %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici"),
         typ_rozpoctu == "SKUT") %>%
  #filter(!kap_num %in% c(314,306)) %>% #not included in previous
  group_by(rok, kategorie_2014_cz) %>%
  summarise(
    prumerny_plat_agg = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)) / 12,
    base_2023 = mean(base_2023),
    max_change_kap = kap_name[which.max(wage_in_2023_change)],
    max_change = round(max(wage_in_2023_change, na.rm = T), 4),
    min_change_kap = kap_name[which.min(wage_in_2023_change)],
    min_change = round(min(wage_in_2023_change, na.rm = T), 4)
  ) %>%
  mutate(wage_in_2023 = prumerny_plat_agg * base_2023) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci"))
graf_5_dt <- plyr::rbind.fill(graf_5_dt,data.frame(rok = 2003))
graf_5 <- plot_ly(graf_5_dt,
                  x = ~rok, y = ~ wage_in_2023 / 1000, type = "scatter", color = ~kategorie_2014_cz,
                  colors = color_map,
                  mode = "line", line = list(width = 7),
                  marker = list(size=5,symbol="circle-dot",line = list(color="Black",width=3)),
                  text = ~ paste(
                    " Rok:", rok, "<br>", "Kategorie:", kategorie_2014_cz, "<br>", "Hodnota:",
                    format(round(wage_in_2023, 0), big.mark = " "), "K\u010D", "<br>",
                    "Nejv\u011Bt\u0161\u00ED nárůst:", "<br>", max_change_kap, ": ",
                    max_change * 100, " %", "<br>",
                    ifelse(min_change>0,"Nejmen\u0161\u00ED nárůst:","Nejv\u011Bt\u0161\u00ED pokles:"), "<br>",
                    min_change_kap, ": ",
                    min_change * 100, " %"),
                  hoverlabel = list(font=list(size=hover_size,family=uni_font)),
                  hoverinfo = "text",
                  legendgroup = ~kategorie_2014_cz) %>%
  layout(
    legend = legend_below,
    # annotations = c(list(text = str_wrap("<i>Pozn.: Reálné hrubé měsíční platy jsou uvedeny v cenách roku 2023.</i>",wrap_len),
    #                      font = pozn_font_small),annot_below),
    title =list(text = "<b>Graf 5. Průměrné platy státních úředníků (2004–2023) v cenách roku 2023</b>",
                y =0.98,
                font=title_font),
    xaxis = c(num_ticks,frame_y,list(title = "<b>Rok</b>",titlefont = axis_font),
              list(tickvals = seq(2003,2023,5))),
    yaxis = c(num_ticks,frame_y,list(title = "<b>Reálné průměrné mzdy (tis. Kč, ceny roku 2023)</b>",titlefont = axis_font)),
    margin = mrg4
  ) %>% config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)
graf_5


# G5 static ---------------------------------------------------------------

graf_5_static <- ggplot(graf_5_dt, aes(rok, wage_in_2023/1e3, colour = kategorie_2014_cz)) +
  geom_line(size = 1.9) +
  geom_point(colour = "black", size = 1.9) +
  theme_minimal(base_family = uni_font, base_size = 14) +
  theme_urednici +
  scale_color_manual(values = color_map, name = NULL, limits = force) +
  scale_x_continuous(breaks = seq(2003, 2023, 2)) +
  labs(title = "Graf 5. Průměrné platy státních úředníků (2004–2023) v cenách roku 2023",
       x = "Rok",
       y = "Reálné průměrné hrubé měsíční mzdy (tis. Kč) v cenách roku 2023",
       caption = "Pozn.: reálné hrubé měsíční platy, uvedené v cenách roku 2023")
graf_5_static

# ggsave("graphs-static/graf-5.png", plot = graf_5_static, width = 8, height = 5, scale = 1.5, bg = "white")

## ----mean_wage_pct_change_2023-----------------------------------------------------------------------------------
graf_A7_dt <- dta %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni"),
         typ_rozpoctu == "SKUT") %>%
  filter(!kap_num %in% c(314, 306)) %>%
  # not included in previous
  group_by(kategorie_2014_cz, rok) %>%
  summarise(
    prumerny_plat_agg = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)) / 12,
    base_2023 = mean(base_2023),
    max_change_kap = kap_name[which.max(mzda_prumer_skut_ke_skut)],
    max_change = round(max(mzda_prumer_skut_ke_skut, na.rm = T), 4),
    min_change_kap = kap_name[which.min(mzda_prumer_skut_ke_skut)],
    min_change = round(min(mzda_prumer_skut_ke_skut, na.rm = T), 4)
  ) %>%
  mutate(wage_in_2023 = prumerny_plat_agg * base_2023) %>%
  group_by(kategorie_2014_cz) %>%
  arrange(rok) %>%
  mutate(wage_base = wage_in_2023[1]) %>%
  mutate(cum_pct_wage_change = (wage_in_2023 - wage_base) / wage_base) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci"))
graf_A7_dt <- plyr::rbind.fill(graf_A7_dt,data.frame(rok = 2003))

graf_A7 <- plot_ly(graf_A7_dt,
                   x = ~rok, y = ~ cum_pct_wage_change * 100, type = "scatter",
                   color = ~kategorie_2014_cz, colors = color_map, mode = "line",
                   line = list(width = 7),
                   marker = list(size=5,symbol = "circle",line = list(width = 3,color="black")),
                   text = ~ paste(
                     " Rok:", rok, "<br>", "Kategorie:", kategorie_2014_cz, "<br>", "Hodnota:",
                     round(cum_pct_wage_change * 100,2), "%","<br>",
                     "Nejv\u011Bt\u0161\u00ED nárůst:", "<br>", max_change_kap, ": ",
                     max_change * 100, " %", "<br>",
                     ifelse(min_change>0,"Nejmen\u0161\u00ED nárůst:","Nejv\u011Bt\u0161\u00ED pokles:"), "<br>",
                     min_change_kap, ": ", min_change * 100, " %", "<br>"
                   ),
                   hoverlabel = list(font=list(size=hover_size,family=uni_font)),
                   hoverinfo = "text",
                   legendgroup = ~kategorie_2014_cz
) %>%
  layout(
    title = list(font=title_font,
                 text="<b>Graf A7. Změna reálných průměrných platů státních úředníků (k základně roku 2003)</b>",
                 y = 0.96),
    # annotations = c(list(text = str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie. </i>",wrap_len),
    #                      font = pozn_font_small),annot_below),
    xaxis = c(num_ticks,frame_y,list(title = list(text="<b>Rok</b>",standoff=10),
                                     titlefont = axis_font),
              list(tickvals = seq(2003,2023,5))),
    yaxis = c(num_ticks,frame_y,list(title = "<b>Změna reálného průměrného platu oproti roku 2003 \n (v %, ceny roku 2023)</b>",
                                     # tickprefix = "+",
                                     # showtickprefix = "last",
                                     ticktext = lapply(seq(0,40,10), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                     tickvals = seq(0,40,10),
                                     tickmode = "array",
                                     ticksuffix = "%",
                                     showticksuffix = "all",
                                     titlefont = axis_font)),
    legend = legend_below, margin = mrg6) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)




## ----wage_to_general---------------------------------------------------------------------------------------------

annot_6<-list(                       align='left',
                                     xref='paper',
                                     yref="paper",
                                     x=0,
                                     y=-0.3,
                                     font = list(size = pozn_long_size),
                                     bordercolor = 'rgba(0,0,0,0)',
                                     borderwidth=1,
                                     showarrow = FALSE)

graf_6_dt <- dta %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni"),
         typ_rozpoctu == "SKUT") %>%
  #filter(!kap_num %in% c(314, 306)) %>%
  group_by(kategorie_2014, kategorie_2014_cz, rok) %>%
  summarise(
    prumerny_plat_agg = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)) / 12,
    max_change_kap = kap_name[which.max(mzda_k_nh)],
    phasal_all = mean(phasal_all),
    czsal_all = mean(czsal_all),
    max_change = round(max(mzda_k_nh, na.rm = T), 4),
    min_change_kap = kap_name[which.min(mzda_k_nh)],
    min_change = round(min(mzda_k_nh, na.rm = T), 4)
  ) %>%
  mutate(wage_to_general = (ifelse(kategorie_2014 %in% c("Ministerstva", "Ostatni ustredni"),
                                   prumerny_plat_agg / phasal_all,
                                   prumerny_plat_agg / czsal_all))) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci"))
graf_6_dt <- plyr::rbind.fill(graf_6_dt,data.frame(rok = 2003))

graf_6 <- plot_ly(graf_6_dt,
                  line = list(width = 7),
                  x = ~rok, y = ~ wage_to_general * 100, type = "scatter",
                  color = ~kategorie_2014_cz,
                  colors = color_map,mode = "line",
                  marker = list(size=5,symbol="circle-dot",
                                line = list(color="Black",width=3)),
                  text = ~ paste(
                    " Rok:", rok, "<br>", "Kategorie:", kategorie_2014_cz, "<br>",
                    "Nejv\u011Bt\u0161\u00ED nárůst:", "<br>", max_change_kap, ": ",
                    max_change * 100, " %", "<br>",
                    ifelse(min_change > 0, "Nejmen\u0161\u00ED nárůst:","Nejv\u011Bt\u0161\u00ED pokles:"),
                    "<br>", min_change_kap, ": ", min_change * 100, " %", "<br>"
                  ),
                  hoverlabel = list(font=list(size=hover_size,family=uni_font)),
                  hoverinfo = "text",
                  legendgroup = ~kategorie_2014_cz
) %>%layout(
  shapes = list(hline(100)),
  title = list(font=title_font,
               text = "<b>Graf 6. Průměrný plat státních úředníků \n ve vztahu k průměrné mzdě v národním hospodářství (2004-2023)</b>"),
  # annotations = c(annot_6,list(text = str_wrap("<i>Pozn.: Pro ministerstva a ostatní ústřední orgány použité hodnoty průměrné mzdy v Praze. V ostatních případech je jako reference použitý průměrný plat v národním hospodářství. Hodnota 100% znamená, že průměrný plat v kategorii je stejný jako průměrný plat v národním hospodářství.</i>",wrap_len),
  #                              font = pozn_font_small)),
  xaxis = c(num_ticks,frame_y,list(title = "<b>Rok</b>",titlefont = axis_font),
            list(tickvals = seq(2003,2023,5))),
  yaxis = c(num_ticks,frame_y,list(title = "<b>Poměr platů státních úředníku a prům. mzdy (v %)</b>",
                                   # ticksuffix = "%",
                                   titlefont = axis_font)),
  margin = mrg5,
  legend=legend_below) %>%config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)

graf_6

# G6 static ---------------------------------------------------------------

graf_6_static <- ggplot(graf_6_dt, aes(rok, wage_to_general, colour = kategorie_2014_cz)) +
  geom_line(size = 1.9) +
  geom_point(colour = "black", size = 1.9) +
  theme_minimal(base_family = uni_font, base_size = 14) +
  theme_urednici +
  scale_color_manual(values = color_map, name = NULL, limits = force) +
  scale_x_continuous(breaks = seq(2003, 2023, 2)) +
  ptrr::scale_y_percent_cz() +
  labs(title = "Graf 6. Průměrný plat státních úředníků \nve vztahu k průměrné mzdě v národním hospodářství (2004-2023)",
       y = "Poměr platů státních úředníku a prům. mzdy (v %)",
       x = "Rok",
       caption = str_wrap("Pozn.: pro ministerstva a ostatní ústřední orgány použité hodnoty průměrné mzdy v Praze. V ostatních případech je jako reference použitý průměrný plat v národním hospodářství. Hodnota 100% znamená, že průměrný plat v kategorii je stejný jako průměrný plat v národním hospodářství.",
                          150))

graf_6_static

# ggsave("graphs-static/graf-6.png", plot = graf_6_static, width = 8, height = 5, scale = 1.5, bg = "white")


## ----2023_effect-------------------------------------------------------------------------------------------------

line <- list(
  type = "line",
  line = list(color = "pink"),
  xref = "paper",
  yref = "paper",
  "y0" = 0,
  "y1" = 1,
  "x0" = 0,
  "x1" = 1
)

infl <- dta %>% filter(rok == 2004) %>% select(base_2023) %>% first() %>% pull()

graf_A8_dt <- dta %>%
  filter(!is.na(kategorie_2014_cz), typ_rozpoctu == "SKUT",
         !kategorie_2014 %in% c("Statni sprava", "Statni urednici")) %>%
  filter(rok %in% c(2004, 2023)) %>%
  filter(!kap_num %in% c(314, 306)) %>%
  group_by(kategorie_2014_cz, rok) %>%
  summarise(
    base_2023 = base_2023[1],
    zam_skutecnost = sum(pocet_zamestnancu),
    plat_skutecnost = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)) / 12
  ) %>%
  as.data.table() %>%
  dcast(kategorie_2014_cz ~ rok, value.var = c("zam_skutecnost", "plat_skutecnost")) %>%
  mutate(
    zam_change = (zam_skutecnost_2023/zam_skutecnost_2004 - 1),
    plat_change = (plat_skutecnost_2023/(plat_skutecnost_2004*infl) - 1)
  )

graf_A8 <- graf_A8_dt %>%
  plot_ly(
    x = ~ plat_change * 100, y = ~ zam_change * 100, color = ~kategorie_2014_cz,
    colors = color_map,
    text = ~ paste(
      " Zm\u011Bna platu:", round(plat_change, 4) * 100, "%", "<br>",
      "Zm\u011Bna po\u010Dtu zam\u011Bstnanc\u016F:", round(zam_change, 4) * 100, "%"
    ),
    hoverlabel = list(font=list(size=hover_size)),
    hoverinfo = "text",
    legendgroup = ~kategorie_2014_cz
  ) %>%
  layout(
    title = list(font=title_font,
                 text = "<b>Graf A8. Celkové změny platů a počtu zaměstnanců v období 2004-2023</b>", y = 0.98),
    # annotations = c(annot_below,list(text = str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie. </i>",wrap_len),
    #                                  font = pozn_font_small)),
    showlegend = FALSE,
    xaxis = c(num_ticks,frame_x,list(title = "Zm\u011Bna pr\u016Fm\u011Brn\u00E9ho platu (v %)",
                                          titlefont = axis_font,
                                          ticksuffix = "%",range = c(-5,65))),
    yaxis = c(num_ticks,frame_y,list(title = "Zm\u011Bna po\u010Dtu zam\u011Bstnanc\u016F (v %)",
                                     titlefont = axis_font,
                                     range = c(-20,20),
                                     # tickprefix = "+",
                                     # showtickprefix = "last",
                                     ticksuffix = "%",
                                     ticktext = lapply(seq(-20,20,5), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                     tickvals = seq(-20,20,10),
                                     tickmode = "array",
                                     showticksuffix = "all")),
    legend = legend_below, margin = mrg2
  ) %>%
  add_markers(marker=list(size=mrk_maj_size)) %>%
  add_text(text = ~ str_wrap(kategorie_2014_cz, 10), textposition = "top center",
           textfont = list(size=num_tick_size)) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)
graf_A8



## ----prac_mista_skut_rozp_kap------------------------------------------------------------------------------------
kaps_to_exclude <- dta %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici")) %>%
  filter(typ_rozpoctu != "SCHV") %>%
  select(rok, kategorie_2014_cz, typ_rozpoctu, kap_num, pocet_zamestnancu,
         full_kap_name) %>%
  spread(key = typ_rozpoctu, value = pocet_zamestnancu) %>%
  group_by(rok, kap_num) %>%
  summarise(
    SKUT = sum(SKUT),
    UPRAV = sum(UPRAV)
  ) %>%
  ungroup() %>%
  group_by(kap_num) %>%
  summarise(max_skut = max(SKUT)) %>%
  filter(max_skut < 100) %>%
  pull(kap_num)


graf_A9 <- dta %>%filter(!is.na(kap_name)) %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici")) %>%
  filter(typ_rozpoctu != "SCHV") %>%
  filter(!kap_num %in% c(kaps_to_exclude, 312)) %>%
  select(rok, kategorie_2014_cz, typ_rozpoctu, kap_name, pocet_zamestnancu,
         cz_kap_name) %>%
  spread(key = typ_rozpoctu, value = pocet_zamestnancu) %>%
  group_by(rok, kap_name, cz_kap_name) %>%
  summarise(
    SKUT = sum(SKUT),
    UPRAV = sum(UPRAV)
  ) %>%
  mutate(diff = (SKUT - UPRAV) / UPRAV) %>%
  group_by(kap_name) %>%
  arrange(rok) %>%
  group_map(~ plot_ly(
    data = ., x = ~rok, y = ~ diff * 100, type = "bar", color = ~kap_name,
    colors = color_map_kap,
    hovertemplate = ~ paste(
      "<extra></extra>",
      "Rok:", rok, "<br>",
      "Kapitola:", cz_kap_name, "<br>",
      "Schválený po\u010Det zam\u011Bstnanc\u016F", ": ",
      format(UPRAV, big.mark = " "), "<br>",
      "Skute\u010Dn\u00FD po\u010Det zam\u011Bstnanc\u016F", ": ",
      format(SKUT, big.mark = " ")
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
    add_annotations(
      text =~paste("<b>",unique(kap_name),"</b>"),
      x = .5,
      y = 1.05,
      yref = "paper",
      xref = "paper",
      font = list(family = "verdana", size = kat_tick_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE
    ) %>%
    layout(margin=c(t=5),
           xaxis = c(list(title= "<b>Rok</b>",titlefont = axis_font,tickangle = -90),
                     list(tickvals = seq(2003,2023,5))),
           yaxis = c(list(title = "",titlefont = axis_font,range = c(-30, 10),
                          ticktext = lapply(seq(-30, 0, 10), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                          tickvals = seq(-30, 0, 10),
                          tickmode = "array"))),
  keep = TRUE,margin=mrg2) %>%
  subplot(nrows = 5,shareX = T,shareY = T,titleY = FALSE,titleX=T) %>%
  layout(title = list(font=title_font,
                      text = "<b>Graf A9. Rozdíl mezi schváleným a skutečným počtem zaměstnanců (v %)</b>",
                      y = 0.98),
         showlegend = FALSE,
         annotations = list(x = 0 , y = 0.5, text = "<b>Záporné = méně skutečných než schválených</b>",
                            font = list(size = axis_size),
                            xshift = -65, textangle = 270, showarrow = F,
                            xref='paper', yref='paper'),
         margin = mrg7) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)


## ----platy_skut_rozp_kap-----------------------------------------------------------------------------------------
kap_order <- ifelse(substr(kaps,1,1) == "M",1,2)
names(kap_order) <- kaps
kap_order <- sort(kap_order)

graf_A10 <- dta %>%filter(!is.na(kap_name)) %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici")) %>%
  filter(typ_rozpoctu != "SCHV") %>%
  filter(!kap_num %in% c(kaps_to_exclude, 312)) %>%
  select(rok, kategorie_2014_cz, typ_rozpoctu, kap_name, prostredky_na_platy,
         pocet_zamestnancu, cz_kap_name) %>%
  group_by(rok, cz_kap_name,kap_name, typ_rozpoctu) %>%
  summarise(prumerny_plat = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)/12)) %>%
  spread(key = typ_rozpoctu, value = prumerny_plat) %>%
  group_by(rok, kap_name, cz_kap_name) %>%
  mutate(diff = (SKUT - UPRAV) / UPRAV) %>%
  group_by(kap_name) %>%
  arrange(rok) %>%
  group_map(~ plot_ly(
    data = ., x = ~rok, y = ~ diff * 100, type = "bar", color = ~kap_name,
    colors = color_map_kap,
    hovertemplate = ~ paste(
      "<extra></extra>",
      "Rok:", rok, "<br>",
      "Kapitola:", cz_kap_name, "<br>",
      "Schválený pr\u016Fm\u011Brn\u00FD plat", ": ",
      format(round(UPRAV, 0), big.mark = " "), "K\u010D", "<br>",
      "Skute\u010Dn\u00FD pr\u016Fm\u011Brn\u00FD plat", ": ",
      format(round(SKUT, 0), big.mark = " "), "K\u010D"
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
    add_annotations(
      text = ~paste("<b>",unique(kap_name),"</b>"),
      x = 0.5,
      y = 1.15,
      yref = "paper",
      xref = "paper",
      font = list(family = "verdana", size = kat_tick_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE
    ) %>%
    layout(margin = mrg7,
           xaxis = list(title = "<b>Rok</b>",titlefont = axis_font,
                        tickangle = -90,tickvals = seq(2003,2023,5)),
           # yaxis = c(list(title = "",titlefont = axis_font,range = c(-30, 10),
           #                ticktext = lapply(seq(-30, 10, 10), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
           #                tickvals = seq(-30, 10, 10),
           #                tickmode = "array")),
           legend = list(x = 100, y = 0.5)),
  keep = TRUE) %>%
  subplot(nrows = 5, shareX = TRUE, shareY = T, margin = c(0.01,0.01,0.05,0),
          titleY = F) %>%
  layout(title = list(font=title_font,
                      text = "<b>Graf A10. Rozdíl v průměrných platech mezi schváleným rozpočtem a skutečností (v %)</b>",
                      y = 0.98), showlegend = FALSE,
         annotations = list(x = 0 , y = 0.5, text = "<b>Kladné = skutečný průměrný plat vyšší než schválený</b>",
                            font = list(size = axis_size),
                            xshift = -65, textangle = 270, showarrow = F,
                            xref='paper', yref='paper')) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)
graf_A10


## ----prac_mista_skut_rozp----------------------------------------------------------------------------------------
graf_A11 <- dta %>% filter(!is.na(kategorie_2014_cz)) %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici")) %>%
  filter(typ_rozpoctu != "SCHV") %>%
  select(rok, kategorie_2014_cz, typ_rozpoctu, kap_num, pocet_zamestnancu) %>%
  spread(key = typ_rozpoctu, value = pocet_zamestnancu) %>%
  group_by(rok, kategorie_2014_cz) %>%
  summarise(
    SKUT = sum(SKUT),
    UPRAV = sum(UPRAV)
  ) %>%
  mutate(diff = (SKUT - UPRAV) / UPRAV) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci")) %>%
  group_by(kategorie_2014_cz) %>%
  arrange(rok) %>%
  group_map(~ plot_ly(
    data = ., x = ~rok, y = ~ diff * 100, type = "bar",
    color = ~kategorie_2014_cz, colors = color_map,
    hovertemplate = ~ paste(
      "<extra></extra>",
      "Rok:", rok, "<br>",
      "Schválený po\u010Det zam\u011Bstnanc\u016F", ": ",
      format(UPRAV, big.mark = " "), "<br>",
      "Skute\u010Dn\u00FD po\u010Det zam\u011Bstnanc\u016F", ": ",
      format(SKUT, big.mark = " ")
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
    add_annotations(
      text = ~paste("<b>",unique(kategorie_2014_cz),"</b>"),
      x = 0.5,
      y = 1.25,
      yref = "paper",
      xref = "paper",
      font = list(family = "verdana", size = axis_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE
    ) %>%
    layout(bargap=0.5,margin = list(t = 100,b=0,l=70),
           xaxis = c(num_ticks,frame_y,title="<b>Rok</b>",titlefont = axis_font,
                     list(tickvals = seq(2003,2023,5))),
           yaxis = c(num_ticks,frame_y,list(title = "<b></b>",titlefont = axis_font,
                                            range = c(-18, 1))),showlegend = F),keep = TRUE) %>%
  subplot(nrows = 2, shareY = F, margin = c(0.07,0.07,0.15,0.15),titleY =T) %>%
  layout(title = list(font=title_font,
                      text = "<b>Graf A11. Rozdíl mezi schváleným a skutečným počtem zaměstnanců (%)</b>",
                      y = 0.98),
         annotations = list(x = 0 , y = 0.5, text = "<b>Záporné = skutečný počet nižší než schválený</b>",
                            font = list(size = axis_size),
                            xshift = -70, textangle = 270, showarrow = F,
                            xref='paper', yref='paper')) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)
## ----platy_skut_rozp---------------------------------------------------------------------------------------------
graf_A12 <- dta %>% filter(!is.na(kategorie_2014_cz)) %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici")) %>%
  filter(typ_rozpoctu != "SCHV") %>%
  select(rok, kategorie_2014_cz, typ_rozpoctu, kap_num, prostredky_na_platy,
         pocet_zamestnancu) %>%
  group_by(rok, kategorie_2014_cz, typ_rozpoctu) %>%
  summarise(prumerny_plat_agg = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)) / 12) %>%
  spread(key = typ_rozpoctu, value = prumerny_plat_agg) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci")) %>%
  group_by(rok, kategorie_2014_cz) %>%
  summarise(
    SKUT = sum(SKUT),
    UPRAV = sum(UPRAV)
  ) %>%
  mutate(diff = (SKUT - UPRAV) / UPRAV) %>%
  group_by(kategorie_2014_cz) %>%
  arrange(rok) %>%
  group_map(~ plot_ly(
    data = ., x = ~rok, y = ~ diff * 100, type = "bar",
    color = ~kategorie_2014_cz,colors= color_map,
    hovertemplate = ~ paste(
      "<extra></extra>",
      "Rok:", rok, "<br>",
      "Schválený pr\u016Fm\u011Brn\u00FD plat", ": ",
      format(round(UPRAV, 0), big.mark = " "), "K\u010D", "<br>",
      "Skute\u010Dn\u00FD pr\u016Fm\u011Brn\u00FD plat", ": ",
      format(round(SKUT, 0), big.mark = " "), "K\u010D"
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
    add_annotations(
      text = ~paste("<b>",unique(kategorie_2014_cz),"</b>"),
      x = 0.5,
      y = 1.25,
      yref = "paper",
      xref = "paper",
      font = list(family = "verdana", size = axis_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE
    ) %>%
    layout(bargap=0.5,margin = list(t = 120,b=0,l=70),
           xaxis = c(num_ticks,frame_y,list(title="<b>Rok</b>",titlefont = axis_font),
                     list(tickvals = seq(2003,2023,5))),
           yaxis = c(frame_y,list(title = "",
                                  titlefont = axis_font,
                                  # tickprefix = "+",
                                  ticksuffix = "%",
                                  # showticksuffix = "all",
                                  ticktext = lapply(seq(0,20,5), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                  tickvals = seq(0,20,5),
                                  tickmode = "array"
                                  )),
           showlegend = F),
  keep = TRUE) %>%
  subplot(nrows = 2, shareY = F, margin = c(0.07,0.07,0.15,0.15),titleY =T) %>%
  layout(title = list(font=title_font,
                      text = "<b>Graf A12. Rozdíl v průměrných platech mezi schváleným rozpočtem a skutečností</b>",
                      y = 0.98), annotations = list(x = 0 , y = 0.5, text = "<b>Kladné = skutečný průměrný plat vyšší než schválený</b>",
                                                    font = list(size = axis_size),
                                                    xshift = -70, textangle = 270,
                                                    showarrow = F,
                                                    xref='paper', yref='paper')) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)


## ----Scatter plat narust-----------------------------------------------------------------------------------------

infl <- dta %>% filter(rok == 2003) %>% select(base_2023) %>% first() %>% pull()
graf_A13 <- dta %>%filter(!is.na(kategorie_2014_cz))%>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici")) %>%
  filter(typ_rozpoctu == "SKUT") %>%
  filter(rok %in% c(2003,2023)) %>%
  select(rok, kategorie_2014_cz, cz_kap_name,kap_name, prumerny_plat, pocet_zamestnancu) %>%
  pivot_wider(names_from = c("rok"),values_from = c("prumerny_plat","pocet_zamestnancu")) %>%
  mutate( #wont be used
    zam_change = (pocet_zamestnancu_2023/pocet_zamestnancu_2003 - 1),
    plat_change = (prumerny_plat_2023/(prumerny_plat_2003*infl) - 1)
  ) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci")) %>%
  group_by(kategorie_2014_cz) %>%
  group_map(~ plot_ly(
    data = ., x = ~prumerny_plat_2003/1000, y = ~ plat_change*100,
    type = "scatter" , mode = "markers",marker=list(size=mrk_min_size),
    color = ~kategorie_2014_cz, colors = color_map,
    hovertemplate = ~ paste(
      "<extra></extra>",
      "Kapitola: ", cz_kap_name, "<br>",
      "Pr\u016Fm\u011Brn\u00FD plat 2003 ", ": ",
      format(round(prumerny_plat_2003, 0), big.mark = " "), "K\u010D", "<br>",
      "Pr\u016Fm\u011Brn\u00FD plat 2023", ": ",
      format(round(prumerny_plat_2023, 0), big.mark = " "), "K\u010D", "<br>",
      "Zm\u011Bna", ": ", format(round(plat_change*100, 1),big.mark = " "), "%"
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
    add_annotations(
      text = ~paste("<b>",unique(kategorie_2014_cz),"</b>"),
      x = 0.5,
      y = 1.2,
      yref = "paper",
      xref = "paper",
      font = list(family = "verdana", size = axis_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = F
    ) %>%
    layout(
      yaxis = c(num_ticks,frame_y,list(title = "Nárůst průměrného platu od roku 2003",titlefont = axis_font,
                                       range = c(-2, 37),
                                       ticktext = lapply(seq(0,30,10), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                       tickvals = seq(0,30,10),
                                       tickmode = "array",
                                       ticksuffix = "%")),
      xaxis = c(num_ticks,frame_y,list(title = "Pr\u016Fm\u011Brn\u00FD plat v roce 2003 (tisíce Kč)", titlefont = axis_font,range = c(15, 35))),
      legend = list(x = 100, y = 0.5), showlegend = F) %>%
    add_text(text = ~ str_wrap(kap_name, 10), textposition = "bottom left",
             textfont = list(size = kat_tick_size)),keep = TRUE) %>%
  subplot(nrows = 2, titleY = F, titleX = F,margin=c(0.05,0.05,0.1,0.1)) %>%
  layout(title = list(font=title_font,
                      text = "<b>Graf A13. Nárůst průměrných platů od roku 2003 (v %)</b>",
                      xaxis = list(title = "",titlefont = axis_font), y = 0.98), margin=c(t=50,l=90,b=80),
         annotations = list(list(x = 0 , y = 0.5, text = "<b>Nárůst průměrného platu od roku 2003</b>",
                                 font = list(size = axis_size),
                                 xshift = -80, textangle = 270, showarrow = F,
                                 xref='paper', yref='paper'),
                            list(y = 0 , x = 0.5, text = "<b>Průměrný plat v roce 2003 (tis. Kč)</b>",
                                 font = list(size = axis_size),
                                 yshift = -70,
                                 textangle = 0, showarrow = F,
                                 xref='paper', yref='paper'
                            ))) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)


## ----graf_A14----------------------------------------------------------------------------------------------------
graf_A14 <- dta %>%
  filter(typ_rozpoctu == "SKUT",
         kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni")) %>%
  #filter(!kap_num %in% c(314, 306)) %>%
  group_by(kategorie_2014_cz, rok) %>%
  summarise(pocet_zamestnancu = sum(pocet_zamestnancu)) %>%
  group_by(rok) %>%
  mutate(pocet_zamestnancu_agg = sum(pocet_zamestnancu)) %>%
  plot_ly(
    x = ~rok, y = ~ pocet_zamestnancu / 1000,
    color = ~kategorie_2014_cz, colors = color_map,
    hovertemplate = ~ paste(
      "<extra></extra>", "Rok:", rok, "<br>", "Kategorie:", kategorie_2014_cz, "<br>",
      "Zam\u011Bstnanc\u016F:", format(pocet_zamestnancu, big.mark = " "), "<br>",
      "Celkem: ", format(pocet_zamestnancu_agg, big.mark = " ")
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font)),
    hoverinfo = "text"
  ) %>% add_bars()%>%
  layout(barmode='stack',bargap=0.5,
         title = list(font=title_font,
                      text = "<b>Graf A14. Počet státních úředníků (2003-2023)</b>",
                      y = 0.98),
         # annotations = c(list(text =str_wrap("<i>Pozn.: Zahrnuty kapitoly ministerstva vnitra a zahraničních věcí</i>",wrap_len),
         #                      font = pozn_font_small),
         #                 annot_below),
         xaxis = c(num_ticks,frame_x,list(title = list(text="<b>Rok</b>",standoff=10),
                                          titlefont = axis_font),
                                          # xaxis = list(categoryarray = seq(2003,2023),
                                          #              categoryorder = "array"),
                   list(tickvals = seq(2003,2023,2))),
         yaxis = c(num_ticks,frame_y,list(title = "<b>Počet státních úředníků (v tisících)</b>",
                                          titlefont = axis_font)),
         legend = legend_below, margin = mrg2
  ) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)


## ----graf_A16----------------------------------------------------------------------------------------------------
pubsec <- read_csv("./data-input/ver-sektor-csu-rocenka.csv")
graf_A16_dt <- plyr::rbind.fill(pubsec,data.frame(rok = 1993))

graf_A16 <- plot_ly(graf_A16_dt, type = "scatter", mode = "lines+markers",
                    line = list(width = 7),
                    marker = list(size = 5,symbol = "circle",
                                  line = list(width = 2,color="black")),
                    hovertemplate = ~ paste(
                      "<extra></extra>",
                      "Rok: ", rok, "<br>",
                      "Počet: ", pocet),
                    hoverlabel = list(font=title_font)) %>%
  add_trace(x = ~rok, y = ~pocet, color = I("grey"),
            name = "Počet zaměstnanců veřejného sektoru") %>%
  layout(
    yaxis = c(num_ticks,frame_y,list(title = "<b>Počet zaměstnanců (v tisících přepočtených osob)</b>",titlefont = axis_font, range = c(0, 1800))),
    xaxis = c(num_ticks,frame_y,list(title = list(text="<b>Rok</b>",standoff=10),titlefont = axis_font)
              # list(tickvals = seq(1993,2023,3))
              ),showlegend=FALSE,
    # annotations = c(list(text ='<i>Pozn.: Zdroj: Statistické ročenky České republiky za jednotlivé roky, zde například údaje za rok 2020:</i><br><a href="https://www.czso.cz/csu/czso/10-trh-prace-o73cun42om" target="_blank"><i>https://www.czso.cz/csu/czso/10-trh-prace-o73cun42om</i></a>',
    #                      font = pozn_font_small),annot_below),
    title = list(font=list(color = cap_col,size=cap_size,family=uni_font),
                 text = "<b>Graf A16. Počet zaměstnanců veřejného sektoru</b>",
                 y = 0.98), margin = mrg3) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)
graf_A16


## ----graphs, eval = FALSE, include = FALSE-----------------------------------------------------------------------
graf_list<-list(
  graf_1,
  graf_2,
  graf_3,
  graf_4,
  graf_5,
  graf_6,
  graf_A1,
  graf_A2,
  graf_A3,
  graf_A4,
  graf_A5,
  graf_A6,
  graf_A7,
  graf_A8,
  graf_A9,
  graf_A10,
  graf_A11,
  graf_A12,
  graf_A13,
  graf_A14,
  graf_A16)
names(graf_list)<-c(
  "graf_1",
  "graf_2",
  "graf_3",
  "graf_4",
  "graf_5",
  "graf_6",
  "graf_A1",
  "graf_A2",
  "graf_A3",
  "graf_A4",
  "graf_A5",
  "graf_A6",
  "graf_A7",
  "graf_A8",
  "graf_A9",
  "graf_A10",
  "graf_A11",
  "graf_A12",
  "graf_A13",
  "graf_A14",
  "graf_A16")

saveRDS(graf_list,"data-interim/graf_list.rds")
saveRDS(tree_data,"data-interim/tree_data.rds")

for (i in 1:length(graf_list)){
  htmlwidgets::saveWidget(as_widget(graf_list[[i]]), paste0("graphs/",names(graf_list)[i],".html"), libdir = "js", selfcontained = FALSE)
}

