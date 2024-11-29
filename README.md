# Státní úředníci: studie / aplikace IDEA při CERGE-EI

Datová analýza: Státní zaměstnanci a úředníci

## Aktuální výsledky

[Hlavní výsledky](https://idea-cergeei.github.io/studie-urednici/index.html) aktualizovány v červnu 2022 na základě dat do roku 2021, tj. včetně skutečného plnění rozpočtu 2021. Studie a její grafy na webu IDEA stále odrážejí stav s daty do roku 2020.

Aktualizace s daty za rok 2022 je v přípravě - výsledek bude grafová miniaplikace bez studie.

Technický log k využití aktualizovaných dat je v log.md, potažmo [log.html](https://idea-cergeei.github.io/studie-urednici/log.html)

## Studie

Zveřejněno IDEA v lednu 2022:

Bartušek, Daniel, Bouchal, Petr, & Janský, Petr. (2022). Státní zaměstnanci a úředníci: Kde pracují a za kolik? IDEA. https://idea.cerge-ei.cz/files/IDEA_Studie_2_2022_Statni_zamestnanci_a_urednici/IDEA_Studie_2_2022_Statni_zamestnanci_a_urednici.html#p=2

Interaktivní grafy zveřejněny jako online apendix studie na https://ideaapps.cerge-ei.cz/urednici_2021/

Text v souboru `results.Rmd` neodpovídá textu studie, který prošel odděleným editačním procesem. Odpovídají ale interaktivní grafy.

## Data

Exportována v CSV a parquet do adresáře `data-export`, spolu s codebookem 

- lidsky čitelný popis na [webu](https://idea-cergeei.github.io/studie-urednici/codebook.html)
- YAML export v souboru `codebook.yml`

Názvy sloupců v exportech jsou upraveny pro srozumitelnost, neodpovídají názvům v kódu. Rozdíly lze odvodit ze skriptu `export_data.R`.

### Data o platech z ČSÚ

Toto je poznámka for posterity

- datová sada ČSÚ 110080, kterou ČSÚ publikuje v otevřených datech vychází ze šetření struktury výdělků
- oproti tomu čísla ve VDB pochází *většinou* z firemních výkazů
- průměrné platy dle VDB jsou o cca 5 % nižší
- ve VDB jsou ale místy i výstupy ze strukturálních statistik - pozná se to tak, že tam je jiný výstup než průměr (medián), např. MZD11/13 <https://vdb.czso.cz/vdbvo2/faces/cs/index.jsf?page=vystup-objekt&pvo=MZD11&z=T&f=TABULKA&katalog=30852&str=v377&c=v3~8__RP2023>, ty potom lícují přesně s čísly v open datové sadě
- viz k tomu [dokumentaci dat od ČSÚ](https://csu.gov.cz/docs/107508/a54c8df8-eeb1-b403-5382-951daf59ab43/110080-22dds.docx?version=1.0)

## Reprodukování výstupů a dat

```r
install.packages("renv")
renv::restore()
source("standardize_input_data.R")
rmarkdown::render("results.Rmd")
source("graphs.R")
source("graphs_mod.R")
source("export_data.R")
rmarkdown::render("codebook.Rmd")
```

