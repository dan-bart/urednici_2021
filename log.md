---
title: "Tech log"
author: "Petr Bouchal"
date: 2021-06-27
output: html_document
knit: (function(input_file, encoding) {
  out_dir <- 'docs';
  rmarkdown::render(input_file,
 encoding=encoding,
 output_file=file.path(dirname(input_file), out_dir, 'log.html'))})
---

## 2022-11

- do zdrojových dat o zaměstnanosti ve veřejném sektoru (`ver-sektor-csu-rocenka.csv`) doplněn rok 2020 ze [Statistické ročenky 2022](https://www.czso.cz/csu/czso/statisticka-rocenka-ceske-republiky-2022), tabulka 10.A-3: [Excel online](https://www.czso.cz/documents/10180/199842475/3201982210a03.xlsx/f7446df4-2b25-4817-8059-3778d24a9604?version=1.2), [seznam tabulek](https://www.czso.cz/csu/czso/10-trh-prace-2d1v7b3uqe)

## 2022-06

### Přidána data za rok 2021 včetně skutečnosti
- vyžaduje to dvě změny:
  - 1. při načítání ostatních tabulek bylo potřeba změnit kód tak, aby načetl i rok 2021
  - 2. dále jsem pouze upravoval popisky a místa, kde byl v kódu zafixován rok 2021; data z ČSÚ už jsou aktuální, s výjimkou statistické ročenky o počtech zaměstnanců celého veřejného sektoru, ta bude až v listopadu.
- v některých listech pak autoři vymazali data ze sloupečku pořadí průměrného platu, což ale pro nás nemá žádný význam

Protože nevyužíváme data za jednotlivé organizace, jejich načítání z nového soubory jsem neřešil a obecně ho ve skriptu vypnul.

V nových datech také chybí tabulky s výseky za jednotlivé paragrafy (např. státní služba, vojáci atd.). Ta sice nepoužíváme, ale minimálně státní služba má tu výhodu, že jde napojit na data ze [systemizace](https://github.com/petrbouchal/systemizace), tj. časem bude možná užitečné si to dožádat.

Co z toho plyne:

- pokud opět někdy dostaneme data s pouze částečnými údaji za daný rok (tj. např. jen rozpočet, bez skutečnosti), bude potřeba se vrátit k původní logice skriptu - jde o logiku iterací ve funkcích `divide_*()` - popř. vytvořit univerzálnější logiku načítání této tabulky
