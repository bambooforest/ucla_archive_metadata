Combine UCLA Phonetics Lab Archive metadata with Glottolog metadata and
do some stats and tests
================
Steven Moran
2023-10-09

- [Overview](#overview)
- [Explore VoxAngeles TextGrid data](#explore-voxangeles-textgrid-data)
- [Tests](#tests)
- [TODOS](#todos)

Load some libraries

``` r
library(tidyverse)
library(knitr)
```

------------------------------------------------------------------------

# Overview

I took the the UCLA Phonetics Archive:

- <http://archive.phonetics.ucla.edu/Language%20Indices/index_available.htm>

And first created a CSV file of their language names and ISO codes by
hand.

Then we load the hand created table.

``` r
ucla <- read_csv('ucla_metadata.csv')
```

Then load the Glottolog geo data.

``` r
glottolog_geo <- read_csv(url('https://cdstar.eva.mpg.de//bitstreams/EAEA0-B701-6328-C3E3-0/languages_and_dialects_geo.csv'))
```

Let’s see what doesn’t match by joining them. Actually, looks
surprisingly pretty good – only a few retired ISO codes that we can
curate by hand, if needed.

``` r
ucla$iso_6393 <- tolower(ucla$`Ethnologue Code`)
ucla_glottlog <- left_join(ucla, glottolog_geo, by=c("iso_6393"="isocodes"))
```

Now we can map them. :)

``` r
ggplot(data=ucla_glottlog, aes(x=longitude, y=latitude)) + 
  borders("world", colour="gray50", fill="gray50") + 
  geom_point() +
  theme_bw()
```

    ## Warning: Removed 6 rows containing missing values (`geom_point()`).

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Let’s add in language family data. This must be downloaded from
Glottolog (updated per release) instead of read directly from the
website, like above.

``` r
glottolog_families <- read_csv('glottolog_languoid/languoid.csv')
```

Join them again.

``` r
ucla_glottlog <- left_join(ucla_glottlog, glottolog_families)
```

    ## Joining with `by = join_by(name, level, latitude, longitude)`

And map them by family.

``` r
ggplot(data=ucla_glottlog, aes(x=longitude, y=latitude, color=family_id)) + 
  borders("world", colour="gray50", fill="gray50") + 
  geom_point() +
  theme_bw() +
  theme(legend.position="none")
```

    ## Warning: Removed 6 rows containing missing values (`geom_point()`).

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Write the combined data as csv.

``` r
head(ucla_glottlog) %>% kable()
```

| Language  | Ethnologue Code | iso_6393 | glottocode | name      | level    | macroarea |  latitude | longitude | id       | family_id | parent_id | bookkeeping | iso639P3code | description | markup_description | child_family_count | child_language_count | child_dialect_count | country_ids       |
|:----------|:----------------|:---------|:-----------|:----------|:---------|:----------|----------:|----------:|:---------|:----------|:----------|:------------|:-------------|:------------|:-------------------|-------------------:|---------------------:|--------------------:|:------------------|
| Abaza     | ABQ             | abq      | abaz1241   | Abaza     | language | Eurasia   |  44.25000 |  42.00000 | abaz1241 | abkh1242  | abkh1243  | FALSE       | abq          | NA          | NA                 |                  0 |                    0 |                   3 | RU TR             |
| Abkhaz    | ABK             | abk      | abkh1244   | Abkhaz    | language | Eurasia   |  43.05622 |  41.15911 | abkh1244 | abkh1242  | abkh1243  | FALSE       | abk          | NA          | NA                 |                  0 |                    0 |                   3 | GE RU TR          |
| Aceh      | ACE             | ace      | achi1257   | Acehnese  | language | Papunesia |   3.90757 |  96.60320 | achi1257 | aust1307  | cham1327  | FALSE       | ace          | NA          | NA                 |                  0 |                    0 |                   7 | ID                |
| Adyghe    | ADY             | ady      | adyg1241   | Adyghe    | language | Eurasia   |  44.00000 |  39.33000 | adyg1241 | abkh1242  | circ1239  | FALSE       | ady          | NA          | NA                 |                  0 |                    0 |                   5 | IL JO RU SY TR    |
| Afrikaans | AFR             | afr      | afri1274   | Afrikaans | language | Africa    | -22.00000 |  30.00000 | afri1274 | indo1319  | afri1273  | FALSE       | afr          | NA          | NA                 |                  0 |                    0 |                   3 | BW MZ NA ZA ZM ZW |
| Aghul     | AGX             | agx      | aghu1253   | Aghul     | language | Eurasia   |  41.92420 |  47.58430 | aghu1253 | nakh1245  | aghu1260  | FALSE       | agx          | NA          | NA                 |                  0 |                    0 |                   6 | AZ RU             |

``` r
write_csv(ucla_glottlog, 'ucla_glottlog.csv')
```

How many languages (according to ISO 639-3 code) are present in the
sample?

``` r
# Less than the number of ISO codes
nrow(ucla_glottlog %>% select(glottocode) %>% distinct())
```

    ## [1] 309

``` r
nrow(ucla_glottlog %>% select(iso_6393) %>% distinct())
```

    ## [1] 310

How many language families are present in the sample?

``` r
nrow(ucla_glottlog %>% select(family_id) %>% distinct())
```

    ## [1] 51

# Explore VoxAngeles TextGrid data

Read in Eleanor’s TextGrid table. First I converted it to UTF-8 (from
UTF-16) and to CSV (from TSV).

``` r
df <- read_csv('ucla_durations.csv')
```

Get UCLA phoneme counts per language / phone.

``` r
lang_phone_count <- df %>% group_by(lang, phone) %>% summarize(phoneme_count = n())
```

    ## `summarise()` has grouped output by 'lang'. You can override using the
    ## `.groups` argument.

Get UCLA phoneme counts per language.

``` r
ucla_lang_phoneme_count <- lang_phone_count %>% group_by(lang) %>% summarize(ucla_phonemes = n())
```

Get PHOIBLE CLDF data.

``` r
values <- read_csv(url('https://raw.githubusercontent.com/cldf-datasets/phoible/master/cldf/values.csv'))
languages <- read_csv(url('https://raw.githubusercontent.com/cldf-datasets/phoible/master/cldf/languages.csv'))
inventories <- read_csv(url('https://raw.githubusercontent.com/cldf-datasets/phoible/master/cldf/inventories.csv'))
parameters <- read_csv(url('https://raw.githubusercontent.com/cldf-datasets/phoible/master/cldf/parameters.csv'))
```

Merge into a data frame for the language index with counts.

``` r
phoible <- values %>% select(Language_ID, Inventory_ID) %>% distinct()
phoible <- left_join(phoible, inventories, by=c("Inventory_ID" = "ID"))
phoible <- phoible %>% rename(Source_name = Name)
phoible <- left_join(phoible, languages, by=c("Language_ID" = "ID"))
```

Merge CLDF inventories together with languages index.

``` r
tmp <- phoible %>% select(Glottocode, Name, ISO639P3code, count_phonemes) %>% group_by(ISO639P3code) %>% filter(row_number()==1)

ucla_phoible_phoneme_counts <- left_join(ucla_lang_phoneme_count, tmp, by=c("lang"="ISO639P3code"))
```

We have some NAs in the phoible.count_phonemes data column. Replace with
0.

``` r
# ucla_phoible_phoneme_counts$count_phonemes <- ucla_phoible_phoneme_counts$count_phonemes %>% replace_na(0)
```

Get the delta between the phoneme counts and compare coverage. This
doesn’t compare phonemes – just counts between what is unique in
VoxAngeles and the number of phonemes in the inventory in PHOIBLE.

To do a fair comparison between the CMU-UCLA / VoxAngeles corpus and
PHOIBLE the segments would have to follow the phoible segment
conventions.

``` r
ucla_phoible_phoneme_counts$delta <- abs(ucla_phoible_phoneme_counts$ucla_phonemes - ucla_phoible_phoneme_counts$count_phonemes)

ucla_phoible_phoneme_counts %>% kable()
```

| lang | ucla_phonemes | Glottocode | Name                    | count_phonemes | delta |
|:-----|--------------:|:-----------|:------------------------|---------------:|------:|
| abk  |            49 | abkh1244   | Abkhaz                  |             62 |    13 |
| ace  |            21 | achi1257   | Acehnese                |             55 |    34 |
| ady  |            41 | adyg1241   | Adyghe                  |             59 |    18 |
| aeb  |            35 | NA         | NA                      |             NA |    NA |
| afn  |            37 | defa1248   | Defaka                  |             42 |     5 |
| afr  |            44 | afri1274   | Afrikaans               |             39 |     5 |
| agx  |            45 | NA         | NA                      |             NA |    NA |
| ajp  |            42 | NA         | NA                      |             NA |    NA |
| aka  |            27 | akan1250   | Akan                    |             40 |    13 |
| apc  |            55 | NA         | NA                      |             NA |    NA |
| ape  |            36 | NA         | NA                      |             NA |    NA |
| apw  |            57 | NA         | NA                      |             NA |    NA |
| asm  |            27 | assa1263   | Assamese                |             37 |    10 |
| azb  |            32 | sout2697   | South Azerbaijani       |             33 |     1 |
| bam  |            33 | bamb1269   | Bambara                 |             35 |     2 |
| bem  |            20 | bemb1257   | Bemba (Zambia)          |             25 |     5 |
| ben  |            33 | beng1280   | Bengali                 |             53 |    20 |
| bfd  |            33 | bafu1246   | Bafut                   |             49 |    16 |
| bfq  |            31 | bada1257   | Badaga                  |             38 |     7 |
| bhk  |            24 | NA         | NA                      |             NA |    NA |
| bin  |            30 | bini1246   | Bini                    |             48 |    18 |
| brv  |            33 | west2397   | Western Bru             |             42 |     9 |
| bsq  |            31 | nucl1418   | Bassa                   |             36 |     5 |
| bwr  |            36 | NA         | NA                      |             NA |    NA |
| cbv  |            51 | cacu1241   | Kakua                   |             22 |    29 |
| ces  |            40 | czec1258   | Czech                   |             38 |     2 |
| cha  |            28 | cham1312   | Chamorro                |             36 |     8 |
| cji  |            48 | NA         | NA                      |             NA |    NA |
| col  |            48 | NA         | NA                      |             NA |    NA |
| cpn  |            33 | cher1271   | Cherepon                |             38 |     5 |
| dag  |            30 | dagb1246   | Dagbani                 |             37 |     7 |
| dan  |            65 | dani1285   | Danish                  |             69 |     4 |
| deg  |            18 | dege1246   | Degema                  |             36 |    18 |
| dyo  |            24 | jola1263   | Jola-Fonyi              |             29 |     5 |
| efi  |            26 | efik1245   | Efik                    |             20 |     6 |
| ell  |            25 | mode1248   | Modern Greek            |             26 |     1 |
| ema  |            33 | emai1241   | Emai-Iuleha-Ora         |             48 |    15 |
| eus  |            38 | basq1248   | Basque                  |             28 |    10 |
| ewe  |            38 | ewee1241   | Ewe                     |             45 |     7 |
| ffm  |            31 | maas1239   | Maasina Fulfulde        |             37 |     6 |
| fin  |            38 | finn1318   | Finnish                 |             42 |     4 |
| fub  |            26 | adam1253   | Adamawa Fulfulde        |             34 |     8 |
| gaa  |            33 | gaaa1244   | Ga                      |             47 |    14 |
| gla  |            43 | scot1245   | Scottish Gaelic         |             82 |    39 |
| guj  |            51 | guja1252   | Gujarati                |             60 |     9 |
| gwx  |            35 | NA         | NA                      |             NA |    NA |
| hak  |            33 | hakk1236   | Hakka Chinese           |             31 |     2 |
| hau  |            46 | haus1257   | Hausa                   |             45 |     1 |
| haw  |            21 | hawa1245   | Hawaiian                |             19 |     2 |
| heb  |            39 | hebr1245   | Modern Hebrew           |             30 |     9 |
| hil  |            38 | hili1240   | Hiligaynon              |             25 |    13 |
| hin  |            47 | hind1269   | Hindi                   |             94 |    47 |
| hni  |            22 | NA         | NA                      |             NA |    NA |
| hrv  |            44 | croa1245   | Croatian Standard       |             36 |     8 |
| hun  |            61 | hung1274   | Hungarian               |             65 |     4 |
| hye  |            49 | nucl1235   | Eastern Armenian        |             37 |    12 |
| ibb  |            24 | ibib1240   | Ibibio                  |             32 |     8 |
| ibo  |            32 | nucl1417   | Igbo                    |             65 |    33 |
| idu  |            21 | idom1241   | Idoma                   |             75 |    54 |
| ilo  |            28 | ilok1237   | Iloko                   |             21 |     7 |
| isl  |            76 | icel1247   | Icelandic               |             39 |    37 |
| its  |            26 | NA         | NA                      |             NA |    NA |
| kan  |            54 | nucl1305   | Kannada                 |             47 |     7 |
| kea  |            43 | kabu1256   | Kabuverdianu            |             28 |    15 |
| khm  |            54 | cent1989   | Central Khmer           |             42 |    12 |
| klu  |            38 | klao1243   | Klao                    |             27 |    11 |
| knn  |            58 | konk1267   | Maharashtrian Konkani   |             37 |    21 |
| kri  |            30 | krio1253   | Krio                    |             32 |     2 |
| kub  |            27 | kute1248   | Kutep                   |             46 |    19 |
| kye  |            34 | krac1238   | Krache                  |             44 |    10 |
| lad  |            33 | ladi1251   | Ladino                  |             34 |     1 |
| lar  |            35 | NA         | NA                      |             NA |    NA |
| lav  |            44 | NA         | NA                      |             NA |    NA |
| led  |            13 | lend1245   | Lendu                   |             55 |    42 |
| lgq  |            33 | logb1245   | Ikpana                  |             35 |     2 |
| lit  |            93 | lith1251   | Lithuanian              |             47 |    46 |
| lkt  |            33 | lako1247   | Lakota                  |             36 |     3 |
| lug  |            42 | gand1255   | Ganda                   |             33 |     9 |
| mak  |            23 | maka1311   | Makasar                 |             23 |     0 |
| mal  |            25 | mala1464   | Malayalam               |             43 |    18 |
| mlt  |            55 | malt1254   | Maltese                 |             55 |     0 |
| mya  |            60 | nucl1310   | Burmese                 |             50 |    10 |
| nan  |            33 | minn1241   | Min Nan Chinese         |             25 |     8 |
| njm  |            37 | anga1288   | Angami Naga             |             49 |    12 |
| nld  |            49 | dutc1256   | Dutch                   |             39 |    10 |
| ozm  |            29 | koon1245   | Koonzime                |             52 |    23 |
| pam  |            47 | pamp1243   | Pampanga                |             22 |    25 |
| pes  |            46 | west2369   | Western Farsi           |             30 |    16 |
| prs  |            56 | NA         | NA                      |             NA |    NA |
| run  |            34 | rund1242   | Rundi                   |             29 |     5 |
| sbc  |            20 | NA         | NA                      |             NA |    NA |
| tsw  |            33 | NA         | NA                      |             NA |    NA |
| tzm  |            26 | cent2194   | Central Moroccan Berber |             39 |    13 |
| wuu  |            45 | wuch1236   | Wu Chinese              |             41 |     4 |
| yue  |            45 | yuec1235   | Yue Chinese             |             32 |    13 |

# Tests

``` r
lang_phone_count %>% filter(lang=="lit") %>% distinct(phone) %>% kable()
```

| lang | phone |
|:-----|:------|
| lit  | a     |
| lit  | aă    |
| lit  | aː    |
| lit  | b     |
| lit  | bʲ    |
| lit  | d     |
| lit  | dʲ    |
| lit  | d͡z    |
| lit  | d͡zʲ   |
| lit  | d͡ʒ    |
| lit  | d͡ʒʲ   |
| lit  | e     |
| lit  | eĕ    |
| lit  | eː    |
| lit  | f     |
| lit  | fʲ    |
| lit  | h     |
| lit  | hʲ    |
| lit  | i     |
| lit  | iĭ    |
| lit  | iː    |
| lit  | ĭ     |
| lit  | ĭi    |
| lit  | j     |
| lit  | k     |
| lit  | kʲ    |
| lit  | l     |
| lit  | lʲ    |
| lit  | m     |
| lit  | mʲ    |
| lit  | m̆     |
| lit  | n     |
| lit  | nʲ    |
| lit  | n̆     |
| lit  | o     |
| lit  | oŏ    |
| lit  | oː    |
| lit  | p     |
| lit  | pʲ    |
| lit  | r     |
| lit  | rʲ    |
| lit  | r̆     |
| lit  | s     |
| lit  | sʲ    |
| lit  | t     |
| lit  | tʲ    |
| lit  | t͡s    |
| lit  | t͡sʲ   |
| lit  | t͡ʃ    |
| lit  | t͡ʃʲ   |
| lit  | u     |
| lit  | uŭ    |
| lit  | uː    |
| lit  | v     |
| lit  | vʲ    |
| lit  | x     |
| lit  | xʲ    |
| lit  | z     |
| lit  | zʲ    |
| lit  | æ     |
| lit  | ææ̆    |
| lit  | æː    |
| lit  | æ̆     |
| lit  | æ̆æ    |
| lit  | ă     |
| lit  | ăa    |
| lit  | ĕe    |
| lit  | ĭ     |
| lit  | ĭi    |
| lit  | ŋ     |
| lit  | ŋʲ    |
| lit  | ŋ̆     |
| lit  | ŏo    |
| lit  | ŭu    |
| lit  | ɔ     |
| lit  | ɔ̆     |
| lit  | ə     |
| lit  | ɛ     |
| lit  | ɛ̞     |
| lit  | ɡ     |
| lit  | ɡʲ    |
| lit  | ɪ     |
| lit  | ɪ̆     |
| lit  | ɺ     |
| lit  | ɺʲ    |
| lit  | ɾ     |
| lit  | ɾʲ    |
| lit  | ʃ     |
| lit  | ʃʲ    |
| lit  | ʊ     |
| lit  | ʊ̆     |
| lit  | ʒ     |
| lit  | ʒʲ    |

# TODOS

In progress:

- read in Eleanor’s table and do some rough stats

Refs if checking differences in phoneme inventories:

- <https://blogs.umass.edu/phonolist/2019/12/03/dockum-bowern-2019-swadesh-wordlists-are-not-long-enough/>

- <https://www.researchgate.net/publication/332751956_Swadesh_lists_are_not_long_enough_Drawing_phonological_generalizations_from_limited_data>

- <https://researchprofiles.anu.edu.au/en/publications/blowing-in-the-wind-using-north-wind-and-the-sun-texts-to-sample->
