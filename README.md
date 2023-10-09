Combine UCLA Phonetics Lab Archive metadata with Glottolog metadata
================
Steven Moran
2023-10-09

``` r
library(tidyverse)
```

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
write_csv(ucla_glottlog, 'ucla_glottlog.csv')
```
