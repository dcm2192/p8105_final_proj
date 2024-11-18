TP_analysis
================

## Load Packages

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggmap)
```

    ## ℹ Google's Terms of Service: <https://mapsplatform.google.com>
    ##   Stadia Maps' Terms of Service: <https://stadiamaps.com/terms-of-service/>
    ##   OpenStreetMap's Tile Usage Policy: <https://operations.osmfoundation.org/policies/tiles/>
    ## ℹ Please cite ggmap if you use it! Use `citation("ggmap")` for details.

## Read in Data

``` r
elk = read_csv(file = "../clean_data/elk.csv")
```

    ## Rows: 104913 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (7): elk_id, year, month, day, hour, lat, long
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
water_quality = read_csv(file = "../clean_data/water_quality.csv")
```

    ## Rows: 23190 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): location_id, location_name, park_code, location_type, activity_id,...
    ## dbl  (5): latitude, longitude, year, month, day
    ## date (1): activity_start_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Elk Data

Each elk has an `elk_id`. The `year`, `month`, `day`, and `hour` give
the time. The `lat` and `long` give the location.

``` r
head(elk)
```

    ## # A tibble: 6 × 7
    ##   elk_id  year month   day  hour   lat  long
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1    572  2006     3     1    18  43.8 -110.
    ## 2    572  2006     3     1    20  43.8 -110.
    ## 3    572  2006     3     1    22  43.8 -110.
    ## 4    572  2006     3     2     0  43.8 -110.
    ## 5    572  2006     3     2     2  43.8 -110.
    ## 6    572  2006     3     2     4  43.8 -110.

## Water Quality

- The `year`, `month`, and `day` variables give the time.
- The `latitude` and `longitude` give the location.
- `characteristic_name` gives the type of measurement.
- `result_text` gives the measurement.

``` r
head(water_quality)
```

    ## # A tibble: 6 × 14
    ##   location_id location_name           park_code location_type latitude longitude
    ##   <chr>       <chr>                   <chr>     <chr>            <dbl>     <dbl>
    ## 1 BICA_BHR1   Bighorn River near St.… BICA      River/Stream      45.3     -108.
    ## 2 BICA_BHR1   Bighorn River near St.… BICA      River/Stream      45.3     -108.
    ## 3 BICA_BHR1   Bighorn River near St.… BICA      River/Stream      45.3     -108.
    ## 4 BICA_BHR1   Bighorn River near St.… BICA      River/Stream      45.3     -108.
    ## 5 BICA_BHR1   Bighorn River near St.… BICA      River/Stream      45.3     -108.
    ## 6 BICA_BHR1   Bighorn River near St.… BICA      River/Stream      45.3     -108.
    ## # ℹ 8 more variables: activity_id <chr>, activity_type <chr>,
    ## #   activity_start_date <date>, year <dbl>, month <dbl>, day <dbl>,
    ## #   characteristic_name <chr>, result_text <chr>

This is an incredibly rich data set. I have only kept the 20 most common
quantitative measurements, but there are so many more. I have kept this
data set in the long format, because there is very spotty measuring.

``` r
water_quality |> 
  group_by(characteristic_name) |> 
  summarize(n = n()) |> 
  arrange(desc(n))
```

    ## # A tibble: 21 × 2
    ##    characteristic_name            n
    ##    <chr>                      <int>
    ##  1 Calcium mg/l                1682
    ##  2 Magnesium mg/l              1682
    ##  3 Sodium mg/l                 1636
    ##  4 Potassium mg/l              1635
    ##  5 pH                          1307
    ##  6 Temperature, water deg C    1292
    ##  7 Specific conductance uS/cm  1287
    ##  8 Arsenic mg/l                1219
    ##  9 Dissolved oxygen (DO) mg/l  1177
    ## 10 Chloride mg/l               1131
    ## # ℹ 11 more rows

### Water Quality pH

This code chunk filters for all measurements of pH. `result_text` is a
character vector, so make sure to change the vector to a numeric type if
you’re working with numeric data, like pH

``` r
calcium = 
  water_quality |> 
  filter(characteristic_name == 'Calcium mg/l') |> 
  mutate(calcium = as.numeric(result_text)) # turning the vector numeric


calcium |>    
  ggplot(aes(x = calcium)) + 
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](TP_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Create a static map plot

[using
ggmap](https://www.nceas.ucsb.edu/sites/default/files/2020-04/ggmapCheatsheet.pdf)

[from stack
exchange](https://stackoverflow.com/questions/33942186/using-r-and-ggplot-to-draw-separate-lines-between-gps-coordinates/33944974)

### Step 1: Download map

Find the minimum and maximum latitude and longitude of elk’s journey.
This will give us the range of map to download.

``` r
min_lat = elk |> pull(lat) |> min()
max_lat = elk |> pull(lat) |> max()
rng_lat = abs(min_lat - max_lat)
lowerleftlat = min_lat 
upperrightlat = max_lat 


min_long = elk |> pull(long) |> min()
max_long = elk |> pull(long) |> max()
rng_long = abs(min_long - max_long)
lowerleftlon = min_long - rng_long 
upperrightlon = max_long + rng_long

myLocation <- c(left = lowerleftlon,
                 bottom = lowerleftlat,
                 right = upperrightlon,
                 top = upperrightlat)
```

``` r
register_stadiamaps(key = '29074900-bb6e-4a71-8f91-454c28190f88', write = FALSE)

myMap <- get_stadiamap(
  bbox=myLocation,
  maptype = "stamen_terrain",
  crop=FALSE)
```

    ## ℹ © Stadia Maps © Stamen Design © OpenMapTiles © OpenStreetMap contributors.

### Step 2: Plot map with elk movement

``` r
ggmap(myMap) +
geom_path(
  data = elk, 
  aes(x=long, y=lat, color = month))+
  geom_line(alpha = 0) +
  scale_color_gradientn(colours = rainbow(12))
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(
  data = elk, 
  aes(x=long, y=lat)) +
geom_path(alpha = 0.5) +
geom_line(alpha = 0) +
geom_point(
  data = calcium,
  aes(x = longitude, y = latitude, color = 'red')
  )
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Add in water calcium

``` r
ggmap(myMap) +
geom_path(
  data = elk, 
  aes(x=long, y=lat, color = month))+
  geom_line(alpha = 0) +
  scale_color_gradientn(colours = rainbow(12)) +
geom_point(
  data = calcium,
  aes(x = longitude, y = latitude)
)
```

    ## Warning: Removed 1160 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
