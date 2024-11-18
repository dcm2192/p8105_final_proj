Elk Migration
================
Brooklynn McNeil
2024-11-18

Load packages and set options.

Import elk migration data.

``` r
elk_df = read_csv("../clean_data/elk.csv")
```

    Rows: 104913 Columns: 7
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    dbl (7): elk_id, year, month, day, hour, lat, long

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
elk_df
```

    # A tibble: 104,913 × 7
       elk_id  year month   day  hour   lat  long
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
     1    572  2006     3     1    18  43.8 -110.
     2    572  2006     3     1    20  43.8 -110.
     3    572  2006     3     1    22  43.8 -110.
     4    572  2006     3     2     0  43.8 -110.
     5    572  2006     3     2     2  43.8 -110.
     6    572  2006     3     2     4  43.8 -110.
     7    572  2006     3     2     6  43.8 -110.
     8    572  2006     3     2     8  43.8 -110.
     9    572  2006     3     2    10  43.8 -110.
    10    572  2006     3     2    12  43.8 -110.
    # ℹ 104,903 more rows

The elk IDs are: 572, 595, 654, 656, 671, 706, 900, 902, 903, 907, 909,
911, 913, 914, 916, 917, 918, and we are going to follow them around
Yellowstone! I found a cute picture of
[here](https://powertraveller.com/yellowstone-winter-wildlife-safari-from-gardiner/)

<img src="pics/elk.png" style="width:75%"/>

## Exploratory Analysis of Elk Migration

Some questions we want to answer about the elk:

- Do the 17 elk move together as a pack or separately?
- Do the migration patterns change by year?
- Does time of year change the migration patterns?

Fun stuff I want to do - Make a moving map of the elk
