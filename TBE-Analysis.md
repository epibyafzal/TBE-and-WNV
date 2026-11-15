TBE maps
================
Mohammed Afzal
2026-11-13

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.5.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.5.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tibble)
library(tidyr)
library(eurostat)
```

    ## Warning: package 'eurostat' was built under R version 4.5.3

``` r
library(giscoR)
```

    ## Warning: package 'giscoR' was built under R version 4.5.3

``` r
library(sf)
```

    ## Warning: package 'sf' was built under R version 4.5.3

    ## Linking to GEOS 3.14.1, GDAL 3.12.1, PROJ 9.7.1; sf_use_s2() is TRUE

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.5.3

``` r
library(magick)
```

    ## Warning: package 'magick' was built under R version 4.5.3

    ## Linking to ImageMagick 6.9.13.29
    ## Enabled features: cairo, freetype, fftw, ghostscript, heic, lcms, pango, raw, rsvg, webp
    ## Disabled features: fontconfig, x11

``` r
# Study period
years <- 2012:2024

# Country codes
countries_8 <- c(
  "CZ", "DE", "EE", "HU",
  "LT", "PL", "SI", "SK"
)

countries_10 <- c(
  countries_8, "SE", "FI"
)
```

``` r
ecdc <- read_csv(
  "data/ecdc_tbe_local_case_13_November.csv",
  col_types = cols(.default = col_character())
)



tbe_8 <- ecdc%>%
  filter(
    HealthTopic == "Tick-borne encephalitis",
    Population == "Locally acquired confirmed cases",
    Indicator == "Reported cases",
    Unit == "N",
    as.integer(Time) %in% 2012:2024,
    substr(RegionCode, 1, 2) %in% countries_8
  ) %>%
  mutate(
    raw_value = trimws(NumValue),

    data_status = case_when(
      raw_value %in% c("-", "–") ~
        "Indicator not calculated",
      is.na(raw_value) | raw_value %in% c("", ".") ~
        "Missing surveillance data",
      TRUE ~ "Reported value"
    ),

    cases = as.numeric(
      if_else(
        data_status == "Reported value",
        raw_value,
        NA_character_
      )
    ),

    data_status = case_when(
      data_status == "Reported value" & cases == 0 ~
        "Zero cases",
      TRUE ~ data_status
    )
  ) %>%
  transmute(
    original_region_code = RegionCode,
    region_name = RegionName,
    country = substr(RegionCode, 1, 2),
    year = as.integer(Time),
    cases,
    official_rate = NA_real_,
    data_status,
    source = "ECDC"
  )
```

``` r
# Choose tbe_ySWE_DOMESTIC_20261117.csv
sw_raw <- read.csv(
  file.choose(),
  header = FALSE,
  skip = 1,
  fill = TRUE,
  strip.white = TRUE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)


names(sw_raw) <- c(
  "county",
  as.vector(rbind(
    paste0(1997:2026, "_cases"),
    paste0(1997:2026, "_rate")
  ))
)

sw_long <- sw_raw %>%
  mutate(
    county = trimws(as.character(county)),
    across(-county, ~as.numeric(as.character(.x)))
  ) %>%
  filter(county != "Totalt") %>%
  pivot_longer(
    cols = -county,
    names_to = c("year", ".value"),
    names_pattern = "^(\\d{4})_(cases|rate)$"
  ) %>%
  mutate(year = as.integer(year)) %>%
  filter(year %in% years)


sw_codes <- tribble(
  ~county,           ~region_code,
  "Blekinge",        "SE221",
  "Dalarna",         "SE312",
  "Gotland",         "SE214",
  "Gävleborg",       "SE313",
  "Halland",         "SE231",
  "Jämtland",        "SE322",
  "Jönköping",       "SE211",
  "Kalmar",          "SE213",
  "Kronoberg",       "SE212",
  "Norrbotten",      "SE332",
  "Skåne",           "SE224",
  "Stockholm",       "SE110",
  "Södermanland",    "SE122",
  "Uppsala",         "SE121",
  "Värmland",        "SE311",
  "Västerbotten",    "SE331",
  "Västernorrland",  "SE321",
  "Västmanland",     "SE125",
  "Västra Götaland", "SE232",
  "Örebro",          "SE124",
  "Östergötland",    "SE123"
)

tbe_sw <- sw_long %>%

  left_join(
    sw_codes,
    by = "county",
    relationship = "many-to-one"
  ) %>%

  transmute(
    region_code,
    region_name = county,
    country = "SE",
    year,
    cases,
    official_rate = rate,
    data_status = if_else(
      cases == 0, "Zero cases", "Reported value"
    ),
    source = "Sweden domestic"
  )
```

``` r
# Choose fact_ttr_cases (1).csv
fi_raw <- read_delim(
  file.choose(),
  delim = ";",
  col_types = cols(.default = col_character())
)

fi_cases <- fi_raw %>%

  filter(
    `Reporting group` == "TBE (tick borne encephalitis)",
    Area != "All areas",
    Time %in% paste("Year", years)
  ) %>%

  transmute(
    area = Area,
    year = as.integer(
      sub("Year ", "", Time)
    ),
    cases = as.numeric(val)
  )



fi_codes <- tribble(
  ~area, ~region_code,
  "North Ostrobothnia wellbeing services county", "FI1D9",
  "Satakunta wellbeing services county", "FI196",
  "South Ostrobothnia wellbeing services county", "FI194",
  "East Uusimaa wellbeing services county", "FI1B1",
  "Lapland wellbeing services county", "FI1D7",
  "Central Finland wellbeing services county", "FI193",
  "North Karelia wellbeing services county", "FI1D3",
  "Central Uusimaa wellbeing services county", "FI1B1",
  "Pirkanmaa wellbeing services county", "FI197",
  "Åland", "FI200",
  "West Uusimaa wellbeing services county", "FI1B1",
  "South Savo wellbeing services county", "FI1D1",
  "Päijät-Häme wellbeing services county", "FI1C3",
  "North Savo wellbeing services county", "FI1D2",
  "Kanta-Häme wellbeing services county", "FI1C2",
  "Southwest Finland wellbeing services county", "FI1C1",
  "Kainuu wellbeing services county", "FI1D8",
  "Kymenlaakso wellbeing services county", "FI1C4",
  "Central Ostrobothnia wellbeing services county", "FI1D5",
  "South Karelia wellbeing services county", "FI1C5",
  "Ostrobothnia wellbeing services county", "FI195",
  "Vantaa and Kerava wellbeing services county", "FI1B1",
  "City of Helsinki", "FI1B1"
)

fi_mapped <- fi_cases %>%
  left_join(
    fi_codes,
    by = "area",
    relationship = "many-to-one"
  )



tbe_fi <- fi_mapped %>%
  group_by(region_code, year) %>%
  summarise(
    cases = sum(cases),
    .groups = "drop"
  ) %>%
  transmute(
    region_code,
    region_name = region_code,
    country = "FI",
    year,
    cases,
    official_rate = NA_real_,
    source = "Finland THL"
  )
```

``` r
tbe_8 <- tbe_8 %>%
  rename(
    region_code = original_region_code
  )

tbe_10 <- bind_rows(
  tbe_8,
  tbe_sw,
  tbe_fi
)


tbe_10 %>%
  group_by(country) %>%
  summarise(
    records = n(),
    years = n_distinct(year),
    regions = n_distinct(region_code),
    .groups = "drop"
  )
```

    ## # A tibble: 10 × 4
    ##    country records years regions
    ##    <chr>     <int> <int>   <int>
    ##  1 CZ          182    13      14
    ##  2 DE         1824    13     331
    ##  3 EE           64    13       7
    ##  4 FI          247    13      19
    ##  5 HU          143    13      21
    ##  6 LT          128    13      20
    ##  7 PL          410    13      70
    ##  8 SE          273    13      21
    ##  9 SI          136    13      12
    ## 10 SK           98    13       8

``` r
pop_raw <- get_eurostat(
  "demo_r_pjanaggr3",
  filters = list(
    freq = "A",
    unit = "NR",
    sex = "T",
    age = "TOTAL",
    sinceTimePeriod = 2012,
    untilTimePeriod = 2024
  ),
  time_format = "raw",
  type = "code"
)
```

    ## Table demo_r_pjanaggr3 cached at C:\Users\MOHAMM~1\AppData\Local\Temp\RtmpSk1mIU/eurostat/6106443f1e0b545d3fb19f98f535eda5.rds

``` r
pop <- pop_raw %>%

  transmute(
    region_code = trimws(as.character(geo)),
    year = as.integer(
      substr(as.character(time), 1, 4)
    ),
    population = as.numeric(values)
  ) %>%

  filter(
    nchar(region_code) == 5,
    year %in% years
  )
```

``` r
nuts_crosswalk <- tribble(
  ~old_code, ~new_code,

  # Lithuania
  "LT001", "LT021",  # Alytus
  "LT002", "LT022",  # Kaunas
  "LT003", "LT023",  # Klaipeda
  "LT004", "LT024",  # Marijampole
  "LT005", "LT025",  # Panevezys
  "LT006", "LT026",  # Siauliai
  "LT007", "LT027",  # Taurage
  "LT008", "LT028",  # Telsiai
  "LT009", "LT029",  # Utena
  "LT00A", "LT011",  # Vilnius

  # Hungary
  "HU101", "HU110",  # Budapest
  "HU102", "HU120",   # Pest
  
   # Finland - NUTS 2021 to NUTS 2024
  "FI194", "FI199",
  "FI1D1", "FI1DA",
  "FI193", "FI198",
  "FI1C4", "FI1C7",
  "FI197", "FI19B",
  "FI195", "FI19A",
  "FI1D3", "FI1DC",
  "FI1D2", "FI1DB",
  "FI1C3", "FI1C6"
)




code_aliases <- bind_rows(
  nuts_crosswalk %>%
    transmute(
      region_code = old_code,
      alternative_code = new_code
    ),

  nuts_crosswalk %>%
    transmute(
      region_code = new_code,
      alternative_code = old_code
    )
)
```

``` r
# Eurostat classification versions:
#
# 2012–2014 -> NUTS 2010
# 2015–2017 -> NUTS 2013
# 2018–2020 -> NUTS 2016
# 2021–2023 -> NUTS 2021
# 2024      -> NUTS 2024

nuts_version <- function(y) {
  case_when(
    y <= 2014 ~ 2010L,
    y <= 2017 ~ 2013L,
    y <= 2020 ~ 2016L,
    y <= 2023 ~ 2021L,
    TRUE ~ 2024L
  )
}

versions <- c(2010, 2013, 2016, 2021, 2024)


nuts_historical <- bind_rows(
  lapply(versions, function(v) {

    gisco_get_nuts(
      year = v,
      nuts_level = 3,
      epsg = 4326,
      resolution = 20
    ) %>%

      filter(
        substr(NUTS_ID, 1, 2) %in% countries_10,
        !grepl("ZZZ$", NUTS_ID)
      ) %>%

      select(NUTS_ID) %>%

      mutate(nuts_vintage = v)
  })
)

# Build one boundary dataset per observation year.
#
# Unlike the previous map, we DO NOT repeat the
# NUTS 2021 geometries for every year.

map_boundaries <- bind_rows(
  lapply(years, function(y) {

    nuts_historical %>%

      filter(nuts_vintage == nuts_version(y)) %>%

      mutate(year = y)
  })
)
```

``` r
# Create a lookup of year and geographical code.

map_keys <- paste(
  map_boundaries$year,
  map_boundaries$NUTS_ID,
  sep = "|"
)

tbe_coded <- tbe_10 %>%

  # Preserve the surveillance code
  rename(original_region_code = region_code) %>%

  # Find a candidate historical correspondence
  left_join(
    code_aliases,
    by = c("original_region_code" = "region_code"),
    relationship = "many-to-one"
  ) %>%

  mutate(

    # Does the original code exist in this year's map?
    original_match = paste(
      year, original_region_code, sep = "|"
    ) %in% map_keys,

    # Does the alternative code exist?
    alternative_match =
      !is.na(alternative_code) &
      paste(
        year, alternative_code, sep = "|"
      ) %in% map_keys,

    # Prefer the original historical code.
    map_code = case_when(
      original_match ~ original_region_code,

      alternative_match ~ alternative_code,

      TRUE ~ NA_character_
    ),

    geometry_match = case_when(
      original_match ~ "Original NUTS code",
      alternative_match ~ "Correspondence - review",
      TRUE ~ "Unmatched"
    )
  )



# Check unmatched geographical codes
unmatched_geometry <- tbe_coded %>%
  filter(is.na(map_code)) %>%
  select(
    country, year, original_region_code,
    region_name, cases, data_status
  )

print(unmatched_geometry, n = 50)
```

    ## # A tibble: 125 × 6
    ##    country  year original_region_code region_name           cases data_status   
    ##    <chr>   <int> <chr>                <chr>                 <dbl> <chr>         
    ##  1 PL       2012 PL21A                Oswiecimski               1 Reported value
    ##  2 PL       2012 PL711                Miasto Lódz               1 Reported value
    ##  3 PL       2012 PL714                Sieradzki                 1 Reported value
    ##  4 PL       2012 PL721                Kielecki                  1 Reported value
    ##  5 PL       2012 PL841                Bialostocki              53 Reported value
    ##  6 PL       2012 PL842                Lomzynski                12 Reported value
    ##  7 PL       2012 PL843                Suwalski                  3 Reported value
    ##  8 PL       2012 PL911                Miasto Warszawa           2 Reported value
    ##  9 PL       2012 PL912                Warszawski wschodni       1 Reported value
    ## 10 PL       2012 PL913                Warszawski zachodni       1 Reported value
    ## 11 PL       2012 PL922                Ciechanowski              1 Reported value
    ## 12 PL       2012 PL924                Ostrolecki                4 Reported value
    ## 13 SI       2012 SI031                Pomurska                  5 Reported value
    ## 14 SI       2012 SI032                Podravska                20 Reported value
    ## 15 SI       2012 SI033                Koroska                  10 Reported value
    ## 16 SI       2012 SI034                Savinjska                32 Reported value
    ## 17 SI       2012 SI035                Zasavska                  1 Reported value
    ## 18 SI       2012 SI036                Posavska                  5 Reported value
    ## 19 SI       2012 SI037                Jugovzhodna Slovenija     9 Reported value
    ## 20 SI       2012 SI038                Primorsko-notranjska     11 Reported value
    ## 21 SI       2012 SI041                Osrednjeslovenska        29 Reported value
    ## 22 SI       2012 SI042                Gorenjska                36 Reported value
    ## 23 SI       2012 SI043                Goriska                   5 Reported value
    ## 24 SI       2012 SI044                Obalno-kraska             1 Reported value
    ## 25 DE       2013 DEB1C                Cochem-Zell               1 Reported value
    ## 26 PL       2013 PL21A                Oswiecimski               2 Reported value
    ## 27 PL       2013 PL616                Grudziadzki               1 Reported value
    ## 28 PL       2013 PL712                Lódzki                    2 Reported value
    ## 29 PL       2013 PL721                Kielecki                  1 Reported value
    ## 30 PL       2013 PL811                Bialski                   2 Reported value
    ## 31 PL       2013 PL815                Pulawski                  1 Reported value
    ## 32 PL       2013 PL824                Tarnobrzeski              1 Reported value
    ## 33 PL       2013 PL841                Bialostocki              62 Reported value
    ## 34 PL       2013 PL842                Lomzynski                10 Reported value
    ## 35 PL       2013 PL843                Suwalski                  2 Reported value
    ## 36 PL       2013 PL911                Miasto Warszawa           4 Reported value
    ## 37 PL       2013 PL913                Warszawski zachodni       1 Reported value
    ## 38 PL       2013 PL924                Ostrolecki                5 Reported value
    ## 39 SI       2013 SI031                Pomurska                  8 Reported value
    ## 40 SI       2013 SI032                Podravska                22 Reported value
    ## 41 SI       2013 SI033                Koroska                  22 Reported value
    ## 42 SI       2013 SI034                Savinjska                34 Reported value
    ## 43 SI       2013 SI037                Jugovzhodna Slovenija     6 Reported value
    ## 44 SI       2013 SI041                Osrednjeslovenska       123 Reported value
    ## 45 SI       2013 SI042                Gorenjska                80 Reported value
    ## 46 SI       2013 SI043                Goriska                   7 Reported value
    ## 47 SI       2013 SI044                Obalno-kraska             5 Reported value
    ## 48 PL       2014 PL21A                Oswiecimski               1 Reported value
    ## 49 PL       2014 PL711                Miasto Lódz               2 Reported value
    ## 50 PL       2014 PL712                Lódzki                    1 Reported value
    ## # ℹ 75 more rows

``` r
# Prevent two records from being assigned to
# the same region/year polygon.

duplicate_map_codes <- tbe_coded %>%
  filter(!is.na(map_code)) %>%
  count(map_code, year) %>%
  filter(n > 1)
```

``` r
# First try the original surveillance code.
# Then try the actual mapped code.
# Finally try a documented alternative code.
#
# An unmatched value remains NA.
# We will not interpolate population automatically.

pop_original <- pop %>%
  transmute(
    original_region_code = region_code,
    year,
    population_original = population
  )

pop_mapped <- pop %>%
  transmute(
    map_code = region_code,
    year,
    population_mapped = population
  )

pop_alternative <- pop %>%
  transmute(
    alternative_code = region_code,
    year,
    population_alternative = population
  )

tbe_rates_10 <- tbe_coded %>%

  left_join(
    pop_original,
    by = c("original_region_code", "year"),
    relationship = "many-to-one"
  ) %>%

  left_join(
    pop_mapped,
    by = c("map_code", "year"),
    relationship = "many-to-one"
  ) %>%

  left_join(
    pop_alternative,
    by = c("alternative_code", "year"),
    relationship = "many-to-one"
  ) %>%

  mutate(

    # Use the first available population
    population = coalesce(
      population_original,
      population_mapped,
      population_alternative
    ),

    population_source = case_when(
      !is.na(population_original) ~
        "Original code",

      !is.na(population_mapped) ~
        "Map code",

      !is.na(population_alternative) ~
        "Alternative code - review",

      TRUE ~ "Unavailable"
    )
  )
```

``` r
tbe_rates_10 <- tbe_rates_10 %>%

  mutate(
    rate_per_100k = case_when(

      # Missing/not-calculated case count
      is.na(cases) ~ NA_real_,

      # Swedish official rate
      country == "SE" &
        !is.na(official_rate) ~ official_rate,

      # Calculate when population is available
      !is.na(population) &
        population > 0 ~
          cases / population * 100000,

      TRUE ~ NA_real_
    )
  )



# Check rate availability by country
tbe_rates_10 %>%

  group_by(country) %>%

  summarise(
    total = n(),
    available = sum(!is.na(rate_per_100k)),
    unavailable = sum(is.na(rate_per_100k)),
    .groups = "drop"
  ) %>%

  print(n = Inf)
```

    ## # A tibble: 10 × 4
    ##    country total available unavailable
    ##    <chr>   <int>     <int>       <int>
    ##  1 CZ        182       182           0
    ##  2 DE       1824      1616         208
    ##  3 EE         64        57           7
    ##  4 FI        247       247           0
    ##  5 HU        143       143           0
    ##  6 LT        128       128           0
    ##  7 PL        410       390          20
    ##  8 SE        273       273           0
    ##  9 SI        136       136           0
    ## 10 SK         98        98           0

``` r
map_10 <- map_boundaries %>%

  left_join(
    tbe_rates_10 %>%

      filter(!is.na(map_code)) %>%

      select(
        map_code,
        year,
        cases,
        rate_per_100k,
        data_status,
        source
      ),

    by = c(
      "NUTS_ID" = "map_code",
      "year" = "year"
    ),

    relationship = "many-to-one"
  )

# Classify each region by its TBE rate.
#
# Grey = explicitly zero cases
# Purple = no usable rate or regional record
# Yellow -> red = positive rates



map_10 <- map_10 %>%
  mutate(
    rate_category = case_when(
      data_status == "Indicator not calculated" ~
        "Local indicator not calculated",

      is.na(cases) ~ "Other data unavailable",

      cases == 0 ~ "0 cases",

      is.na(rate_per_100k) ~
        "Other data unavailable",

      rate_per_100k < 1 ~ ">0–<1",
      rate_per_100k < 2 ~ "1–<2",
      rate_per_100k < 5 ~ "2–<5",
      rate_per_100k < 10 ~ "5–<10",

      TRUE ~ "≥10"
    )
  )
```

``` r
year_summary <- map_10 %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(
    affected = sum(cases > 0, na.rm = TRUE),
    total = n_distinct(NUTS_ID),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = round(100 * affected / total, 1),
    panel_label = paste0(
      affected, "/", total,
      " regions (", percentage, "%)"
    )
  )
```

``` r
rate_levels <- c(
  "0 cases",
  ">0–<1",
  "1–<2",
  "2–<5",
  "5–<10",
  "≥10",
  "Data unavailable"
)

map_10 <- map_10 %>%
  mutate(
    rate_category = factor(
      rate_category,
      levels = rate_levels
    )
  )



colours <- c(
  "0 cases" = "#D9D9D9",
  ">0–<1" = "#FFF1B8",
  "1–<2" = "#FEC867",
  "2–<5" = "#F99545",
  "5–<10" = "#E34B35",
  "≥10" = "#A51D43",
  "Local indicator not calculated" = "#9568AE",
  "Other data unavailable" = "#C7DAEA"
)

map_10 <- map_10 %>%
  mutate(
    rate_category = case_when(

      data_status %in% c(
        "Indicator not calculated",
        "Locally acquired indicator not calculated"
      ) ~ "Local indicator not calculated",

      !is.na(cases) & cases == 0 ~ "0 cases",

      is.na(cases) | is.na(rate_per_100k) ~
        "Other data unavailable",

      rate_per_100k < 1 ~ ">0–<1",
      rate_per_100k < 2 ~ "1–<2",
      rate_per_100k < 5 ~ "2–<5",
      rate_per_100k < 10 ~ "5–<10",

      TRUE ~ "≥10"
    ),

    rate_category = factor(
      rate_category,
      levels = names(colours)
    )
  )
```

``` r
tbe_plot_10 <- ggplot(map_10) +

  geom_sf(
    aes(fill = rate_category),
    color = "white",
    linewidth = 0.08
  ) +

  # Affected-region ratio below each map
  geom_text(
    data = year_summary,
    aes(
      x = 19,
      y = 40,
      label = panel_label
    ),
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold",
    lineheight = 1.1
  ) +

  # One panel per year
  facet_wrap(
    ~year,
    ncol = 4
  ) +

  # Shared legend
  scale_fill_manual(
    values = colours,
    limits = names(colours),
    breaks = names(colours),
    drop = FALSE,
    na.value="#C7DAEA",
    name = "TBE cases per 100,000"
  ) +

  # Same map extent for all panels
  coord_sf(
    xlim = c(3, 36),
    ylim = c(38.5, 72),
    expand = FALSE,
    datum = NA
  ) +

  labs(
    title = "Geographical distribution of TBE in 10 European countries",
    subtitle = paste(
      "NUTS3 level notification rates, 2012–2024",
      "| Historical NUTS 3 boundaries"
    ),

    caption = paste(
      "Sources: ECDC Surveillance ATLAS, Folkhälsomyndigheten, THL.",
      "ECDC: locally acquired confirmed cases;",
      "Sweden: domestically acquired cases;",
      "Finland: all reported TBE cases.",
      "Purple: locally acquired indicator not available.",
      "Light blue: other data unavailable.",
      "Grey: explicitly reported zero cases.",
      "Regional boundaries and reporting completeness vary by year.",
      sep = "\n"
    )
  ) +

  theme_void() +

  theme(
    plot.title = element_text(
      face = "bold",
      size = 19
    ),

    plot.subtitle = element_text(
      size = 11
    ),

    strip.text = element_text(
      face = "bold",
      size = 12
    ),

    strip.background = element_rect(
      fill = "#F1F3F6",
      color = NA
    ),

    legend.position = "bottom",

    panel.spacing = grid::unit(0.7, "lines"),

    plot.caption = element_text(
      size = 8,
      hjust = 0
    )
  ) +

  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

# Save as PNG
ggsave(
  "figures/TBE_10_countries_2012_2024.png",
  plot = tbe_plot_10,
  width = 19,
  height = 19,
  dpi = 300,
  bg = "white"
)
```

``` r
make_tbe_map <- function(selected_years) {

  map_subset <- map_10 %>%
    filter(year %in% selected_years)

  summary_subset <- year_summary %>%
    filter(year %in% selected_years)

  ggplot(map_subset) +

    geom_sf(
      aes(fill = rate_category),
      colour = "white",
      linewidth = 0.1
    ) +

    geom_text(
      data = summary_subset,
      aes(
        x = 19,
        y = 43.5,
        label = panel_label
      ),
      inherit.aes = FALSE,
      size = 4,
      fontface = "bold"
    ) +

    facet_wrap(~year, ncol = 2) +

    scale_fill_manual(
      values = colours,
      limits = names(colours),
      drop = FALSE,
      na.value = "#C7DAEA",
      name = "TBE cases per 100,000"
    ) +

    coord_sf(
      xlim = c(5, 33),
      ylim = c(43, 71),
      expand = FALSE,
      datum = NA
    ) +

    labs(
      title = paste0(
        "TBE in Europe: ",
        min(selected_years), "–",
        max(selected_years)
      ),
      caption = paste(
        "Sources: ECDC, Folkhälsomyndigheten,THL"
      )
    ) +

    theme_void() +

    theme(
      plot.title = element_text(
        size = 20,
        face = "bold"
      ),
      strip.text = element_text(
        size = 16,
        face = "bold"
      ),
      panel.spacing = grid::unit(1.5, "cm"),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12),
      plot.caption = element_text(size = 9)
    ) +

    guides(
      fill = guide_legend(
        nrow = 2,
        byrow = TRUE
      )
    )
}
```

``` r
map_1 <- make_tbe_map(2012:2015)
map_2 <- make_tbe_map(2016:2019)
map_3 <- make_tbe_map(2020:2024)

ggsave(
  "TBE_2012_2015.png",
  map_1,
  width = 12,
  height = 12,
  dpi = 300,
  bg = "white"
)
```

    ## Warning in grDevices::dev.off(): agg could not write to the given file

``` r
ggsave(
  "TBE_2016_2019.png",
  map_2,
  width = 12,
  height = 12,
  dpi = 300,
  bg = "white"
)

ggsave(
  "TBE_2020_2024.png",
  map_3,
  width = 12,
  height = 16,
  dpi = 300,
  bg = "white"
)
```

``` r
# Read the image generated by ggplot
img <- image_read("figures/TBE_10_countries_2012_2024.png")

# Remove surrounding white space
img_cropped <- image_trim(
  img,
  fuzz = 5
)

# Save the cropped image
image_write(
  img_cropped,
  "TBE_10_countries_2012_2024_cropped.png"
)
```
