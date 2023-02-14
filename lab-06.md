Lab 06 - Ugly charts and Simpson’s paradox
================
Zheqi Hu
2/13

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 
library(usethis)
use_git_config(user.name = "jessieeeee77", 
               user.email = "huz220@wfu.edu")
```

### Exercise 1

``` r
staff <- read_csv("data/instructional-staff.csv")
```

    ## Rows: 5 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): faculty_type
    ## dbl (11): 1975, 1989, 1993, 1995, 1999, 2001, 2003, 2005, 2007, 2009, 2011
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))
staff_long %>%
  ggplot(aes(x = year, y = value, color = faculty_type)) +
  geom_line()
```

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

![](lab-06_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
png(filename="faculty_type.png", width=600, height=400)
staff_long %>%
  ggplot(aes(x = year,
             y = value,
             group = faculty_type,
             color = faculty_type)) +
  labs (x = "year", y = "Amount", title = "hiring by faculty_type", fill = "faculty_type") + 
  geom_line()
```

\#Exercise 2

``` r
ggplot(staff_long, aes(y = faculty_type, fill = value)) +
  geom_bar(position = "fill") +
  facet_wrap(. ~ year) +
  scale_x_continuous() +
  labs(title = "hiring by faculty_type",
       x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "bottom")
```

![](lab-06_files/figure-gfm/2-1.png)<!-- -->

``` r
png(filename="faculty_type2.png", width=600, height=400)
staff_long %>%
  ggplot(aes(x = year,
             y = value,
             group = faculty_type,
             color = faculty_type)) +
  labs (x = "year", y = "Amount", title = "hiring by faculty_type", fill = "faculty_type") + 
  facet_wrap(. ~ faculty_type) +
  geom_line()
```

### Exercise 3

``` r
fisheries <- read_csv("data/fisheries.csv")
```

    ## Rows: 216 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): country
    ## dbl (3): capture, aquaculture, total
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#I think it might be better to show the proportion of the capture vs. aquaculture of fishes in each country, it might look better.


fish<- filter(fisheries, total > 1000000)
fish$PROP <- fish$capture/fish$total
png(filename="fish.png", width=900, height=400)
ggplot(fish,aes(x = country, y = PROP)) +  geom_line() +
  geom_point(position = "identity") 
```

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

``` r
ggplot(fish,aes(x = country, y = aquaculture)) + 
  geom_point(position = "identity") 
```

![](lab-06_files/figure-gfm/try%20geom_point-1.png)<!-- -->

``` r
ggplot(fish, aes(x ="country", fill = aquaculture, color=country))+
    geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y")
```

![](lab-06_files/figure-gfm/try%20geom_point-2.png)<!-- -->

``` r
ggplot(fish, aes(x="", y=capture, fill=country)) + 
geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
```

![](lab-06_files/figure-gfm/try%20pie-1.png)<!-- -->

``` r
ggplot(fish, aes(x="", y=aquaculture, fill=country)) + 
geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
```

![](lab-06_files/figure-gfm/try%20pie-2.png)<!-- -->

``` r
ggplot(fish, aes(x="", y=total, fill=country)) + 
geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
```

![](lab-06_files/figure-gfm/try%20pie-3.png)<!-- -->

``` r
#I think pie charts represent the country difference in capture and aquaculture significantly better.
```

\#Exercise 4

Add exercise headings as needed.
