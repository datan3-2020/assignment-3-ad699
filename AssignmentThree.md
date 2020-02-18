AssignmentThree
================
ad699 - 680006536
14/02/20

In this assignment we will explore political interest (*vote6*) and how it changes over time.

Read data
---------

First we want to read and join the data for the first 7 waves of the Understanding Society. (Wave 8 does not have a variable for political interest). We only want five variables: personal identifier, sample origin, sex, age and political interest. It is tedious to join all the seven waves manually, and it makes sense to use a loop in this case. Since you don't yet know about iteration I'll provide the code for you; please see the explanation of the code here: <http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is to provide a path to the directory where the data are stored on your computer.

``` r
library(tidyverse)
library(data.table)
# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.
# create a vector with the file names and paths
files <- dir(
             # Select the folder where the files are stored.
             "/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)
# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab"
    ## [2] "/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab"
    ## [3] "/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab"
    ## [4] "/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab"
    ## [5] "/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab"
    ## [6] "/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab"
    ## [7] "/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab"
    ## [8] "/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab"
    ## [9] "/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")
for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

Reshape data (20 points)
------------------------

Now we have got the data from all 7 waves in the same data frame **all7** in the wide format. Note that the panel is unbalanced, i.e. we included all people who participated in at least one wave of the survey. Reshape the data to the long format. The resulting data frame should have six columns for six variables.

``` r
Long <- all7 %>%
  pivot_longer(a_memorig:g_vote6, names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("wave", "variable"), sep="_") %>%
  pivot_wider(names_from = "variable", values_from = "value")
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 1168468 rows [2, 3,
    ## 6, 7, 10, 11, 14, 15, 18, 19, 22, 23, 26, 27, 30, 31, 34, 35, 38, 39, ...].

``` r
Long
```

    ## # A tibble: 584,234 x 6
    ##        pidp wave  memorig   sex   age vote6
    ##       <int> <chr>   <int> <int> <int> <int>
    ##  1 68001367 a           1     1    39     3
    ##  2 68001367 b          NA    NA    NA    NA
    ##  3 68001367 c          NA    NA    NA    NA
    ##  4 68001367 d          NA    NA    NA    NA
    ##  5 68001367 e          NA    NA    NA    NA
    ##  6 68001367 f          NA    NA    NA    NA
    ##  7 68001367 g          NA    NA    NA    NA
    ##  8 68004087 a           1     1    59     2
    ##  9 68004087 b           1     1    60     2
    ## 10 68004087 c           1     1    61     2
    ## # … with 584,224 more rows

Filter and recode (20 points)
-----------------------------

Now we want to filter the data keeping only respondents from the original UKHLS sample for Great Britain (memorig == 1). We also want to clean the variables for sex (recoding it to "male" or "female") and political interest (keeping the values from 1 to 4 and coding all negative values as missing). Tabulate *sex* and *vote6* to make sure your recodings were correct.

``` r
Long <- Long %>%
        filter(memorig==1) %>%
        mutate(sex_dv = ifelse(
          sex==1, "male", ifelse(
            sex==2, "female", NA
          )
        )) %>%
        mutate(vote6 = ifelse(
          vote6==1, 1, ifelse(
            vote6==2, 2, ifelse(
              vote6==3, 3, ifelse(
                vote6==4, 4, NA
              )
            )
          )
        ))

Long
```

    ## # A tibble: 218,015 x 7
    ##        pidp wave  memorig   sex   age vote6 sex_dv
    ##       <int> <chr>   <int> <int> <int> <dbl> <chr> 
    ##  1 68001367 a           1     1    39     3 male  
    ##  2 68004087 a           1     1    59     2 male  
    ##  3 68004087 b           1     1    60     2 male  
    ##  4 68004087 c           1     1    61     2 male  
    ##  5 68004087 d           1     1    62     1 male  
    ##  6 68004087 e           1     1    63     2 male  
    ##  7 68004087 f           1     1    64     2 male  
    ##  8 68004087 g           1     1    65     2 male  
    ##  9 68006127 a           1     2    39     4 female
    ## 10 68006127 b           1     2    40     4 female
    ## # … with 218,005 more rows

``` r
Long %>%
  select(sex_dv) %>%
  table()
```

    ## .
    ## female   male 
    ## 117665 100342

``` r
Long %>%
  select(vote6) %>%
  table()
```

    ## .
    ##     1     2     3     4 
    ## 21660 70952 56134 52145

Calculate mean political interest by sex and wave (10 points)
-------------------------------------------------------------

Political interest is an ordinal variable, but we will treat it as interval and calculate mean political interest for men and women in each wave.

``` r
meanVote6 <- Long %>%
  filter(!is.na(sex_dv)) %>%
  group_by(sex_dv, wave) %>%
  summarise(meanVote = mean(vote6, na.rm=TRUE))
  
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   sex_dv [2]
    ##    sex_dv wave  meanVote
    ##    <chr>  <chr>    <dbl>
    ##  1 female a         2.84
    ##  2 female b         2.82
    ##  3 female c         2.87
    ##  4 female d         2.89
    ##  5 female e         2.87
    ##  6 female f         2.81
    ##  7 female g         2.73
    ##  8 male   a         2.53
    ##  9 male   b         2.51
    ## 10 male   c         2.54
    ## 11 male   d         2.55
    ## 12 male   e         2.51
    ## 13 male   f         2.47
    ## 14 male   g         2.42

Reshape the data frame with summary statistics (20 points)
----------------------------------------------------------

Your resulting data frame with the means is in the long format. Reshape it to the wide format. It should look like this:

| sex\_dv | a   | b   | c   | d   | e   | f   | g   |
|---------|-----|-----|-----|-----|-----|-----|-----|
| female  |     |     |     |     |     |     |     |
| male    |     |     |     |     |     |     |     |

In the cells of this table you should have mean political interest by sex and wave. Write a short interpretation of your findings.

``` r
Wide <- meanVote6 %>%
  pivot_wider(names_from = "wave", values_from = "meanVote")
Wide
```

    ## # A tibble: 2 x 8
    ## # Groups:   sex_dv [2]
    ##   sex_dv     a     b     c     d     e     f     g
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 female  2.84  2.82  2.87  2.89  2.87  2.81  2.73
    ## 2 male    2.53  2.51  2.54  2.55  2.51  2.47  2.42

*Interpretation* Across all waves the female respondants, on average, are less interested in politics than their male counterparts. This difference remains constant (at ~0.3) throughout the waves. Furthermore, for both sexes, the interest in politics increases on average from wave a to wave h (not a linear relationship). However, for both sexes, this is very slight (~0.1).

Estimate stability of political interest (30 points)
----------------------------------------------------

Political scientists have been arguing how stable the level of political interest is over the life course. Imagine someone who is not interested in politics at all so that their value of *vote6* is always 4. Their level of political interest is very stable over time, as stable as the level of political interest of someone who is always very interested in politics (*vote6* = 1). On the other hand, imagine someone who changes their value of *votes6* from 1 to 4 and back every other wave. Their level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is going to be equal to the sum of the absolute values of changes in political interest from wave to wave. Let us call this measure Delta. It is difficult for me to typeset a mathematical formula in Markdown, but I'll explain this informally.

Imagine a person with the level of political interest that is constant over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from "very interested in politics" to "fairly interested in politics": {1, 1, 1, 1, 2, 2, 2}. For them, Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from "very interested in politics" to "not at all interested" every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta = (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3 \* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a constant level of political interest.

Write the R code that does the following:

1.  To simplify interpretation, keep only the respondents with non-missing values for political interest in all seven waves.

``` r
# 1 : Filter out incompletes

Long <- Long %>%
  group_by(pidp) %>%
  filter(!is.na(vote6)) %>%
  filter(length(pidp[vote6]) == 7)

Long
```

    ## # A tibble: 103,747 x 7
    ## # Groups:   pidp [14,821]
    ##        pidp wave  memorig   sex   age vote6 sex_dv
    ##       <int> <chr>   <int> <int> <int> <dbl> <chr> 
    ##  1 68004087 a           1     1    59     2 male  
    ##  2 68004087 b           1     1    60     2 male  
    ##  3 68004087 c           1     1    61     2 male  
    ##  4 68004087 d           1     1    62     1 male  
    ##  5 68004087 e           1     1    63     2 male  
    ##  6 68004087 f           1     1    64     2 male  
    ##  7 68004087 g           1     1    65     2 male  
    ##  8 68006127 a           1     2    39     4 female
    ##  9 68006127 b           1     2    40     4 female
    ## 10 68006127 c           1     2    41     4 female
    ## # … with 103,737 more rows

1.  Calculate Delta for each person in the data set.

``` r
# 2 : Create delta variable

Long <- Long %>% 
  group_by(pidp) %>%
  mutate(delta =
    sum(
    abs(
    diff(c(vote6[wave=="a"], 
           vote6[wave=="b"], 
           vote6[wave=="c"], 
           vote6[wave=="d"], 
           vote6[wave=="e"], 
           vote6[wave=="f"],
           vote6[wave=="g"])))))

Long
```

    ## # A tibble: 103,747 x 8
    ## # Groups:   pidp [14,821]
    ##        pidp wave  memorig   sex   age vote6 sex_dv delta
    ##       <int> <chr>   <int> <int> <int> <dbl> <chr>  <dbl>
    ##  1 68004087 a           1     1    59     2 male       2
    ##  2 68004087 b           1     1    60     2 male       2
    ##  3 68004087 c           1     1    61     2 male       2
    ##  4 68004087 d           1     1    62     1 male       2
    ##  5 68004087 e           1     1    63     2 male       2
    ##  6 68004087 f           1     1    64     2 male       2
    ##  7 68004087 g           1     1    65     2 male       2
    ##  8 68006127 a           1     2    39     4 female     0
    ##  9 68006127 b           1     2    40     4 female     0
    ## 10 68006127 c           1     2    41     4 female     0
    ## # … with 103,737 more rows

1.  Calculate mean Delta for men and women.

``` r
with(Long, 
     tapply(X=delta, 
            INDEX=sex_dv,
            FUN=mean))
```

    ##   female     male 
    ## 2.494727 2.527760

1.  Calculate mean Delta by age (at wave 1) and plot the local polynomial curve showing the association between age at wave 1 and mean Delta. You can use either **ggplot2** or the *scatter.smooth()* function from base R.

``` r
DeltaAge <- with(Long, 
     tapply(X=delta[wave=="a"],
            INDEX=age[wave=="a"],
            FUN=mean))

DeltaAge %>%
  scatter.smooth(
    col = "red",
    pch = 16,
    cex = 0.6
  )
```

![](AssignmentThree_files/figure-markdown_github/unnamed-chunk-9-1.png)

1.  Write a short interpretation of your findings.

Political stability is asymmetrically quadratic in nature. One is, according to the sample, the most politically stable in middle age; being more politically unstable earlier and later in life. However, it is asymmetrical as political instability is more prominent in later life than it is in earlier life.
