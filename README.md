
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src = "inst/images/scicomptools_hex.png" align = "right" width = "15%" />

# `scicomptools`

<!-- badges: start -->
<!-- badges: end -->

The goal of `scicomptools` is to house all of the standalone functions
written by NCEAS Scientific Computing Team staff that lack a specific
project. Currently contains various tools to import, summarize and
analyze data

## Installation

You can install the development version of `scicomptools` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NCEAS/scicomptools")
```

## Current Functions

### Data Description / Wrangling

-   **`read_xl_sheets()`**: Reads in all the sheets in a supplied
    Microsoft Excel workbook and returns a list of those contents

-   **`read_xl_format()`**: Identifies the formatting of every cell in a
    supplied Microsoft Excel workbook (including comment text)

-   **`zoom_webinar_fix()`**: Processes the raw CSVs output by Zoom
    after a webinar (handles both the attendance and post-webinar survey
    dataframes)

-   **`word_cloud_prep()`**: Performs text mining on a given text column
    of a dataframe to create a dataframe that is ready for word cloud
    creation.

### Visualization

-   **`word_cloud_plot()`**: Performs text mining (using
    `word_cloud_prep()`) *and* creates a simple `ggplot2` word cloud
    (for those who don’t want to handle their own plotting aesthetics)

### Modeling / Analysis

-   **`stat_export()`**: Exports a tidy CSV of summary statistics of a
    supported statistical test. Currently the following tests are
    supported: `lmerTest::lmer()`, `stats::lm()`, `stats::nls()`, and
    `stats::t.test()`

## Deprecated Functions

These functions used to be included in the repository but have since
been flagged as deprecated and filed in the eponymous folder in this
repository. Installing this package using `install_github()` **will
not** get these functions, so if you’d like them you’ll need to clone
this repository (or download the specific files that you want).
Deprecated functions are as follows:

### Data Description / Wrangling

-   **`csv_summary()`**: Convenient wrapper for running
    `Hmisc::describe()` on a CSV and exporting the result as another CSV

-   **`categorical_frequency()`**: Counts the frequency of levels of all
    categorical variables in a given dataframe

### Spatial

-   **`regrid_to_regular()`**: Interpolates a netCDF file to World
    Geodetic System 84 (WGS84)
    -   Removed reason: uses superseded spatial R packages
