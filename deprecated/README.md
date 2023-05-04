## Deprecated Functions

These functions used to be included in the repository but have since been flagged as deprecated. Installing this package using `install_github` **will not** get these functions, so if you'd like them you'll need to clone this repository (or download the specific files that you want from this folder). Deprecated functions are as follows:

- **`csv_summary`**: Convenient wrapper for running `Hmisc::describe` on a CSV and exporting the result as another CSV
    - Removed reason: `Hmisc::describe` is very similar to `base::summary` and an exporting function that doesn't do significant manipulation could just run in the Console instead of making a new file (personal preference there though) 

- **`categorical_frequency`**: Counts the frequency of levels of all categorical variables in a given dataframe
    - Removed reason: Not often necessary to count the relative frequency of categorical variables for its own sake
    
- **`morpho_wiz`**: Identifies and sorts the unique values of every column in a supplied CSV. The function name comes from its original *raison d'etre* as a helper function for pre-processing before submission to dataONE's Morpho software
    - Removed reason: Very narrow use-case that may be deprecated (dataONE's current Morpho submission process may now differ from when this function was written)

- **`regrid_to_regular`**: Interpolates a netCDF file to World Geodetic System 84 (WGS84)
    - Removed reason: Uses superseded spatial R packages

- **`zoom_webinar_fix`**: Processes the raw CSVs output by Zoom after a webinar (handles both the attendance and post-webinar survey dataframes)
    - Removed reason: Does not quite fit the theme of `scicomptools` and is quite context-dependent 