# COVID-19 and Smoking repository
[![DOI](https://zenodo.org/badge/290467489.svg)](https://zenodo.org/badge/latestdoi/290467489)

This repository contains the code required to replicate the analysis for our manuscript currently hosted at Qeios.

The most recent version is available [here](https://www.qeios.com/read/latest-UJR2AW)

Instructions to perform analysis:

  1. Conduct search and extract data as outlined in the study manuscript.
  2. Open the R project and begin with the `bayesian_report.rmd` the first script to be run is the project library. All package dependencies are listed in the `all_packages.rds` file in the `/reports` directory.
  3. Copy the shareable link of the google sheet, make sure that the file is set as shareable, into the `r_google_data` chunk in the `drive_download()` function
  4. This will download the file and split it into multiple excel files containing a single sheet. We will use these for the rest of the analysis.
  5. The next chunks produce datatable to show the number of studies included in each sheet.
  6. The `clean_data.R` script is run. If this is your first time running this analysis you may need to manually produce a `clean_data` folder for the files to be saved into.

to be continued...
