Code to reproduce data analysis for manuscript: Bias due to Berkson error: Issues arising from the use of predicted values in place of observed covariates by Gregory Haber, Joshua Sampson, and Barry Graubard.

System information for manuscript analysis:

    OS: x86_64-apple-darwin15.6.0
    R version: 3.6.0
    R Packages: data.table (1.13.2)
                survey     (3.36)
                mitools    (2.4)
                magrittr   (1.5)

To create the dataset, run the following files in order:

   R/01_download_data.R

   R/02_data_clean.R

   R/03_get_pred_values.R

To replicate the analysis, run the file R/04_analysis.R
