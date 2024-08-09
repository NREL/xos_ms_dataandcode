xos_ms_dataandcode
================
Zofia Tillman
2024-08-07

Data and code used to run analysis and create visualizations/tables for
‘Rapid measurement of soluble xylo-oligomers using near-infrared
spectroscopy (NIRS) and multivariate statistics: calibration model
development and practical approaches to model optimization’
(Biotechnology for Biofuels and Bioproducts 2024,
<https://doi.org/10.1186/s13068-024-02558-6>)

## How to Reproduce Analysis

1.  Open R studio and change to version 4.2.3 via *Tools -\> Global
    Options -\> R version -\> Change…*
2.  In R studio, source, `./code/runAnalysis.R`

## Dependencies

- R version 4.2.3 (2023-03-15 ucrt)
  - `tidyverse` (v. 2.0.0)
  - `pls` (v. 2.8.1)
  - `prospectr` (v. 0.2.6)
  - `ggfortify` (v. 0.4.16)
  - `ggpmisc` (v. 0.5.2)
  - `rmarkdown` (v. 2.21)
  - `openxlsx` (v. 4.2.5.2)
  - `ggh4x` (v. 0.2.8)
  - `ggmagnify` (v. 0.4.0.9000)
  - `patchwork` (v. 1.2.0)
  - `egg` (v. 0.4.5)

## My Computer

    ## R version 4.2.3 (2023-03-15 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] patchwork_1.2.0      egg_0.4.5            gridExtra_2.3       
    ##  [4] ggmagnify_0.4.0.9000 ggh4x_0.2.8          openxlsx_4.2.5.2    
    ##  [7] ggpmisc_0.5.2        ggpp_0.5.2           ggfortify_0.4.16    
    ## [10] prospectr_0.2.6      pls_2.8-1            lubridate_1.9.2     
    ## [13] forcats_1.0.0        stringr_1.5.0        dplyr_1.1.2         
    ## [16] purrr_1.0.1          readr_2.1.4          tidyr_1.3.0         
    ## [19] tibble_3.2.1         ggplot2_3.4.4        tidyverse_2.0.0     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.0   xfun_0.39          splines_4.2.3      lattice_0.21-8    
    ##  [5] colorspace_2.1-0   vctrs_0.6.3        generics_0.1.3     htmltools_0.5.5   
    ##  [9] yaml_2.3.7         survival_3.5-5     utf8_1.2.3         rlang_1.1.1       
    ## [13] pillar_1.9.0       glue_1.6.2         withr_3.0.0        foreach_1.5.2     
    ## [17] lifecycle_1.0.4    MatrixModels_0.5-1 munsell_0.5.0      gtable_0.3.4      
    ## [21] zip_2.3.0          codetools_0.2-19   evaluate_0.20      knitr_1.42        
    ## [25] SparseM_1.81       tzdb_0.3.0         fastmap_1.1.1      quantreg_5.95     
    ## [29] fansi_1.0.4        Rcpp_1.0.11        polynom_1.4-1      scales_1.3.0      
    ## [33] hms_1.1.3          digest_0.6.33      stringi_1.7.12     grid_4.2.3        
    ## [37] mathjaxr_1.6-0     cli_3.6.1          tools_4.2.3        magrittr_2.0.3    
    ## [41] pkgconfig_2.0.3    MASS_7.3-59        Matrix_1.5-4       timechange_0.2.0  
    ## [45] rmarkdown_2.21     rstudioapi_0.15.0  iterators_1.0.14   R6_2.5.1          
    ## [49] compiler_4.2.3
