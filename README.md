# Aging-ExplicitAdaptation


The R code here implement the statistics for the paper here (preprint): 

https://doi.org/10.31234/osf.io/jx79q


Based on the data here (OSF):

https://osf.io/qzhmy/



Running all chunks in the R markdown notebook should replicate all statistics and figures. Here is the session info with the versions of R and all loaded packages:


```
R version 3.6.1 (2019-07-05)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Linux Mint 19.1

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1

locale:
 [1] LC_CTYPE=en_CA.UTF-8       LC_NUMERIC=C               LC_TIME=en_CA.UTF-8        LC_COLLATE=en_CA.UTF-8    
 [5] LC_MONETARY=en_CA.UTF-8    LC_MESSAGES=en_CA.UTF-8    LC_PAPER=en_CA.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_CA.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] gdtools_0.2.1 svglite_1.2.2 emmeans_1.4.2 afex_0.25-1   lme4_1.1-21   Matrix_1.2-17 car_3.0-5     carData_3.0-2 ez_4.4-0     

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.3          mvtnorm_1.0-11      lattice_0.20-38     zoo_1.8-5           assertthat_0.2.1    packrat_0.5.0      
 [7] R6_2.4.1            cellranger_1.1.0    plyr_1.8.4          coda_0.19-3         ggplot2_3.2.1       pillar_1.4.3       
[13] rlang_0.4.5         lazyeval_0.2.2      curl_4.2            multcomp_1.4-10     readxl_1.3.1        rstudioapi_0.11    
[19] minqa_1.2.4         data.table_1.12.2   nloptr_1.2.1        splines_3.6.1       stringr_1.4.0       foreign_0.8-72     
[25] munsell_0.5.0       compiler_3.6.1      numDeriv_2016.8-1.1 xfun_0.11           systemfonts_0.1.1   pkgconfig_2.0.3    
[31] lmerTest_3.1-0      mgcv_1.8-31         tidyselect_1.0.0    tibble_2.1.3        codetools_0.2-16    rio_0.5.16         
[37] crayon_1.3.4        dplyr_0.8.3         MASS_7.3-51.4       grid_3.6.1          nlme_3.1-142        xtable_1.8-4       
[43] gtable_0.3.0        magrittr_1.5        scales_1.0.0        zip_2.0.2           estimability_1.3    stringi_1.4.3      
[49] reshape2_1.4.3      vctrs_0.2.4         boot_1.3-23         sandwich_2.5-1      openxlsx_4.1.0      TH.data_1.0-10     
[55] tools_3.6.1         forcats_0.4.0       glue_1.3.1          purrr_0.3.3         hms_0.5.2           abind_1.4-5        
[61] parallel_3.6.1      survival_3.1-7      colorspace_1.4-1    knitr_1.26          haven_2.2.0
```
