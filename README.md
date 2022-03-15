## Reproducibility archive for paper "Network Inference with the Lasso"

This reproducibility archive contains the code to reproduce the simulation results and all figures in the paper "Network Inference with the Lasso".

### Main Simulation

 - `Simulation.R` contains the simulation script to run one full iteration of the simulation reported in Section 5 of the paper; the script uses the `foreach` package to parallelize on each node over 16 cores; this file has two outputs, one containing the true network and the corresponding estimates of the seven considered methods; and one file containing the running time for each method
 - `submit_all.sh` and `submit_jobs.sh` are bash scripts to run `Simulation.R` 100 times on the LISA cluster of the University of Amsterdam; technically, one could also run `Simulation.R` locally 100 times with seeds `1:100` to reproduce our results; one iteration of the simulation takes a maximum of 45 minutes on a node with 16 cores with 2.10 GHz
 - the folder "output" contains the 2x100 output files (estimates and running time) from `Simulation.R`
 - `Evaluation.R` takes the simulation output files as an input, preprocesses the results and plots the main results figure (Figure 3) in the paper

### Other Files

 - `CodeFigure2.R` contains the code to create Figure 2 in the text illustrating unbiased and biased sampling distributions


### Session Info

sessionInfo()
R version 4.1.0 (2021-05-18)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 10 (buster)

Matrix products: default
BLAS/LAPACK: /sara/eb/AVX2/Debian10/EB_production/2021/software/FlexiBLAS/3.0.4-GCC-10.3.0/lib/libflexiblas.so.3.0

locale:
 [1] LC_CTYPE=en_US       LC_NUMERIC=C         LC_TIME=en_US       
 [4] LC_COLLATE=en_US     LC_MONETARY=en_US    LC_MESSAGES=en_US   
 [7] LC_PAPER=en_US       LC_NAME=C            LC_ADDRESS=C        
[10] LC_TELEPHONE=C       LC_MEASUREMENT=en_US LC_IDENTIFICATION=C 

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] doParallel_1.0.16 iterators_1.0.13  foreach_1.5.1     qgraph_1.6.9     
 [5] mgm_1.2-12        inet_0.1.0       glmnet_4.1-1      Matrix_1.3-4     
 [9] hdi_0.1-9         scalreg_1.0.1     lars_1.2          igraph_1.2.6     
[13] MASS_7.3-54      

loaded via a namespace (and not attached):
 [1] splines_4.1.0       tmvnsim_1.0-2       gtools_3.8.2       
 [4] Formula_1.2-4       assertthat_0.2.1    stats4_4.1.0       
 [7] latticeExtra_0.6-29 pbivnorm_0.6.0      pillar_1.6.1       
[10] backports_1.2.1     lattice_0.20-44     glue_1.4.2         
[13] digest_0.6.27       RColorBrewer_1.1-2  checkmate_2.0.0    
[16] colorspace_2.0-1    htmltools_0.5.1.1   plyr_1.8.6         
[19] psych_2.1.3         pkgconfig_2.0.3     purrr_0.3.4        
[22] corpcor_1.6.9       scales_1.1.1        glasso_1.11        
[25] jpeg_0.1-8.1        fdrtool_1.2.16      htmlTable_2.2.1    
[28] tibble_3.1.2        generics_0.1.0      ggplot2_3.3.3      
[31] ellipsis_0.3.2      pbapply_1.4-3       nnet_7.3-16        
[34] mnormt_2.0.2        survival_3.2-11     magrittr_2.0.1     
[37] crayon_1.4.1        fansi_0.5.0         nlme_3.1-152       
[40] foreign_0.8-81      tools_4.1.0         data.table_1.14.0  
[43] lifecycle_1.0.0     stringr_1.4.0       munsell_0.5.0      
[46] cluster_2.1.2       linprog_0.9-2       compiler_4.1.0     
[49] rlang_0.4.11        grid_4.1.0          rstudioapi_0.13    
[52] htmlwidgets_1.5.3   lavaan_0.6-8        base64enc_0.1-3    
[55] gtable_0.3.0        codetools_0.2-18    abind_1.4-5        
[58] DBI_1.1.1           reshape2_1.4.4      R6_2.5.0           
[61] gridExtra_2.3       knitr_1.33          dplyr_1.0.6        
[64] utf8_1.2.1          Hmisc_4.5-0         shape_1.4.6        
[67] stringi_1.6.2       Rcpp_1.0.6          vctrs_0.3.8        
[70] rpart_4.1-15        png_0.1-7           tidyselect_1.1.1   
[73] xfun_0.23 
