## Reproducibility archive for paper "Network Inference with the Lasso"

This reproducibility archive contains the code to reproduce the simulation results and all figures in the paper "Network Inference with the Lasso". Preprint: https://psyarxiv.com/v5yzu

### Main Simulation

 - `Simulation.R` contains the simulation script to run the entire simulation reported in Section 5 of the paper; the script uses the `foreach` package to parallelize on each node over 100 cores; This script creates the output file `L1Sim_2023_output_FIX.RDS`
 - `submit_all.sh` and `submit_jobs.sh` are bash scripts to run `Simulation.R` times on the SNELLIUS cluster of the University of Amsterdam; With the parallelization across 100 cores, this took a bit under 20 hours
 - `Evaluation.R` takes the simulation output file `L1Sim_2023_output_FIX.RDS` as an input, pre-processes the results and plots the main results figure (Figure 3) in the paper and the additional results figure in the appendix

### Other Files

 - `CodeFigure2.R` contains the code to create Figure 2 in the text illustrating unbiased and biased sampling distributions


### Session Info

sessionInfo()
R version 4.1.0 (2021-05-18)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Rocky Linux 8.7 (Green Obsidian)

Matrix products: default
BLAS/LAPACK: /gpfs/admin/_hpc/sw/arch/AMD-ZEN2/Centos8/EB_production/2021/software/FlexiBLAS/3.0.4-GCC-10.3.0/lib64/libflexiblas.so.3.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] doParallel_1.0.16 iterators_1.0.14  foreach_1.5.2     corpcor_1.6.10   
 [5] qgraph_1.9.3      mgm_1.2-13        inet_0.1.0        glmnet_4.1-6     
 [9] Matrix_1.3-4      hdi_0.1-9         scalreg_1.0.1     lars_1.2         
[13] igraph_1.4.0      MASS_7.3-54      

loaded via a namespace (and not attached):
 [1] splines_4.1.0       gtools_3.9.4        Formula_1.2-4      
 [4] stats4_4.1.0        latticeExtra_0.6-30 pbivnorm_0.6.0     
 [7] pillar_1.8.1        backports_1.4.1     lattice_0.20-44    
[10] quadprog_1.5-8      glue_1.6.2          digest_0.6.31      
[13] RColorBrewer_1.1-3  checkmate_2.1.0     colorspace_2.1-0   
[16] htmltools_0.5.4     plyr_1.8.8          psych_2.2.9        
[19] pkgconfig_2.0.3     scales_1.2.1        glasso_1.11        
[22] jpeg_0.1-10         fdrtool_1.2.17      htmlTable_2.4.1    
[25] tibble_3.1.8        generics_0.1.3      ggplot2_3.4.1      
[28] pbapply_1.7-0       nnet_7.3-16         cli_3.6.0          
[31] mnormt_2.1.1        survival_3.2-11     magrittr_2.0.3     
[34] deldir_1.0-6        fansi_1.0.4         nlme_3.1-152       
[37] foreign_0.8-81      tools_4.1.0         data.table_1.14.8  
[40] lifecycle_1.0.3     stringr_1.5.0       interp_1.1-3       
[43] munsell_0.5.0       cluster_2.1.2       linprog_0.9-2      
[46] compiler_4.1.0      rlang_1.0.6         grid_4.1.0         
[49] rstudioapi_0.14     htmlwidgets_1.6.1   lavaan_0.6-14      
[52] base64enc_0.1-3     gtable_0.3.1        codetools_0.2-18   
[55] abind_1.4-5         reshape2_1.4.4      R6_2.5.1           
[58] gridExtra_2.3       knitr_1.42          dplyr_1.1.0        
[61] fastmap_1.1.0       utf8_1.2.3          Hmisc_4.8-0        
[64] shape_1.4.6         stringi_1.7.12      Rcpp_1.0.10        
[67] vctrs_0.5.2         rpart_4.1-15        png_0.1-8          
[70] tidyselect_1.2.0    xfun_0.37          
