# jonashaslbeck@protonmail.com; March 2, 2023

# --------------------------------------------------------------
# ---------- Get Iteration Number ------------------------------
# --------------------------------------------------------------

# !/usr/bin/env Rscript
iter <- commandArgs(trailingOnly=TRUE)
print(iter)
iter <- as.numeric(iter)

# -----------------------------------------------------
# -------- Load Packages & Aux functions --------------
# -----------------------------------------------------

# -------- Data generation ----------------------------
library(MASS)
library(igraph)

# -------- Algorithms ----------------------------
library(hdi)
library(glmnet)
library(inet)
library(mgm)
library(qgraph)
library(corpcor)

# Parallel
library(foreach)
library(parallel)
library(doParallel)


# -------- Source aux functions -----------------------
source("aux_functions.R")

# -----------------------------------------------------
# -------- Simulation Specs ---------------------------
# -----------------------------------------------------

# Fixed
p <- 20  			       # number of nodes
alpha <- 0.05		     # significance level (pvalues are corrected)

# Variations
n_seq <- c(50, 100, 200, 500, 800, 1000) # added 800 & 1000 on Jan 20, 22
pe_seq <- c(0.2, 0.4)

# Table with simulation variations
var_grid <- expand.grid(n_seq, pe_seq)
n_gs <- nrow(var_grid)

nIter <- 110 # a few more in case there are technical errors

# Algorithm settings

# -----------------------------------------------------
# -------- Simulate -----------------------------------
# -----------------------------------------------------

# Setup parallelization
cluster <- nIter
cl <- makeCluster(cluster, outfile="")
registerDoParallel(cl)

# Initialize timer
timer_total <- proc.time()[3]

out <- foreach(iter = 1:nIter,
               .packages = c("MASS", "igraph", "corpcor", "hdi",
                             "glmnet", "inet", "qgraph", "mgm"),
               .export = c("GenData2", "alpha", "p", "nIter", "n_gs"),
               .verbose = TRUE) %dopar% {

                 ## Storage
                 # Structure
                 a_structure <- array(NA, dim=c(p, p, 8, n_gs)) # pxp, true+7methods, nIter
                 # Point estimates
                 a_PE <- array(NA, dim=c(p, p, 8, n_gs)) # pxp, true+7methods, nIter
                 # Confidence intervals (where applicable)
                 a_CI <- array(NA, dim=c(p, p, 2, 8, n_gs)) # pxp, upper/lower CI, true+7methods, nIter
                 m_time <- matrix(NA, 7, n_gs)

                 for(ni in 1:n_gs) {

                   print(ni)

                   ## Reproducibility
                   the_seed <- iter * 1000 + ni
                   set.seed(the_seed)
                   print(the_seed)

                   # --- Data Generation ---

                   graph_data_fix <- tryCatch(GenData2(p = p,
                                                       pe = var_grid[ni, 2],
                                                       n = var_grid[ni, 1]))

                   # Here I catch an occasional error coming from near-singular covariance
                   # .. matrices in the data generation
                   if(is.null(graph_data_fix)) {

                     outlist <- NULL

                   } else {

                     data <- graph_data_fix$data

                     a_structure[, , 1, ni] <- graph_data_fix$graph
                     a_PE[, , 1, ni] <- graph_data_fix$pcors
                     print(paste0("ni = ", ni, " Data Generated"))

                     # --- Estimation ---

                     # 1) Standard OLS
                     timer_m <- proc.time()[3]
                     out_OLS <- tryCatch(OLS(data,
                                             correction = TRUE,
                                             ci.level = 0.95,
                                             pbar = FALSE,
                                             rulereg = "and"))
                     if(!is.null(out_OLS$signif)) {
                       a_structure[, , 2, ni] <- out_OLS$signif
                       a_PE[, , 2, ni] <- out_OLS$est
                       a_CI[, , 1, 2, ni] <- out_OLS$ci.lower
                       a_CI[, , 2, 2, ni] <- out_OLS$ci.upper
                     }
                     m_time[1, ni] <- proc.time()[3] - timer_m
                     print(paste0("ni = ", ni, " OLS Estimated"))

                     # 2) LASSO
                     timer_m <- proc.time()[3]
                     out_lasso <- tryCatch(suppressMessages(lasso(data,
                                                                  nfold = 10,
                                                                  pbar = FALSE,
                                                                  rulereg = "and")))
                     if(!is.null(out_lasso$select)) {
                       a_structure[, , 3, ni] <- out_lasso$select
                       a_PE[, , 3, ni] <- out_lasso$est
                     }
                     m_time[2, ni] <- proc.time()[3] - timer_m
                     print(paste0("ni = ", ni, " LASSO Estimated"))

                     # 3) Multi-split
                     timer_m <- proc.time()[3]
                     out_ms <- tryCatch(suppressMessages(lasso_ms(data,
                                                                  B = 50,
                                                                  fraction = 0.5,
                                                                  correction = TRUE,
                                                                  ci.level = 0.95,
                                                                  pbar = FALSE,
                                                                  rulereg = "and")))
                     if(!is.null(out_ms$signif)) {
                       a_structure[, , 4, ni] <- out_ms$signif
                       a_PE[, , 4, ni] <- out_ms$est
                       a_CI[, , 1, 4, ni] <- out_ms$ci.lower
                       a_CI[, , 2, 4, ni] <- out_ms$ci.upper
                     }
                     m_time[3, ni] <- proc.time()[3] - timer_m
                     print(paste0("ni = ", ni, " Multi-split Estimated"))


                     # 4) Desparsified Lasso
                     timer_m <- proc.time()[3]
                     out_dsp <- tryCatch(suppressMessages(lasso_dsp(data,
                                                                    betainit = "cv lasso",
                                                                    correction = TRUE,
                                                                    ci.level = 0.95,
                                                                    pbar = FALSE,
                                                                    rulereg = "and")))
                     if(!is.null(out_dsp$signif)) {
                       a_structure[, , 5, ni] <- out_dsp$signif
                       a_PE[, , 5, ni] <- out_dsp$est
                       a_CI[, , 1, 5, ni] <- out_dsp$ci.lower
                       a_CI[, , 2, 5, ni] <- out_dsp$ci.upper
                     }
                     m_time[4, ni] <- proc.time()[3] - timer_m
                     print(paste0("ni = ", ni, " DespLass Estimated"))


                     # 5) Desparsified Lasso (bootstrap)
                     timer_m <- proc.time()[3]
                     out_dsp_bt <- tryCatch(suppressMessages(lasso_dsp_boot(data,
                                                                            B = 1000,
                                                                            betainit = "cv lasso",
                                                                            correction = TRUE,
                                                                            ci.level = 0.95,
                                                                            pbar = FALSE,
                                                                            rulereg = "and")))
                     if(!is.null(out_dsp_bt$signif)) {
                       a_structure[, , 6, ni] <- out_dsp_bt$signif
                       a_PE[, , 6, ni] <- out_dsp_bt$est
                       a_CI[, , 1, 6, ni] <- out_dsp_bt$ci.lower
                       a_CI[, , 2, 6, ni] <- out_dsp_bt$ci.upper
                     }
                     m_time[5, ni] <- proc.time()[3] - timer_m
                     print(paste0("ni = ", ni, " DespLass(Boot) Estimated"))


                     # 6) EBIC glasso
                     timer_m <- proc.time()[3]
                     out_EBICg <- EBICglasso(cor(data), n=nrow(data), gamma=0.5)
                     a_structure[, , 7, ni] <- (out_EBICg != 0)*1
                     a_PE[, , 7, ni] <- out_EBICg
                     m_time[6, ni] <- proc.time()[3] - timer_m
                     print(paste0("ni = ", ni, " EBIC glasso Estimated"))

                     # 7) mgm cv 10fold + thresholding
                     timer_m <- proc.time()[3]
                     out_mgm <- suppressMessages(mgm(data = data,
                                                     type = rep("g", p),
                                                     level = rep(1, p),
                                                     lambdaSel = "CV",
                                                     lambdaFolds = 10,
                                                     pbar = FALSE))
                     a_structure[, , 8, ni] <- (out_mgm$pairwise$wadj != 0)*1
                     a_PE[, , 8, ni] <- out_mgm$pairwise$wadj
                     m_time[7, ni] <- proc.time()[3] - timer_m
                     print(paste0("ni = ", ni, " MGM CV10+thr Estimated"))


                   } # end for: 12 conditions


                   print(paste0("Iter = ", iter, " finished"))

                   # --- Return ---

                   outlist <- list("a_structure" = a_structure,
                                   "a_PE" = a_PE,
                                   "a_CI" = a_CI,
                                   "m_time" = m_time)

                 } # end if: error in data generation?

                 return(outlist)


               } # end:foreach


# print total time of nodes
print(paste0("Full Timing Iteration ", iter, ":"))
proc.time()[3] - timer_total

stopCluster(cl)

# -----------------------------------------------------
# -------- Postprocess & Save -------------------------
# -----------------------------------------------------

saveRDS(out, file="L1Sim_2023_output_FIX.RDS")


