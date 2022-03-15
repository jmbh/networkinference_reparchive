# jonashaslbeck@gmail.com; March 14, 2022

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


# -----------------------------------------------------
# -------- Simulate -----------------------------------
# -----------------------------------------------------

# Setup parallelization
cluster <- 12
cl <- makeCluster(cluster, outfile="")
registerDoParallel(cl)

# Initialize timer
timer_total <- proc.time()[3]

out <- foreach(ni = 1:12,
               .packages = c("MASS", "igraph", "corpcor", "hdi",
                             "glmnet", "inet", "qgraph", "mgm"),
               .export = c("GenData2", "alpha", "p"),
               .verbose = TRUE) %dopar% {

                 # Reproducibility
                 the_seed <- iter * 1000 + ni
                 set.seed(the_seed)

                 print(the_seed)

                 # Storage
                 a_results <- array(NA, dim=c(p, p, 8)) # true + 7 methods
                 v_time <- rep(NA, 7)

                 # --- Data Generation ---

                 graph_data_fix <- GenData2(p = p,
                                           pe = var_grid[ni,2],
                                           n = var_grid[ni,1])

                 data <- graph_data_fix$data

                 a_results[, , 1] <- graph_data_fix$graph
                 print(paste0("ni = ", ni, " Data Generated"))

                 # --- Estimation ---

                 # 1) Standard OLS
                 timer_m <- proc.time()[3]
                 out_OLS <- tryCatch(OLS(data,
                                         correction = TRUE,
                                         ci.level = 0.95,
                                         pbar = FALSE,
                                         rulereg = "and"))
                 if(!is.null(out_OLS$signif)) a_results[, , 2] <- out_OLS$signif
                 v_time[1] <- proc.time()[3] - timer_m
                 print(paste0("ni = ", ni, " OLS Estimated"))

                 # 2) LASSO
                 timer_m <- proc.time()[3]
                 out_lasso <- tryCatch(suppressMessages(lasso(data,
                                                              nfold= 10,
                                                              pbar = FALSE,
                                                              rulereg = "and")))
                 if(!is.null(out_lasso$select)) a_results[, , 3] <- out_lasso$select
                 v_time[2] <- proc.time()[3] - timer_m
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
                 if(!is.null(out_ms$signif)) a_results[, , 4] <- out_ms$signif
                 v_time[3] <- proc.time()[3] - timer_m
                 print(paste0("ni = ", ni, " Multi-split Estimated"))


                 # 4) Desparsified Lasso
                 timer_m <- proc.time()[3]
                 out_dsp <- tryCatch(suppressMessages(lasso_dsp(data,
                                                                betainit = "cv lasso",
                                                                correction = TRUE,
                                                                ci.level = 0.95,
                                                                pbar = FALSE,
                                                                rulereg = "and")))
                 if(!is.null(out_dsp$signif)) a_results[, , 5] <- out_dsp$signif
                 v_time[4] <- proc.time()[3] - timer_m
                 print(paste0("ni = ", ni, " DespLass Estimated"))


                 # 5) Desparsified Lasso (bootstrap)
                 timer_m <- proc.time()[3]
                 out_dsp_bt <- tryCatch(suppressMessages(lasso_dsp_boot(data,
                                                                        B = 1000, # Increase to 1000 later
                                                                        betainit = "cv lasso",
                                                                        correction = TRUE,
                                                                        ci.level = 0.95,
                                                                        pbar = FALSE,
                                                                        rulereg = "and")))
                 if(!is.null(out_dsp_bt$signif)) a_results[, , 6] <- out_dsp_bt$signif
                 v_time[5] <- proc.time()[3] - timer_m
                 print(paste0("ni = ", ni, " DespLass(Boot) Estimated"))


                 # 6) EBIC glasso
                 timer_m <- proc.time()[3]
                 out <- EBICglasso(cor(data), n=nrow(data), gamma=0.5)
                 a_results[, , 7] <- (out != 0)*1
                 v_time[6] <- proc.time()[3] - timer_m
                 print(paste0("ni = ", ni, " EBIC glasso Estimated"))

                 # 7) mgm cv 10fold + thresholding
                 timer_m <- proc.time()[3]
                 out_mgm <- suppressMessages(mgm(data = data,
                                type = rep("g", p),
                                level = rep(1, p),
                                lambdaSel = "CV",
                                lambdaFolds = 10,
                                pbar=FALSE))
                 a_results[, , 8] <- (out_mgm$pairwise$wadj != 0)*1
                 v_time[7] <- proc.time()[3] - timer_m
                 print(paste0("ni = ", ni, " MGM CV10+thr Estimated"))

                 # --- Return ---

                 outlist <- list("results" = a_results,
                                 "timing" = v_time)

                 return(outlist)


               } # end:foreach


# print total time of nodes
print(paste0("Full Timing Iteration ", iter, ":"))
proc.time()[3] - timer_total

stopCluster(cl)

# -----------------------------------------------------
# -------- Postprocess & Save -------------------------
# -----------------------------------------------------

# Combine n-variations
a_out_all <- array(NA, dim=c(p, p, 6, 2, 8)) # Storage: pxp, n-var, pe-var, true+7methods
count <- 1
for(i in 1:2) {
  for(j in 1:6) {
    a_out_all[, , j, i, ] <- out[[count]][[1]]
    count <- count + 1
  }
}

# Reorder time info
a_timing <- array(NA, dim=c(6,2,7)) # n-var, pe-var, 7 methods
count <- 1
for(i in 1:2) {
  for(j in 1:6) {
    a_timing[j, i, ] <- out[[count]][[2]]
    count <- count + 1
  }
}


# Output file
saveRDS(a_out_all, file = paste0("Simres_L1_Iter_", iter, ".RDS"))
saveRDS(a_timing, file = paste0("Simres_L1_Iter_time_", iter, ".RDS"))



