# jonashaslbeck@protonmail.com; March 22, 2023

# ------------------------------------------------------
# -------- Load Packages & Source ----------------------
# ------------------------------------------------------

library(RColorBrewer)
library(scales)
source("aux_functions.R") # for label plotting function

# ------------------------------------------------------
# -------- Load Results --------------------------------
# ------------------------------------------------------

# Load sim results
l_results <- readRDS("L1Sim_2023_output_FIX.RDS")
nIter <- 100

# Loop into proper design structure
a_structure <- array(NA, dim=c(nIter, 20, 20, 6, 2, 8)) # iter, pxp, n-var, Pe-var, true+7methods
a_PE <- array(NA, dim=c(nIter, 20, 20, 6, 2, 8))
a_CI  <- array(NA, dim=c(nIter, 20, 20, 2, 6, 2, 8))

for(i in 1:nIter) {
  for(m in 1:8) {
    ni <- 1
    for(pe_i in 1:2) {
      for(n in 1:6) {
        a_structure[i, , , n, pe_i, m] <- l_results[[i]]$a_structure[, , m, ni]
        a_PE[i, , , n, pe_i, m] <- l_results[[i]]$a_PE[, , m, ni]
        a_CI[i, , , , n, pe_i, m] <- l_results[[i]]$a_CI[, , , m, ni]
        ni <- ni + 1
      }
    }
  }
}


# ------------------------------------------------------
# -------- Pre-processing ------------------------------
# ------------------------------------------------------

# ------ Compute Sensitivity, Precision, Specificity, and Coverage -----

nIter <- n_files

a_sen <- a_pre <- a_spe <- a_cov <- a_cov0 <- a_cov1 <- array(NA, dim = c(nIter, 6, 2, 7))

for(i in 1:nIter) {
  for(n in 1:6) {
    for(pe_i in 1:2) {
      for(m in 1:7) { # methods

        # Get Estimated and True Graph
        g_est <- a_structure[i, , , n, pe_i, m+1]
        g_true <- a_structure[i, , , n, pe_i, 1]
        pe_true <- a_PE[i, , , n, pe_i, 1]
        ci_est <- a_CI[i, , , , n, pe_i, m+1]

        # Sensitivity
        a_sen[i, n, pe_i, m] <- mean(g_est[g_true==1])
        # Precision
        a_pre[i, n, pe_i, m] <- mean(g_true[g_est==1])
        # Specificity
        a_spe[i, n, pe_i, m] <- mean(g_est[g_true==0]==0)
        # Coverage
        m_coverage <- (ci_est[, , 1] <= pe_true) & (pe_true <= ci_est[, , 2])
        # [all]
        a_cov[i, n, pe_i, m] <- mean(m_coverage[upper.tri(m_coverage)])
        # [present]
        m_coverage_1 <- m_coverage
        m_coverage_1[g_true==0] <- NA
        a_cov1[i, n, pe_i, m] <- mean(m_coverage_1, na.rm = TRUE)
        # [absent]
        m_coverage_0 <- m_coverage
        m_coverage_0[g_true==1] <- NA
        a_cov0[i, n, pe_i, m] <- mean(m_coverage_0, na.rm = TRUE)

      }
    }
  }
  print(i)
}

# Make list of all outcomes
l_out <- list(a_sen, a_pre, a_spe, a_cov0, a_cov1, a_cov)


# ------------------------------------------------------
# -------- Make Figure: Main Text  ---------------------
# ------------------------------------------------------

# Define Colors
cols <- brewer.pal(8, "Set1")[-6] # avoid yellow, which is hard to see on white background

st_s <- seq(-.35, .35, length=7+1)
v_ord_meth <- c(3, 4, 5, 1, 2, 7, 6) # set ordering of methods
v_legend <- c("OLS", "Lasso", "Multi-split", "DespLasso", "DespLasso (boot)", "EBIC glasso", "Lasso (threshold)")


sc <- .7
pdf("figures/Fig_MainResult_2023_Main.pdf",  width = 10*sc, height = 12*sc)

# ----- Setup layout -----

lmat <- rbind(c(0,1,2),
              c(3,6,7),
              c(4,8,9),
              c(5,10,11))

lo <- layout(lmat,
             widths = c(.1, 1, 1),
             heights = c(.1, 1, 1, 1))

# layout.show(lo)

# ----- Plot Labels -----
cex <- 1.6
plotLab("P(E) = 0.2", cex=cex)
plotLab("P(E) = 0.4", cex=cex)
plotLab("         Sensitivity", cex=cex, rot = 90)
plotLab("         Precision", cex=cex, rot = 90)
# plotLab("         Specificity", cex=cex, rot = 90)
plotLab("         Coverage", cex=cex, rot = 90)

# ----- Plot: Sensitivity -----

par(mar=c(4,2,1,2))

# Loop over outcomes
for(i in c(1,2,6)) {
  for(j in 1:2) {

    # Canvas
    plot.new()
    plot.window(xlim=c(0.5,6.5), ylim=c(-.1,1))
    axis(1, labels = c(50, 100, 200, 500, 800, 1000), at=1:6)
    axis(2, las=2, seq(0, 1, length=6))
    if(i == 4)  title(xlab="Sample size", line=2.5, cex.lab=1.2)
    if(i>3) abline(h=0.95, lty=2, col="grey")


    # Data
    for(m in 1:7) for(it in 1:nIter) points((1:6)+st_s[m], l_out[[i]][it, , j, v_ord_meth[m]],
                                            col = alpha(cols[m], alpha=0.2),
                                            pch=20)


    # Legend
    if(i==1 & j==1) legend("bottomright",
                           legend=v_legend[v_ord_meth],
                           text.col = cols, cex=1.1, bty="n")



  }
}



dev.off()


# ------------------------------------------------------
# -------- Make Figure: Appendix  ----------------------
# ------------------------------------------------------

# Define Colors
cols <- brewer.pal(8, "Set1")[-6] # avoid yellow, which is hard to see on white background

st_s <- seq(-.35, .35, length=7+1)
v_ord_meth <- c(3, 4, 5, 1, 2, 7, 6) # set ordering of methods
v_legend <- c("OLS", "Lasso", "Multi-split", "DespLasso", "DespLasso (boot)", "EBIC glasso", "Lasso (threshold)")


sc <- .7
pdf("figures/Fig_MainResult_2023_App.pdf",  width = 10*sc, height = 12*sc)

# ----- Setup layout -----

lmat <- rbind(c(0,1,2),
              c(3,6,7),
              c(4,8,9),
              c(5,10,11))

lo <- layout(lmat,
             widths = c(.1, 1, 1),
             heights = c(.1, 1, 1, 1))

# layout.show(lo)

# ----- Plot Labels -----
cex <- 1.6
plotLab("P(E) = 0.2", cex=cex)
plotLab("P(E) = 0.4", cex=cex)
plotLab("         Specificity", cex=cex, rot = 90)
plotLab("         Coverage [absent]", cex=cex, rot = 90)
plotLab("         Coverage [present]", cex=cex, rot = 90)

# ----- Plot: Sensitivity -----

par(mar=c(4,2,1,2))

# Loop over outcomes
for(i in c(3,4,5)) {
  for(j in 1:2) {

    # Canvas
    plot.new()
    plot.window(xlim=c(0.5,6.5), ylim=c(-.1,1))
    axis(1, labels = c(50, 100, 200, 500, 800, 1000), at=1:6)
    axis(2, las=2, seq(0, 1, length=6))
    if(i == 4)  title(xlab="Sample size", line=2.5, cex.lab=1.2)
    if(i>3) abline(h=0.95, lty=2, col="grey")


    # Data
    for(m in 1:7) for(it in 1:nIter) points((1:6)+st_s[m], l_out[[i]][it, , j, v_ord_meth[m]],
                                            col = alpha(cols[m], alpha=0.2),
                                            pch=20)


    # Legend
    if(i==3 & j==1) legend("bottomleft",
                           legend=v_legend[v_ord_meth],
                           text.col = cols, cex=1.1, bty="n")



  }
}



dev.off()




