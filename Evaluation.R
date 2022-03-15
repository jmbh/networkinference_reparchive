# jonashaslbeck@gmail.com; March 15, 2022

# ------------------------------------------------------
# -------- Load Packages & Source ----------------------
# ------------------------------------------------------

library(RColorBrewer)
library(scales)
source("aux_functions.R") # for label plotting function

# ------------------------------------------------------
# -------- Load Results --------------------------------
# ------------------------------------------------------

simResDir <- "output/results/"

v_files <- list.files(simResDir)
n_files <- length(v_files)
l_files <- list()
a_results <-  array(NA, dim=c(n_files, 20, 20, 6, 2, 8)) # iter, pxp, n-var, Pe-var, true+7methods
for(i in 1:n_files) a_results[i, , , , , ] <- readRDS(paste0(simResDir, v_files[i]))


# ------------------------------------------------------
# -------- Pre-processing ------------------------------
# ------------------------------------------------------

# Compute Sensitivity and Precision
nIter <- n_files

a_sen <- a_pre <- array(NA, dim = c(nIter, 6, 2, 7))

for(i in 1:nIter) {
  for(n in 1:6) {
    for(pe_i in 1:2) {
      for(m in 1:7) { # methods

        # Get Estimated and True Graph
        g_est <- a_results[i, , , n, pe_i, m+1]
        g_true <- a_results[i, , , n, pe_i, 1]

        # Sensitivity
        a_sen[i, n, pe_i, m] <- mean(g_est[g_true==1])
        # Precision
        a_pre[i, n, pe_i, m] <- mean(g_true[g_est==1])

      }
    }
  }
}


# ------------------------------------------------------
# -------- Make Figure  --------------------------------
# ------------------------------------------------------

# Define Colors
cols <- brewer.pal(8, "Set1")[-6] # avoid yellow, which is hard to see on white background

st_s <- seq(-.35, .35, length=7+1)
v_ord_meth <- c(3, 4, 5, 1, 2, 7, 6) # set ordering of methods
v_legend <- c("OLS", "Lasso", "Multi-split", "DespLasso", "DespLasso (boot)", "EBIC glasso", "Lasso (threshold)")


sc <- .8
pdf("figures/Fig_MainResult.pdf",  width = 10*sc, height = 7.7*sc)

# ----- Setup layout -----

lmat <- rbind(c(0,1,2),
              c(3,5,7),
              c(4,6,8))

lo <- layout(lmat,
             widths = c(.1, 1, 1),
             heights = c(.1, 1, 1))


# ----- Plot Labels -----
cex <- 1.45
plotLab("Sensitivity", cex=cex)
plotLab("Precision", cex=cex)
plotLab("         P(E) = 0.2", rot = 90, cex=cex)
plotLab("         P(E) = 0.4", rot = 90, cex=cex)

# ----- Plot Data -----

par(mar=c(4,2,1,2))

for(i in 1:4) {

  plot.new()
  plot.window(xlim=c(0.5,6.5), ylim=c(-.1,1))
  axis(1, labels = c(50, 100, 200, 500, 800, 1000), at=1:6)
  axis(2, las=2, seq(0, 1, length=6))
  if(i %in% c(2,4))  title(xlab="Sample size", line=2.5, cex.lab=1.2)

  # Loop over methods
  if(i %in% 1:2) a_res <- a_sen else a_res <- a_pre
  if(i %in% c(1,3)) pe_i <- 1 else pe_i <- 2
  for(m in 1:7) for(i in 1:nIter) points((1:6)+st_s[m], a_res[i, , pe_i, v_ord_meth[m]],
                                         col = alpha(cols[m], alpha=0.2),
                                         pch=20)

} # end for

# ----- Plot Legend -----

legend("bottomright",
       legend=v_legend[v_ord_meth],
       text.col = cols, cex=1.1, bty="n")

dev.off()








