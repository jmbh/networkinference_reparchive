# jonashaslbeck@gmail.com; March 15, 2022

# ------------------------------------------------------
# -------- What is this about? -------------------------
# ------------------------------------------------------

# Visualize biased / unbiased sampling distributions of parameters at/close to zero

# ------------------------------------------------------
# -------- Load packages -------------------------------
# ------------------------------------------------------

library(MASS)
library(mgm)
library(corpcor)

# ------------------------------------------------------
# -------- Generate Data -------------------------------
# ------------------------------------------------------

p <- 20 # number of variables

# define mean vector
mu <- rep(0, p)

# define covariance matrix
Sigma <- diag(p)
Sigma[1,2] <- Sigma[2,1] <- 0.1
Sigma[3,4] <- Sigma[4,3] <- 0.2
Sigma[5,6] <- Sigma[6,5] <- 0.3
Sigma[8,9] <- Sigma[9,8] <- 0.4
Sigma[10,11] <- Sigma[11,10] <- 0.5
Sigma[19,20] <- Sigma[20,19] <- 0.6
Sigma[12,13] <- Sigma[13,12] <- 0.7
Sigma[14,15] <- Sigma[15,14] <- 0.8
Sigma[16,17] <- Sigma[17,16] <- 0.9
N <- 500

set.seed(1)
data <- mvrnorm(n = N, mu = mu, Sigma = Sigma, empirical = TRUE)
round(cor(data), 3)

# ------------------------------------------------------
# -------- Bootstrap Sampling distribution -------------
# ------------------------------------------------------

# (This runs for a while)

# out_mgm <- mgm(data = data,
#                type = rep("g", p),
#                level = rep(1, p),
#                lambdaSel = "EBIC")
#
# out_mgm$pairwise$wadj
#
# B <- 1000
# out_res <- resample(object = out_mgm,
#                     data = data,
#                     nB = B)

# # I can't share this file, because it exceeds the Github limit for file sizes
# # saveRDS(out_res, "out_resB1000.RDS")
# # out_res <- readRDS("files/out_resB1000.RDS")

# ------------------------------------------------------
# -------- Compute CIs ---------------------------------
# ------------------------------------------------------

alpha <- 0.05
probs <- c(alpha/2, 1-alpha/2)

# 18-19 (0)
bs_0 <- out_res$bootParameters[18,19,]
quantile(bs_0, probs = probs)

# 1-2 (0.1)
bs_0.1 <- out_res$bootParameters[1,2,]
quantile(bs_0.1, probs = probs)

# 3-4 (0.2)
bs_0.2 <- out_res$bootParameters[3,4,]
quantile(bs_0.2, probs = probs)

# 5-6 (0.3)
bs_0.3 <- out_res$bootParameters[5,6,]
quantile(bs_0.3, probs = probs)

# 8-9 (0.4)
bs_0.4 <- out_res$bootParameters[8,9,]
quantile(bs_0.4, probs = probs)

# 10-11 (0.5)
bs_0.5 <- out_res$bootParameters[10,11,]
quantile(bs_0.5, probs = probs)

# 12-13 (0.7)
bs_0.7 <- out_res$bootParameters[12,13,]
quantile(bs_0.7, probs = probs)

# 14-15 (0.8)
bs_0.8 <- out_res$bootParameters[14,15,]
quantile(bs_0.8, probs = probs)

# 16-17 (0.9)
bs_0.9 <- out_res$bootParameters[16,17,]
quantile(bs_0.9, probs = probs)


# ------------------------------------------------------
# -------- Get Unbiased Sampling distributions ---------
# ------------------------------------------------------

B <- 1000
a_unbiased_pcor <- array(NA, dim=c(p, p, B))

for(b in 1:B) {
        bs <- sample(1:N, size=N, replace=T)
        corm <- cor(data[bs,])
        pcorm <- cor2pcor(corm)
        a_unbiased_pcor[, , b] <- pcorm
        print(b)
}

bs_0_ub <- a_unbiased_pcor[18,19,]
bs_0.2_ub <- a_unbiased_pcor[3,4,]
bs_0.5_ub <- a_unbiased_pcor[10,11,]



# ------------------------------------------------------
# -------- Make Figure ---------------------------------
# ------------------------------------------------------

# Put the distributions into lists
l_BSd <- list(bs_0, bs_0.2, bs_0.5,
              bs_0_ub, bs_0.2_ub, bs_0.5_ub)

n_breaks <- 15
max_y <- 0.6

sc <- 0.6
pdf("figures/Fig_SampDist.pdf", width = 10*sc, height = 7*sc)

# ----- Setup Layout ------

lmat <- rbind(c(0:3),
              c(4,6:8),
              c(5,9:11))
lo <- layout(mat = lmat,
             widths = c(0.09, 1,1,1),
             heights = c(0.09, 1, 1))

# ----- Add Labels ------

# Function for labels
fLabel <- function(tex, srt=0, cex=1.3, x=0.5, y=0.5) {
        par(mar=rep(0,4))
        plot.new()
        plot.window(xlim=c(0,1), ylim=c(0,1))
        text(x, y, labels = tex, srt=srt, cex=cex)
}

# Top labels
fLabel(expression(paste(rho, " = 0")), x=.55)
fLabel(expression(paste(rho, " = 0.2")), x=.55)
fLabel(expression(paste(rho, " = 0.5")), x=.55)

# Side Labels
fLabel("Biased", srt=90, x=.55, y=.6)
fLabel("Unbiased", srt=90, x=.55, y=0.6)


# ----- Loop through Panels ------

par(mar=c(4,3,1,1))

v_ab <- rep(c(0, .2, .5), 2)

for(i in 1:6) {

        # Conditional axis labels
        if(i %in% c(1,4)) ylab <- "" else ylab = ""
        if(i %in% c(4:6)) xlab <- expression(hat(rho)) else xlab = ""

        # Plot histograms
        hist(l_BSd[[i]],
             main="",
             axes=FALSE,
             breaks=seq(-.2, 0.6, length=50),
             xlim=c(-.2, max_y),
             xlab = xlab,
             ylab = ylab, border=FALSE, col="darkgrey")
        axis(1, seq(-0.2, .6, length=9))
        axis(2, las=2)

        abline(v=v_ab[i], lty=2, col="black")



}


dev.off()

