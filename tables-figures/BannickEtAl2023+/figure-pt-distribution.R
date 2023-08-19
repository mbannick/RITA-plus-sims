# ----------------------------------------------------------
# FIGURE A3: DISTRIBUTION OF TIME TO HIV TEST FROM INFECTION
# ----------------------------------------------------------

rm(list=ls())

library(data.table)
library(flexsurv)
library(gridExtra)
library(magrittr)
library(ggplot2)

# SET UP DIRECTORIES ----------------------------------------

# Put your root directory for your Hutch path in this file
# It will load a variable called "FHCC" which has the root directory
# Or you can comment this out and just make your own path in this script
source("~/hutch-path.R")

rootdir <- paste0(FHCC, "/HPTN071_RecencyTesting/PriorTestingFiles/")
outdir <- paste0(rootdir, "tables-figures/")

# ANALYSIS ------------------------

x <- seq(0, 10, by=0.01)

# Data from Maheau-Giroux et al. 2017; extracted via an x-y point from figure extractor online
dat <- fread(paste0(rootdir, "X-Y Points from Maheau-Giroux 2017 Figure 1a.csv"))
setorder(dat, x, y)
plot(dat$y ~ dat$x)

# GENERALIZED GAMMA

ss <- function(vec){
  y <- dat$y / 100
  cdf <- pgengamma(dat$x, mu=vec[1], sigma=vec[2], Q=vec[3])
  plot(cdf ~ y, type='p')
  sum((cdf - y)**2)
}
par(mfrow=c(4, 4))
res <- optim(par=c(0, 0, 0), ss)
pars <- res$par

par(mfrow=c(1,1))
plot(pgengamma(x, mu=pars[1], sigma=pars[2], Q=pars[3]) ~ x, type='l')
points(dat$y / 100 ~ dat$x)

plot(dgengamma(x, mu=pars[1], sigma=pars[2], Q=pars[3]) ~ x, type='l')

# SIMULATION

set.seed(10)
n <- 10000

sim <- function(a){
  us <- runif(n, min=0, max=10)
  t1 <- runif(n, 0, 4)
  as <- rbinom(n, size=1, prob=a)
  t2 <- us - rgengamma(n=n, mu=pars[1], sigma=pars[2], Q=pars[3])
  take_t1 <- (((t1 < t2) | (t2 < 0)) & as == 1)
  t <- ifelse(
    take_t1,
    t1,
    t2
  )
  q <- t >= 0
  return(data.table(u=us, t1=t1, t2=t2, take_t1, a=as, pa=a, t=t, q=q))
  # return(c(mean(t[q==1]), mean(q)))
}

dat1 <- sim(a=0.4)
dat2 <- sim(a=0.8)

dat <- rbind(dat1, dat2)

dat[, type := ifelse(q == 0,
                     "unavailable",
                     ifelse(a == 0,
                     "no t1",
                     "available t1"))]
dat[, type := factor(type,
                     levels=c("available t1",
                              "no t1",
                              "unavailable"))]
dat[, used_t1 := ifelse(take_t1, "Routine", "Post-Infection")]
dat[, pa_lab := ifelse(pa == 0.4, "40% with Routine Test", "80% with Routine Test")]

p1 <- ggplot(dat[q == 1]) +
  geom_point(aes(x=u, y=t, color=used_t1), alpha=0.2) +
  geom_smooth(aes(x=u, y=t), method="loess", formula="y~x") +
  facet_wrap(~pa_lab) +
  ggtitle("Prior Test Time by Infection Duration") +
  labs(x="Infection Duration",
       y="Time Since Test",
       color="Test Mechanism Used") +
  theme(legend.position="bottom")

p2 <- ggplot(dat) +
  geom_smooth(aes(x=u, y=as.numeric(q)), method="loess", formula="y~x") +
  facet_wrap(~pa_lab) +
  labs(x="Infection Duration",
       y="Proportion with Test Available") +
  ggtitle("Proportion with Prior Test by Infection Duration")

pdf(paste0(outdir, "/figureA3.pdf"),
    height=10, width=10)
grid.arrange(p1, p2, nrow=2)
dev.off()
