# --------------------------------------------------------------
# PIECEWISE LINEAR INCIDENCE FIGURE ----------------------------
# --------------------------------------------------------------

# SET UP DIRECTORIES ----------------------------------------

# Put your root directory for your Hutch path in this file
# It will load a variable called "FHCC" which has the root directory
# Or you can comment this out and just make your own path in this script
source("~/hutch-path.R")

rootdir <- paste0(FHCC, "/HPTN071_RecencyTesting/PriorTestingFiles/")
outdir <- paste0(rootdir, "tables-figures/")

# ANALYSIS -------------------------------------

Year <- seq(2005,2018,1)
Incidence <- c(0,3.8,3.1,3.9,5.4,5.1,5.3,5.1,
               4.6,4.0,3.7,3.3,3.0,4.0)

# Year 2011 - 2018
Year1 = Year[Year>=2011]
Incidence1 = Incidence[Year>=2011]

notrecent <- !Year1 %in% c(2016, 2017, 2018)
interaction <- notrecent * (Year1 - 2016)
mod <- lm(Incidence1 ~ 1 + interaction)
# rho = 0.39 (*10^-2) lambda(2018) = 3.4 -- but set this to be 3.2 to be consistent with the other incidences

set.seed(10)
e <- runif(10000)
constant.inf <- XSRecency:::infections.con(e, t=0, p=0.29, lambda_0=0.032, rho=NA)
linconst.inf <- XSRecency:::infections.lincon(e, t=0, p=0.29, lambda_0=0.032, rho=0.0039, bigT=2)

pdf(paste0(outdir, "/figureA4.pdf"),
    height=5,width=8)
layout(matrix(c(1, 2, 1, 3), ncol=2, byrow=TRUE), c(2, 1), c(1, 1))
plot(Year1,Incidence1/100,xlab='Year',ylab='Incidence')
lines(Year1, rep(0.032, length(Year1)), col='black', lty=2)
lines(Year1, (3.2 + 0.39 * (2018-Year1-2) * (2018-Year1 > 2))/100, col='red', lty=2)
legend('topright',c('Constant',expression('Linear after T*')),lty=rep(2,3),col=c(1,2,4))
hist(-constant.inf, freq=TRUE, xlab="Years Infected",
     main="Constant Incidence", xlim=c(0, 13), breaks=200, border="grey", col='grey')
hist(-linconst.inf, freq=TRUE, xlab="Years Infected",
     main="Constant-Linear\nIncidence", xlim=c(0, 13), breaks=200, border="#ff5454", col='#ff5454')
dev.off()
