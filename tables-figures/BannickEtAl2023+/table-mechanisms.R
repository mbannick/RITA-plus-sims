# ----------------------------------------------------------------
# Multiple mechanisms of HIV testing
# ----------------------------------------------------------------
# ----------------------------------------------------------------

rm(list=ls())

library(data.table)
library(xtable)
library(ggplot2)
library(ggh4x)
library(RColorBrewer)
library(magrittr)
source("~/repos/XSRecency/R/phi-functions.R")
source("~/repos/XSRecency/R/data-generator.R")

# READ IN VERSIONED RESULTS ---------------------------------

# version <- "~/Documents/FileZilla/xs-recent/enhanced/21-12-2022-12-41-07/"
# version <- "~/Documents/FileZilla/xs-recent/enhanced/13-03-2023-21-32-22"
# version <- "~/Documents/FileZilla/xs-recent/enhanced/2023-04-04-08-48-07"
# version <- "~/Documents/FileZilla/xs-recent/enhanced/2023-04-13-16-11-32"

version <- "2023-05-02-19-55-52"

# SET UP DIRECTORIES ----------------------------------------

# Put your root directory for your Hutch path in this file
# It will load a variable called "FHCC" which has the root directory
# Or you can comment this out and just make your own path in this script
source("~/hutch-path.R")

rootdir <- paste0(FHCC, "/HPTN071_RecencyTesting/PriorTestingFiles/")
indir <- paste0(rootdir, "simulation-results/", version, "/")
outdir <- paste0(rootdir, "tables-figures/")

summ <- fread(paste0(indir , "/summary.csv"))
detail <- fread(paste0(indir, "/detail.csv"))

# DETAIL RESULTS FOR Q EFF ---------------------------------------------

detail <- detail[, .(q, mech2, t_max, q_eff)]
detail <- detail[, lapply(.SD, mean), .SDcols=c("q_eff"), by=c("q", "mech2", "t_max")]

summ <- merge(summ, detail, by=c("q", "mech2", "t_max"))

# TABLE RESULTS ---------------------------------------------

summ[, tname := ifelse(itype == "constant", "Constant", ifelse(itype == "linear", "Linear", "Exponential"))]
summ[, pname := "Constant"]
summ[, sname := "1B"]
summ[(mech2 == FALSE), mechtype := "Base"]
summ[(mech2 == TRUE), mechtype := "Base + RI"]

summ[, trange := paste0("(", t_min, ", ", t_max, ")")]

summ <- summ[, .(q, mechtype, t_max, q_eff, estimator_type, assay_vals, bias, se, mse, cover_rob)]
summ[, bias := bias * 100]
summ[, se := se * 100]
summ[, mse := mse * 1e4]
summ[, cover_rob := cover_rob * 100]
summ[, bias := lapply(bias, function(x) sprintf("%.3f", x))]
summ[, se := lapply(se, function(x) sprintf("%.3f", x))]
summ[, mse := lapply(mse, function(x) sprintf("%.3f", x))]
summ[, cover_rob := lapply(cover_rob, function(x) sprintf("%.3f", x))]
summ[, q_eff := sapply(q_eff, function(x) sprintf("%.3f", x))]

est <- summ[assay_vals == "est"]

bias <- dcast(est, q + t_max + mechtype + q_eff ~ estimator_type, value.var=c("bias"))
se <- dcast(est, q + t_max + mechtype + q_eff ~ estimator_type, value.var=c("se"))
mse <- dcast(est, q + t_max + mechtype + q_eff ~ estimator_type, value.var=c("mse"))
cover <- dcast(est, q + t_max + mechtype + q_eff ~ estimator_type, value.var=c("cover_rob"))

ADJ <- c("--", "--", "--", bias[1,]$adj, se[1,]$adj, mse[1,]$adj, cover[1,]$adj)
EST <- cbind(bias$q, bias$mechtype,  bias$q_eff, bias$eadj, se$eadj, mse$eadj, cover$eadj)
DF <- rbind(unlist(ADJ), EST)
colnames(DF) <- c("q", "mechtype", "q_eff", "Bias x 100", "SE x 100", "MSE x 100", "Coverage")

addtorow <- list()
addtorow$pos <- seq(1, nrow(DF), by=2) %>% as.list
addtorow$command <- rep("\\hline \n", length(addtorow$pos))

tab <- xtable(DF, align=rep("c", 8), digits=2)
print(tab, include.rownames=FALSE,
      add.to.row = addtorow)


