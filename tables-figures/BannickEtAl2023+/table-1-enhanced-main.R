# ----------------------------------------------------------------
# TABLE 1 COMPARISON OF ENHANCED ESTIMATOR V. ADJUSED
# DEC 2022
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

# MAIN VERSION, LAST POINT INTEGRATION + NEW PHI FUNCTION
# version <- "15-12-2022-17-11-12"
version <- "21-02-2023-13-46-31" # version with new code
version <- "2023-05-02-19-48-24" # version with new code (2)
# version <- "2023-05-06-11-46-00" # VERSION WITH LOG UI

# SET UP DIRECTORIES ----------------------------------------

# Put your root directory for your Hutch path in this file
# It will load a variable called "FHCC" which has the root directory
# Or you can comment this out and just make your own path in this script
source("~/hutch-path.R")

rootdir <- paste0(FHCC, "/HPTN071_RecencyTesting/PriorTestingFiles/")
indir <- paste0(rootdir, "simulation-results/", version, "/")
outdir <- paste0(rootdir, "tables-figures/")

# TABLE RESULTS ---------------------------------------------

summ <- fread(paste0(indir , "summary.csv"))

TYPE <- "est"
VARTYPE <- "rob"

summ[, tname := ifelse(itype == "constant", "Constant", ifelse(itype == "linear", "Linear", "Exponential"))]
summ[, pname := "Constant"]
summ[, sname := "1B"]

summ <- summ[assay_vals == TYPE]

summ[, trange := paste0("(", t_min, ", ", t_max, ")")]

adj.mse <- summ[estimator_type == "adj" & q == 1 & trange == "(0, 2)", mse]
summ[, rmse := 1 - (mse / adj.mse)]

if(VARTYPE == "rob"){
  summ[, see := see_rob]
  summ[, cover := cover_rob]
} else {
  summ[, see := see_asm]
  summ[, cover := cover_asm]
}

summ <- summ[, .(trange, q, estimator_type, bias, se, see, mse, rmse, cover)]
summ[, bias := bias * 100]
summ[, se := se * 100]
summ[, see := see * 100]
summ[, mse := mse * 100]
summ[, rmse := rmse * 100]
summ[, cover := cover * 100]
summ[, bias := lapply(bias, function(x) sprintf("%.2f", x))]
summ[, se := lapply(se, function(x) sprintf("%.2f", x))]
summ[, see := lapply(see, function(x) sprintf("%.2f", x))]
summ[, mse := lapply(mse, function(x) sprintf("%.2f", x))]
summ[, rmse := lapply(rmse, function(x) sprintf("%.2f", x))]
summ[, cover := lapply(cover, function(x) sprintf("%.2f", x))]

bias   <- dcast(summ, trange + q ~ estimator_type, value.var=c("bias"))
se     <- dcast(summ, trange + q ~ estimator_type, value.var=c("se"))
see    <- dcast(summ, trange + q ~ estimator_type, value.var=c("see"))
cover  <- dcast(summ, trange + q ~ estimator_type, value.var=c("cover"))
re     <- dcast(summ, trange + q ~ estimator_type, value.var=c("rmse"))

ADJ <- c("--", "--", bias[5,]$adj, se[5,]$adj, see[5,]$adj, cover[5,]$adj, re[5,]$adj)
EST <- cbind(bias$trange, bias$q, bias$eadj, se$eadj, see$eadj, cover$eadj, re$eadj)
DF <- rbind(ADJ, EST)
colnames(DF) <- c("trange", "q", "Bias x 100", "SE x 100", "SEE x 100", "Coverage", "Reduction in Variance")

addtorow <- list()
addtorow$pos <- as.list(c(1, seq(1+5, nrow(DF), by=5)))
addtorow$command <- rep("\\hline \n", length(addtorow$pos))

tab <- xtable(DF, align=rep("c", 8), digits=2)
print(tab, include.rownames=FALSE,
      add.to.row = addtorow)

