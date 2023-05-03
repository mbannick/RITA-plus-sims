# ----------------------------------------------------------------
# TABLES AND FIGURES FOR CROSS-SECTIONAL RECENCY ASSAY PERFORMANCE
# FEB 2021
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
version <- "~/Documents/FileZilla/xs-recent/enhanced/2023-04-13-16-11-32"
summ <- fread(paste0(version , "/summary.csv"))
detail <- fread(paste0(version, "/detail.csv"))

# DETAIL RESULTS FOR Q EFF ---------------------------------------------

detail <- detail[, .(q, mech2, t_min_exclude, t_max, q_eff)]
detail <- detail[, lapply(.SD, mean), .SDcols=c("q_eff"), by=c("q", "mech2", "t_min_exclude", "t_max")]

summ <- merge(summ, detail, by=c("q", "mech2", "t_min_exclude", "t_max"))

# TABLE RESULTS ---------------------------------------------

summ[, tname := ifelse(itype == "constant", "Constant", ifelse(itype == "linear", "Linear", "Exponential"))]
summ[, pname := "Constant"]
summ[, sname := "1B"]
summ[(mech2 == FALSE) & (t_min_exclude == 0), mechtype := "Base"]
summ[(mech2 == TRUE) & (t_min_exclude == 0), mechtype := "Base + RI"]
summ[(mech2 == FALSE) & (t_min_exclude == 0.25), mechtype := "Base + Exclude 3 Mos"]
summ[(mech2 == TRUE) & (t_min_exclude == 0.25), mechtype := "Base + RI + Exclude 3 Mos"]

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

EST <- cbind(bias$q, bias$t_max, bias$mechtype,  bias$q_eff, bias$adj, bias$eadj, se$adj, se$eadj, mse$adj, mse$eadj, cover$adj, cover$eadj)
colnames(EST) <- c("q", "t_max", "mechtype", "q_eff",
                   "Bias x 100 (ADJ)", "Bias x 100 (EADJ)",
                   "SE x 100 (ADJ)", "SE x 100 (EADJ)",
                   "MSE x 1E4 (ADJ)", "MSE x 1E4 (EADJ)",
                   "Coverage (ADJ)", "Coverage (EDJ)")

addtorow <- list()
addtorow$pos <- seq(1, nrow(DF), by=4) %>% as.list
addtorow$command <- rep("\\hline \n", length(addtorow$pos))

tab <- xtable(DF, align=rep("c", 8), digits=2)
print(tab, include.rownames=FALSE,
      add.to.row = addtorow)

