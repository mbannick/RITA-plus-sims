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
# version <- "2023-04-13-16-11-32"
version <- "2023-05-11-08-59-15"

# version <- "2023-05-02-19-55-52"
indir <- paste0("~/Documents/FileZilla/xs-recent/enhanced/", version, "/")

summ <- fread(paste0(indir , "/summary.csv"))
detail <- fread(paste0(indir, "/detail.csv"))

# DETAIL RESULTS FOR Q EFF ---------------------------------------------

detail <- detail[, .(q, mech2, t_min_exclude, t_max, q_eff)]
detail <- detail[, lapply(.SD, mean), .SDcols=c("q_eff"), by=c("q", "mech2", "t_min_exclude", "t_max")]

summ <- merge(summ, detail, by=c("q", "mech2", "t_min_exclude", "t_max"))

# TABLE RESULTS ---------------------------------------------

summ[, tname := ifelse(itype == "constant", "Constant", ifelse(itype == "linear", "Linear", "Exponential"))]
summ[, pname := "Constant"]
summ[, sname := "1B"]
summ[(mech2 == FALSE) & (t_min_exclude == 0), mechtype := "1. Base"]
summ[(mech2 == TRUE) & (t_min_exclude == 0), mechtype := "2. Base + RI"]
summ[(mech2 == FALSE) & (t_min_exclude == 0.25), mechtype := "3. Base + Exclude 3 Mos"]
summ[(mech2 == TRUE) & (t_min_exclude == 0.25), mechtype := "4. Base + RI + Exclude 3 Mos"]

summ[, trange := paste0("(", t_min, ", ", t_max, ")")]

summ <- summ[, .(q, mechtype, t_max, q_eff, estimator_type, assay_vals, bias, se, mse, cover_rob)]
summ[, bias := bias * 100]
summ[, se := se * 100]
summ[, mse := mse * 1e4]
summ[, cover_rob := cover_rob * 100]
summ[, bias := lapply(bias, function(x) sprintf("%.2f", x))]
summ[, se := lapply(se, function(x) sprintf("%.2f", x))]
summ[, mse := lapply(mse, function(x) sprintf("%.2f", x))]
summ[, cover_rob := lapply(cover_rob, function(x) sprintf("%.2f", x))]
summ[, q_eff := sapply(q_eff, function(x) sprintf("%.2f", x))]

est <- summ[assay_vals == "est"]

bias <- dcast(est, q + t_max + mechtype + q_eff ~ estimator_type, value.var=c("bias"))
se <- dcast(est, q + t_max + mechtype + q_eff ~ estimator_type, value.var=c("se"))
mse <- dcast(est, q + t_max + mechtype + q_eff ~ estimator_type, value.var=c("mse"))
cover <- dcast(est, q + t_max + mechtype + q_eff ~ estimator_type, value.var=c("cover_rob"))

EST <- cbind(bias$q, bias$t_max, bias$mechtype,  bias$q_eff, bias$adj, bias$eadj, se$adj, se$eadj, mse$adj, mse$eadj, cover$adj, cover$eadj)
colnames(EST) <- c("q", "t_max", "mechtype", "q_eff",
                   "Bias x 100 (A)", "Bias x 100 (E)",
                   "SE x 100 (A)", "SE x 100 (E)",
                   "MSE x 1E4 (A)", "MSE x 1E4 (E)",
                   "Coverage (A)", "Coverage (E)")

addtorow <- list()
addtorow$pos <- seq(0, nrow(EST), by=4) %>% as.list
addtorow$command <- rep("\\hline \n", length(addtorow$pos))

tab <- xtable(EST, align=rep("c", 13), digits=2,caption=version)
filename <- paste0(version, ".tex")

write("\\documentclass{article}", file=filename)
write("\\usepackage[legalpaper, landscape, margin=1in]{geometry}", file=filename, append=TRUE)
write("\\begin{document}", file=filename, append=TRUE)
write(print(tab, include.rownames=FALSE,
      add.to.row = addtorow), file=filename, append=TRUE)
write("\\end{document}", file=filename, append=TRUE)

system(paste0("pdflatex ", filename))
