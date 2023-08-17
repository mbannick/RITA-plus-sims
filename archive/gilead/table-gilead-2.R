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
version <- "2023-05-16-10-21-05"

# version <- "2023-05-02-19-55-52"
indir <- paste0("~/Documents/FileZilla/xs-recent/enhanced/", version, "/")

summ <- fread(paste0(indir , "/summary.csv"))

# TABLE RESULTS ---------------------------------------------

summ[, tname := ifelse(itype == "constant", "Constant", ifelse(itype == "linear", "Linear", "Exponential"))]
summ[, pname := "Constant"]
summ[, sname := "1B"]
summ[(mech2 == FALSE) & (t_min_exclude == 0), mechtype := "1. Base"]
summ[(mech2 == TRUE) & (t_min_exclude == 0), mechtype := "2. Base + RI"]
summ[(mech2 == FALSE) & (t_min_exclude == 0.25), mechtype := "3. Base + Exclude 3 Mos"]
summ[(mech2 == TRUE) & (t_min_exclude == 0.25), mechtype := "4. Base + RI + Exclude 3 Mos"]

summ[, trange := paste0("(", t_min, ", ", t_max, ")")]

summ <- summ[, .(q, mechtype, t_max, estimator_type, assay_vals, bias, se, mse, cover_rob)]
summ[, bias := bias * 100]
summ[, se := se * 100]
summ[, mse := mse * 1e4]
summ[, cover_rob := cover_rob * 100]

est <- summ[assay_vals == "est"]
est <- summ[estimator_type == "adj"]
est <- summ[t_max == 1.0]

ggplot(data=est, aes(x=q, y=mse, color=mechtype)) + geom_point()
ggplot(data=est, aes(x=q, y=bias, color=mechtype)) + geom_point()
