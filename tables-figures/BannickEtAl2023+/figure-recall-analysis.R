# ----------------------------------------------------------------
# FIGURE 2 -- RECALL BIAS
# ----------------------------------------------------------------
# ----------------------------------------------------------------

rm(list=ls())

library(data.table)
library(xtable)
library(utile.visuals)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpattern)
library(ggh4x)
library(ggtext)
library(RColorBrewer)
library(magrittr)
source("~/repos/XSRecency/R/phi-functions.R")
source("~/repos/XSRecency/R/data-generator.R")

# READ IN VERSIONED RESULTS ---------------------------------
# version <- "12-01-2023-06-50-52"
# version <- "21-02-2023-13-46-31"
version <- "2023-05-02-20-28-46" # version with new code

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

# TABLE RESULTS ---------------------------------------------

TRUTH <- summ$truth %>% unique

summ[, tname := ifelse(itype == "constant", "Constant", ifelse(itype == "linear", "Linear", "Exponential"))]
summ[, pname := "Constant"]
summ[, sname := "1B"]
summ[, trange := paste0("(", t_min, ", ", t_max, ")")]

detail[, tname := ifelse(itype == "constant", "Constant", ifelse(itype == "linear", "Linear", "Exponential"))]
detail[, pname := "Constant"]
detail[, sname := "1B"]
detail[, trange := paste0("(", t_min, ", ", t_max, ")")]

vars <- c("q", "gamma", "eta", "xi", "estimator_type", "assay_vals")

detail <- detail[, c(vars, "estimate"), with=F]
summ <- summ[, c(vars, "cover_rob"), with=F]
summ <- summ[xi %in% c(0.0, 0.1) & eta %in% c(0.0, 0.1)]
detail <- detail[xi %in% c(0.0, 0.1) & eta %in% c(0.0, 0.1)]
summ <- summ[assay_vals == "est"]
detail <- detail[assay_vals == "est"]

detail_ref <- detail[estimator_type == "adj" &
                       q == 0.5 & xi == 0.0 & gamma == 0.0 & eta == 0.0]
detail_ref[, q := 0.0]

summ_ref <- summ[estimator_type == "adj" &
                   q == 0.5 & xi == 0.0 & gamma == 0.0 & eta == 0.0]
summ_ref[, q := 0.0]

detail_enh <- detail[estimator_type == "eadj"]
summ_enh <- summ[estimator_type == "eadj"]

detail_plot <- rbind(detail_enh, detail_ref)
summ_plot <- rbind(summ_enh, summ_ref)

# **IMPORTANT**!!
# NOTE ON AUG 17: In the version of the XSRecency code that this version of simulation results was from,
# I switched p_misrep (xi) and d_misrep (eta)
# by accident. Switching the group labels here.

# g1 <- "Correct and complete\nreporting of prior test results"
# g2 <- "10% with positive prior test\nsay they've never been tested"
# g3 <- "10% with positive prior test\nsay it was negative"

g1 <- "Correct and complete\nreporting of prior test results"
g3 <- "10% with positive prior test\nsay they've never been tested"
g2 <- "10% with positive prior test\nsay it was negative"

summ_plot[eta == 0.0 & xi == 0.0, group := g1]
summ_plot[eta == 0.1 & xi == 0.0, group := g3] # These are intentionally in this order
summ_plot[eta == 0.0 & xi == 0.1, group := g2]
detail_plot[eta == 0.0 & xi == 0.0, group := g1]
detail_plot[eta == 0.1 & xi == 0.0, group := g3]
detail_plot[eta == 0.0 & xi == 0.1, group := g2]
summ_plot[estimator_type == "adj", group := "Adjusted estimator"]
detail_plot[estimator_type == "adj", group := "Adjusted estimator"]

summ_plot[, group := factor(group,
                            levels=c("Adjusted estimator",
                                     g1,
                                     g3,
                                     g2))]
summ_plot[, gamma_type := factor(gamma,
                                   levels=c(-99, 0, 0.083, 0.5),
                                   labels=c("No Tests",
                                            "No error",
                                            "1 month",
                                            "6 months"))]
detail_plot[, group := factor(group,
                              levels=c("Adjusted estimator",
                                       g1,
                                       g3,
                                       g2))]

detail_plot[estimator_type == "adj", gamma := -99]
detail_plot[, gamma_type := factor(gamma,
                                   levels=c(-99, 0, 0.083, 0.5),
                                   labels=c("No Tests",
                                            "No error",
                                            "1 month",
                                            "6 months"))]
detail_plot <- detail_plot[q != 1]
cols <- c("#000000", rev(brewer.pal(n=3,"Set1")))
patts <- c("magick", "stripe", "crosshatch", "circle")
summ_plot[, cover_labs := paste0(sprintf("%.1f", cover_rob*100), "%")]

fig <- ggplot(detail_plot) +
  geom_hline(yintercept=TRUTH, color="black", linetype="dashed") +
  geom_boxplot_pattern(aes(x=group,
                           y=estimate,
                           pattern=gamma_type,
                           group=interaction(gamma_type, group)),
                       outlier.size=1.5,
                       outlier.alpha=0.5,
                       pattern_density=0.5,
                       pattern_spacing=0.01,
                       pattern_fill="white",
                       position=position_dodge2(preserve="single",
                                                width=0.8, padding=0.2)) +
  # scale_color_manual(values=cols) +
  scale_pattern_manual(values=patts) +
  labs(y="Incidence Estimate per Person-Year",
       x="",
       pattern="Measurement error in\ntiming of prior test (SD)") +
  theme(legend.position="top",
        text=element_text(size=16))

dt <- ggplot(summ_plot) +
  geom_richtext(size=5, aes(
    group=interaction(gamma_type, group),
    x=group, y=factor(""),
    label=cover_labs),
    show.legend=FALSE,
    label.color=NA,
    inherit.aes=TRUE,
    position=position_dodge2(preserve="single", width=0.8, padding=0.2)
  ) +
  scale_color_manual(values=cols) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, vjust=0.3,
                                  size = 14, face = "italic")) +
  ggtitle("Coverage") +
  theme(panel.grid.major=element_blank(),
        panel.border=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none") +
  xlab(NULL) +
  ylab(NULL)

# Combine plot and table
plot_cmbd <- append_table(
  plot=fig,
  table=dt,
  extract.legend=FALSE
)

# Draw in RStudio viewer
plot <- grid.arrange(plot_cmbd)
ggsave(paste0(outdir, "figure2.tiff"), plot=plot, dpi=600, width=11, height=7, units="in")

dev.off()
