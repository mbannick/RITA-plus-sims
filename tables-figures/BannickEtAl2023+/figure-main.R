# FIGURE 1: ENHANCED V. ADJUSTED ESTIMATOR

rm(list=ls())

library(data.table)
library(xtable)
library(ggplot2)
library(utile.visuals)
library(ggpattern)
library(viridis)
library(grid)
library(gridExtra)
library(ggtext)
library(ggh4x)
library(RColorBrewer)
library(magrittr)
source("~/repos/XSRecency/R/phi-functions.R")
source("~/repos/XSRecency/R/data-generator.R")

# READ IN VERSIONED RESULTS ---------------------------------
# version <- "15-12-2022-17-11-12"
# version <- "21-02-2023-13-46-31" # version with new code
version <- "2023-05-02-19-48-24" # version with new code (2)

# SET UP DIRECTORIES ----------------------------------------

# Put your root directory for your Hutch path in this file
# It will load a variable called "FHCC" which has the root directory
# Or you can comment this out and just make your own path in this script
source("~/hutch-path.R")

rootdir <- paste0(FHCC, "/HPTN071_RecencyTesting/PriorTestingFiles/")
indir <- paste0(rootdir, "simulation-results/", version, "/")
outdir <- paste0(rootdir, "tables-figures/")

# READ IN FILES ---------------------------------------------

df <- fread(paste0(indir, "detail.csv"))
summ <- fread(paste0(indir, "summary.csv"))
adj.mse <- summ[estimator_type == "adj" & q == 1 & t_min == 0 & t_max == 2, mse]
summ[, rmse := (1 - (mse / adj.mse))*100]
summ <- summ[assay_vals == "est" & (estimator_type == "eadj" | (estimator_type == "adj" & t_min == 0 & t_max == 2 & q == 1))]

TRUTH <- df$truth %>% unique

df[, tname := ifelse(itype == "constant", "Constant", ifelse(itype == "linear", "Linear", "Exponential"))]
df[, pname := "Constant"]
df[, sname := "1B"]

trange_levels <- c("2-4 Yrs", "0-4 Yrs", "0-2 Yrs")
df[, trange := paste0(t_min, "-", t_max, " Yrs")]
df[, trange := factor(trange, levels=trange_levels)]
summ[, trange := paste0(t_min, "-", t_max, " Yrs")]
summ[, trange := factor(trange, levels=trange_levels)]

# Get reference values for the adjusted estimator
ref_df <- df[estimator_type == "adj" & assay_vals == "est"
             & q == 0.2 & trange == "0-2 Yrs"]
ref_df[, q := 0.0]
ref_df[, trange := "No Testing"]

enh_df <- df[estimator == "eadj_est"]

plot_df <- rbind(enh_df, ref_df)
plot_df[, tlabs := factor(trange,
                          levels=c("No Testing", trange_levels),
                          ordered=TRUE)]
plot_df[, id := paste0(estimator_type, q, tlabs, sep="_")]
plot_df[, qlabs := factor(
  q, levels=c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
  labels=c("Standard\nEstimator", "20%", "40%", "60%", "80%", "100%"))
]
summ[, qlabs := factor(
  q, levels=c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
  labels=c("Standard\nEstimator", "20%", "40%", "60%", "80%", "100%"))
]
summ[, tlabs := factor(trange,
                          levels=c("No Testing", trange_levels),
                          ordered=TRUE)]
summ[estimator_type == "adj", qlabs := "Standard\nEstimator"]
summ[estimator_type == "adj", tlabs := "No Testing"]
summ[, rmse_labs := paste0(sprintf("%.0f", rmse), "%")]

# UPDATE FROM Co-AUTHOR REVISIONS: remove 0%
summ[rmse_labs == "0%", rmse_labs := ""]

cols <- c("#000000", brewer.pal(n=3,"Set2"))
patts <- c("magick", "stripe", "crosshatch", "circle")

fig <- ggplot(plot_df) +
  geom_hline(yintercept=TRUTH, color="black", linetype="dashed") +
  geom_boxplot_pattern(aes(x=qlabs,
                 y=estimate,
                 pattern=tlabs,
                 group=interaction(tlabs, q)),
               outlier.size=1.5,
               outlier.alpha=0.5,
               pattern_density=0.5,
               pattern_spacing=0.01,
               pattern_fill="white",
               position=position_dodge2(preserve="single",
                                        width=0.8, padding=0.2)) +
  # scale_color_manual(values=cols) +
  scale_pattern_manual(values=patts) +
  labs(x="Proportion with Tests Available",
       y="Incidence Estimate per Person-Year",
       pattern="Range of Prior Testing Times") +
  theme(legend.position="top",
        text=element_text(size=16))

dt <- ggplot(summ) +
  geom_richtext(size=5, aes(
            group=interaction(tlabs, q),
            x=qlabs, y=factor(""),
            #color=tlabs,
            label=rmse_labs),
            show.legend=FALSE,
            label.color=NA,
            inherit.aes=TRUE,
            position=position_dodge2(preserve="single", width=0.8, padding=0.2)
  ) +
  scale_color_manual(values=cols) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, vjust=0.3,
                                  size = 14, face = "italic")) +
  ggtitle("% Reduction in MSE Compared to Standard Estimator") +
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
ggsave(paste0(outdir, "figure1.tiff"), plot=plot, dpi=600, width=11, height=7, units="in")

# UPDATE FROM CO-AUTHOR COMMENTS

# Range of estimates from standard estimator
df[estimator == "adj_est", estimate] %>% range

# MSE of standard estimator
summ[estimator_type == "adj", mse]
