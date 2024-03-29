simulate.pt.gil <- function(n_sims, n, infection.function, phi.func,
                        baseline_incidence, prevalence, rho, bigT, tau,
                        ext_FRR, ext_df=NULL, max_FRR=NULL, last_point=FALSE,
                        ptest.dist=NULL, ptest.prob=1.0,
                        t_min=NULL, t_max=NULL, t_noise=NULL,
                        d_misrep=0.0, q_misrep=0.0, p_misrep=0.0,
                        ptest.dist2=NULL,
                        exclude_pt_bigT=FALSE,
                        t_min_exclude=NULL, logUI=FALSE, remove_pt=FALSE){

  # Generate assay simulations
  cat("Generating assay simulations\n")
  assay.nsim <- XSRecency:::assay.nsim.pt(
    n_sims=n_sims, phi.func=phi.func, tau=tau, bigT=bigT,
    ext_FRR=ext_FRR, ext_df=ext_df,
    max_FRR=max_FRR
  )

  # Generate screening data at time 0 with n people
  cat("Generating screening data\n")
  sim <- sim.screening.generator(
    prevalence=prevalence,
    phi.func=phi.func,
    e.func=function(e) infection.function(e, t=0,
                                          p=prevalence,
                                          lambda_0=baseline_incidence,
                                          rho=rho)
  )
  dfs_screening <- replicate(n_sims, sim(n, return_n=T), simplify=F)

  # Generate prior testing data
  cat("Generating prior testing data\n")
  sim.pt <- sim.pt.generator(
    ptest.dist=ptest.dist,
    ptest.prob=ptest.prob,
    ptest.dist2=ptest.dist2
  )
  pt.dfs <- lapply(dfs_screening, sim.pt)

  if(exclude_pt_bigT){
    if(is.null(t_range)){
      t_range.m <- c(0, bigT)
    } else {
      t_range.m <- c(min(t_range), min(max(t_range), bigT))
    }
  } else if(!is.null(t_min)){
    t_range.m <- c(t_min, t_max)
  }

  if(!is.null(t_min_exclude)){
    exclude.mark <- function(df){
      df.new <- df
      df.new$no.exclude <- ((df.new$ti > t_min_exclude) | (is.na(df.new$ti)))
      return(df.new)
    }
    pt.dfs <- lapply(pt.dfs, exclude.mark)
    ORIG.pt.dfs <- pt.dfs
    exclude <- function(df){
      df.new <- df[df$no.exclude, ]
      return(df.new)
    }
    pt.dfs <- lapply(pt.dfs, exclude)
    ns <- sapply(pt.dfs, nrow)
  } else {
    ns <- rep(n, n_sims)
  }
  print(ns)

  subset <- function(df){
    df.new <- df[df$di == 1,]
    return(df.new)
  }
  pt.dfs <- lapply(pt.dfs, subset)
  n_p <- lapply(pt.dfs, function(x) nrow(x))

  rename <- function(x) setnames(x, c("id", "ui", "ri"))
  lapply(assay.nsim$studies, rename)

  # Remove ui's of 0 from the phi dataframe
  if(logUI){
    remove0 <- function(x) x[x$ui > 0,]
    assay.nsim$studies <- lapply(assay.nsim$studies, remove0)
    formula <- "ri ~ poly(log(ui), 3, raw=TRUE)"
    min_dt <- TRUE
  } else {
    formula <- "ri ~ poly(ui, 3, raw=TRUE)"
    min_dt <- FALSE
  }
  cat("Applying enhanced estimator\n")
  if(remove_pt){
    remove.pt <- function(df){
      df[, "qi"] <- 0
      df[, "ti"] <- NA
      df[, "deltai"] <- NA
      return(df)
    }
    pt.dfs <- lapply(pt.dfs, remove.pt)
  }
  eadj <- mapply(
    FUN=get.adjusted.pt,
    n=ns,
    n_p=n_p,
    ptdf=pt.dfs,
    beta=assay.nsim$beta_sim[1,],
    beta_var=assay.nsim$beta_sim[2,],
    phidat=assay.nsim$studies,
    use_geese=TRUE,
    formula=formula,
    family=replicate(n_sims, binomial(link="logit"), simplify=FALSE),
    big_T=bigT,
    plot_phi=FALSE,
    min_dt=min_dt
  )

  adj <- mapply(
    FUN=get.adjusted,
    n=ns,
    n_p=n_p,
    n_n=eadj["n_n",],
    n_r=eadj["n_r",],
    omega=eadj["omega",],
    omega_var=eadj["omega_var",],
    beta=assay.nsim$beta_sim[1,],
    beta_var=assay.nsim$beta_sim[2,],
    big_T=bigT
  )

  results <- list(
    truth=rep(baseline_incidence, n_sims),
    adj_est_est=unlist(adj["est",]),
    adj_est_var=unlist(adj["var",]),
    eadj_est_est=unlist(eadj["est",]),
    eadj_est_var_rob=unlist(eadj["var",]),
    eadj_est_var_asm=unlist(eadj["var",]),
    omega_est=unlist(eadj["omega",]),
    omega_var=unlist(eadj["omega_var",]),
    beta_est=unlist(assay.nsim$beta_sim[1,]),
    beta_var=unlist(assay.nsim$beta_sim[2,]),
    n_n=unlist(eadj["n_n",]),
    n_r=unlist(eadj["n_r",]),
    n_p=unlist(eadj["n_p",]),
    n_r_pt=unlist(eadj["n_r_pt",]),
    q_eff=unlist(eadj["q_eff",]),
    n=unlist(eadj["n",])
  )
  for(elem in rownames(eadj)){
    results[[elem]] <- unlist(eadj[elem,])
  }

  return(list(results=results, dfs=ORIG.pt.dfs))
}
