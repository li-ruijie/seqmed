#' Generate sensitivity analysis u arguments
#' @keywords internal
gen.u.args <- function(l.mo) {
    attr2env(l.mo)
    m.u.cmbn <- gen.u.cmbn(v.nm.dv)
    v.u.dir <- get.u.dir(l.mo, m.u.cmbn, v.nm.dv)
    m.u.b <- matrix(0, nrow(m.u.cmbn), ncol(m.u.cmbn), dimnames = dimnames(m.u.cmbn))
    v.res.var <- get.var.res(l.mo)
    m.res.var <- apply(m.u.cmbn, 2, function(x) v.res.var[x])
    v.res.sdp <- apply(m.res.var, 2, function(x) prod(sqrt(x)))
    as.list(environment())
}
#' Generate u beta from variance and covariance
#' @keywords internal
gen.u.beta <- function(v.var, s.cov) {
    s.ratio <- v.var[1]/v.var[2]
    s.var <- sqrt(s.cov/s.ratio)
    structure(c(s.var * s.ratio, s.var), .Names = names(v.var))
}
#' Generate u beta matrix
#' @keywords internal
gen.u.beta.m <- function(s.cor, m.u.b,
                         m.res.var, v.res.sdp, v.u.dir) {
    n.u <- ncol(m.u.b)
    m.u.b[] <- mapply(function(n) gen.u.beta(m.res.var[,n], s.cor * v.res.sdp[n]), 1:n.u)
    m.u.b[1,] <- m.u.b[1,] * v.u.dir
    m.u.b
}

#' Get direction of u
#' @keywords internal
get.u.dir <- function(l.mo, m.cmbn, v.nm.dv) {
    v <- apply(m.cmbn, 2, function(x)
               {
                   v.coef <- coef(l.mo[[min(which(v.nm.dv %in% x))]])
                   na.omit(v.coef[x]) > 0
               })
    (v - 0.5)/0.5
}
#' Get residual variance
#' @keywords internal
get.var.res <- function(l.mo) {
    attr2env(l.mo)
    s.var.lgd <- (pi^2)/3
    mapply(function(s.nm, mo, v)
        switch(sw.b(v), var(residuals(mo)), s.var.lgd),
        v.nm.dv, l.mo, get.fam.l(l.mo))
}
#' Get maximum correlation (single step)
#' @keywords internal
get.sa.maxcor.step <- function(v.step.int, s.cor.start,
                               m.u.b, m.res.var, v.res.sdp, v.u.dir,
                               l.mo, v.nm.dv, v.nm.iv) {
    v.step.cur <- ((0:10) * v.step.int) + s.cor.start
    s.step.cur.len <- length(v.step.cur)
    ind.step.cur <- 1
    b.cont <- TRUE
    while(b.cont) {
        s.cor <- v.step.cur[ind.step.cur]
        m.u.test <- gen.u.beta.m(s.cor, m.u.b, m.res.var, v.res.sdp, v.u.dir)
        mp.out <- fit.mp(l.mo, m.u.test, v.nm.dv, v.nm.iv,
                         int.cores = parallel::detectCores(), s.iter = 1e4L)
        b.err <- attr(mp.out[["raw"]], "warn")[["b.warn"]]
        b.ind <- ind.step.cur == s.step.cur.len
        b.cont <- !any(b.err, b.ind)
        if(b.err) {
            s.cor <- v.step.cur[ind.step.cur - 1]
        }
        ind.step.cur <- ind.step.cur + 1
    }
    s.cor
}
#' Get maximum correlation (all steps)
#' @keywords internal
get.sa.maxcor.allsteps <- function(s.dec, m.u.b,
                                   m.res.var, v.res.sdp, v.u.dir,
                                   l.mo, v.nm.dv, v.nm.iv) {
    v.step <- 10^-(1:s.dec)
    s.cor.start <- 0
    for(s.step in v.step) {
        if(s.cor.start >= 1) break()
        s.cor.start <- get.sa.maxcor.step(s.step, s.cor.start,
                                          m.u.b, m.res.var, v.res.sdp, v.u.dir,
                                          l.mo, v.nm.dv, v.nm.iv)
    }
    if(s.cor.start == 1) {
        s.cor.start <- 0.95
    }
    s.cor.start
}
#' Get maximum correlation
#' @keywords internal
get.sa.mcor <- function(s.dec, l.mo, v.nm.dv, v.nm.iv) {
    list2env(gen.u.args(l.mo), environment())
    get.sa.maxcor.allsteps(s.dec, m.u.b,
                           m.res.var, v.res.sdp, v.u.dir,
                           l.mo, v.nm.dv, v.nm.iv)
}
#' Generate sensitivity analysis test sequence
#' @keywords internal
gen.sa.testseq <- function(s.mcor, s.dec) {
    v.test <- c(5, 10)
    s.fac <- 10^s.dec
    v.div <- (abs(s.mcor) * s.fac) %/% v.test
    v.div.use <- switch(sw.b(v.div[1] < 10), c(5, v.div[1]), c(10, v.div[2]))
    v.seq <- seq(from = 0, by = v.div.use[1], length.out = v.div.use[2] + 1) / s.fac
    if(s.mcor < 0) v.seq <- -v.seq
    v.seq
}

#' Sensitivity analysis for simulation-based mediation
#'
#' Perform sensitivity analysis for simulation-based mediation analysis
#' by varying the correlation between residuals.
#'
#' @param l.mo List of fitted GLM models.
#' @param s.cof.mth Coefficient simulation method: "mvn" or "boot".
#' @param s.ci Confidence interval level (default 0.95).
#' @param int.sims Number of simulations (default 10).
#' @param s.seed Random seed.
#' @param b.parallel Parallel computation: TRUE, FALSE, or "auto".
#' @param int.cores Number of cores: integer or "max".
#' @return List of simulation results at each correlation level.
#' @examples
#' \dontrun{
#' # Requires Mplus software and the MplusAutomation package
#' dd <- dummy.data()
#' res <- sim.med.sa(dd$l.mo[[1]], int.sims = 10L, s.seed = 1L,
#'                   b.parallel = FALSE)
#' }
#' @export
sim.med.sa <- function(l.mo,
                       s.cof.mth = "mvn",
                       s.ci = 0.95,
                       int.sims = 1e1L,
                       s.seed = gen.seeds(1),
                       b.parallel = "auto",
                       int.cores = "max")
{
    setup.env(l.mo,
              int.sims,
              b.parallel,
              int.cores)
    list2env(gen.u.args(l.mo),
             envir = environment())
    s.dec <- 2

    message("Working out maximum correlation between residuals...")
    s.mcor <- get.sa.mcor(s.dec,
                          l.mo,
                          v.nm.dv,
                          v.nm.iv)
    message(paste0("    Maximum correlation : ", s.mcor))

    v.seq <- gen.sa.testseq(s.mcor, s.dec)
    l.m.u <- Map(function(s.cor)
                 gen.u.beta.m(s.cor,
                              m.u.b,
                              m.res.var,
                              v.res.sdp,
                              v.u.dir),
                 v.seq)
    `attributes<-`(Map(function(m, s, s.len)
                       {
                           v.msg <- paste0("Run ", s, " of ", s.len, " runs.")
                           message(v.msg)
                           sim.med(l.mo,
                                   s.mode = "sa",
                                   m.b = m,
                                   s.cof.mth,
                                   s.ci,
                                   int.sims,
                                   s.seed,
                                   b.parallel,
                                   int.cores,
                                   b.raw = FALSE)
                       },
                       l.m.u,
                       seq_along(l.m.u),
                       length(l.m.u)),
      list(cor = v.seq,
           max.cor = s.mcor))
}
