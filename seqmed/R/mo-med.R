#' Generate model parameters for causal effects
#' @keywords internal
gen.mo.par <- function(l.mo, v.nm.dv, v.nm.iv, b.exv) {
    l.call <- list(nm = list(dv = v.nm.dv, iv = v.nm.iv), ev = b.exv)
    add.attr(Map(function(mo, b.ev)
                 {
                     m <- model.matrix(mo)
                     v.coef <- coefficients(mo)
                     v.cnm <- c(1, names(v.coef)[-1])
                     m.coef <- matrix(v.coef, nrow = 1, dimnames = list(NULL, v.cnm))
                     colnames(m) <- v.cnm
                     list(coef = m.coef, data = m, ev = b.ev, fam = family(mo))
                 }, l.mo, b.exv),
             l.call)
}

#' Combine model families
#' @keywords internal
combi.mo <- function(v.nm, s.ret) {
    l.bin.resp <- rep(list(c("g", "b")), length(v.nm))
    m.combi <- `colnames<-`(cbind(Reduce(combi, l.bin.resp)), v.nm)
    mat.t.list(m.combi, 1, s.ret)
}
#' Identify solution type
#' @keywords internal
id.soln <- function(v) {
    s.len <- length(v)
    v.g <- which(v == "g")
    v.b <- which(v == "b")
    if(any(length(v.g) == s.len,
           length(v.b) == s.len)) {
        TRUE
    } else {
        !any(mapply(function(s) any(v.g > s), v.b))
    }
}
#' Generate model solution flags
#' @keywords internal
gen.mo.soln <- function(v.nm.dv, flatten = TRUE) {
    l.combi <- combi.mo(v.nm.dv, "v")
    v.soln <- mapply(id.soln, l.combi)
    if(flatten) {
        add.attr(structure(v.soln, .Names = mapply(paste0, l.combi, collapse = "")),
                 list(order = v.nm.dv))
    } else {
        data.frame(do.call(rbind, l.combi), soln = v.soln)
    }
}
#' Update models with bootstrap data
#' @keywords internal
boot.upd.mo <- function(d, l.mo, s.seed) {
    set.seed(s.seed)
    n <- nrow(d)
    d.sample <- d[sample(1:n, n, replace = TRUE),]
    Map(function(mo) update(mo, formula(mo),
                            family = family(mo),
                            data = d.sample), l.mo)
}
#' Model counterfactuals (0 dependencies)
#' @keywords internal
mo.0dep <- function(l.set.x, l.mo.cf, s.nm.mo) {
    l.mo.cf[[s.nm.mo]] <- Map(do.x, l.set.x[[s.nm.mo]])
    l.mo.cf
}
#' Model counterfactuals (1+ dependencies)
#' @keywords internal
mo.1dep <- function(l.set.x, l.mo.cf, s.nm.mo,
                    l.eff.cft, l.eff.cfg,
                    l.dep, l.fam, b.exv) {
    l.cf.def <- l.eff.cft[[s.nm.mo]]
    v.nm.dep <- l.dep[["dep"]][[s.nm.mo]]
    l.dep.len <- list(l.eff.cfg[["len"]][v.nm.dep])
    l.dep.cf <- Map(function(v.eff, v.len)
                    {
                        l.dep.cf.id <- vec.t.list(v.eff[-1], v.len)
                        l.dep.cf <- Map(function(s.nm, v) l.mo.cf[[s.nm]][[paste0(v, collapse = "")]],
                                        names(l.dep.cf.id), l.dep.cf.id)
                        m.dep.cf <- `colnames<-`(unname(do.call(cbind, l.dep.cf)), names(l.dep.cf))
                        m.dep.p <- m.dep.cf
                        m.dep.p[] <- 1
                        list(add.attr(m.dep.cf, list(prob = m.dep.p)))
                    }, l.cf.def, l.dep.len)
    l.dox.mo <- Map(function(l.d, l.x) within(l.x, dep <- l.d),
                    l.dep.cf, l.set.x[[s.nm.mo]])
    rm(l.cf.def, l.dep.len, l.dep.cf)

    b.dep.fam <- `names<-`(do.call(rbind, l.fam)[v.nm.dep,"b"], v.nm.dep)
    b.dep.exv <- b.exv[v.nm.dep]
    b.test <- mapply(all, b.dep.fam, b.dep.exv)
    v.mar <- v.nm.dep[b.test]
    rm(b.dep.fam, b.dep.exv, b.test)

    l.mo.cf[[s.nm.mo]] <- Map(function(l.cf)
                              {
                                  l <- Map(function(m)
                                           {
                                               l.cf[["data"]][,colnames(m)] <- m
                                               do.x(l.cf) * apply(attr(m, "prob"), 1, prod)
                                           }, l.cf[["dep"]])
                                  m <- do.call(cbind, l)
                                  apply(m, 1, sum)
                              }, l.dox.mo)
    l.mo.cf
}
#' Model-based counterfactual estimates
#' @keywords internal
mo.cf.est <- function(l.mo, l.fam, v.nm.dv, v.nm.iv, b.exv,
                      l.dep, l.eff.cfg, l.eff.cft, b.mn = TRUE) {
    l.mo.par <- gen.mo.par(l.mo, v.nm.dv, v.nm.iv, b.exv)
    l.set.x <- set.x.par(l.mo.par, l.eff.cft, v.nm.iv[1])
    l.mo.cf <- Map(function(v) list(), v.nm.dv)
    env <- environment()
    v <- l.dep[["n.sorted"]]
    invisible(Map(function(s.nm.mo, s.n.dep)
                  {
                      if(s.n.dep == 0) {
                          l.cf.i <- mo.0dep(l.set.x, l.mo.cf, s.nm.mo)
                      }
                      if(s.n.dep > 0)  {
                          l.cf.i <- mo.1dep(l.set.x, l.mo.cf, s.nm.mo,
                                            l.eff.cft, l.eff.cfg, l.dep, l.fam, b.exv)
                      }
                      assign("l.mo.cf", l.cf.i, envir = env)
                  }, names(v), v))
    m <- do.call(cbind, l.mo.cf[[1]])

    m.mn <- t(as.matrix(apply(m, 2, mean)))
    if.t(b.mn,
        add.attr(m.mn, list(raw = m)),
        add.attr(m, list(raw = m.mn)))
}
#' Model-based causal effect estimates
#' @keywords internal
mo.ce.est <- function(l.mo, l.fam, v.nm.dv, v.nm.iv, b.exv,
                      l.dep, l.eff.cfg, l.eff.cft,l.eff.def.split) {
    m.cf <- mo.cf.est(l.mo, l.fam, v.nm.dv, v.nm.iv, b.exv,
                      l.dep, l.eff.cfg, l.eff.cft, b.mn = TRUE)
    cf.t.ce(add.attr(m.cf, list(def = l.eff.def.split,
                                fam = l.fam[[1]])))
}
#' Update model for total effects
#' @keywords internal
mo.te.upd <- function(mo, v.nm.m) {
    fml <- paste0(c(". ~ .", paste0("-", v.nm.m)), collapse = " ")
    update(mo, formula = fml, family = family(mo), data = model.frame(mo))
}
#' Estimate total effects
#' @keywords internal
mo.te.est <- function(mo, d, s.nm.r, v.r, b.fam, b.raw = FALSE) {
    m.raw <- mapply(function(s) predict(mo, newdata = set.x(d, s.nm.r, s),
                                        type = "response"), v.r)
    m.mn <- apply(m.raw, 2, mean)
    s.eff <- switch(sw.b(b.fam), ce.dif, ce.odr)(m.mn)
    m.eff <- matrix(s.eff, dimnames = list("est", "te.y"))
    if(b.raw) {
        add.attr(m.eff, list(raw = list(te.y = `colnames<-`(m.raw, as.character(v.r))),
                             fam = b.fam))
    } else {
        m.eff
    }
}

#' Model-based mediation analysis
#'
#' Perform model-based mediation analysis for sequential mediators
#' using bootstrap confidence intervals.
#'
#' @param l.mo List of fitted GLM models.
#' @param s.ci Confidence interval level (default 0.95).
#' @param int.boot Number of bootstrap iterations (default 10000).
#' @param s.seed Random seed.
#' @param b.parallel Parallel computation: TRUE, FALSE, or "auto".
#' @param int.cores Number of cores: integer or "max".
#' @return Matrix of estimates and confidence intervals.
#' @examples
#' \donttest{
#' dd <- dummy.data()
#' # Two-mediator model (continuous outcome, continuous mediators)
#' res <- mo.med(dd$l.mo[[1]], int.boot = 50L, s.seed = 1L,
#'               b.parallel = FALSE)
#' res
#' }
#' @export
mo.med <- function(l.mo,
                   s.ci       = 0.95,
                   int.boot   = 1e4L,
                   s.seed     = sample(-.Machine[["integer.max"]]:.Machine[["integer.max"]], 1),
                   b.parallel = "auto",
                   int.cores  = "max")
{
    # Setup +------------------------------------------------------------+
    ## Environment +-------------------------------------------+
    setup.env(list.models = l.mo,
              int.iter = int.boot,
              b.parallel = b.parallel,
              int.cores = int.cores)
    b.exv <- set.exv(v.nm.dv, rep("auto", n.mo), l.fam)
    ## Set up counterfactuals +--------------------------------+
    l.eff.def <- ce.def(n.med, TRUE)
    l.eff.def.split <- Map(str.half, l.eff.def)
    l.eff.cft <- ce.cft(l.eff.def, v.nm.dv)
    l.eff.cfg <- ce.cfg(n.med, v.nm.dv)
    ## Identify dependencies +---------------------------------+
    l.dep <- get.mo.dep(l.mo)
    if(!any(l.dep[["n"]] == 0)) {
        stop("Error: No solution when all models have dependencies.")
    }
    ## Set seeds and indexes +---------------------------------+
    v.seeds <- gen.seeds(int.boot, s.seed)
    # ++++ # ++++ # ++++ # ++++ # ++++ # ++++ # ++++ # ++++ # +----------+

    # Get total effects formula and causal effects estimate +------------+
    mo.te <- mo.te.upd(l.mo[[1]], v.nm.m)
    m.te <- mo.te.est(mo.te, d, s.nm.r, v.r, l.fam[[1]], TRUE)
    # Identify whether or not model based solution exist +---------------+
    b.soln <- gen.mo.soln(v.nm.dv)[s.fam]
    ## Branch off according to whether a solutuon exist +------+
    if(b.soln) { # Compute the model based solution when there is one.
        m.ce <- mo.ce.est(l.mo, l.fam, v.nm.dv, v.nm.iv, b.exv,
                          l.dep, l.eff.cfg, l.eff.cft,l.eff.def.split)
        l.mo.boot <- c(list(mo.te), l.mo)
        l.ci <- pblapply.sw(b.parallel, int.cores, environment(), v.seeds,
                            function(s.seed.i)
                            {
                                l.mo.boot.upd <- boot.upd.mo(d, l.mo.boot, s.seed.i)
                                l.mo.upd <- l.mo.boot.upd[-1]
                                mo.te.upd <- l.mo.boot.upd[[1]]
                                m.te.upd <- mo.te.est(mo.te.upd, d, s.nm.r, v.r, l.fam[[1]])
                                m.ce.upd <- mo.ce.est(l.mo.upd, l.fam, v.nm.dv, v.nm.iv, b.exv,
                                                      l.dep, l.eff.cfg, l.eff.cft,l.eff.def.split)
                                cbind(m.te.upd, m.ce.upd)
                            })
    } else { # Compute only the total effects when there is no model based solution.
        m.ce <- matrix(NA, ncol = length(l.eff.def), nrow = 1,
                       dimnames = list(NULL, names(l.eff.def)))
        l.mo.boot <- list(mo.te)
        l.ci <- pblapply.sw(b.parallel, int.cores, environment(),
                            v.seeds, function(s.seed.i)
                            {
                                l.mo.boot.upd <- boot.upd.mo(d, l.mo.boot, s.seed.i)
                                mo.te.upd <- l.mo.boot.upd[[1]];
                                mo.te.est(mo.te.upd, d, s.nm.r, v.r, l.fam[[1]])
                            })
    }

    # Gather estimates and confidence intervals +------------------------+
    m.est <- cbind(m.te, m.ce)
    m.ci <- `rownames<-`(apply(do.call(rbind, l.ci),
                               2, \(v) quantile(v, probs = gen.ci(s.ci), type = 7)),
                         paste0("ci.", gsub("\\.", "", gen.ci(s.ci))))
    if(ncol(m.ci) == 1) {
        m.ci <- cbind(m.ci, matrix(NA, ncol = length(l.eff.def), nrow = 2,
                                   dimnames = list(NULL, names(l.eff.def))))
    }
    # ++++ # ++++ # ++++ # ++++ # ++++ # ++++ # ++++ # ++++ # +----------+
    l.attr <- list(seed = v.seeds)
    l.attr.raw <- attributes(m.te)[["raw"]]
    l.attr.m.ce <- attributes(m.ce)
    if(any("raw" == names(l.attr.m.ce))) {
        l.attr.raw <- c(l.attr.raw, l.attr.m.ce[["raw"]])
    }
    add.attr(rbind(m.est, m.ci), c(l.attr, list(raw = l.attr.raw)))
    # +------------------------------------------------------------------+
}
