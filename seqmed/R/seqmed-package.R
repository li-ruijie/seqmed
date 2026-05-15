#' @importFrom methods is
#' @importFrom stats binomial coef coefficients confint cov family formula
#'   gaussian glm lm model.frame model.matrix na.omit predict quantile
#'   residuals rlogis rnorm sd update var vcov
#' @importFrom utils combn head tail write.table
#' @keywords internal
"_PACKAGE"

# Variables injected by setup.env(), attr2env(), list2env(), list.var()
utils::globalVariables(c(
    "b.exv", "b.parallel", "d", "index", "int.cores", "int.sims",
    "l.dep", "l.eff.cfg", "l.eff.cft", "l.fam", "l.mo", "l.mo.par",
    "l.nm.av", "m.b", "m.res.var", "m.u.b",
    "mo.m1", "mo.m2m1", "mo.ym1", "mo.ym1m2",
    "n.med", "n.mo", "s.cof.mth", "s.fam", "s.mode", "s.nm.r",
    "s.seed", "v.nm.cv", "v.nm.dv", "v.nm.iv", "v.nm.m", "v.r",
    "v.res.sdp", "v.seeds", "v.u.dir"
))
