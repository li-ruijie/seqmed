#' Generate correlated variables
#' @keywords internal
cor.m <- function(m, v.rho, s.dist) {
    s.n <- nrow(m)
    v.rand <- switch(s.dist,
                     normal = rnorm(s.n),
                     logistic = rlogis(s.n))
    m.scaled <- scale(m)
    l.m.svd <- svd(m.scaled)
    svd.u <- l.m.svd[["u"]]
    svd.v <- l.m.svd[["v"]]
    svd.d <- l.m.svd[["d"]]
    v.svd.d <- ifelse(svd.d > 0, 1/svd.d, 0)
    if(length(svd.d) == 1) {
        m.svd.d <- matrix(v.svd.d, ncol = 1, nrow = 1)
    } else {
        m.svd.d <- diag(v.svd.d)
    }
    m.sc <- (s.n - 1) * svd.u %*% m.svd.d %*% t(svd.v)
    v.res <- residuals(lm(v.rand ~ m.scaled))
    s.sigma.sq <- c((1 - (v.rho %*% cov(m.sc) %*% v.rho)) / var(v.res))
    if(s.sigma.sq < 0) {
        stop("Error")
    }
    m.sc %*% v.rho + sqrt(s.sigma.sq) * v.res
}
#' Scale to target mean and sd
#' @keywords internal
scale_par <- function(v, s.mn = 0, s.sd = 1, b.mat = TRUE) {
    scaled <- scale(v, center = FALSE, scale = sd(v)/s.sd)
    scaled <- scale(scaled, center = mean(scaled) - s.mn, scale = FALSE)
    if(!b.mat) {
        v.scaled <- c(scaled)
        attributes(v.scaled) <- attributes(scaled)[2:3]
        scaled <- v.scaled
    }
    scaled
}
#' Add error term
#' @keywords internal
add.err <- function(v.coef, m.static, s.e.sd, s.nm, i.bc, other.err = NULL) {
    s.dist <- switch(i.bc, b = "logistic", c = "normal")
    m.pred <- t(tcrossprod(v.coef, m.static))
    m.cor0 <- cbind(m.static[,-1], other.err)
    m.err <- cor.m(m.cor0, rep(0, ncol(m.cor0)), s.dist)
    m.obs <- switch(i.bc,
                    b = ((m.pred + m.err) > 0) + 0,
                    c = m.pred + scale_par(m.err, 0, s.e.sd, TRUE))
    m <- cbind(m.obs, m.err)
    colnames(m) <- c(s.nm, paste0(s.nm, "e"))
    m
}
#' Generate binary variable
#' @keywords internal
gen.b <- function(s.n, s.prop, b.rand = FALSE) {
    s.n1 <- round(s.prop * s.n)
    s.n0 <- s.n - s.n1
    b <- c(rep(1L, s.n1), rep(0L, s.n0))
    if(b.rand) {
        b <- b[sample(1:s.n, s.n, replace = FALSE)]
    }
    matrix(b, ncol = 1)
}
#' Generate static variables
#' @keywords internal
gen.static <- function(n, l.par) {
    r    <- gen.b(n, l.par[["r"]]   , FALSE)
    cv1b <- gen.b(n, l.par[["cv1b"]], TRUE)
    cv2c <- scale_par(cor.m(cbind(r, cv1b), c(0, 0), "normal"),
                    l.par[["cv2c"]][["mn"]],
                    l.par[["cv2c"]][["sd"]])
    cbind(i = 1, matrix(c(r, cv1b, cv2c), ncol = 3, dimnames = list(NULL, names(l.par))))
}
#' Generate model from data generation parameters
#' @keywords internal
gen_model <- function(n, s.seed, l.static, l.coef, v.norm.sd, s.dat) {
    # Setup generation +---------------------------------------+
    set.seed(s.seed)
    # Generate static elements: r, cv1b, cv2c +----------------+
    m.static <- gen.static(n, l.static)
    # Identify data types needed +-----------------------------+
    i.m1 <- gsub("^.*m1(.)$", "\\1",   s.dat)
    i.m2 <- gsub("^.*m2(.).*$", "\\1", s.dat)
    i.y  <- gsub("^y(.).*$", "\\1",    s.dat)

    # Start generating mediator 1 +----------------------------+
    s.cur <- "m1"
    m.m1 <- add.err(l.coef[[s.cur]], m.static, v.norm.sd[s.cur], s.cur, i.m1, NULL)
    m.static <- cbind(m.static, m1 = m.m1[,s.cur])
    # Start generating mediator 2 +----------------------------+
    s.cur <- "m2"
    m.m2 <- add.err(l.coef[[s.cur]], m.static, v.norm.sd[s.cur], s.cur, i.m2,
                    m.m1[,"m1e"])
    m.static <- cbind(m.static, m2 = m.m2[,s.cur])
    # Start generating outcome +-------------------------------+
    s.cur <- "y"
    m.y <- add.err(l.coef[[s.cur]], m.static, v.norm.sd[s.cur], s.cur, i.y,
                   cbind(m.m1[,"m1e"], m.m2[,"m2e"]))

    # Prepare data +-------------------------------------------+
    m <- cbind(m.static, y = m.y[,"y"])[,-1]
    d <- as.data.frame(m)

    # Fit models according to the types of data +--------------+
    c(switch(i.m1,
             b = list(mo.m1 = glm(m1 ~ r + cv1b + cv2c          , data = d, family = binomial(link = "logit"))),
             c = list(mo.m1 = glm(m1 ~ r + cv1b + cv2c          , data = d, family = gaussian(link = "identity")))),
      switch(i.m2,
             b = list(mo.m2   = glm(m2 ~ r + cv1b + cv2c          , data = d, family = binomial(link = "logit")),
                      mo.m2m1 = glm(m2 ~ r + cv1b + cv2c + m1     , data = d, family = binomial(link = "logit"))),
             c = list(mo.m2   = glm(m2 ~ r + cv1b + cv2c          , data = d, family = gaussian(link = "identity")),
                      mo.m2m1 = glm(m2 ~ r + cv1b + cv2c + m1     , data = d, family = gaussian(link = "identity")))),
      switch(i.y,
             b = list(mo.y     = glm( y ~ r + cv1b + cv2c          , data = d, family = binomial(link = "logit")),
                      mo.ym1   = glm( y ~ r + cv1b + cv2c + m1     , data = d, family = binomial(link = "logit")),
                      mo.ym2   = glm( y ~ r + cv1b + cv2c + m2     , data = d, family = binomial(link = "logit")),
                      mo.ym1m2 = glm( y ~ r + cv1b + cv2c + m1 + m2, data = d, family = binomial(link = "logit"))),
             c = list(mo.y     = glm( y ~ r + cv1b + cv2c          , data = d, family = gaussian(link = "identity")),
                      mo.ym1   = glm( y ~ r + cv1b + cv2c + m1     , data = d, family = gaussian(link = "identity")),
                      mo.ym2   = glm( y ~ r + cv1b + cv2c + m2     , data = d, family = gaussian(link = "identity")),
                      mo.ym1m2 = glm( y ~ r + cv1b + cv2c + m1 + m2, data = d, family = gaussian(link = "identity")))))
}
#' Generate models from data type
#' @keywords internal
gen.mo <- function(s.dat, l.par) {
    gen_model(l.par[["n"]], l.par[["seed"]], l.par[["static"]], l.par[["coef"]], l.par[["norm.e.sd"]], s.dat)
}
#' Generate all model configurations
#' @keywords internal
gen.mo.all <- function(v.ind, l.par) {
    v.mo <- `names<-`(c("ycm2cm1c", "ycm2cm1b", "ybm2cm1c", "ybm2cm1b",
                        "ycm2cm1c", "ycm2bm1c", "ycm2cm1b", "ycm2bm1b",
                        "ybm2cm1c", "ybm2bm1c", "ybm2cm1b", "ybm2bm1b"),
                      c("ycm1c", "ycm1b", "ybm1c", "ybm1b",
                        "ycm2cm1c", "ycm2bm1c", "ycm2cm1b", "ycm2bm1b",
                        "ybm2cm1c", "ybm2bm1c", "ybm2cm1b", "ybm2bm1b"))
    Map(function(s.nm, s)
        {
            list.var(gen.mo(v.mo[s], l.par))
            if(s <= 4) n.med <- "two"
            if(s > 4)  n.med <- "three"
            switch(n.med,
                   two   = list(mo.ym1, mo.m1),
                   three = list(mo.ym1m2, mo.m2m1, mo.m1))
        }, names(v.mo)[v.ind], v.ind)
}

#' Generate dummy data for testing
#'
#' Generate a list of fitted GLM models from simulated data for use in
#' examples and testing.
#'
#' @param l.par Optional list of data generation parameters. If NULL, uses
#'   built-in defaults with 1000 observations, two continuous mediators, and
#'   a continuous outcome.
#' @return A list with components \code{l.par} (generation parameters) and
#'   \code{l.mo} (list of 12 model configurations).
#' @examples
#' dd <- dummy.data()
#' names(dd$l.mo)
#' @export
dummy.data <- function(l.par = NULL) {
    if(is.null(l.par)) {
        l.par <- list(n = 1e3L,
                      seed = 42L,
                      static = list(r    = c(p = 0.5),
                                    cv1b = c(p = 0.2),
                                    cv2c = c(mn = 2, sd = 4)),
                      coef = list(m1 = c(i = 0.2, r = 0.4, cv1b = 0.1, cv2c = 0.1),
                                  m2 = c(i = 0.3, r = 0.6, cv1b = 0.1, cv2c = 0.1, m1 = 0.9),
                                  y  = c(i = 0.4, r = 0.8, cv1b = 0.1, cv2c = 0.1, m1 = 0.12, m2 = 0.16)),
                      norm.e.sd = c(m1 = 0.6, m2 = 0.9, y = 1.2))
    }
    l.mo <- local({
        l.mo <- gen.mo.all(1:12, l.par)
        Map(`names<-`,
            l.mo, Map(function(n)
                      c("y", paste0("m", rev(seq(n - 1)))),
                      Map(length, l.mo)))
    })
    list(l.par = l.par, l.mo = l.mo)
}
