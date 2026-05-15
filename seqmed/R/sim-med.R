#' Extract Mplus Tech0 counterfactuals
#' @keywords internal
mp.ext.t0.cf <- function(mp.out, var.est = "mplus") {
    mp.varindex <- attributes(mp.out)[["varindex"]]
    l.mo <- mp.out[["models"]]
    attr2env(l.mo)
    v.nm.u <- mp.out[["raw"]][["nm.u"]]
    v.nm.n <- mp.out[["raw"]][["nm.n"]]
    v.nm.o <- c(names(v.nm.n), v.nm.u, 1)
    names(v.nm.o) <- toupper(c(v.nm.n, v.nm.u, 1))
    d <- within(mp.out[["est"]][["parameters"]][["unstandardized"]],
                est[paramHeader == "Thresholds"] <-
                    est[paramHeader == "Thresholds"] * (-1))
    v.parmheaders <-
        c("Means",
          "Intercepts",
          "Thresholds",
          "Variances",
          "Residual\\.Variances")
    v.swap.reg <- paste0(c("\\.BY$",
                           paste0("^",
                                  c("", v.parmheaders),
                                  "$")),
                         collapse = "|")
    v.headers <-
        mapply(function(d.i)
               {
                   d.new <- d.i
                   if(grepl(v.swap.reg,
                            d.i[,"paramHeader"])) {
                       d.new[,"paramHeader"] <- d.i[,"param"]
                       d.new[,"param"] <- d.i[,"paramHeader"]
                   }
                   d.new <- within(d.new,
                                   {
                                       paramHeader <- gsub("\\$1$", "", paramHeader)
                                       paramHeader <- gsub("\\.ON$", "", paramHeader)
                                       paramHeader <- gsub("\\.WITH$", "", paramHeader)
                                       param <- gsub("\\.BY$", "", param)
                                       param <- gsub("^Means$|^Intercepts$|^Thresholds$", "1", param)
                                       b.var <- grepl("Variances$", param)
                                       param[b.var] <- paramHeader[b.var]
                                       paramHeader <- v.nm.o[paramHeader]
                                       param <- v.nm.o[param]
                                       rm(b.var)
                                   })
                            paste0(d.new, collapse = "~")
                        }, split(d[,c("paramHeader", "param")], 1:nrow(d)))
    d <- data.frame(param = v.headers,
                    d[,c("est", "se", "est_se", "pval")])
    d <- d[!grepl("^u\\d+?~|~u\\d+?$",
                  v.headers),]
    v.headers <- d[,"param"]
    b.var <- mapply(function(v) v[1] == v[2],
                    strsplit(v.headers, "~"))
    l.coefvar <- mp.ext.sav(mp.out)
    v.coef <- l.coefvar[["coef"]][,"est"]
    v.res.var <- NULL
    v.rss <- NULL
    if(any(b.var)) {
        n <- with(mp.out[["raw"]][["np"]], n)
        p <- with(mp.out[["raw"]][["np"]], p)
        v.df.res <- n - p

        switch(var.est,
               mplus =
               {
                   v.rss <- local({
                       v <- d[b.var, "est"]
                       v.nm <- mapply(function(v) v[1], strsplit(v.headers[b.var], "~"))
                       names(v) <- v.nm
                       v[v.nm.dv] * n
                   })
                   v.res.var <- v.rss/v.df.res
               },
               manual =
               {

                   l.coef <- local({
                       l <- do.call(rbind, names(v.coef) |>
                                    strsplit("~"))       |>
                                   data.frame(v.coef) |>
                                   `colnames<-`(c("dv", "iv", "coef")) |>
                                   with({
                                       iv[iv == "1"] <- "(Intercept)"
                                       split(data.frame(iv, coef), dv)
                                   })
                                   l <- Map(function(d) with(d, structure(coef, .Names = iv)), l)
                                   l[v.nm.dv]
                   })
                   l.fitted <- Map(function(v.coef, mo)
                                   {
                                       m <- model.matrix(mo)
                                       v <- v.coef[colnames(m)]
                                       v.fitted <- tcrossprod(v, m) |> as.vector()
                                   }, l.coef, l.mo)
                   l.obs <- Map(function(v.dv, mo) model.frame(mo)[,v.dv],
                                v.nm.dv, l.mo)
                   v.rss <- Map(function(v.obs, v.fitted)
                                {
                                    v <- v.obs - v.fitted
                                    crossprod(v) |> as.vector()
                                }, l.obs, l.fitted) |> unlist()
                   v.res.var <- v.rss/v.df.res
               })
        list(coef = v.coef,
             res.var = v.res.var,
             rss = v.rss, np = mp.out[["raw"]][["np"]])
    }
}
#' Extract Mplus Tech1 variable index
#' @keywords internal
mp.ext.t1.vi <- function(mp.out) {
    v.nm.n <-  mp.out[["raw"]][["nm.n"]]
    v.nm.o <- names(v.nm.n)
    names(v.nm.o) <- v.nm.n
    names(v.nm.o) <- toupper(names(v.nm.o))
    mp.vind <- do.call(rbind, Map(m.flat, mp.out[["est"]][["tech1"]][["parameterSpecification"]], na.rm = TRUE))
    mp.vind <- mp.vind[mp.vind[,"val"] > 0,]
    mp.vind[,"col"] <- gsub("\\$1$", "", mp.vind[,"col"])
    mp.vind <- data.frame(mapply(function(v)
                                 {
                                     v[v != "1"] <- v.nm.o[v[v != "1"]]
                                     v
                                 }, mp.vind[,c("row", "col")]),
                          mp.vind["val"])
    sort(mapply(function(s.r, s.c, s.v)
                {
                    v <- c(s.r, s.c)
                    if(s.r == 1) {
                        v <- rev(v)
                    }
                    names(s.v) <- paste0(v, collapse = "~")
                    s.v
                }, mp.vind[,"row"], mp.vind[,"col"], mp.vind[,"val"],
                USE.NAMES = FALSE))
}
#' Extract Mplus Tech3 variance-covariance
#' @keywords internal
mp.ext.t3.vc <- function(mp.out) {
    mp.varindex <- attributes(mp.out)[["varindex"]]
    mp.t3.vc <- mp.out[["est"]][["tech3"]][["paramCov"]]
    mp.t3.vc[upper.tri(mp.t3.vc)] <- t(mp.t3.vc)[upper.tri(mp.t3.vc)]
    v.nm <- names(mp.varindex)
    v.var.ind <- which(mapply(function(i)
                              {
                                  v  <- unlist(strsplit(i, "~"))
                                  v[1] == v[2]
                              }, v.nm))
    rownames(mp.t3.vc) <- colnames(mp.t3.vc) <- v.nm
    if(length(v.var.ind) > 0) {
        mp.t3.vc <- mp.t3.vc[-v.var.ind, -v.var.ind]
    }

    list(vcov = mp.t3.vc[sort(rownames(mp.t3.vc)), sort(colnames(mp.t3.vc))])
}
#' Extract Mplus saved results
#' @keywords internal
mp.ext.sav <- function(mp.out) {
    mp.varindex <- attributes(mp.out)[["varindex"]]
    v.sav <- local({
        v <- mp.out[["raw"]][["s"]] |>
        paste0(collapse = " ") |>
        strsplit("\\s+")       |>
        unlist()

        grep("^$", v, invert = TRUE,
             value = TRUE) |>
                   as.numeric()
    })
    mp.ind <- names(mp.varindex)
    mp.sav <- local({
        mp.ind.len <- length(mp.ind)

        l.ind <- list(est = 1:mp.ind.len,
                      se  = (mp.ind.len + 1):(mp.ind.len * 2))

        l <- Map(function(v) structure(v.sav[v], .Names = mp.ind), l.ind)
        do.call(cbind, Map(function(v) v[names(v) |> sort()], l))
    })
    b.coef <- do.call(rbind, strsplit(rownames(mp.sav), "~")) |>
    apply(1, function(x) Reduce(`!=`, x))
    list(coef = mp.sav[b.coef,], var = mp.sav[!b.coef,])
}

#' Get model parameters for causal effects
#' @keywords internal
get.mo.par.ce <- function(l.args) {
    list.var(l.args)
    get.glm.par(l.mo, v.nm.dv)
}
#' Get model parameters for sensitivity analysis
#' @keywords internal
get.mo.par.sa <- function(l.args) {
    list.var(l.args)
    get.mp.par(fit.mp(l.mo, m.b, v.nm.dv, v.nm.iv, int.cores))
}
#' Extract GLM parameters
#' @keywords internal
get.glm.par <- function(l.mo, v.nm.dv) {
    v.coef <- do.call(c,
                      unname(Map(function(s.nm.mo, mo)
                                 {
                                     v.coef <- coef(mo)
                                     names(v.coef) <- paste0(s.nm.mo, "~", gsub("^\\(Intercept\\)$", "1", names(v.coef)))
                                     v.coef
                                 }, v.nm.dv, l.mo)))
    v.coef <- v.coef[sort(names(v.coef))]
    v.res.var <- mapply(function(s.nm.mo, mo)
                        {
                            v.var <- NULL
                            if(all(get.fam.raw(mo) == c("gaussian", "identity"))) {
                                v.var <- var(residuals(mo))
                            }
                            v.var
                        }, v.nm.dv, l.mo)
    v.rss <- structure(mapply(function(mo) crossprod(residuals(mo)), l.mo),
                       .Names = v.nm.dv)
    l.np <- get.np(l.mo, "np")
    m.vcov <- m.rcon(do.call(rbind,
                             Map(function(s.nm.mo, mo)
                                 {
                                     m.vcov <- vcov(mo)
                                     dimnames(m.vcov) <- Map(function(v.nm) paste0(s.nm.mo, "~", gsub("^\\(Intercept\\)$", "1", v.nm)),
                                                             dimnames(m.vcov))
                                     m.flat(m.vcov)
                                 }, v.nm.dv, l.mo)))
    list(coef = v.coef, res.var = v.res.var,
         rss = v.rss, np = l.np, vcov = m.vcov)
}
#' Extract Mplus parameters
#' @keywords internal
get.mp.par <- function(mp.out) {
    v.nm.n <-  mp.out[["raw"]][["nm.n"]]
    mp.out <- add.attr(mp.out, list(varindex = mp.ext.t1.vi(mp.out)))
    mp.t0.coef <- mp.ext.t0.cf(mp.out)
    mp.t3.vcov <- mp.ext.t3.vc(mp.out)

    s.nrow.vc <- nrow(mp.t3.vcov[["vcov"]])
    if(s.nrow.vc != ncol(mp.t3.vcov[["vcov"]]))   stop("Error")
    if(s.nrow.vc != length(mp.t0.coef[["coef"]])) stop("Error")

    c(mp.t0.coef, mp.t3.vcov)
}
#' Get model parameters by mode
#' @keywords internal
get.mo.par <- function(l.mo, s.mode, v.nm.dv, v.nm.iv, m.b, int.cores) {
    l.args <- list(l.mo = l.mo,
                   v.nm.dv = v.nm.dv,
                   v.nm.iv = v.nm.iv,
                   m.b = m.b, int.cores = int.cores)
    switch(s.mode,
           ce = get.mo.par.ce,
           sa = get.mo.par.sa)(l.args)
}

#' Generate coefficients
#' @keywords internal
gen.coef <- function(l.mo.par, s.meth, s.mode, v.nm.dv, v.nm.iv, m.b, int.cores) {
    m.coef <- switch(s.meth,
                     mvn = {
                         with(l.mo.par, mvtnorm::rmvnorm(1, mean = coef, sigma = vcov))
                     },
                     boot = {
                         l.mo.boot <- upd.mo.boot1(l.mo)
                         switch(s.mode,
                                ce = get.glm.par(Map(function(mo)
                                                     update(mo,
                                                            data = model.frame(mo)),
                                                     l.mo.boot),
                                                 v.nm.dv),
                                sa = get.mp.par(fit.mp(l.mo.boot,
                                                       m.b,
                                                       v.nm.dv,
                                                       v.nm.iv,
                                                       int.cores)))[["coef"]] |>
                     as.matrix() |> t()
                     })
    v.nm.coef <- colnames(m.coef)
    Map(function(s.nm)
        {
            m.coef[,grep(paste0("^", s.nm),
                         v.nm.coef, value = TRUE)] |>
        as.matrix()                            |>
        t()                                    |>
        cbind(err = 1)
        }, v.nm.dv)
}
#' Generate error term (single model)
#' @keywords internal
gen.err.1 <- function(mo, s.nm, s.mode, l.mo.par, b.ev) {
    v.err <- rep(0, l.mo.par[["np"]][["n"]])
    if(!b.ev) {
        s.err.type <- switch(sw.b(get.fam.bool(mo)),
                             switch(s.mode,
                                    ce = 1,
                                    sa = 2), 3)
        v.err <- with(l.mo.par,
                      switch(s.err.type,
                             rnorm(with(np, n),
                                   mean = 0,
                                   sd = sqrt(rss[s.nm]/with(np, n - p[s.nm]))),
                             rnorm(with(np, n),
                                   mean = 0,
                                   sd = sqrt(rss[s.nm]/with(np, n - p[s.nm]))),
                             rlogis(with(np, n), location = 0, scale = 1)))
    }
    matrix(v.err, ncol = 1, dimnames = list(NULL, "err"))
}
#' Generate error terms
#' @keywords internal
gen.err <- function(l.mo,
                    v.nm.dv,
                    s.mode,
                    l.mo.par,
                    b.ev) {
    Map(function(mo, s.nm, b.ev.i)
        gen.err.1(mo,
                  s.nm,
                  s.mode,
                  l.mo.par,
                  b.ev.i),
        l.mo,
        v.nm.dv,
        b.ev)
}
#' Generate simulation parameters
#' @keywords internal
gen.sim.par <- function(s.seed,
                        l.mo,
                        l.mo.par,
                        b.ev,
                        m.b,
                        s.meth,
                        s.mode,
                        int.cores) {
    attr2env(l.mo)
    l.call <- list(nm = list(dv = v.nm.dv, iv = v.nm.iv),
                   ev = b.ev,
                   beta.constr = m.b,
                   coef.meth = s.meth,
                   mode = s.mode,
                   seed = s.seed)
    set.seed(s.seed)
    l.coef <- gen.coef(l.mo.par, s.meth, s.mode, v.nm.dv, v.nm.iv, m.b, int.cores)
    l.err <- gen.err(l.mo, v.nm.dv, s.mode, l.mo.par, b.ev)
    l.data <- Map(function(mo) model.matrix(mo), l.mo)
    l.fam <- Map(family, l.mo)
    add.attr(Map(function(m.coef, m.data, m.err, fam, b.exp)
                 {
                     colnames(m.coef) <- gsub("^.*~", "", colnames(m.coef))
                     colnames(m.data)[1] <- 1
                     m.data <- cbind(m.data, m.err)[,colnames(m.coef)]
                     list(fam = fam,  ev = b.exp, coef = m.coef, data = m.data)
                 }, l.coef, l.data, l.err, l.fam, b.ev), l.call)
}

#' Generate latent u mean statements
#' @keywords internal
gen.u.vmn <- function(m) {
    v <- apply(combi(colnames(m), c("@0]", "@1")), 1,
               function(x) paste0(x, collapse = "", sep = ""))
    gsub("^(.*@0])$", "[\\1", v)
}
#' Generate latent u by statements
#' @keywords internal
gen.u.by <- function(m.u, m.b) {
    paste(colnames(m.u), "BY",
          apply(structure(paste0(m.u, "@", m.b),
                          .Dim = dim(m.u)), 2,
                function(v) paste0(v, collapse = " ", sep = "")),
          sep = " ")
}

#' Generate latent u with statements
#' @keywords internal
gen.u.with <- function(v.nm.u) {
    apply(gen.u.cmbn(v.nm.u), 2,
          function(x) paste0(paste(x, collapse = " WITH "), "@0"))
}
#' Parse Mplus warnings
#' @keywords internal
gen.mp.warn <- function(mp.output) {
    s.ln.warn <- grepl("^\\s+WARNING: |(AVOID SINGULARITY)", mp.output)
    list(b.warn = any(s.ln.warn), n.warn = sum(s.ln.warn))
}
#' Generate Mplus batch file
#' @keywords internal
gen.mplus.run <- function(fn) {
    c("@ECHO OFF",
      "SET DIR=%~dp1",
      "SET FNM=%~nx1",
      "FOR /F \"USEBACKQ tokens=*\" %%F IN (`where mplus`) DO (SET MPLUS=%%F)",
      "PUSHD \"%DIR%\"",
      "\"%MPLUS%\" \"%FNM%\"",
      "POPD",
      "SET MPLUS=",
      "SET DIR=",
      "SET FNM=",
      "GOTO :EOF") |>
    catf("\r\n", file = fn)
}
#' Generate Mplus variable names
#' @keywords internal
mp.nm <- function(v.nm.dv, v.nm.iv) {
    v.nm.o <- c(v.nm.dv[1], v.nm.iv, v.nm.dv[-1])
    v.nm.n.iv <- if.t(length(v.nm.iv) == 1, "r",
                     c("r", paste0("cv", sprintf("%02d", seq_along(v.nm.iv[-1])))))
    v.nm.n.m <- paste0("m", sprintf("%02d", rev(seq_along(v.nm.dv[-1]))))
    v.nm.n <- c("y", v.nm.n.iv, v.nm.n.m)
    `names<-`(v.nm.n, v.nm.o)
}
#' Generate Mplus data file
#' @keywords internal
mp.d <- function(l.mo, v.mp.nm, s.fn) {
    d <- model.frame(l.mo[[1]])[names(v.mp.nm)]
    con.f <- file(s.fn, open = "wb")
    write.table(d, file = con.f,
                sep = ",", na = ".", dec = ".",
                quote = FALSE, row.names = FALSE, col.names = FALSE,
                fileEncoding = "utf8", eol = "\n")
    close(con.f)
    list(d = readLines(s.fn))
}
#' Convert Mplus input list to vector
#' @keywords internal
mp.i.l2v <- function(mp.inp) {
    mp.inp <- rm.null(mp.inp)
    v.inp <- Map(function(i.nm, i)
                 {
                     m <- switch(is.list(i) + 1,
                                 matrix(i, ncol = 1),
                                 matrix(c(names(i),
                                          rep("=", length(i)),
                                          unlist(i)),
                                        ncol = 3))
                     if(ncol(m) > 1)
                         m[,1] <- formatC(m[,1], width = max(nchar(m[,1])), flag = "-")
                     c(paste0(i.nm, ":", collpase = ""),
                       apply(m, 1, function(n) paste0(c(rep(" ", 4), paste0(n, collapse = " "), ";"), collapse = "")),
                       "")
                 }, names(mp.inp), mp.inp) |> unlist() |> unname()
    if(tail(v.inp, 1) == "")
        v.inp <- v.inp[-length(v.inp)]
    v.inp
}
#' Mplus analysis model
#' @keywords internal
mp.an.mo <- function(l.mo, m.b, v.nm.dv.n, v.nm.n) {
    v.model.on <-
        Map(function(mo)
            {
                v.nm.n <- v.nm.n[formula(mo) |> all.vars()]
                paste0(c(v.nm.n[1], "ON", v.nm.n[-1]), collapse = " ")
            }, l.mo) |> unlist()
    m.u.cmb <- gen.u.cmbn(v.nm.dv.n)
    n.u.cmb <- ncol(m.u.cmb)
    v.nm.u = colnames(m.u.cmb)

    if(!do.call(all.equal, Map(dim, list(m.u.cmb, m.b))))
        stop("Error: Constrain matrix m.b is incorrectly specified.")

    v.latu.meanvar <- gen.u.vmn(m.u.cmb)
    v.latu.by <- gen.u.by(m.u.cmb, m.b)
    v.latu <- c(v.latu.meanvar, v.latu.by)

    if(n.u.cmb > 1)
        v.latu <- c(v.latu, gen.u.with(v.nm.u))
    v.vars.force <- c(setdiff(v.nm.n, v.nm.dv.n), v.nm.u)
    l.combn.force <- combn(v.vars.force, 2, simplify = FALSE)

    add.attr(c(v.model.on, v.latu),
             list(s.nm.u = v.nm.u))
}
#' Generate Mplus input file
#' @keywords internal
mp.i <- function(l.mo, m.b, v.nm.n, v.nm.dv, int.cores, s.iter) {
    v.nm.dv.n <- v.nm.n[v.nm.dv]
    v.nm.f.type <- c("dat", "inp", "out", "sav", "cmd")
    v.nm.f <- tempfile(pattern = "mplus-") |>
    paste(v.nm.f.type, sep = ".")      |>
    structure(.Names = v.nm.f.type)
    s.n.mo <- length(l.mo)
    l.np <- get.np(l.mo, "np")
    mp.inp <- list(DATA     = list(FILE        = NULL,
                                   TYPE        = "INDIVIDUAL"),
                   VARIABLE = list(NAMES       = NULL,
                                   USEVARIABLE = NULL,
                                   CATEGORICAL = NULL,
                                   MISSING     = "."),
                   MODEL    = NULL,
                   ANALYSIS = list(TYPE        = "GENERAL",
                                   ESTIMATOR   = "ML",
                                   LINK        = NULL,
                                   PROCESSORS  = NULL,
                                   ITERATIONS  = NULL),
                   OUTPUT   = c("TECH1", "TECH3"),
                   SAVEDATA = list(RESULTS     = NULL))
    v.an.mo <- mp.an.mo(l.mo, m.b, v.nm.dv.n, v.nm.n)
    mp.inp[["DATA"]][["FILE"]] <- paste0('"', v.nm.f["dat"], '"')
    mp.inp[["VARIABLE"]][["NAMES"]] <- paste0(v.nm.n, collapse = " ")
    mp.inp[["VARIABLE"]][["USEVARIABLE"]] <- mp.inp[["VARIABLE"]][["NAMES"]]
    bv.mo.cat <- mapply(function(mo) all(get.fam.raw(mo) == c("binomial", "logit")), l.mo)
    if(any(bv.mo.cat)) {
        mp.inp[["VARIABLE"]][["CATEGORICAL"]] <- paste0(v.nm.dv.n[which(bv.mo.cat)], collapse = " ")
        mp.inp[["ANALYSIS"]][["LINK"]] <- "LOGIT"
    }
    mp.inp[["MODEL"]] <- v.an.mo
    mp.inp[["ANALYSIS"]][["PROCESSORS"]] <- int.cores
    mp.inp[["ANALYSIS"]][["ITERATIONS"]] <- s.iter
    mp.inp[["SAVEDATA"]][["RESULTS"]] <- paste0('"', v.nm.f["sav"], '"')
    mp.inp <- mp.i.l2v(mp.inp)
    catf(mp.inp, "\r\n", file = v.nm.f["inp"])
    gen.mplus.run(v.nm.f["cmd"])
    list(nm = list(f = v.nm.f, n = v.nm.n, u = attr(v.an.mo, "s.nm.u")),
         np = l.np, i = mp.inp)
}
#' Run Mplus and read output
#' @keywords internal
mp.o <- function(l.mo,
                 m.b,
                 v.nm.n,
                 v.nm.dv,
                 int.cores,
                 s.iter) {
    if(!requireNamespace("MplusAutomation", quietly = TRUE))
        stop("Error: library MplusAutomation is not installed.")

    mp.inp <- mp.i(l.mo, m.b, v.nm.n, v.nm.dv, int.cores, s.iter)
    v.nm.f <- mp.inp[["nm"]][["f"]]
    mp.inp <- list(nm.n = mp.inp[["nm"]][["n"]],
                   nm.u = mp.inp[["nm"]][["u"]],
                   np = mp.inp[["np"]],
                   i = mp.inp[["i"]])
    mp.dat <- mp.d(l.mo, v.nm.n, v.nm.f["dat"])

    system(paste(v.nm.f["cmd"], v.nm.f["inp"]),
           intern = TRUE) |>
invisible()

mp.out <- list(o = readLines(v.nm.f["out"]),
               s = readLines(v.nm.f["sav"]))
s.warning <- gen.mp.warn(mp.out[[1]])
mp.results <- MplusAutomation::readModels(target = v.nm.f["out"],
                         quiet = TRUE)
unlink(v.nm.f)
mp.raw <- add.attr(c(mp.inp, mp.dat, mp.out), list(warn = s.warning))
list(est = mp.results, models = l.mo, raw = mp.raw)
}
#' Fit Mplus model
#' @keywords internal
fit.mp <- function(l.mo,
                   m.b = "no_confound",
                   v.nm.dv = v.nm.dv,
                   v.nm.iv = v.nm.iv,
                   int.cores = parallel::detectCores(),
                   s.iter = 1e4L) {
    v.nm.n <- mp.nm(v.nm.dv, v.nm.iv)
    if(is(m.b, "character")) {
        if(m.b == "no_confound") {
            m.b <- local({
                m.u.cmb <- v.nm.n[v.nm.dv] |> gen.u.cmbn()
                structure(rep(0, length(m.u.cmb)), .Dim = dim(m.u.cmb))
            })
        } else {
            stop("m.b incorrectly specified")
        }
    }
    mp.o(l.mo, m.b, v.nm.n, v.nm.dv, int.cores, s.iter)
}

## bootstrap +--------------------------------------------+
#' Bootstrap model update
#' @keywords internal
upd.mo.boot1 <- function(l.mo) {
    d <- model.frame(l.mo[[1]])
    n <- nrow(d)
    d.new <- d[sample(1:n, n, replace = TRUE),]
    Map(function(mo) update(mo, formula(mo), family = family(mo), data = d.new), l.mo)
}

#' Generate u combinations
#' @keywords internal
gen.u.cmbn <- function(v.nm, type = "m", lat.u.prefix = "u") {
    m <- combn(v.nm, 2)
    s.n.col <- ncol(m)
    if(s.n.col > 1) {
        v.col.ord <- order(m[1,], m[2,],
                           decreasing = c(TRUE, FALSE),
                           method = "radix")
        m <- m[,v.col.ord]
    }
    v.col.i <- 1:s.n.col
    v.nm.u <- paste0(lat.u.prefix, v.col.i)
    switch(type, m =
           {
               colnames(m) <- v.nm.u
               m
           },
           l =
           {
               l <- Map(function(i) m[,i], v.col.i)
               names(l) <- v.nm.u
               l
           })
}

#' Check input types
#' @keywords internal
chk.typ <- function(l) {
    list2env(l, environment())
    if(!is(l.mo, "list"))
        stop("Error: Wrong object type for models argument (list).")
    if(!all( Map(is.v, l.mo, list(c("lm", "glm"))) |> unlist()))
        stop("Error: Wrong object type for elements of model list (glm, lm).")
    if(!is(v.nm.dv, "character"))
        stop("Error: Wrong object type for DV names.")
    if(!is(s.nm.r,  "character"))
        stop("Error: Wrong object type for treatment indicator name.")
    if(!is(v.nm.cv, "character"))
        stop("Error: Wrong object type for covariate names.")
    if(!is(s.cof.mth, "character"))
        stop("Error: Wrong object type for method to use to draw regression coefficients.")
    if(!is(s.mode, "character"))
        stop("Error: Wrong object type for analysis mode.")
    if(!is(s.seed, "integer"))
        stop("Error: Wrong object type for seed.")
    if(!is(int.sims, "integer"))
        stop("Error: Wrong object type for number of simulations.")
    if(!is.v(b.parallel, c("logical", "character")))
        stop("Error: Wrong object type for parallel threshold.")
    if(!is(int.cores, "integer"))
        stop("Error: Wrong object type for number of cores to use.")
}
#' Check input lengths
#' @keywords internal
chk.len <- function(l) {
    list2env(l, environment())
    if(!identical.v(mapply(length, list(l.mo, v.nm.dv))))
        stop("Error: Lengths of input arguments incorrect.")
}
#' Check input values
#' @keywords internal
chk.val <- function(l) {
    list2env(l, environment())
    if(!any(match(s.cof.mth, c("mvn", "boot"), nomatch = 0) > 0)) {
        warning("Invalid path coefficient simulation method specified. Replacing with defaults.")
        assign("s.cof.mth", "mvn", envir = parent.frame())
    }
    if(!any(match(s.mode, c("ce", "sa"), nomatch = 0) > 0)) {
        warning("Invalid path coefficient simulation method specified. Replacing with defaults.")
        assign("s.mode", "ce", envir = parent.frame())
    }
    b.na <- Map(as.integer,
                c(s.seed, int.sims, int.cores)) |>
            unlist()                            |>
            is.na()                             |>
            suppressWarnings()
    if(b.na[1]) {
        warning("Invalid seed specified. Replacing with defaults.")
        assign("s.seed", 8L, envir = parent.frame())
    }
    if(b.na[2]) {
        warning("Invalid number of simulations specified. Replacing with defaults.")
        assign("int.sims", 1e4L, envir = parent.frame())
    }
    if(b.na[3]) {
        warning("Invalid number of cores specified. Replacing with defaults.")
        assign("int.cores", parallel::detectCores(), envir = parent.frame())
    }
}

#' Set x parameters for binary
#' @keywords internal
set.x.par.b <- function(l.sim, l.combi) {
    within(l.sim,
           {
               dep <- Map(function(v, m.dep)
                          {
                              v.nm <- names(v)
                              m.prob <- attr(m.dep, "prob")
                              m.prob[,v.nm] <- do.call(cbind,
                                                       Map(function(s.nm, s)
                                                           {
                                                               v.prob <- m.dep[,s.nm]
                                                               if(s == 0) {
                                                                   v.prob <- 1 - v.prob
                                                               }
                                                               v.prob
                                                           }, v.nm, v))
                              attr(m.dep, "prob") <- m.prob
                              m.dep[,v.nm] <- matrix(v, ncol = length(v), nrow = nrow(m.dep), byrow = TRUE)
                              m.dep
                          }, l.combi, dep)
           })
}
#' Simulation counterfactuals (0 dependencies)
#' @keywords internal
sim.0dep <- function(l.set.x, l.sim.cf, s.nm.mo) {
    l.sim.cf[[s.nm.mo]] <- Map(do.x, l.set.x[[s.nm.mo]])
    l.sim.cf
}
#' Simulation counterfactuals (1+ dependencies)
#' @keywords internal
sim.1dep <- function(l.set.x, l.sim.cf, s.nm.mo,
                     l.eff.cf, l.eff.cfg,
                     l.dep, l.fam, b.exv) {
    l.cf.def <-  l.eff.cf[[s.nm.mo]]
    v.nm.dep <- l.dep[["dep"]][[s.nm.mo]]
    l.dep.len <- list(l.eff.cfg[["len"]][v.nm.dep])
    l.dep.cf <- Map(function(v.eff, v.len)
                    {
                        l.dep.cf.id <- vec.t.list(v.eff[-1], v.len)
                        l.dep.cf <-
                            Map(function(s.nm, v)
                                l.sim.cf[[s.nm]][[paste0(v, collapse = "")]],
                                names(l.dep.cf.id), l.dep.cf.id)
                        m.dep.cf <- `colnames<-`(unname(do.call(cbind,
                                                               l.dep.cf)),
                                                names(l.dep.cf))
                        m.dep.p <- m.dep.cf
                        m.dep.p[] <- 1
                        list(add.attr(m.dep.cf, list(prob = m.dep.p)))
                    }, l.cf.def, l.dep.len)
    l.sim.mo <- Map(function(l.d, l.x) within(l.x, dep <- l.d),
                    l.dep.cf, l.set.x[[s.nm.mo]])
    rm(l.cf.def, l.dep.len, l.dep.cf)

    b.dep.fam <- `names<-`(do.call(rbind, l.fam)[v.nm.dep,"b"], v.nm.dep)
    b.dep.exv <- b.exv[v.nm.dep]
    b.test <- mapply(all, b.dep.fam, b.dep.exv)
    v.mar <- v.nm.dep[b.test]
    rm(b.dep.fam, b.dep.exv, b.test)

    if(length(v.mar) > 0) {
        l.combi.bin <- combi.bin(v.mar, "v")
        l.sim.mo <- Map(function(l) set.x.par.b(l, l.combi.bin),
                        l.sim.mo)
        rm(l.combi.bin)
    }

    l.sim.cf[[s.nm.mo]] <- Map(function(l.cf)
                               {
                                   m <- do.call(cbind,
                                                Map(function(m)
                                                    {
                                                        l.cf[["data"]][,colnames(m)] <- m
                                                        do.x(l.cf) * apply(attr(m, "prob"), 1, prod)
                                                    }, l.cf[["dep"]]))
                                   apply(m, 1, sum)
                               }, l.sim.mo)
    l.sim.cf
}

#' Single simulation run
#' @keywords internal
sim.med.1 <- function(s.seed,
                      l.mo,
                      l.mo.par,
                      l.eff.cf,
                      l.eff.cfg,
                      l.dep,
                      b.exv,
                      m.b,
                      s.cof.mth,
                      s.mode) {
    attr2env(l.mo)
    l.sim.par <- gen.sim.par(s.seed = s.seed,
                             l.mo = l.mo,
                             l.mo.par = l.mo.par,
                             b.ev = b.exv,
                             m.b = m.b,
                             s.meth = s.cof.mth,
                             s.mode = s.mode,
                             int.cores = int.cores)
    l.set.x <- set.x.par(l.sim.par,
                         l.eff.cf,
                         v.nm.iv[1])
    l.sim.cf <- Map(function(v)
                    list(),
                    v.nm.dv)
    env <- environment()
    v <- l.dep[["n.sorted"]]
    invisible(Map(function(s.nm, s.n.dep)
                  {
                      if(s.n.dep == 0) {
                          l.cf.i <- sim.0dep(l.set.x, l.sim.cf, s.nm)
                      }
                      if(s.n.dep > 0)  {
                          l.cf.i <- sim.1dep(l.set.x, l.sim.cf, s.nm,
                                             l.eff.cf, l.eff.cfg, l.dep, l.fam, b.exv)
                      }
                      assign("l.sim.cf", l.cf.i, envir = env)
                  }, names(v), v))
    m <- do.call(cbind, l.sim.cf[[1]])
    apply(m, 2, mean)
}
#' Single simulation run (list version)
#' @keywords internal
sim.med.1l <- function(l.mo1) {
    attr2env(l.mo1)
    i <- index[["i"]]
    sim.med.1(v.seeds[i],
              l.mo1,
              l.mo.par,
              l.eff.cft,
              l.eff.cfg,
              l.dep,
              b.exv,
              m.b,
              s.cof.mth,
              s.mode)
}
#' Run simulation counterfactuals
#' @keywords internal
sim.cf <- function(l.mo, s.mode, m.b, s.cof.mth,
                   int.sims, s.seed, b.parallel, int.cores)
{
    attr2env(l.mo)
    chk.val(list(s.cof.mth = s.cof.mth,
                 int.cores = int.cores,
                 s.mode = s.mode,
                 b.parallel = b.parallel,
                 s.seed = s.seed,
                 int.sims = int.sims))
    chk.typ(list(l.mo = l.mo,
                 v.nm.dv = v.nm.dv,
                 s.nm.r = s.nm.r,
                 v.nm.cv = v.nm.cv,
                 s.cof.mth = s.cof.mth,
                 s.mode = s.mode,
                 s.seed = s.seed,
                 int.sims = int.sims,
                 b.parallel = b.parallel,
                 int.cores = int.cores))
    chk.len(list(l.mo = l.mo,
                 v.nm.dv = v.nm.dv))
    v.ev <- structure(rep("auto", length(l.mo)),
                      .Names = v.nm.dv)
    b.exv <- set.exv(v.nm.dv, v.ev, l.fam)
    if(!is.matrix(m.b)) {
        m.b <- switch(m.b,
                      no_confound = matrix(0, 2, choose(length(v.nm.dv), 2)),
                      stop("Error"))
    }
    l.call <- list(mo = l.mo,
                   nm = list(dv = v.nm.dv,
                             r = s.nm.r,
                             cv = v.nm.cv),
                   ev = list(v.ev,
                             b.exv),
                   coef = s.cof.mth,
                   mode = s.mode,
                   seed = s.seed,
                   sims = int.sims,
                   parallel = list(parallel = b.parallel,
                                   cores = int.cores))
    l.eff.def <- ce.def(n.med)
    l.eff.def.split <- Map(str.half, l.eff.def)
    l.eff.cft <- ce.cft(l.eff.def, v.nm.dv)
    l.eff.cfg <- ce.cfg(n.med, v.nm.dv)
    l.dep <- get.mo.dep(l.mo)
    if(!any(l.dep[["n"]] == 0))
        stop("Error: No solution when all models have dependencies.")
    v.seeds <- gen.seeds(int.sims, s.seed)
    l.mo.par <- get.mo.par(l.mo,
                           s.mode,
                           v.nm.dv,
                           v.nm.iv,
                           m.b,
                           int.cores)
    l.mo <- do.call(structure,
                      list(l.mo,
                           l.mo.par  = l.mo.par,
                           l.eff.cft = l.eff.cft,
                           l.eff.cfg = l.eff.cfg,
                           l.dep     = l.dep,
                           b.exv     = b.exv,
                           m.b       = m.b,
                           s.cof.mth = s.cof.mth,
                           s.mode    = s.mode,
                           v.seeds   = v.seeds))

    l.sim <- rep(list(l.mo), int.sims) |> set.list.index()
    m.cf.raw <- do.call(rbind,
                        pblapply.sw(b.parallel,
                                    int.cores,
                                    environment(),
                                    l.sim,
                                    sim.med.1l))
    m.cf.mn <- t(as.matrix(apply(m.cf.raw, 2, mean)))
    add.attr(Map(function(m) add.attr(m, list(def = l.eff.def.split, fam = l.fam[[1]])),
                 list(mn = m.cf.mn, raw = m.cf.raw)), list(seeds = v.seeds))
}

#' Simulation-based mediation analysis
#'
#' Perform simulation-based mediation analysis for sequential mediators.
#'
#' @param l.mo List of fitted GLM models.
#' @param s.mode Analysis mode: "ce" (causal effects) or "sa" (sensitivity analysis).
#' @param m.b Confounding constraint matrix or "no_confound".
#' @param s.cof.mth Coefficient simulation method: "mvn" or "boot".
#' @param s.ci Confidence interval level (default 0.95).
#' @param int.sims Number of simulations (default 10000).
#' @param s.seed Random seed.
#' @param b.parallel Parallel computation: TRUE, FALSE, or "auto".
#' @param int.cores Number of cores: integer or "max".
#' @param b.raw Include raw simulation output (default TRUE).
#' @return Matrix of estimates and confidence intervals.
#' @examples
#' \donttest{
#' dd <- dummy.data()
#' # Two-mediator model (continuous outcome, continuous mediators)
#' res <- sim.med(dd$l.mo[[1]], int.sims = 50L, s.seed = 1L,
#'                b.parallel = FALSE)
#' res
#' }
#' @export
sim.med <- function(l.mo,
                    s.mode = "ce",
                    m.b = "no_confound",
                    s.cof.mth = "mvn",
                    s.ci = 0.95,
                    int.sims = 1e4L,
                    s.seed = gen.seeds(1),
                    b.parallel = "auto",
                    int.cores = "max",
                    b.raw = TRUE)
{
    setup.env(l.mo,
              int.sims,
              b.parallel,
              int.cores)
    s.nm.ci <- paste0("ci.", gsub("\\.", "", gen.ci(s.ci)))

    l.cf <- sim.cf(l.mo,
                   s.mode,
                   m.b,
                   s.cof.mth,
                   int.sims,
                   s.seed,
                   b.parallel,
                   int.cores)

    l.ce <- Map(function(s.nm, m) cf.t.ce(m),
                c("est", "ci"), l.cf)
    v.fam <- local({
        v <- attributes(l.cf[["mn"]])[["fam"]]
        switch(names(v)[v],
               g = "difference",
               b = "oddsratio")
    })
    m.ce <- do.call(rbind,
                    within(l.ce,
                           {
                               est <- `rownames<-`(est, "est")
                               ci <- `rownames<-`(apply(ci, 2, \(v) quantile(v, probs = gen.ci(s.ci), type = 7)), s.nm.ci)
                           })) |> add.attr(list(efftype = v.fam))
    if.t(b.raw,
         add.attr(m.ce, list(raw = l.cf)),
         m.ce)
}
