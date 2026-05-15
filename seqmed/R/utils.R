## parallel +---------------------------------------------+
#' pblapply with parallel switch
#' @keywords internal
pblapply.sw <- function(b.parallel, int.cores, env, ...) {
    f <- pbapply::pblapply
    args <- list(...)
    if(b.parallel) {
        cl <- parallel::makeCluster(int.cores)
        parallel::clusterExport(cl, ls(envir = env), envir = env)
        parallel::clusterCall(cl, function() loadNamespace("seqmed"))
        args <- c(args, list(cl = cl))
    }
    obj <- do.call(f, args)
    if(b.parallel) parallel::stopCluster(cl)
    obj
}
## functional constructs
#' Swap argument positions
#' @keywords internal
Swap <- function(f, pos = 2L) {
    f <- match.fun(f)
    pos.coerce <- as.integer(pos) |> suppressWarnings()
    s <- `if`(is.na(pos.coerce),
              {
                  paste0("function(arg1, ...) f(", pos, " = arg1, ...)", collapse = "")
              },
              {
                  if(!all.equal(pos, pos.coerce))
                      stop("pos needs to be either an integer or character string.")
                  paste0("function(",
                         paste0("arg", c(pos, seq(pos.coerce - 1)), collapse = ", "),
                         ", ...)", " f(",
                         paste0("arg", seq(pos), collapse = ", "),
                         ", ...)", collapse = "")
              })
    structure(parse(text = s,
                    keep.source = FALSE)[[1]] |>
              eval(),
              func.name = all.names(sys.call())[-1],
              func = substitute(f))
}
## class/type checks
#' Multivariate is() check
#' @keywords internal
is.v <- function(obj, v.class, simplify = any) {
    f <- match.fun(simplify)
    f(unlist(mapply(function(s.class)
                    is(obj, s.class),
                    v.class,
                    SIMPLIFY = FALSE,
                    USE.NAMES = TRUE)))
}
#' Check if all elements are identical
#' @keywords internal
identical.v <- function(v) {
    if(length(v) == 1)
        return(TRUE)
    bv.na <- is.na(v)
    if(all(bv.na)) {
        warning("Input contains NA.")
        return(TRUE)
    }
    if(any(bv.na)) {
        warning("Input contains NA.")
        return(FALSE)
    }
    all(v[1] == v)
}

## flow control
#' Ternary if
#' @keywords internal
if.t <- function(b.test, v.true, v.false) {
    if(missing(v.false)) {
        if(b.test) {
            v.true
        }
    } else {
        if(b.test) {
            v.true
        } else {
            v.false
        }
    }
}
#' Switch on boolean
#' @keywords internal
sw.b <- function(...) {
    b <- c(...)
    s.len <- length(b)
    if(any(!is.logical(b), (s.len == 0), (sum(b) > 1)))
        stop("Error")
    switch(which(c((s.len == 1),
                   (s.len > 1))),
           ifelse(b, 1, 2),
           which(b))
}
## attributes
#' Copy attributes to environment
#' @keywords internal
attr2env <- function(obj, env = parent.frame()) {
    l <- attributes(obj)
    b.include <- match(names(l), c("dim", "names"), nomatch = 0) == 0
    if(any(b.include))
        list2env(l[b.include], envir = env)
}
#' Set list index
#' @keywords internal
set.list.index <- function(l) {
    l.index <- list(i = seq_along(l))
    v.names <- names(l)
    if(!is.null(v.names))
        l.index <- c(l.index, list(names = v.names))

    l.index.rstruc <- do.call(Map,
                              c(list(list),
                                Map(as.list, l.index)))

    mapply(function(i, l.attr)
           structure(i, index = l.attr),
           l, l.index.rstruc,
           SIMPLIFY = FALSE,
           USE.NAMES = TRUE)
}

# helper +----------------------------------------------------------+
## environment and variables management +-----------------+
#' Add attributes
#' @keywords internal
add.attr <- function(o, l) {
    `attributes<-`(o, c(l, attributes(o)))
}
#' Unpack list to environment
#' @keywords internal
list.var <- function(l) {
    if.t(class(l) != "list",
         as.list(l), l)               |>
    list2env(envir = parent.frame(1)) |>
    invisible()
}
#' Check class membership
#' @keywords internal
class.check <- function(obj, v.valid.classes, invert = FALSE) {
    v.checks <- match(obj, v.valid.classes, nomatch = 0)
    if(invert) {
        all(v.checks == 0)
    } else {
        any(v.checks > 0)
    }
}
#' Setup consistent environment across functions
#' @keywords internal
setup.env <- function(list.models,
                      int.iter,
                      b.parallel = "auto",
                      int.cores = "max",
                      canonical = FALSE)
{
    # hardcoded values
    int.parallel.threshold <- 1e4L

    l.nm.av <- Map(function(mo) formula(mo) |> all.vars(), list.models)
    v.nm.dv <- Map(`[`, l.nm.av, 1) |> unlist()

    m.df <- do.call(rbind, Map(`[[`, Map(summary, list.models), "df"))

    p <- m.df[,3] |> `names<-`(v.nm.dv) |> sort(decreasing = TRUE)

    v.n <- (m.df[,2] + m.df[,3]) |> unique()
    n <- if.t(length(v.n) > 1,
              stop("Unequal sample sizes across models."),
              v.n)

    df.res <- (m.df[,2] |> `names<-`(v.nm.dv))[names(p)]

    l.np <- list(n = n, p = p, df.res = df.res)

    list.models <- `names<-`(list.models, v.nm.dv)[names(p)]

    d <- model.frame(list.models[[1]])

    v.nm.dv <- names(p)
    v.nm.m <- v.nm.dv[-1]

    l.nm.iv <- Map(`[`, l.nm.av, -1)
    v.nm.common <- Reduce(intersect, l.nm.iv)

    v.nm.r <- Map(`[`, l.nm.iv, 1) |> unlist()
    if(!identical.v(v.nm.r))
        stop("Error with treatment variable in models.")
    s.nm.r <- v.nm.r[1] |> unname()
    v.r <- d[[s.nm.r]] |>
           unique()    |>
           sort(decreasing = TRUE)

    b.cv <- match(v.nm.common, s.nm.r, nomatch = 0) == 0
    v.nm.cv <- if.t(any(b.cv), v.nm.common[b.cv], NA)

    if(canonical) {
        names(v.nm.dv) <- c("y", sprintf("m%02d", seq_along(v.nm.dv[-1])))
        names(s.nm.r) <- "r"
        names(v.nm.cv) <- sprintf("cv%02d", seq_along(v.nm.cv))
    } else {
        names(v.nm.dv) <- v.nm.dv
        names(s.nm.r) <- s.nm.r
        names(v.nm.cv) <- v.nm.cv
    }

    v.nm.iv <- c(s.nm.r, v.nm.cv)
    v.nm.av <- c(v.nm.dv, v.nm.iv)

    v.fam <- Map(`[[`,
                 Map(family, list.models),
                 "family") |>
             unlist()      |>
             Swap(gsub, 3)("^(.).*", "\\1")
    s.fam <- paste0(v.fam, collapse = "")
    l.fam <- local({
        v.abv <- c("g", "b")
        Map(function(x)
            structure(v.abv == x,
                      names = v.abv),
            v.fam)})

    n.mo <- length(list.models)
    n.med <- n.mo - 1

    if(!is.logical(b.parallel)) {
        if(!(b.parallel == "auto"))
            stop("Invalid parallel option")
        b.parallel <- int.iter >= int.parallel.threshold
    }
    int.cores <-
        if.t(b.parallel,
             if.t(int.cores == "max",
                  parallel::detectCores(),
                  {
                      cores <- as.integer(int.cores)
                      if.t(is.na(cores),
                           {
                               warning("Invalid cores option, setting it to maximum number of cores.")
                               parallel::detectCores()
                           }, cores)
                  }), 1L)
    l.base <- list(list.models,
                   d          = d,
                   n.mo       = n.mo,
                   n.med      = n.med,
                   s.n        = n,
                   p          = p,
                   df.res     = df.res,
                   s.fam      = s.fam,
                   l.fam      = l.fam,
                   v.nm.dv    = v.nm.dv,
                   v.nm.m     = v.nm.m,
                   v.nm.iv    = v.nm.iv,
                   s.nm.r     = s.nm.r,
                   v.r        = v.r,
                   v.nm.cv    = v.nm.cv,
                   v.nm.av    = v.nm.av,
                   l.nm.av    = l.nm.av,
                   int.iter   = int.iter,
                   b.parallel = b.parallel,
                   int.cores  = int.cores)

    l.base[[1]] <- do.call(structure, l.base)
    names(l.base)[1] <- "l.mo"

    list2env(c(list(l.base = l.base), l.base),
             envir = parent.frame(1)) |>
    invisible()
}
## objects manipulation +---------------------------------+
#' Convert matrix to list
#' @keywords internal
mat.t.list <- function(m, by, ret) {
    if(!any(by %in% c(1, 2))) stop("Error")
    l <- unlist(apply(m, by, list), recursive = FALSE)
    switch(ret, v = l, m = Map(rbind, l))
}
#' Convert vector to list by lengths
#' @keywords internal
vec.t.list <- function(v, v.len) {
    if(length(v) != sum(v.len)) stop("Error")
    env <- environment()
    Map(function(s)
        {
            v.ind <- seq(s)
            v.ret <- v[v.ind]
            assign("v", v[-v.ind], envir = env)
            v.ret
        }, v.len)
}
#' Combine two vectors/matrices
#' @keywords internal
combi <- function(vm1, vm2) {
    m1 <- cbind(vm1)
    m2 <- cbind(vm2)
    n.m1 <- nrow(m1)
    n.m2 <- nrow(m2)
    m <- cbind(m1[sort(rep(1:n.m1, n.m2)),],
               m2[rep(1:n.m2, n.m1),])
    m[order(apply(m, 1, paste, collapse = "")),]
}
#' Binary combinations
#' @keywords internal
combi.bin <- function(v.nm, s.ret) {
    l.bin.resp <- rep(list(0:1), length(v.nm))
    m.combi <- `colnames<-`(cbind(Reduce(combi, l.bin.resp)), v.nm)
    mat.t.list(m.combi, 1, s.ret)
}
#' Split string in half
#' @keywords internal
str.half <- function(v, s.type = "v", b.clp = TRUE) {
    s.len <- length(v)
    if((s.len %% 2) != 0) stop("Error")
    s.lenh <- s.len/2
    l <- Map(function(v.ind) v[v.ind], list(1:s.lenh, (s.lenh + 1):s.len))
    if(b.clp) {
        l <- Map(paste0, l, collapse = "")
    }
    if(s.type == "v") {
        l <- do.call(c, l)
    }
    l
}
## comparisons and flow control +-------------------------+
#' Get raw family info
#' @keywords internal
get.fam.raw <- function(mo) {
    with(family(mo), c(family, link))
}
#' Get boolean family info
#' @keywords internal
get.fam.bool <- function(mo) {
    b <- get.fam.raw(mo)[1] == "gaussian"
    structure(c(b, !b), .Names = c("g", "b"))
}
#' Get family list from model list
#' @keywords internal
get.fam.l <- function(l.mo) {
    attr2env(l.mo)
    `names<-`(Map(get.fam.bool, l.mo), v.nm.dv)
}
#' Get model dependencies
#' @keywords internal
get.mo.dep <- function(l.mo) {
    attr2env(l.mo)
    l.dep <- Map(function(v.nm, v.var)
                 {
                     v <- intersect(v.nm.dv, v.var[-(1:2)])
                     if(!is.null(v) & (length(v) == 0)) {
                         NULL
                     } else {
                         v
                     }
                 }, v.nm.dv, l.nm.av)
    l.dep.n <- mapply(function(v) length(v), l.dep)
    list(dep = l.dep, n = l.dep.n, n.sorted = sort(l.dep.n))
}
#' Set expected values
#' @keywords internal
set.exv <- function(v.nm.dv, v.ev, l.fam) {
    mapply(function(s.nm, s.ev, vb.fam)
           {
               b <- if(s.ev == "auto") {
                   if(s.nm == "y") {
                       TRUE
                   } else {
                       vb.fam[2]
                   }
               } else {
                   s.ev
               }
               structure(b, .Names = "")
           }, v.nm.dv, v.ev, l.fam)
}
## get n (sample size) & p (variables) +------------------+
#' Get n and p from models
#' @keywords internal
get.np <- function(l.mo, s.type) {
    m.np <- mapply(function(mo) dim(model.matrix(mo)), l.mo)
    if(!identical.v(m.np[1,]))
        stop("Error: Unequal sample sizes across models.")
    switch(s.type,
           n = m.np[1,1],
           np =
           {
               v.nm <- mapply(function(mo)
                              as.character(formula(mo))[2], l.mo)
               switch(s.type,
                      p = structure(m.np[2,], .Names = v.nm),
                      np = list(n = m.np[1,1],
                                p = structure(m.np[2,], .Names = v.nm)))
           })
}
## confidence intervals +---------------------------------+
#' Generate CI bounds
#' @keywords internal
gen.ci <- function(s.ci) {
    if(any(!is.numeric(s.ci),
           length(s.ci) > 1,
           s.ci < 0,
           s.ci > 1)) stop("Error")
    s.lb <- (1 - s.ci)/2
    s.ub <- s.ci + s.lb
    c(lb = s.lb, ub = s.ub)
}

# mathematical +----------------------------------------------------+
#' Generate seeds
#' @keywords internal
gen.seeds <- function(int.sims, s.seed = NULL) {
    s.int.max <- .Machine[["integer.max"]]
    set.seed(s.seed)
    sample((-s.int.max):s.int.max, int.sims,
           replace = FALSE)
}

# model fitting and parameters extraction +--------------------------+
#' Flatten matrix to data.frame
#' @keywords internal
m.flat <- function(m, na.rm = FALSE) {
    v <- dim(m)
    v.r <- rep(1:v[1], v[2])
    v.c <- sort(rep(1:v[2], v[1]))
    m.mask <- cbind(v.r, v.c)

    v.nm.r <- rownames(m)
    v.nm.c <- colnames(m)

    if(!is.null(v.nm.r)) v.r <- v.nm.r[v.r]
    if(!is.null(v.nm.c)) v.c <- v.nm.c[v.c]
    d <- data.frame(row = v.r, col = v.c, val = m[m.mask])
    if(na.rm) {
        d <- na.omit(d)
    }
    d[with(d, order(col, row)),]
}
#' Reconstruct matrix from flattened data.frame
#' @keywords internal
m.rcon <- function(d) {
    v.nm.r <- local({
        v.uniq <- unique(d[,"row"])
        structure(seq_along(v.uniq), .Names = v.uniq)
    })
    v.nm.c <- local({
        v.uniq <- unique(d[,"col"])
        structure(seq_along(v.uniq), .Names = v.uniq)
    })

    m <- matrix(0,
                nrow = length(v.nm.r),
                ncol = length(v.nm.c),
                dimnames = list(names(v.nm.r),
                                names(v.nm.c)))
    m.mask <- cbind(row = v.nm.r[d[,"row"]], col = v.nm.c[d[,"col"]])
    m[m.mask] <- d[,"val"]

    b.nm.r.int <- !suppressWarnings(any(is.na(as.integer(names(v.nm.r)))))
    b.nm.c.int <- !suppressWarnings(any(is.na(as.integer(names(v.nm.c)))))

    if(b.nm.r.int) {
        rownames(m) <- NULL
    } else {
        m <- m[sort(rownames(m)),]
    }
    if(b.nm.c.int) {
        colnames(m) <- NULL
    } else {
        m <- m[,sort(colnames(m))]
    }
    m
}
#' Remove NULL elements (recursive)
#' @keywords internal
rm.null <- function(l) {
    l <- l[!vapply(l, is.null, logical(1))]
    Map(function(l1)
        {
            if(is.list(l1)) {
                l1[!vapply(l1, is.null, logical(1))]
            } else {
                l1
            }
        }, l)
}
#' Write formatted text to file
#' @keywords internal
catf <- function(v, lineend, file) {
    writeBin(paste0(c(v, ""), collapse = lineend) |>
             charToRaw(), file)
}
#' Generate potential outcome definitions
#' @keywords internal
po.def <- function(n.med) {
    s.len <- sum(2^(0:(n.med - 1))) + 1
    v0 <- rep(0L, s.len)
    Map(function(s) `[<-`(v0, 0:s, 1),
        0:s.len)
}
#' Generate causal effect definitions
#' @keywords internal
ce.def <- function(n.med, te = TRUE) {
    l.po <- po.def(n.med)
    s.po.len <- length(l.po)
    l <- Map(function(s) c(l.po[[s]], l.po[[s - 1]]),
             2:s.po.len)
    names(l) <- c("de", paste0("ie", 1:(s.po.len - 2)))
    if(te) {
        l <- c(list(c(l.po[[s.po.len]], l.po[[1]])), l)
        names(l)[1] <- "te"
    }
    structure(l, n.med = n.med)
}
#' Get counterfactuals from effects
#' @keywords internal
ce.cft <- function(l.eff, v.nm) {
    n.med <- attr(l.eff, "n.med")
    m.eff <- do.call(rbind, l.eff)

    nc.m.eff <- ncol(m.eff)
    nc.m.eff.half <- nc.m.eff/2
    m.unq <- unname(unique(rbind(m.eff[,1:nc.m.eff.half],
                                 m.eff[,(nc.m.eff.half + 1):nc.m.eff])))
    v.pos <- 2^(n.med:0)

    if(ncol(m.unq) != v.pos[1]) stop("Error")
    v.colnm <- c(v.nm[1],
                 rev(unlist(Map(function(s, s.nm)
                                rep(s.nm, s),
                                v.pos[-1], v.nm[-1]))))
    l.pos <- Map(function(s.nm) which(v.colnm == s.nm), v.nm)
    l.pos[[1]] <- 1:v.pos[1]

    Map(function(v)
        {
            l <- mat.t.list(unique(as.matrix(m.unq[,v])), 1, "v")
            names(l) <- Map(paste, l, collapse = "")
            l[sort(names(l))]
        }, l.pos)
}
#' Get causal effect configuration
#' @keywords internal
ce.cfg <- function(n.med, v.nm) {
    s.len.nm <- length(v.nm)
    if((n.med + 1) != s.len.nm)
        stop("Error: Length of vector of v.nm is incorrect.")

    ce.pos.f <- function(n.med.i, v.nm.i) {
        v.pos <- 2^(n.med.i:0)
        c(v.nm.i[1],
          rev(unlist(Map(function(s, s.nm)
                         rep(s.nm, s),
                         v.pos[-1], v.nm.i[-1]))))
    }
    list(len = structure(2^(n.med:0), .Names = v.nm),
         cfg = Map(function(s.nm, s.med)
                   {
                       if.t(s.med > 0,
                            ce.pos.f(s.med, v.nm[which(s.nm == v.nm):s.len.nm]),
                            v.nm[s.len.nm])

                   }, v.nm, n.med:0))
}
## do and set "x" +---------------------------------------+
#' Set variable values in data
#' @keywords internal
set.x <- function(d, ...) {
    l.len <- ...length()
    l <- list(...)

    if(l.len == 1 & is(l[[1]], "list")) {
        l <- l[[1]]
        l.len <- length(l)
    }
    if((l.len %% 2) != 0) stop("Error")

    s.n <- nrow(d)
    Reduce(function(d.rarg, l)
           {
               s.nm <- l[[1]]
               v <- l[[2]]
               class(v) <- class(d.rarg[,s.nm])
               v.len <- length(v)

               b.br.org <- v.len == s.n
               b.br.rep <- v.len == 1

               if(!any(b.br.org, b.br.rep)) stop("Error")

               d.rarg[,s.nm] <- v
               d.rarg
           }, c(list(d), Map(function(s.nm, v) list(s.nm, v),
                             l[seq(1, l.len, 2)],
                             l[seq(2, l.len, 2)])),
           accumulate = FALSE)
}
#' Set x parameters
#' @keywords internal
set.x.par <- function(l.sim.par, l.caus.eff, s.nm.r) {
    Map(function(l.par, l.eff, s.nm.r)
        Map(function(s.nm, l, s.nm.set, s.eff)
            within(l, data <- set.x(data, s.nm.set, head(s.eff, 1))),
            names(l.eff), list(l.par), s.nm.r, l.eff),
        l.sim.par, l.caus.eff, s.nm.r)
}
#' Compute do(x)
#' @keywords internal
do.x <- function(l.par) {
    c(with(l.par,
           {
               m <- tcrossprod(coef, data)
               if(all(fam[["family"]] == "binomial", !ev)) {
                   (m > 0) + 0
               } else {
                   fam[["linkinv"]](m)
               }
           }))
}
## compute causal effects +-------------------------------+
#' Causal effect (difference)
#' @keywords internal
ce.dif <- function(...) {
    s.arg.len <- ...length()
    if(s.arg.len == 1) {
        obj.arg <- (...)
        cl.args <- class(obj.arg)
        if(class.check(cl.args, c("integer", "numeric", "matrix"), TRUE))
            stop("Error")
        if(class.check(cl.args, c("integer", "numeric")))
            m <- matrix(obj.arg, nrow = 1)
        if(class.check(cl.args, c("matrix")))
            m <- obj.arg
    }
    if(s.arg.len > 1) {
        l <- list(...)
        m <- do.call(cbind, l)
    }
    apply(m, 1, function(v) Reduce(`-`, v))
}
#' Causal effect (odds ratio)
#' @keywords internal
ce.odr <- function(...) {
    s.arg.len <- ...length()
    if(!any(s.arg.len == 1:2)) stop("Error")
    if(s.arg.len == 1) {
        obj.arg <- (...)
        cl.args <- class(obj.arg)
        if(class.check(cl.args, c("integer", "numeric", "matrix"), TRUE))
            stop("Error")
        if(class.check(cl.args, c("integer", "numeric"))) {
            if(length(obj.arg) != 2) stop("Error")
            m <- matrix(obj.arg, nrow = 1)
        }
        if(class.check(cl.args, c("matrix"))) {
            if(ncol(obj.arg) != 2) stop("Error")
            m <- obj.arg
        }
    }
    if(s.arg.len == 2) {
        l <- list(...)
        m <- do.call(cbind, l)
    }
    exp(log(m[,1]) - log(1 - m[,1]) -
        log(m[,2]) + log(1 - m[,2]))
}
### compute causal effects from counterfactuals +---------+
#' Convert counterfactuals to causal effects
#' @keywords internal
cf.t.ce <- function(m.cft) {
    l.def <- attr(m.cft, "def")
    v.fam <- attr(m.cft, "fam")
    l.cft.m <- Map(function(v.nm) m.cft[,v.nm], l.def)
    l.cft.m.raw <- Map(function(v.nm) attributes(m.cft)[["raw"]][,v.nm], l.def)
    f <- switch(sw.b(v.fam), ce.dif, ce.odr)
    add.attr(do.call(cbind, Map(f, l.cft.m)),
             list(raw = l.cft.m.raw))
}
