#!/usr/bin/env r
library("ggplot2")
library("parallel")
# PREAMBLE +-------------------------------------------------------------------+
gen.utilities <- function() {
# functions
## utilities
    ut.box <- function(v.contents, chr.head = "┼", chr.sep = "─",
                       chr.tail = "┼", chr.wall = "│", s.align = "l") {
        v.box <- c(lh = "─",
                   ctl = "┌", ctr = "┐", cbl = "└", cbr = "┘",
                   lvhl = "├", lvhr = "┤", lhvt = "┬", lhvb = "┴",
                   lc = "┼")
        if(class(v.contents) |>
           match(c("integer", "numeric", "character"),
                 nomatch = 0) |>
           (any |> Negate())())
            stop("Incorrect contents format.")

        s.contents.lenmax <- nchar(v.contents) |> max()
        s.box.lenmax <- length(c(chr.head, chr.sep, chr.tail)) |> sum()
        if(s.contents.lenmax < s.box.lenmax)
            s.contents.lenmax <- s.box.lenmax
        s.contents.lenpadded <- s.contents.lenmax + 2
        s.contents.lenpadded <- (4 - (s.contents.lenpadded %% 4)) + s.contents.lenpadded

        s.contents.lenpadded.use <-
            switch(s.align,
                   l = -s.contents.lenpadded,
                   r = s.contents.lenpadded,
                   c = s.contents.lenpadded/2)
        v.messages <-
            switch((s.align == "c") + 1,
                   sprintf(paste0(chr.wall, " %*s ", chr.wall),
                           s.contents.lenpadded.use, v.contents),
                   {
                       l.contents <-
                           Map(function(x)
                               {
                                   s.len <- length(x) %/% 2
                                   Map(function(v) paste0(v, collapse = ""),
                                       list(head(x, s.len), tail(x, -s.len)))
                               }, strsplit(v.contents, ""))

                       mapply(function(x)
                              {
                                  paste0(sprintf(paste0(chr.wall, " %*s"),
                                                 s.contents.lenpadded.use, x[[1]]),
                                         sprintf(paste0("%*s ", chr.wall),
                                                 -s.contents.lenpadded.use, x[[2]]),
                                         collapse = "")
                              }, l.contents)
                   })
        v.sep <- paste0(c(chr.head,
                          rep(chr.sep,
                              s.contents.lenpadded +
                                  ((nchar(c(" ", chr.wall)) * 2) |> sum()) -
                                  (nchar(c(chr.head, chr.tail))  |> sum())),
                          chr.tail),
                        collapse = "")
        mapply(message, c(v.sep, v.messages, v.sep)) |> invisible()
    }
    ut.clear.disp <- function() {
        if(!is.null(dev.list())) {
            invisible(mapply(dev.off, dev.list()))
        }
        switch(commandArgs()[1],
               RStudio = cat("\014"),
               system("clear"))
    }
    ut.clear.dispdev <- function() {
    if(!is.null(dev.list()))
        ut.silent(mapply(dev.off, dev.list()))
    }
    ut.clear.globalwspc <- function() {
        local(rm(list = ls()), envir = .GlobalEnv)
    }
    ut.get.all <- function() {
        env <- parent.frame(1)
        mget(ls(envir = env), envir = env)
    }
    ut.suppress <- function(...) {
        suppressMessages(...) |> suppressWarnings()
    }
    ut.silent <- function(...) {
        sink(nullfile())
        try(..., silent = TRUE, outFile = nullfile()) |> ut.suppress()
        sink()
    }
    sup.wm <- function(..., mode = "wm") {
        switch(mode,
               w  = suppressWarnings(...),
               m  = suppressMessages(...),
               wm = suppressWarnings(suppressMessages(...)))
    }
    ut.permargs <- function(...) {
        l.args <- list(...) |> rev()
        Map(function(l, v.ind) l[v.ind],
            l.args,
            mapply(function(v)
                   seq_along(v),
                   l.args,
                   SIMPLIFY = FALSE) |>
            expand.grid())           |>
        rev()
    }
    ut.save.all <- function() {
        Map(function(n)
            save(list = n,
                 file = paste0(n, ".RData"),
                 envir = .GlobalEnv),
            grep("^df|^list",
                 ls(".GlobalEnv"), value = TRUE))
    }
    ut.time <- function() {
        mapply(function(x)
               format(Sys.time(), x),
               c("Date: %Y%m%d %z", "Time: %H%M%S")) |>
        ut.box()
    }
    ut.pad.dl0 <- function(int, width) {
        sprintf(paste0("%0", width, "d"), int)
    }
    ## parallel +---------------------------------------------+
    make.cl <- function(int.cores = parallel::detectCores(),
                        env = environment()) {
        parallel::clusterExport(cl <- parallel::makeCluster(int.cores),
                                ls(envir = env), envir = env)
        cl
    }
    max.cores <- function() {
        parallel::detectCores()
    }
    set.env.name <- function() {
        env.parent <- parent.frame(1)
        if(!identical(env.parent, .GlobalEnv)) {
            func.name <- as.character(sys.call(-1))[1]
            if(is(get(func.name, envir = env.parent), "function")) {
                invisible(structure(env.parent,
                                    env.name = func.name))
            }
        }
    }
    ## random number generation
    gen.rand.int <- function(n) {
        s.int.max <- .Machine[["integer.max"]]
        sample(-s.int.max:s.int.max, n, replace = FALSE)
    }
    ## functional constructs
    Compose <- function(...) {
        func.list <- list(...)
        if(!all(unlist(lappy(func.list, is.function))))
            stop("Argument is not a function")
        function(...) {
            Reduce(function(x, f) f(x), func.list, ...)
        }
    }
    ComposeM <- function(...) {
        fs <- list(...)
        if(!all(unlist(lappy(fs, is.function))))
            stop("Argument is not a function")
        fs.tail <- fs[-1]
        fs.head <- fs[[1]]
        function(...) {
            Reduce(function(x, f) f(x), fs.tail, fs.head(...))
        }
    }
    Curry <- function(f, ...) {
        .orig <- list(...)
        function(...) {
            do.call(f, c(.orig, list(...)))
        }
    }
    CurryL <- function(f, ...) {
        .curried <- as.list(match.call())[c(-1, -2)]
        function(...) {
            .args <- as.list(match.call())[-1]
            eval(substitute(do.call(f, c(.curried, .args))))
        }
    }
    Filter <- function(f, x, nomatch = NA_integer_) {
        `if`(any((ind <- (unlist(lapply(x, f))) > 0)),
             x[ind], nomatch)
    }
    Find <- function(f, x, right = FALSE, nomatch = NULL) {
        `if`(any((pos <- Position(f, x, right, nomatch = 0L))),
             x[[pos]], nomatch)
    }
    Map.TT <- function(f, ...) {
        mapply(FUN = match.fun(f), ...,
               SIMPLIFY  = TRUE,
               USE.NAMES = TRUE)
    }
    Map.TF <- function(f, ...) {
        mapply(FUN = match.fun(f), ...,
               SIMPLIFY  = TRUE,
               USE.NAMES = FALSE)
    }
    Map.FT <- function(f, ...) {
        mapply(FUN = match.fun(f), ...,
               SIMPLIFY  = FALSE,
               USE.NAMES = TRUE)
    }
    Map.FF <- function(f, ...) {
        mapply(FUN = match.fun(f), ...,
               SIMPLIFY  = FALSE,
               USE.NAMES = FALSE)
    }
    Map <- Map.FT
    Negate.v <- function(...) {
        # Negate a vector.
        !(...)
    }
    Position <- function(f, x, right = FALSE,
                         nomatch = NA_integer_) {
        f <- match.fun(f)
        ind <- seq_along(x)
        if(right) ind <- rev(ind)
        `for`(i, ind, if(f(x[[i]])) return(i))
        nomatch
    }
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
    ## names
    nms <- function(v, v.nm) {
        # - functional version of assigning names
        # - the main use of this is to facilitate piping in results.
        # v    : object to be renamed
        # v.nm : character vector of new names
        `names<-`(v, v.nm)
    }
    cnms <- function(v, v.nm) {
        # - functional version of assigning names
        # - the main use of this is to facilitate piping in results.
        # v    : object to be renamed
        # v.nm : character vector of new names
        `colnames<-`(v, v.nm)
    }
    rnms <- function(v, v.nm) {
        # functional version of assigning row names
        #   - the main use of this is to facilitate piping in results.
        # v    : object to be renamed
        # v.nm : character vector of new names
        `rownames<-`(v, v.nm)
    }
    nms.sort <- function(obj, ...) {
        # sort an object by its name
        # obj : object to be sorted
        # ... : any arguments to pass to `sort`
        v.nm <- names(obj)
        `if`(is.null(v.nm), obj,
             obj[sort(v.nm, ...)])
    }
    cnms.sort <- function(obj, ...) {
        # sort an object by its col names
        # obj : object to be sorted
        # ... : any arguments to pass to `sort`
        v.nm <- colnames(obj)
        `if`(is.null(v.nm), obj,
             obj[,sort(v.nm, ...)])
    }
    rnms.sort <- function(obj, ...) {
        # sort an object by its row names
        # obj : object to be sorted
        # ... : any arguments to pass to `sort`
        v.nm <- rownames(obj)
        `if`(is.null(v.nm), obj,
             obj[sort(v.nm, ...),])
    }
    ## class/type checks
    is.0 <- function(v) {
        # check if object is `0`
        # to return true:
        #   1. the object must not be null
        #   2. have length of 0
        !is.null(v) & (length(v) == 0)
    }
    is.v <- function(obj, v.class, simplify = any) {
        # multivariate version of is
        # obj:
        #   object to be tested for its class
        # v.class:
        #   character vector of classes
        # simplify:
        #   what function to apply to simplify the result.
        #   default is `any`
        f <- match.fun(simplify)
        f(unlist(mapply(function(s.class)
                        is(obj, s.class),
                        v.class,
                        SIMPLIFY = FALSE,
                        USE.NAMES = TRUE)))
    }
    class.a <- `class<-`
    ## comparisons
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
    all.T <- all
    all.F <- function(...) {
        all(!(...))
    }
    any.T <- any
    any.F <- function(...) {
        any(!(...))
    }
    ee <- `==`
    ne <- `!=`
    gt <- `>`
    lt <- `<`
    gte <- `>=`
    lte <- `<=`

    ## flow control
    if.t <- function(b.test, v.true, v.false) {
        # `if` that switches to binary or ternary version.
        # - if `v.false` is missing, switches to binary version.
        # - ternary version if like ifelse but accepts only singular boolean.

        # notes:
        # - the primary intended use of this function is to facilitate the use of if in pipes.
        # - together with the `Swap` function, allows flexible piping of arguments.
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
    ## subsetting
    ss.b <- function(v, i) {
        `[`(v, i)
    }
    ss.bb <- function(v, i) {
        `[[`(v, i)
    }
    ss.e.nm <- function(obj, v.exclude.names) {
        # subset, excluding by names
        v.obj.names <- names(obj)
        if(is.null(v.obj.names)) {
            stop("Input object has no names.")
        } else {
            b.include <- match(v.obj.names,
                               v.exclude.names,
                               nomatch = 0) == 0
            b.exclude <- !b.include
            if(all(b.exclude))
                stop("All items excluded.")
            if(any(b.include))
                obj[b.include]
        }
    }
    ssa.b <- function(v, i, v.rep) {
        `[<-`(v, i, v.rep)
    }
    ssa.bb <- function(v, i, v.rep) {
        `[[<-`(v, i, v.rep)
    }
    ## attributes
    attr.exa <- function(obj) {
        attributes(obj)
    }
    attr.ex1 <- function(obj, attr.nm) {
        attr.exa(obj)[[attr.nm]]
    }
    attr.set <- function(obj, l.attr) {
        do.call(structure, c(obj, l.attr))
    }
    attr2env <- function(obj, env = parent.frame()) {
        l <- attributes(obj)
        b.include <- match(names(l), c("dim", "names"), nomatch = 0) == 0
        if(any(b.include))
            list2env(l[b.include], envir = env)
    }
    set.list.index <- function(l) {
        # create index
        l.index <- list(i = seq_along(l))
        v.names <- names(l)
        if(!is.null(v.names))
            l.index <- c(l.index, list(names = v.names))

        # index is now a list of lists of the indexes.
        # - this is a linked list and will need to be restructured
        # - restructure to be 1 list for each item present in l containing the attributes.
        l.index.rstruc <- do.call(Map,
                                  c(list(list),
                                    Map(as.list, l.index)))

        # attach the attributes
        mapply(function(i, l.attr)
               structure(i, index = l.attr),
               l, l.index.rstruc,
               SIMPLIFY = FALSE,
               USE.NAMES = TRUE)
    }
    ## glm
    get.glm.dfres <- function(mo) {
        summary(mo)[["df"]][2]
    }
    get.glm.np <- function(mo) {
        v.df <- summary(mo)[["df"]]
        list(n = sum(v.df[2:3]), p = v.df[3])
    }
    get.glm.var <- function(mo) {
        v <- formula(mo) |> all.vars()
        list(dv = v[1], iv = v[-1])
    }
    # packages
    library(compiler)
    # options
    enableJIT(3)
    set.seed(8)
    options(list(stringsAsFactors = FALSE,
                 save.defaults = list(compress = "xz",
                                      compression_level = 9),
                 contrasts = c(unordered = "contr.treatment",
                               ordered = "contr.treatment"),
                 max.print = 40))
    # save functions
    list.ut <- ut.get.all()

    # clear workspace
    ut.clear.globalwspc()
    # turn off display devices
    ut.clear.dispdev()
    # import functions
    list2env(list.ut, envir = .GlobalEnv)

    # clear environment and show time
    ut.clear.disp()
    ut.time()

    # return functions
    list.ut
}
list2env(gen.utilities())
## working directory +-----------------------------------------------+
setwd("/mnt/d/08-academic/PhD/03-code/04-seq-med")

# FUNCTIONS +------------------------------------------------------------------+
gen.funcs.dummydata <- function(envir = new.env()) {
    # generate dummy data +---------------------------------------------+
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
    scale.p <- function(v, s.mn = 0, s.sd = 1, b.mat = TRUE) {
        scaled <- scale(v, center = FALSE, scale = sd(v)/s.sd)
        scaled <- scale(scaled, center = mean(scaled) - s.mn, scale = FALSE)
        if(!b.mat) {
            v.scaled <- c(scaled)
            attributes(v.scaled) <- attributes(scaled)[2:3]
            scaled <- v.scaled
        }
        scaled
    }
    add.err <- function(v.coef, m.static, s.e.sd, s.nm, i.bc, other.err = NULL) {
        s.dist <- switch(i.bc, b = "logistic", c = "normal")
        m.pred <- t(tcrossprod(v.coef, m.static))
        m.cor0 <- cbind(m.static[,-1], other.err)
        m.err <- cor.m(m.cor0, rep(0, ncol(m.cor0)), s.dist)
        m.obs <- switch(i.bc,
                        b = ((m.pred + m.err) > 0) + 0,
                        c = m.pred + scale.p(m.err, 0, s.e.sd, TRUE))
        m <- cbind(m.obs, m.err)
        colnames(m) <- c(s.nm, paste0(s.nm, "e"))
        m
    }
    gen.b <- function(s.n, s.prop, b.rand = FALSE) {
        s.n1 <- round(s.prop * s.n)
        s.n0 <- s.n - s.n1
        b <- c(rep(1L, s.n1), rep(0L, s.n0))
        if(b.rand) {
            b <- b[sample(1:s.n, s.n, replace = FALSE)]
        }
        matrix(b, ncol = 1)
    }
    gen.static <- function(n, l.par) {
        r    <- gen.b(n, l.par[["r"]]   , FALSE)
        cv1b <- gen.b(n, l.par[["cv1b"]], TRUE)
        cv2c <- scale.p(cor.m(cbind(r, cv1b), c(0, 0), "normal"),
                        l.par[["cv2c"]][["mn"]],
                        l.par[["cv2c"]][["sd"]])
        cbind(i = 1, matrix(c(r, cv1b, cv2c), ncol = 3, dimnames = list(NULL, names(l.par))))
    }
    gen_model <- function(n, s.seed, l.static, l.coef, v.norm.sd, s.dat) {
        # Setup generation +---------------------------------------+
        set.seed(s.seed)
        # Generate static elements: r, cv1b, cv2c +----------------+
        # The static elements are made to have 0 correlation with each other.
        # This is difficult for binary r and binary cv1b so this is done by randomising the order of cv1b only and no further efforts are made to ensure they have 0 correlation.
        # cv2c is generated to have 0 correlation with r and cv1b.
        m.static <- gen.static(n, l.static)
        # Identify data types needed +-----------------------------+
        i.m1 <- gsub("^.*m1(.)$", "\\1",   s.dat)
        i.m2 <- gsub("^.*m2(.).*$", "\\1", s.dat)
        i.y  <- gsub("^y(.).*$", "\\1",    s.dat)

    #
        # Start generating mediator 1 +----------------------------+
        # The error term is generated to have 0 correlation with the predicted term (linear combination).
        # This ensures that the coefficients can be recovered accurately.
        # This error term is retained so that subsequent error terms can be generated in a way that ensures that there is 0 correlation.
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

    #
        # Prepare data +-------------------------------------------+
        # Remove the intercept column.
        m <- cbind(m.static, y = m.y[,"y"])[,-1]
        # Make it a data.frame for use in subsequent glm functions.
        d <- as.data.frame(m)

    #
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
    gen.mo <- function(s.dat, l.par) {
        gen_model(l.par[["n"]], l.par[["seed"]], l.par[["static"]], l.par[["coef"]], l.par[["norm.e.sd"]], s.dat)
    }
    gen.mo.all <- function(v.ind, l.par) {
        v.mo <- nms(c("ycm2cm1c", "ycm2cm1b", "ybm2cm1c", "ybm2cm1b",
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
    as.list.environment(environment())
}
gen.funcs.seqmed <- function() {
## utilities
    ut.box <- function(v.contents, chr.head = "+", chr.sep = "-",
                       chr.tail = "+", chr.wall = "|", s.align = "l") {
        if(class(v.contents) |>
           match(c("integer", "numeric", "character"),
                 nomatch = 0) |>
           (any |> Negate())())
            stop("Incorrect contents format.")

        s.contents.lenmax <- nchar(v.contents) |> max()
        s.box.lenmax <- length(c(chr.head, chr.sep, chr.tail)) |> sum()
        if(s.contents.lenmax < s.box.lenmax)
            s.contents.lenmax <- s.box.lenmax
        s.contents.lenpadded <- s.contents.lenmax + 2
        s.contents.lenpadded <- (4 - (s.contents.lenpadded %% 4)) + s.contents.lenpadded

        s.contents.lenpadded.use <-
            switch(s.align,
                   l = -s.contents.lenpadded,
                   r = s.contents.lenpadded,
                   c = s.contents.lenpadded/2)
        v.messages <-
            switch((s.align == "c") + 1,
                   sprintf(paste0(chr.wall, " %*s ", chr.wall),
                           s.contents.lenpadded.use, v.contents),
                   {
                       l.contents <-
                           Map(function(x)
                               {
                                   s.len <- length(x) %/% 2
                                   Map(function(v) paste0(v, collapse = ""),
                                       list(head(x, s.len), tail(x, -s.len)))
                               }, strsplit(v.contents, ""))

                       mapply(function(x)
                              {
                                  paste0(sprintf(paste0(chr.wall, " %*s"),
                                                 s.contents.lenpadded.use, x[[1]]),
                                         sprintf(paste0("%*s ", chr.wall),
                                                 -s.contents.lenpadded.use, x[[2]]),
                                         collapse = "")
                              }, l.contents)
                   })
        v.sep <- paste0(c(chr.head,
                          rep(chr.sep,
                              s.contents.lenpadded +
                                  ((nchar(c(" ", chr.wall)) * 2) |> sum()) -
                                  (nchar(c(chr.head, chr.tail))  |> sum())),
                          chr.tail),
                        collapse = "")
        mapply(message, c(v.sep, v.messages, v.sep)) |> invisible()
    }
    ut.clear.disp <- function() {
        if(!is.null(dev.list())) {
            invisible(mapply(dev.off, dev.list()))
        }
        switch(commandArgs()[1],
               RStudio = cat("\014"),
               system("clear"))
    }
    ut.clear.dispdev <- function() {
    if(!is.null(dev.list()))
        ut.silent(mapply(dev.off, dev.list()))
    }
    ut.clear.globalwspc <- function() {
        local(rm(list = ls()), envir = .GlobalEnv)
    }
    ut.get.all <- function() {
        env <- parent.frame(1)
        mget(ls(envir = env), envir = env)
    }
    ut.suppress <- function(...) {
        suppressMessages(...) |> suppressWarnings()
    }
    ut.silent <- function(...) {
        sink(nullfile())
        try(..., silent = TRUE, outFile = nullfile()) |> ut.suppress()
        sink()
    }
    sup.wm <- function(..., mode = "wm") {
        switch(mode,
               w  = suppressWarnings(...),
               m  = suppressMessages(...),
               wm = suppressWarnings(suppressMessages(...)))
    }
    ut.permargs <- function(...) {
        l.args <- list(...) |> rev()
        Map(function(l, v.ind) l[v.ind],
            l.args,
            mapply(function(v)
                   seq_along(v),
                   l.args,
                   SIMPLIFY = FALSE) |>
            expand.grid())           |>
        rev()
    }
    ut.save.all <- function() {
        Map(function(n)
            save(list = n,
                 file = paste0(n, ".RData"),
                 envir = .GlobalEnv),
            grep("^df|^list",
                 ls(".GlobalEnv"), value = TRUE))
    }
    ut.time <- function() {
        mapply(function(x)
               format(Sys.time(), x),
               c("Date: %Y%m%d %z", "Time: %H%M%S")) |>
        ut.box()
    }
    ut.pad.dl0 <- function(int, width) {
        sprintf(paste0("%0", width, "d"), int)
    }
    ## parallel +---------------------------------------------+
    make.cl <- function(int.cores = parallel::detectCores(),
                        env = environment()) {
        parallel::clusterExport(cl <- parallel::makeCluster(int.cores),
                                ls(envir = env), envir = env)
        cl
    }
    max.cores <- function() {
        parallel::detectCores()
    }
    set.env.name <- function() {
        env.parent <- parent.frame(1)
        if(!identical(env.parent, .GlobalEnv)) {
            func.name <- as.character(sys.call(-1))[1]
            if(is(get(func.name, envir = env.parent), "function")) {
                invisible(structure(env.parent,
                                    env.name = func.name))
            }
        }
    }
    ## random number generation
    gen.rand.int <- function(n) {
        s.int.max <- .Machine[["integer.max"]]
        sample(-s.int.max:s.int.max, n, replace = FALSE)
    }
    ## functional constructs
    Compose <- function(...) {
        func.list <- list(...)
        if(!all(unlist(lappy(func.list, is.function))))
            stop("Argument is not a function")
        function(...) {
            Reduce(function(x, f) f(x), func.list, ...)
        }
    }
    ComposeM <- function(...) {
        fs <- list(...)
        if(!all(unlist(lappy(fs, is.function))))
            stop("Argument is not a function")
        fs.tail <- fs[-1]
        fs.head <- fs[[1]]
        function(...) {
            Reduce(function(x, f) f(x), fs.tail, fs.head(...))
        }
    }
    Curry <- function(f, ...) {
        .orig <- list(...)
        function(...) {
            do.call(f, c(.orig, list(...)))
        }
    }
    CurryL <- function(f, ...) {
        .curried <- as.list(match.call())[c(-1, -2)]
        function(...) {
            .args <- as.list(match.call())[-1]
            eval(substitute(do.call(f, c(.curried, .args))))
        }
    }
    Filter <- function(f, x, nomatch = NA_integer_) {
        `if`(any((ind <- (unlist(lapply(x, f))) > 0)),
             x[ind], nomatch)
    }
    Find <- function(f, x, right = FALSE, nomatch = NULL) {
        `if`(any((pos <- Position(f, x, right, nomatch = 0L))),
             x[[pos]], nomatch)
    }
    Map.TT <- function(f, ...) {
        mapply(FUN = match.fun(f), ...,
               SIMPLIFY  = TRUE,
               USE.NAMES = TRUE)
    }
    Map.TF <- function(f, ...) {
        mapply(FUN = match.fun(f), ...,
               SIMPLIFY  = TRUE,
               USE.NAMES = FALSE)
    }
    Map.FT <- function(f, ...) {
        mapply(FUN = match.fun(f), ...,
               SIMPLIFY  = FALSE,
               USE.NAMES = TRUE)
    }
    Map.FF <- function(f, ...) {
        mapply(FUN = match.fun(f), ...,
               SIMPLIFY  = FALSE,
               USE.NAMES = FALSE)
    }
    Map <- Map.FT
    Negate.v <- function(...) {
        # Negate a vector.
        !(...)
    }
    Position <- function(f, x, right = FALSE,
                         nomatch = NA_integer_) {
        f <- match.fun(f)
        ind <- seq_along(x)
        if(right) ind <- rev(ind)
        `for`(i, ind, if(f(x[[i]])) return(i))
        nomatch
    }
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
    ## names
    nms <- function(v, v.nm) {
        # - functional version of assigning names
        # - the main use of this is to facilitate piping in results.
        # v    : object to be renamed
        # v.nm : character vector of new names
        `names<-`(v, v.nm)
    }
    cnms <- function(v, v.nm) {
        # - functional version of assigning names
        # - the main use of this is to facilitate piping in results.
        # v    : object to be renamed
        # v.nm : character vector of new names
        `colnames<-`(v, v.nm)
    }
    rnms <- function(v, v.nm) {
        # functional version of assigning row names
        #   - the main use of this is to facilitate piping in results.
        # v    : object to be renamed
        # v.nm : character vector of new names
        `rownames<-`(v, v.nm)
    }
    nms.sort <- function(obj, ...) {
        # sort an object by its name
        # obj : object to be sorted
        # ... : any arguments to pass to `sort`
        v.nm <- names(obj)
        `if`(is.null(v.nm), obj,
             obj[sort(v.nm, ...)])
    }
    cnms.sort <- function(obj, ...) {
        # sort an object by its col names
        # obj : object to be sorted
        # ... : any arguments to pass to `sort`
        v.nm <- colnames(obj)
        `if`(is.null(v.nm), obj,
             obj[,sort(v.nm, ...)])
    }
    rnms.sort <- function(obj, ...) {
        # sort an object by its row names
        # obj : object to be sorted
        # ... : any arguments to pass to `sort`
        v.nm <- rownames(obj)
        `if`(is.null(v.nm), obj,
             obj[sort(v.nm, ...),])
    }
    ## class/type checks
    is.0 <- function(v) {
        # check if object is `0`
        # to return true:
        #   1. the object must not be null
        #   2. have length of 0
        !is.null(v) & (length(v) == 0)
    }
    is.v <- function(obj, v.class, simplify = any) {
        # multivariate version of is
        # obj:
        #   object to be tested for its class
        # v.class:
        #   character vector of classes
        # simplify:
        #   what function to apply to simplify the result.
        #   default is `any`
        f <- match.fun(simplify)
        f(unlist(mapply(function(s.class)
                        is(obj, s.class),
                        v.class,
                        SIMPLIFY = FALSE,
                        USE.NAMES = TRUE)))
    }
    class.a <- `class<-`
    ## comparisons
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
    all.T <- all
    all.F <- function(...) {
        all(!(...))
    }
    any.T <- any
    any.F <- function(...) {
        any(!(...))
    }
    ee <- `==`
    ne <- `!=`
    gt <- `>`
    lt <- `<`
    gte <- `>=`
    lte <- `<=`

    ## flow control
    if.t <- function(b.test, v.true, v.false) {
        # `if` that switches to binary or ternary version.
        # - if `v.false` is missing, switches to binary version.
        # - ternary version if like ifelse but accepts only singular boolean.

        # notes:
        # - the primary intended use of this function is to facilitate the use of if in pipes.
        # - together with the `Swap` function, allows flexible piping of arguments.
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
    ## subsetting
    ss.b <- function(v, i) {
        `[`(v, i)
    }
    ss.bb <- function(v, i) {
        `[[`(v, i)
    }
    ss.e.nm <- function(obj, v.exclude.names) {
        # subset, excluding by names
        v.obj.names <- names(obj)
        if(is.null(v.obj.names)) {
            stop("Input object has no names.")
        } else {
            b.include <- match(v.obj.names,
                               v.exclude.names,
                               nomatch = 0) == 0
            b.exclude <- !b.include
            if(all(b.exclude))
                stop("All items excluded.")
            if(any(b.include))
                obj[b.include]
        }
    }
    ssa.b <- function(v, i, v.rep) {
        `[<-`(v, i, v.rep)
    }
    ssa.bb <- function(v, i, v.rep) {
        `[[<-`(v, i, v.rep)
    }
    ## attributes
    attr.exa <- function(obj) {
        attributes(obj)
    }
    attr.ex1 <- function(obj, attr.nm) {
        attr.exa(obj)[[attr.nm]]
    }
    attr.set <- function(obj, l.attr) {
        do.call(structure, c(obj, l.attr))
    }
    attr2env <- function(obj, env = parent.frame()) {
        l <- attributes(obj)
        b.include <- match(names(l), c("dim", "names"), nomatch = 0) == 0
        if(any(b.include))
            list2env(l[b.include], envir = env)
    }
    set.list.index <- function(l) {
        # create index
        l.index <- list(i = seq_along(l))
        v.names <- names(l)
        if(!is.null(v.names))
            l.index <- c(l.index, list(names = v.names))

        # index is now a list of lists of the indexes.
        # - this is a linked list and will need to be restructured
        # - restructure to be 1 list for each item present in l containing the attributes.
        l.index.rstruc <- do.call(Map,
                                  c(list(list),
                                    Map(as.list, l.index)))

        # attach the attributes
        mapply(function(i, l.attr)
               structure(i, index = l.attr),
               l, l.index.rstruc,
               SIMPLIFY = FALSE,
               USE.NAMES = TRUE)
    }
    ## glm
    get.glm.dfres <- function(mo) {
        summary(mo)[["df"]][2]
    }
    get.glm.np <- function(mo) {
        v.df <- summary(mo)[["df"]]
        list(n = sum(v.df[2:3]), p = v.df[3])
    }
    get.glm.var <- function(mo) {
        v <- formula(mo) |> all.vars()
        list(dv = v[1], iv = v[-1])
    }

    # helper +----------------------------------------------------------+
    ## environment and variables management +-----------------+
    rm.attr <- function(o) {
        attributes(o) <- attributes(o)[c("dim", "dimnames")]
        o
    }
    add.attr <- function(o, l) {
        `attributes<-`(o, c(l, attributes(o)))
    }
    add.nm <- function(v, v.nm) {
        if.t(any(!is.vector(v),
                !is.character(v.nm),
                !identical(length(v), length(v.nm))),
            stop("Error"),
          nms(v, v.nm))
    }
    add.cnm <- function(d, v.nm) {
        if(any(!any(class(d) %in% c("matrix", "data.frame")), !is.character(v.nm)))
            stop("Error")
        if(!identical(ncol(d), length(v.nm)))
            stop("Error")
        cnms(d, v.nm)
    }
    add.rnm <- function(d, v.nm) {
        if(any(!any(class(d) %in% c("matrix", "data.frame")), !is.character(v.nm)))
            stop("Error")
        if(!identical(nrow(d), length(v.nm)))
            stop("Error")
        rnms(d, v.nm)
    }
    list.var <- function(l) {
        if.t(class(l) != "list",
             as.list(l), l)               |>
        list2env(envir = parent.frame(1)) |>
        ut.silent()
    }
    class.check <- function(obj, v.valid.classes, invert = FALSE) {
        v.checks <- match(obj, v.valid.classes, nomatch = 0)
        if(invert) {
            all(v.checks == 0)
        } else {
            any(v.checks > 0)
        }
    }
    setup.env <- function(list.models,
                          int.iter,
                          b.parallel = "auto",
                          int.cores = "max",
                          canonical = FALSE)
    {
        # hardcoded values
        int.parallel.threshold <- 1e4L

        # function to setup consistent environment across functions
        # argumemts:
        #     list.models : list of fitted models
        # each fitted model must have as its first term the treatment variable.

        # get all the terms used in all the models
        l.nm.av <- Map(function(mo) formula(mo) |> all.vars(), list.models)
        # get dependent variables
        v.nm.dv <- Map(ss.b, l.nm.av, 1) |> unlist()

        # extract degrees of freedom
        m.df <- do.call(rbind, Map(ss.bb, Map(summary, list.models), "df"))

        # get the number of coefficients
        # - order them from most tp least number
        # - this order should correspond to:
        #   + y model
        #   + m2 model
        #   + m1 model
        p <- m.df[,3] |> nms(v.nm.dv) |> sort(decreasing = TRUE)

        # get the sample size from degrees of freedom and check that the sample size is the same across models.
        v.n <- (m.df[,2] + m.df[,3]) |> unique()
        n <- if.t(length(v.n) > 1,
                  stop("Unequal sample sizes across models."),
                  v.n)

        # get the residual degrees of freedom
        df.res <- (m.df[,2] |> nms(v.nm.dv))[names(p)]

        l.np <- list(n = n, p = p, df.res = df.res)

        # name the models and arrange them in descending order of number of coefficients
        list.models <- nms(list.models, v.nm.dv)[names(p)]

        # dataset
        d <- model.frame(list.models[[1]])

        # replace dv with the names of p as the right order of dv
        v.nm.dv <- names(p)
        v.nm.m <- v.nm.dv[-1]

        # extract iv and identify common terms
        l.nm.iv <- Map(ss.b, l.nm.av, -1)
        v.nm.common <- Reduce(intersect, l.nm.iv)

        # extract treatment variable
        ## the first term should be the treatment variable.
        v.nm.r <- Map(ss.b, l.nm.iv, 1) |> unlist()
        ## making sure that the first term of each model is the same.
        if(!identical.v(v.nm.r))
            stop("Error with treatment variable in models.")
        s.nm.r <- v.nm.r[1] |> unname()
        ## get unique values of treatment
        v.r <- d[[s.nm.r]] |>
               unique()    |>
               sort(decreasing = TRUE)

        # get covariates
        b.cv <- match(v.nm.common, s.nm.r, nomatch = 0) == 0
        v.nm.cv <- if.t(any(b.cv), v.nm.common[b.cv], NA)

        if(canonical) {
            # giving canonical names to dv, r, iv
            names(v.nm.dv) <- c("y", sprintf("m%02d", seq_along(v.nm.dv[-1])))
            names(s.nm.r) <- "r"
            names(v.nm.cv) <- sprintf("cv%02d", seq_along(v.nm.cv))
        } else {
            names(v.nm.dv) <- v.nm.dv
            names(s.nm.r) <- s.nm.r
            names(v.nm.cv) <- v.nm.cv
        }

        # setup new iv and allv names
        v.nm.iv <- c(s.nm.r, v.nm.cv)
        v.nm.av <- c(v.nm.dv, v.nm.iv)

        # identify family of each model
        v.fam <- Map(ss.bb,
                     Map(family, list.models),
                     "family") |>
                 unlist()      |>
                 Swap(gsub, 3)("^(.).*", "\\1")
        s.fam <- paste0(v.fam, collapse = "")
        ## use abbreviations to identify models:
        ##     g for gaussian
        ##     b for binary
        l.fam <- local({
            v.abv <- c("g", "b")
            Map(function(x)
                structure(v.abv == x,
                          names = v.abv),
                v.fam)})

        # number of mediators and models
        n.mo <- length(list.models)
        n.med <- n.mo - 1

        # process parallel parameters
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
        # gather results
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
        ut.silent()
    }
    ## objects manipulation +---------------------------------+
    mat.dupe <- function(m, fill = NA) {
        m[] <- fill
        class(m) <- class(fill)
        m
    }
    mat.mirr <- function(m, s.orient) {
        m <- cbind(m)
        v.dim <- dim(m)
        cbind(switch(s.orient,
                     lr = m[,v.dim[2]:1],
                     ud = m[v.dim[1]:1,]))
    }
    mat.t.list <- function(m, by, ret) {
        if(!any(by %in% c(1, 2))) stop("Error")
        l <- unlist(apply(m, by, list), recursive = FALSE)
        switch(ret, v = l, m = Map(rbind, l))
    }
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
    vec.t.matc <- function(v) {
        v |> as.matrix()
    }
    vec.t.matr <- function(v) {
        v |> as.matrix() |> t()
    }
    combi <- function(vm1, vm2) {
        m1 <- cbind(vm1)
        m2 <- cbind(vm2)
        n.m1 <- nrow(m1)
        n.m2 <- nrow(m2)
        m <- cbind(m1[sort(rep(1:n.m1, n.m2)),],
                   m2[rep(1:n.m2, n.m1),])
        m[order(apply(m, 1, paste, collapse = "")),]
    }
    combi.bin <- function(v.nm, s.ret) {
        l.bin.resp <- rep(list(0:1), length(v.nm))
        m.combi <- add.cnm(cbind(Reduce(combi, l.bin.resp)), v.nm)
        l.combi <- mat.t.list(m.combi, 1, s.ret)
    }
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
    str.pad0 <- function(v, s.len = max(nchar(v))) {
        sprintf(paste0("%0", s.len, "d"), v)
    }
    mo.upd <- function(mo, fml) {
        update(mo,
               formula = fml,
               family = family(mo),
               data = model.frame(mo))
    }
    ## comparisons and flow control +-------------------------+
    pblapply.sw <- function(b.parallel, int.cores, env, ...) {
        f <- pbapply::pblapply
        args <- list(...)
        if(b.parallel) {
            cl <- make.cl(int.cores = int.cores, env = environment())
            args <- c(args, list(cl = cl))
        }
        obj <- do.call(f, args)
        if(b.parallel) parallel::stopCluster(cl)
        obj
    }
    pbMap <- local({
        f <- pbapply::pbmapply
        formals(f)[["SIMPLIFY"]] <- FALSE
        f
    })
    # set up environment +----------------------------------------------+
    ## glm families +-----------------------------------------+
    get.fam.raw <- function(mo) {
        with(family(mo), c(family, link))
    }
    get.fam.bool <- function(mo) {
        b <- get.fam.raw(mo)[1] == "gaussian"
        structure(c(b, !b), .Names = c("g", "b"))
    }
    get.fam.l <- function(l.mo) {
        attr2env(l.mo)
        add.nm(Map(get.fam.bool, l.mo), v.nm.dv)
    }
    get.fam.v <- function(l.mo) {
        b <- unlist(unname(get.fam.l(l.mo)))
        paste0(names(b)[b], collapse = "")
    }
    get.mo.dep <- function(l.mo) {
        attr2env(l.mo)
        l.dep <- Map(function(v.nm, v.var)
                     {
                         v <- intersect(v.nm.dv, v.var[-(1:2)])
                         if(is.0(v)) {
                             NULL
                         } else {
                             v
                         }
                     }, v.nm.dv, l.nm.av)
        l.dep.n <- mapply(function(v) length(v), l.dep)
        list(dep = l.dep, n = l.dep.n, n.sorted = sort(l.dep.n))
    }
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
    gen.ci <- function(s.ci) {
        if(any(!is.numeric(s.ci),
               length(s.ci) > 1,
               s.ci < 0,
               s.ci > 1)) stop("Error")
        s.lb <- (1 - s.ci)/2
        s.ub <- s.ci + s.lb
        c(lb = s.lb, ub = s.ub)
    }
    ci.perc <- function(v, s.ci) {
        quantile(v, probs = gen.ci(s.ci), type = 7)
    }

    # mathematical +----------------------------------------------------+
    dif <- function(...) Reduce(`-`, ...)
    div <- function(...) Reduce(`/`, ...)
    cumdif <- function(...) Reduce(`-`, ..., accumulate <- TRUE)
    cumdiv <- function(...) Reduce(`/`, ..., accumulate <- TRUE)
    gmean <- function(v) v |> log() |> mean() |> exp()
    logit <- qlogis
    expit <- plogis
    gen.seeds <- function(int.sims, s.seed = NULL) {
        # generate seeds
        s.int.max <- .Machine[["integer.max"]]
        set.seed(s.seed)
        sample((-s.int.max):s.int.max, int.sims,
               replace = FALSE)
    }

    # model fitting and parmeters extraction +--------------------------+
    chr.ap.nm <- function(s.nm, v) {
        paste0(s.nm, "~", v)
    }
    chr.clp <- function(v, type = "") {
        # string utility: collapse
        paste0(v, collapse = type)
    }
    chr.pad <- function(v, s.chr = " ") {
        # string utility: pad
        v.nchar <- nchar(v)
        v.padwd <- max(v.nchar) - v.nchar
        mapply(function(s, s.wd) paste0(c(s, rep(s.chr, s.wd)), collapse = "") , v, v.padwd)
    }
    chr.rp.i <- function(v) {
        # Replace "(Intercept)" with 1
        gsub("^\\(Intercept\\)$", "1", v)
    }
    chr.sur <- function(s, s.chr = "\"") {
        # string utility: surround
        chr.clp(c(s.chr, s, s.chr))
    }
    ind <- function(v.char) {
        v.uniq <- unique(v.char)
        structure(1:length(v.uniq), .Names = v.uniq)
    }
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
    m.rcon <- function(d) {
        v.nm.r <- ind(d[,"row"])
        v.nm.c <- ind(d[,"col"])

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
    m.symm <- function(m) {
        m[upper.tri(m)] <- t(m)[upper.tri(m)]
        m
    }
    rm.null.1 <- function(i) {
        i[!mapply(function(l1) is.null(l1), i)]
    }
    rm.null <- function(l) {
        # list utility: remove null elements
        Map(function(l1)
            {
                if(is.list(l1)) {
                    rm.null.1(l1)
                } else {
                    l1
                }
            }, rm.null.1(l))
    }
    catf <- function(v, lineend, file) {
        writeBin(paste0(c(v, ""), collapse = lineend) |>
                 charToRaw(), file)
    }
    ## extract parameters +-----------------------------------+
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
                                       cnms(c("dv", "iv", "coef")) |>
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
    mp.ext.t1.vi <- function(mp.out) {
        # extract names
        v.nm.n <-  mp.out[["raw"]][["nm.n"]]
        v.nm.o <- names(v.nm.n)
        names(v.nm.o) <- v.nm.n
        names(v.nm.o) <- toupper(names(v.nm.o))
        # extract variable index
        mp.vind <- do.call(rbind, Map(m.flat, mp.out[["est"]][["tech1"]][["parameterSpecification"]], na.rm = TRUE))
        # remove those that are non-existent
        mp.vind <- mp.vind[mp.vind[,"val"] > 0,]
        # replace the dollar signs used for threshold in dependent binary variables
        mp.vind[,"col"] <- gsub("\\$1$", "", mp.vind[,"col"])
        # replace new names with original ones
        mp.vind <- data.frame(mapply(function(v)
                                     {
                                         v[v != "1"] <- v.nm.o[v[v != "1"]]
                                         v
                                     }, mp.vind[,c("row", "col")]),
                              mp.vind["val"])
        # output extracted indexes with their respective names
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
    mp.ext.t3.vc <- function(mp.out) {
        mp.varindex <- attributes(mp.out)[["varindex"]]
        mp.t3.vc <- m.symm(mp.out[["est"]][["tech3"]][["paramCov"]])
        v.nm <- names(mp.varindex)
        v.var.ind <- which(mapply(function(i)
                                  {
                                      v  <- unlist(strsplit(i, "~"))
                                      v[1] == v[2]
                                  }, v.nm))
        rownames(mp.t3.vc) <- colnames(mp.t3.vc) <- v.nm
        # m.est.vcov[abs(m.est.vcov) < .machine[["double.eps"]]] <- 0
        if(length(v.var.ind) > 0) {
            mp.t3.vc <- mp.t3.vc[-v.var.ind, -v.var.ind]
        }

        list(vcov = mp.t3.vc[sort(rownames(mp.t3.vc)), sort(colnames(mp.t3.vc))])
    }
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
    get.mo.par.ce <- function(l.args) {
        list.var(l.args)
        get.glm.par(l.mo, v.nm.dv)
    }
    get.mo.par.sa <- function(l.args) {
        list.var(l.args)
        get.mp.par(fit.mp(l.mo, m.b, v.nm.dv, v.nm.iv, int.cores))
    }
    get.glm.par <- function(l.mo, v.nm.dv) {
        # extract coefficients +----------------------------------+
        v.coef <- do.call(c,
                          unname(Map(function(s.nm.mo, mo)
                                     {
                                         v.coef <- coef(mo)
                                         names(v.coef) <- chr.ap.nm(s.nm.mo, chr.rp.i(names(v.coef)))
                                         v.coef
                                     }, v.nm.dv, l.mo)))
        v.coef <- v.coef[sort(names(v.coef))]
        # extract residual variance (n - 1) +---------------------+
        v.res.var <- mapply(function(s.nm.mo, mo)
                            {
                                v.var <- NULL
                                if(all(get.fam.raw(mo) == c("gaussian", "identity"))) {
                                    v.var <- var(residuals(mo))
                                }
                                v.var
                            }, v.nm.dv, l.mo)
        # extract residual sum of squares +-----------------------+
        v.rss <- structure(mapply(function(mo) crossprod(residuals(mo)), l.mo),
                           .Names = v.nm.dv)
        # extract np +--------------------------------------------+
        l.np <- get.np(l.mo, "np")
        # Extract vcov +------------------------------------------+
        m.vcov <- m.rcon(do.call(rbind,
                                 Map(function(s.nm.mo, mo)
                                     {
                                         m.vcov <- vcov(mo)
                                         dimnames(m.vcov) <- Map(function(v.nm) chr.ap.nm(s.nm.mo, chr.rp.i(v.nm)),
                                                                 dimnames(m.vcov))
                                         m.flat(m.vcov)
                                     }, v.nm.dv, l.mo)))
        list(coef = v.coef, res.var = v.res.var,
             rss = v.rss, np = l.np, vcov = m.vcov)
    }
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
    get.mo.par <- function(l.mo, s.mode, v.nm.dv, v.nm.iv, m.b, int.cores) {
        l.args <- list(l.mo = l.mo,
                       v.nm.dv = v.nm.dv,
                       v.nm.iv = v.nm.iv,
                       m.b = m.b, int.cores = int.cores)
        switch(s.mode,
               ce = get.mo.par.ce,
               sa = get.mo.par.sa)(l.args)
    }
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
    gen.coef <- function(l.mo.par, s.meth, s.mode, v.nm.dv, v.nm.iv, m.b, int.cores) {
        m.coef <- switch(s.meth,
                         mvn = {
                             require(mvtnorm)
                             with(l.mo.par, rmvnorm(1, mean = coef, sigma = vcov))
                         },
                         boot = {
                             l.mo.boot <- upd.mo.boot1(l.mo)
                             switch(s.mode,
                                    ce = get.glm.par(Map(function(mo)
                                                         update(mo,
                                                                data = model.frame(mo)),
                                                         l.mo.boot),
                                                     v.nm.dv),
                                    #v.nm.dv[1], v.nm.dv[-1]),
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
        # Random draw of path estimates
        l.coef <- gen.coef(l.mo.par, s.meth, s.mode, v.nm.dv, v.nm.iv, m.b, int.cores)
        # Random draw of error terms
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
    ## fitting: mplus +---------------------------------------+
    gen.u.vmn <- function(m) {
        # mplus modelling: generate latent u mean statements
        v <- apply(combi(colnames(m), c("@0]", "@1")), 1,
                   function(x) paste0(x, collapse = "", sep = ""))
        gsub("^(.*@0])$", "[\\1", v)
    }
    gen.u.by <- function(m.u, m.b) {
        # mplus modelling: generate latent u by statements
        paste(colnames(m.u), "BY",
              apply(structure(paste0(m.u, "@", m.b),
                              .Dim = dim(m.u)), 2,
                    function(v) paste0(v, collapse = " ", sep = "")),
              sep = " ")
    }

    gen.u.with <- function(v.nm.u) {
        # mplus modelling: generate latent u with statements
        apply(gen.u.cmbn(v.nm.u), 2,
              function(x) paste0(paste(x, collapse = " WITH "), "@0"))
    }
    gen.mp.warn <- function(mp.output) {
        # mplus modelling: parse warnings from mplus
        s.ln.warn <- grepl("^\\s+WARNING: |(AVOID SINGULARITY)", mp.output)
        list(b.warn = any(s.ln.warn), n.warn = sum(s.ln.warn))
    }
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
    mp.nm <- function(v.nm.dv, v.nm.iv) {
        # mplus modelling: generate mplus variable names
        v.nm.o <- c(v.nm.dv[1], v.nm.iv, v.nm.dv[-1])
        v.nm.n.iv <- if.t(length(v.nm.iv) == 1, "r",
                         c("r", paste0("cv", sprintf("%02d", seq_along(v.nm.iv[-1])))))
        v.nm.n.m <- paste0("m", sprintf("%02d", rev(seq_along(v.nm.dv[-1]))))
        v.nm.n <- c("y", v.nm.n.iv, v.nm.n.m)
        nms(v.nm.n, v.nm.o)
    }
    mp.d <- function(l.mo, v.mp.nm, s.fn) {
        # mplus modelling: generate mplus data
        # l.mo   : fitted models
        # v.mp.nm: mplus converted names
        # s.fn   : data output filename
        d <- model.frame(l.mo[[1]])[names(v.mp.nm)]
        con.f <- file(s.fn, open = "wb")
        write.table(d, file = con.f,
                    sep = ",", na = ".", dec = ".",
                    quote = FALSE, row.names = FALSE, col.names = FALSE,
                    fileEncoding = "utf8", eol = "\n")
        close(con.f)
        list(d = readLines(s.fn))
    }
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
                             m[,1] <- chr.pad(m[,1])
                         c(paste0(i.nm, ":", collpase = ""),
                           apply(m, 1, function(n) chr.clp(c(rep(" ", 4), chr.clp(n, " "), ";"))),
                           "")
                     }, names(mp.inp), mp.inp) |> unlist() |> unname()
        if(tail(v.inp, 1) == "")
            v.inp <- v.inp[-length(v.inp)]
        v.inp
    }
    mp.an.mo.lu <- function(m.b, v.nm.dv.n) {
        m.u.cmb <- gen.u.cmbn(v.nm.dv.n)
        n.u.cmb <- ncol(m.u.cmb)
        if(!all(dim(m.u.cmb) == dim(m.b))) stop("Error")
        v <- c(gen.u.vmn(m.u.cmb),
               gen.u.by(m.u.cmb, m.b))
        if(n.u.cmb > 1) v <- c(v, gen.u.with(colnames(m.u.cmb)))
        list(v, v.nm.u = colnames(m.u.cmb))
    }

    mp.an.mo <- function(l.mo, m.b, v.nm.dv.n, v.nm.n) {
        # v.nm.n are the new names used in mplus.
        # +------------------------------------------------------+ #
        # Recreate regressions statements.
        # In mplus, these are ON statements.
        # Go through each of the models and recreate the statements.
        v.model.on <-
            Map(function(mo)
                {
                    # model formula for a single model.
                    # Extract formula and get corresponding new names
                    # first element is the dependent variable
                    # second element contai
                    v.nm.n <- v.nm.n[formula(mo) |> all.vars()]

                    # Create statement
                    paste0(c(v.nm.n[1], "ON", v.nm.n[-1]), collapse = " ")
                }, l.mo) |> unlist()
        # +------------------------------------------------------+ #
        # Create latent u statements
        m.u.cmb <- gen.u.cmbn(v.nm.dv.n)
        n.u.cmb <- ncol(m.u.cmb)
        # Names of latent u variables
        v.nm.u = colnames(m.u.cmb)

        if(!do.call(all.equal, Map(dim, list(m.u.cmb, m.b))))
            stop("Error: Constrain matrix m.b is incorrectly specified.")

        # Latent u constrain mean and variances
        v.latu.meanvar <- gen.u.vmn(m.u.cmb)
        v.latu.by <- gen.u.by(m.u.cmb, m.b)
        v.latu <- c(v.latu.meanvar, v.latu.by)

        if(n.u.cmb > 1)
            v.latu <- c(v.latu, gen.u.with(v.nm.u))
        # +------------------------------------------------------+ #
        # Force independent variables to be uncorrelated to each other
        v.vars.force <- c(setdiff(v.nm.n, v.nm.dv.n), v.nm.u)
        l.combn.force <- combn(v.vars.force, 2, simplify = FALSE)

        mp.gen.nocor <- function(v.var, lat.u.prefix = "u") {
            l.combn <- combn(v.var, 2, simplify = FALSE)
            regex.grep <- paste0(lat.u.prefix, "\\d+")

            Map(function(v)
                grepl(regex.grep, v)              |>
                sum()                             |>
                ne(2), l.combn)                   |>
            unlist()                              |>
            Swap(ss.b, 2)(l.combn)                |>
            Swap(do.call, 2)(rbind)               |>
            apply(1, paste0, collapse = " WITH ") |>
            paste0("@0")
        }


        # +------------------------------------------------------+ #
        # Combine statements for MODEL section.
        add.attr(c(v.model.on, v.latu),
                 list(s.nm.u = v.nm.u))
    }
    mp.i <- function(l.mo, m.b, v.nm.n, v.nm.dv, int.cores, s.iter) {
        # function to create mplus input file
        # +----------------------------------+ #
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
        mp.inp[["DATA"]][["FILE"]] <- chr.sur(v.nm.f["dat"])
        mp.inp[["VARIABLE"]][["NAMES"]] <- chr.clp(v.nm.n, " ")
        mp.inp[["VARIABLE"]][["USEVARIABLE"]] <- mp.inp[["VARIABLE"]][["NAMES"]]
        bv.mo.cat <- mapply(function(mo) all(get.fam.raw(mo) == c("binomial", "logit")), l.mo)
        if(any(bv.mo.cat)) {
            mp.inp[["VARIABLE"]][["CATEGORICAL"]] <- chr.clp(v.nm.dv.n[which(bv.mo.cat)], " ")
            mp.inp[["ANALYSIS"]][["LINK"]] <- "LOGIT"
        }
        mp.inp[["MODEL"]] <- v.an.mo
        mp.inp[["ANALYSIS"]][["PROCESSORS"]] <- int.cores
        mp.inp[["ANALYSIS"]][["ITERATIONS"]] <- s.iter
        mp.inp[["SAVEDATA"]][["RESULTS"]] <- chr.sur(v.nm.f["sav"])
        mp.inp <- mp.i.l2v(mp.inp)
        catf(mp.inp, "\r\n", file = v.nm.f["inp"])
        gen.mplus.run(v.nm.f["cmd"])
        list(nm = list(f = v.nm.f, n = v.nm.n, u = attr(v.an.mo, "s.nm.u")),
             np = l.np, i = mp.inp)
    }
    mp.o <- function(l.mo,
                     m.b,
                     v.nm.n,
                     v.nm.dv,
                     int.cores,
                     s.iter) {
        # Function to run mplus models and read in output.
        ## Load mplusautomation
        if(!tryCatch(require(MplusAutomation) |> suppressWarnings() |> suppressMessages(),
                     error = function(e) e))
            stop("Error: library MplusAutomation is not installed.")

        ## Generate input file and command batch to run mplus
        mp.inp <- mp.i(l.mo, m.b, v.nm.n, v.nm.dv, int.cores, s.iter)
        v.nm.f <- mp.inp[["nm"]][["f"]]
        mp.inp <- list(nm.n = mp.inp[["nm"]][["n"]],
                       nm.u = mp.inp[["nm"]][["u"]],
                       np = mp.inp[["np"]],
                       i = mp.inp[["i"]])
        mp.dat <- mp.d(l.mo, v.nm.n, v.nm.f["dat"])

        ## Run mplus via the batch file
        system(paste(v.nm.f["cmd"], v.nm.f["inp"]),
               intern = TRUE) |>
    ut.silent()

    # Read in output and results files.
    mp.out <- list(o = readLines(v.nm.f["out"]),
                   s = readLines(v.nm.f["sav"]))
    s.warning <- gen.mp.warn(mp.out[[1]])
    mp.results <- readModels(target = v.nm.f["out"],
                             quiet = TRUE)
    unlink(v.nm.f)
    mp.raw <- add.attr(c(mp.inp, mp.dat, mp.out), list(warn = s.warning))
    list(est = mp.results, models = l.mo, raw = mp.raw)
    }
    fit.mp <- function(l.mo,
                       m.b = "no_confound",
                       v.nm.dv = v.nm.dv,
                       v.nm.iv = v.nm.iv,
                       int.cores = max.cores(),
                       s.iter = 1e4L) {
        # Process input arguments +--------------------------------+
        # v.nm.n: vector of names normalised for mplus
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
        # Fit mplus model +----------------------------------------+
        mp.o(l.mo, m.b, v.nm.n, v.nm.dv, int.cores, s.iter)
        # +--------------------------------------------------------+
    }
    ## fitting: glm +------------------------------------------+
    upd.mo.d <- function(mo, d) {
        update(mo, formula(mo), family = family(mo), data = d)
    }
    ## bootstrap +--------------------------------------------+
    upd.mo.boot1 <- function(l.mo) {
        d <- model.frame(l.mo[[1]])
        n <- nrow(d)
        d.new <- d[sample(1:n, n, replace = TRUE),]
        Map(function(mo) upd.mo.d(mo, d.new), l.mo)
    }

    # causal effects: definition +--------------------------------------+
    ## definitions +------------------------------------------+
    # Generate causal effect definitions for a given number of mediators
    po.def <- function(n.med) {
        s.len <- sum(2^(0:(n.med - 1))) + 1
        v0 <- rep(0L, s.len)
        Map(function(s) `[<-`(v0, 0:s, 1),
            0:s.len)
    }
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
    ce.cft <- function(l.eff, v.nm) {
        n.med <- attr(l.eff, "n.med")
        m.eff <- do.call(rbind, l.eff)

        nc.m.eff <- ncol(m.eff)
        nc.m.eff.half <- nc.m.eff/2
        m.unq <- unname(unique(rbind(m.eff[,1:nc.m.eff.half],
                                     m.eff[,(nc.m.eff.half + 1):nc.m.eff])))
        v.pos <- 2^(n.med:0)

        # Sanity check to make sure that there are no errors
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
    ce.pos <- function(n.med, v.nm) {
        v.pos <- 2^(n.med:0)
        c(v.nm[1],
          rev(unlist(Map(function(s, s.nm)
                         rep(s.nm, s),
                         v.pos[-1], v.nm[-1]))))
    }
    ce.cfg <- function(n.med, v.nm) {
        s.len.nm <- length(v.nm)
        if((n.med + 1) != s.len.nm)
            stop("Error: Length of vector of v.nm is incorrect.")

        list(len = structure(2^(n.med:0), .Names = v.nm),
             cfg = Map(function(s.nm, s.med)
                       {
                           if.t(s.med > 0,
                                ce.pos(s.med, v.nm[which(s.nm == v.nm):s.len.nm]),
                                v.nm[s.len.nm])

                       }, v.nm, n.med:0))
    }
    ## do and set "x" +---------------------------------------+
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
    set.x.par.1 <- function(l.par, l.eff, s.nm.r) {
        Map(function(s.nm, l, s.nm.set, s.eff)
            within(l, data <- set.x(data, s.nm.set, head(s.eff, 1))),
            names(l.eff), list(l.par), s.nm.r, l.eff)
    }
    set.x.par <- function(l.sim.par, l.caus.eff, s.nm.r) {
        # Set "x" and return object with simulation parameters
        Map(function(l.par, l.eff, s.nm.r) set.x.par.1(l.par, l.eff, s.nm.r),
            l.sim.par, l.caus.eff, s.nm.r)
    }
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
    ### difference, odds ratio and odds +-----------+
    odr <- function(p1, p2) {
        exp(log(p1) - log(1 - p1) -
            log(p2) + log(1 - p2))
    }
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
        apply(m, 1, dif)
    }
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
        odr(m[,1], m[,2])
    }
    odds.f <- function(p) {
        b.NA <- is.na(p)
        p.proc <- p[!b.NA]
        if(any(p.proc < 0 | p.proc > 1)) stop("Error")
        p[!b.NA] <- exp(log(p.proc) - log(1 - p.proc))
        p
    }
    odds.i <- function(v.odds) {
        exp(log(v.odds) - log(1 + v.odds))
    }
    odds <- function(..., inv = FALSE) {
        s.arg.len <- ...length()
        if(s.arg.len == 1) {
            obj.arg <- (...)
            cl.arg <- class(obj.arg)
            if(!any(cl.arg %in% c("integer", "numeric", "matrix"))) stop("Error")
            if(cl.arg %in% c("matrix")) {
                if(!any(dim(obj.arg) == 1)) stop("Error")
            }
        }
        if(s.arg.len > 1) {
            obj.arg <- c(...)
        }
        v <- as.vector(obj.arg)
        if(inv) {
            v.ret <- odds.i(v)
        }
        if(!inv) {
            v.ret <- odds.f(v)
        }
        v.ret
    }
    ### compute causal effects from counterfactuals +---------+
    cf.t.ce <- function(m.cft) {
        l.def <- attr(m.cft, "def")
        v.fam <- attr(m.cft, "fam")
        l.cft.m <- Map(function(v.nm) m.cft[,v.nm], l.def)
        l.cft.m.raw <- Map(function(v.nm) attributes(m.cft)[["raw"]][,v.nm], l.def)
        f <- switch(sw.b(v.fam), ce.dif, ce.odr)
        add.attr(do.call(cbind, Map(f, l.cft.m)),
                 list(raw = l.cft.m.raw))
    }

    # causal effects: estimate +----------------------------------------+
    ## model based causal effects +---------------------------+
    mo.upd.te <- function(mo, v.nm.m) {
        mo.upd(mo, paste0(c(". ~ .", paste0("-", v.nm.m)),
                          collapse = " "))
    }
    mo.ci <- function(mo, ...) {
        b.pkg <- suppressWarnings(require(MASS))
        if(!b.pkg) stop("Error: MASS package is not installed.")
        m <- confint(mo, ...)
        switch(family(mo)[["link"]],
               logit = exp(m), m)
    }
    mo.ci.l <- function(l.mo, ...) {
        Map(function(s.nm, mo) mo.ci(mo, ...),
            mapply(function(mo) as.character(formula(mo)[[2]]), l.mo),
            l.mo)
    }
    combi.mo <- function(v.nm, s.ret) {
        l.bin.resp <- rep(list(c("g", "b")), length(v.nm))
        m.combi <- add.cnm(cbind(Reduce(combi, l.bin.resp)), v.nm)
        mat.t.list(m.combi, 1, s.ret)
    }
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
    gen.mo.soln <- function(v.nm.dv, flatten = TRUE) {
        # Identify which model have a model based solution.
        # Note:
        # 1. All models have a model based total effect.
        # 2. All models have a model based direct effect.
        # The general idea behind this is that any binomial model that has a
        # gaussian dependency does not have a model based solution. This is due
        # primarily to the inability to have a closed form solution for the
        # integration.
        l.combi <- combi.mo(v.nm.dv, "v")
        v.soln <- mapply(id.soln, l.combi)
        if(flatten) {
            add.attr(structure(v.soln, .Names = mapply(paste0, l.combi, collapse = "")),
                     list(order = v.nm.dv))
        } else {
            data.frame(do.call(rbind, l.combi), soln = v.soln)
        }
    }
    boot.gen.d <- function(d, s.seed) {
        set.seed(s.seed)
        n <- nrow(d)
        d[sample(1:n, n, replace = TRUE),]
    }
    boot.upd.mo <- function(d, l.mo, s.seed) {
        d.sample <- boot.gen.d(d, s.seed)
        Map(function(mo) update(mo, formula(mo),
                                family = family(mo),
                                data = d.sample), l.mo)
    }
    mo.0dep <- function(l.set.x, l.mo.cf, s.nm.mo) {
        l.mo.cf[[s.nm.mo]] <- Map(do.x, l.set.x[[s.nm.mo]])
        l.mo.cf
    }
    mo.1dep <- function(l.set.x, l.mo.cf, s.nm.mo,
                        l.eff.cft, l.eff.cfg,
                        l.dep, l.fam, b.exv) {
        # Extract counterfactuals needed for current dependent variable.
        l.cf.def <- l.eff.cft[[s.nm.mo]]
        # Identify the dependencies and dependencies properties
        v.nm.dep <- l.dep[["dep"]][[s.nm.mo]]
        l.dep.len <- list(l.eff.cfg[["len"]][v.nm.dep])
        # Extract satisfied dependencies
        l.dep.cf <- Map(function(v.eff, v.len)
                        {
                            l.dep.cf.id <- vec.t.list(v.eff[-1], v.len)
                            l.dep.cf <- Map(function(s.nm, v) l.mo.cf[[s.nm]][[paste0(v, collapse = "")]],
                                            names(l.dep.cf.id), l.dep.cf.id)
                            m.dep.cf <- add.cnm(unname(do.call(cbind, l.dep.cf)), names(l.dep.cf))
                            m.dep.p <- m.dep.cf
                            m.dep.p[] <- 1
                            list(add.attr(m.dep.cf, list(prob = m.dep.p)))
                        }, l.cf.def, l.dep.len)
        # Attach the data to the do(x) parametrs
        l.dox.mo <- Map(function(l.d, l.x) within(l.x, dep <- l.d),
                        l.dep.cf, l.set.x[[s.nm.mo]])
        rm(l.cf.def, l.dep.len, l.dep.cf)

        # Test for the need to marginalise over binary variable.
        # Only apply the marginalise procedure when:
        # 1. The IV in the current model is a DV is another model.
        # 2. This IV is binary.
        # 3. The expected value is requested in the simulation for this IV.
        # {
        b.dep.fam <- add.nm(do.call(rbind, l.fam)[v.nm.dep,"b"], v.nm.dep)
        b.dep.exv <- b.exv[v.nm.dep]
        b.test <- mapply(all, b.dep.fam, b.dep.exv)
        v.mar <- v.nm.dep[b.test]
        rm(b.dep.fam, b.dep.exv, b.test)
        # }

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
    mo.cf.est <- function(l.mo, l.fam, v.nm.dv, v.nm.iv, b.exv,
                          l.dep, l.eff.cfg, l.eff.cft, b.mn = TRUE) {
        # Generate model parameters +------------------------------+
        l.mo.par <- gen.mo.par(l.mo, v.nm.dv, v.nm.iv, b.exv)
        # Set "x" for all counterfactuals +------------------------+
        l.set.x <- set.x.par(l.mo.par, l.eff.cft, v.nm.iv[1])
        # Empty list to hold counterfactuals +---------------------+
        l.mo.cf <- Map(function(v) list(), v.nm.dv)
        # Get current environment +--------------------------------+
        env <- environment()
        # START computing model based causal effects +-------------+
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

        # END computing model based causal effects +---------------+
        m.mn <- t(as.matrix(apply(m, 2, mean)))
        if.t(b.mn,
            add.attr(m.mn, list(raw = m)),
            add.attr(m, list(raw = m.mn)))
    }
    mo.ce.est <- function(l.mo, l.fam, v.nm.dv, v.nm.iv, b.exv,
                          l.dep, l.eff.cfg, l.eff.cft,l.eff.def.split) {
        m.cf <- mo.cf.est(l.mo, l.fam, v.nm.dv, v.nm.iv, b.exv,
                          l.dep, l.eff.cfg, l.eff.cft, b.mn = TRUE)
        cf.t.ce(add.attr(m.cf, list(def = l.eff.def.split,
                                    fam = l.fam[[1]])))
    }
    mo.te.upd <- function(mo, v.nm.m) {
        mo.upd(mo, paste0(c(". ~ .", paste0("-", v.nm.m)),
                          collapse = " "))
    }
    mo.te.est <- function(mo, d, s.nm.r, v.r, b.fam, b.raw = FALSE) {
        m.raw <- mapply(function(s) predict(mo, newdata = set.x(d, s.nm.r, s),
                                            type = "response"), v.r)
        m.mn <- apply(m.raw, 2, mean)
        s.eff <- switch(sw.b(b.fam), ce.dif, ce.odr)(m.mn)
        m.eff <- matrix(s.eff, dimnames = list("est", "te.y"))
        if(b.raw) {
            add.attr(m.eff, list(raw = list(te.y = add.cnm(m.raw, as.character(v.r))),
                                 fam = b.fam))
        } else {
            m.eff
        }
    }
    ## sensitivity analysis +---------------------------------+
    gen.u.args <- function(l.mo) {
        attr2env(l.mo)
        m.u.cmbn <- gen.u.cmbn(v.nm.dv)
        v.u.dir <- get.u.dir(l.mo, m.u.cmbn, v.nm.dv)
        m.u.b <- mat.dupe(m.u.cmbn, 0)
        v.res.var <- get.var.res(l.mo)
        m.res.var <- apply(m.u.cmbn, 2, function(x) v.res.var[x])
        v.res.sdp <- apply(m.res.var, 2, function(x) prod(sqrt(x)))
        as.list(environment())
    }
    gen.u.beta <- function(v.var, s.cov) {
        s.ratio <- v.var[1]/v.var[2]
        s.var <- sqrt(s.cov/s.ratio)
        structure(c(s.var * s.ratio, s.var), .Names = names(v.var))
    }
    gen.u.beta.m <- function(s.cor, m.u.b,
                             m.res.var, v.res.sdp, v.u.dir) {
        n.u <- ncol(m.u.b)
        m.u.b[] <- mapply(function(n) gen.u.beta(m.res.var[,n], s.cor * v.res.sdp[n]), 1:n.u)
        m.u.b[1,] <- m.u.b[1,] * v.u.dir
        m.u.b
    }
    gen.u.cmbn <- function(v.nm, type = "m", lat.u.prefix = "u") {
        # Generate combinations of u.
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
    get.u.dir <- function(l.mo, m.cmbn, v.nm.dv) {
        v <- apply(m.cmbn, 2, function(x)
                   {
                       v.coef <- coef(l.mo[[min(which(v.nm.dv %in% x))]])
                       na.omit(v.coef[x]) > 0
                   })
        (v - 0.5)/0.5
    }
    get.var.res <- function(l.mo) {
        attr2env(l.mo)
        s.var.lgd <- (pi^2)/3
        mapply(function(s.nm, mo, v)
            switch(sw.b(v), var(residuals(mo)), s.var.lgd),
            v.nm.dv, l.mo, get.fam.l(l.mo))
    }
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
                             int.cores = max.cores(), s.iter = 1e4L)
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
    get.sa.mcor <- function(s.dec, l.mo, v.nm.dv, v.nm.iv) {
        list2env(gen.u.args(l.mo), environment())
        get.sa.maxcor.allsteps(s.dec, m.u.b,
                               m.res.var, v.res.sdp, v.u.dir,
                               l.mo, v.nm.dv, v.nm.iv)
    }
    gen.sa.testseq <- function(s.mcor, s.dec) {
        v.test <- c(5, 10)
        s.fac <- 10^s.dec
        v.div <- (abs(s.mcor) * s.fac) %/% v.test
        v.div.use <- switch(sw.b(v.div[1] < 10), c(5, v.div[1]), c(10, v.div[2]))
        v.seq <- seq(from = 0, by = v.div.use[1], length.out = v.div.use[2] + 1) / s.fac
        if(s.mcor < 0) v.seq <- -v.seq
        v.seq
    }
    ## simulation +-------------------------------------------+
    b.class.v <- function(obj, class, func = all) {
        class.obj <- sort(class(obj))
        class.tgt <- sort(class)
        func(class.obj == class.tgt)
    }
    b.class.l <- function(obj, class, func = all, listfunc = all) {
        listfunc(mapply(function(obj.1) b.class.v(obj.1, class, func), obj))
    }
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
    chk.len <- function(l) {
        list2env(l, environment())
        if(!identical.v(mapply(length, list(l.mo, v.nm.dv))))
            stop("Error: Lengths of input arguments incorrect.")
    }
    chk <- function(obj, env, v.match) {
        v <- get0(obj, envir = env)
        !if.t(length(v) == 0,
             FALSE,
             (match(v, v.match, nomatch = 0) > 0) |> any())
    }
    chk.val <- function(l) {
        list2env(l, environment())
        # Check for valid coefficient method
        if(!any(match(s.cof.mth, c("mvn", "boot"), nomatch = 0) > 0)) {
            warning("Invalid path coefficient simulation method specified. Replacing with defaults.")
            assign("s.cof.mth", "mvn", envir = parent.frame())
        }
        # Check for valid mode
        if(!any(match(s.mode, c("ce", "sa"), nomatch = 0) > 0)) {
            warning("Invalid path coefficient simulation method specified. Replacing with defaults.")
            assign("s.mode", "ce", envir = parent.frame())
        }
        # Check arguments that are suppose to be integers
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
    sim.0dep <- function(l.set.x, l.sim.cf, s.nm.mo) {
        l.sim.cf[[s.nm.mo]] <- Map(do.x, l.set.x[[s.nm.mo]])
        l.sim.cf
    }
    sim.1dep <- function(l.set.x, l.sim.cf, s.nm.mo,
                         l.eff.cf, l.eff.cfg,
                         l.dep, l.fam, b.exv) {
        # Extract counterfactuals needed for current dependent variable.
        l.cf.def <-  l.eff.cf[[s.nm.mo]]
        # Identify the dependencies and dependencies properties
        v.nm.dep <- l.dep[["dep"]][[s.nm.mo]]
        l.dep.len <- list(l.eff.cfg[["len"]][v.nm.dep])
        # Extract simulated dependencies
        l.dep.cf <- Map(function(v.eff, v.len)
                        {
                            l.dep.cf.id <- vec.t.list(v.eff[-1], v.len)
                            l.dep.cf <-
                                Map(function(s.nm, v)
                                    l.sim.cf[[s.nm]][[paste0(v, collapse = "")]],
                                    names(l.dep.cf.id), l.dep.cf.id)
                            # ERROR LOCATION
                            m.dep.cf <- add.cnm(unname(do.call(cbind,
                                                               l.dep.cf)),
                                                names(l.dep.cf))
                            m.dep.p <- m.dep.cf
                            m.dep.p[] <- 1
                            list(add.attr(m.dep.cf, list(prob = m.dep.p)))
                        }, l.cf.def, l.dep.len)
        # Attach the data to the simulation parametrs
        l.sim.mo <- Map(function(l.d, l.x) within(l.x, dep <- l.d),
                        l.dep.cf, l.set.x[[s.nm.mo]])
        rm(l.cf.def, l.dep.len, l.dep.cf)

        # Test for the need to marginalise over binary variable.
        # Only apply the marginalise procedure when:
        # 1. The IV in the current model is a DV is another model.
        # 2. This IV is binary.
        # 3. The expected value is requested in the simulation for this IV.
        # {
        b.dep.fam <- add.nm(do.call(rbind, l.fam)[v.nm.dep,"b"], v.nm.dep)
        b.dep.exv <- b.exv[v.nm.dep]
        b.test <- mapply(all, b.dep.fam, b.dep.exv)
        v.mar <- v.nm.dep[b.test]
        rm(b.dep.fam, b.dep.exv, b.test)
        # }

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
        # Generate simulation parameters +-------------------------+
        l.sim.par <- gen.sim.par(s.seed = s.seed,
                                 l.mo = l.mo,
                                 l.mo.par = l.mo.par,
                                 b.ev = b.exv,
                                 m.b = m.b,
                                 s.meth = s.cof.mth,
                                 s.mode = s.mode,
                                 int.cores = int.cores)
        # Set "x" for all counterfactuals +------------------------+
        l.set.x <- set.x.par(l.sim.par,
                             l.eff.cf,
                             v.nm.iv[1])
        # Empty list to hold counterfactuals +---------------------+
        l.sim.cf <- Map(function(v)
                        list(),
                        v.nm.dv)
        # Get current environment +--------------------------------+
        env <- environment()
        # START simulation: Single run +---------------------------+
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
        # END simulation: Single run +-----------------------------+
        apply(m, 2, mean)
    }
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
    sim.cf <- function(l.mo, s.mode, m.b, s.cof.mth,
                       int.sims, s.seed, b.parallel, int.cores)
    {
        # Setup +------------------------------------------------------------+
        attr2env(l.mo)
        ## Error checking +----------------------------------------+
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
        ## Environment +-------------------------------------------+
        list2env(gen.funcs.seqmed(), envir = environment())
        v.ev <- structure(rep("auto", length(l.mo)),
                          .Names = v.nm.dv)             # Not exposing the setting of expected/predicted values
        b.exv <- set.exv(v.nm.dv, v.ev, l.fam)          # Set expected values flag
        if(!isClass(class(m.b), "matrix")) {
            m.b <- switch(m.b,
                          no_confound = mat.dupe(combn(v.nm.dv, 2), 0),
                          stop("Error"))
        }
        ## Call +--------------------------------------------------+
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
        ## Set up counterfactuals +--------------------------------+
        l.eff.def <- ce.def(n.med)
        l.eff.def.split <- Map(str.half, l.eff.def)
        l.eff.cft <- ce.cft(l.eff.def, v.nm.dv)
        l.eff.cfg <- ce.cfg(n.med, v.nm.dv)
        # Identify dependencies +----------------------------------+
        l.dep <- get.mo.dep(l.mo)
        if(!any(l.dep[["n"]] == 0))
            stop("Error: No solution when all models have dependencies.")
        ## Set seeds +---------------------------------------------+
        v.seeds <- gen.seeds(int.sims, s.seed)
        # +------------------------------------------------------------------+
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

        # Simulation START +---------------------------------------+
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
    int.max <- .Machine[["integer.max"]]
    # user facing +-----------------------------------------------------+
    mo.med <- function(l.mo,
                       s.ci       = 0.95,
                       int.boot   = 1e4L,
                       s.seed     = sample(-int.max:int.max, 1),
                       b.parallel = "auto",
                       int.cores  = "max")
    {
        # Setup +------------------------------------------------------------+
        ## Environment +-------------------------------------------+
        # Names
        setup.env(list.models = l.mo,
                  int.iter = int.boot,
                  b.parallel = b.parallel,
                  int.cores = int.cores)
        # Set up when to use expected versus predicted values
        # - Currently not exposing this function to end users.
        # - Precise rules on when to use expected vs predicted values can be
        #   found in the function "set.exv".
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
        m.ci <- add.rnm(apply(do.call(rbind, l.ci),
                              2, ci.perc, s.ci),
                        paste0("ci.", gsub("\\.", "", gen.ci(s.ci))))
        if(ncol(m.ci) == 1) {
            # Check if there is only "te.y". If so, fill up the rest of the matrix with NA.
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
        # setup environment +------------------------------------------------+
        list2env(gen.funcs.seqmed(), environment())
        setup.env(l.mo,
                  int.sims,
                  b.parallel,
                  int.cores)
        # Setup names for confidence intervals +-----------------------------+
        s.nm.ci <- paste0("ci.", gsub("\\.", "", gen.ci(s.ci)))

        # Run simulations +--------------------------------------------------+
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
                                   est <- add.rnm(est, "est")
                                   ci <- add.rnm(apply(ci, 2, ci.perc, s.ci), s.nm.ci)
                               })) |> add.attr(list(efftype = v.fam))
        if.t(b.raw,
             add.attr(m.ce, list(raw = l.cf)),
             m.ce)
    }
    sim.med.sa <- function(l.mo,
                           s.cof.mth = "mvn",
                           s.ci = 0.95,
                           int.sims = 1e1L,
                           s.seed = gen.seeds(1),
                           b.parallel = "auto",
                           int.cores = "max")
    {
        # setup environment +------------------------------------------------+
        list2env(gen.funcs.seqmed(), environment())
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
    environment() |> as.list.environment()
}
l.funcs.dummydata <- gen.funcs.dummydata()
l.funcs.seqmed <- gen.funcs.seqmed()
list2env(c(l.funcs.dummydata, l.funcs.seqmed), environment()) |>
ut.silent()

# DUMMY DATA TO TEST ALGORITHM +-----------------------------------------------+
dummy.data <- function() {
    l.par <- list(n = 1e3L,
                  seed = 42L,
                  static = list(r    = c(p = 0.5),
                                cv1b = c(p = 0.2),
                                cv2c = c(mn = 2, sd = 4)),
                  coef = list(m1 = c(i = 0.2, r = 0.4, cv1b = 0.1, cv2c = 0.1),
                              m2 = c(i = 0.3, r = 0.6, cv1b = 0.1, cv2c = 0.1, m1 = 0.9),
                              y  = c(i = 0.4, r = 0.8, cv1b = 0.1, cv2c = 0.1, m1 = 0.12, m2 = 0.16)),
                  norm.e.sd = c(m1 = 0.6, m2 = 0.9, y = 1.2))
    l.mo <- local({
        l.mo <- gen.mo.all(1:12, l.par)
        Map(function(l.mo1, v.nm) nms(l.mo1, v.nm),
            l.mo, Map(function(n)
                      c("y", paste0("m", rev(seq(n -1)))),
                      Map(length, l.mo)))})
    v.mo <- c("ycm1c", "ycm1b", "ybm1c", "ybm1b",
              "ycm2cm1c", "ycm2bm1c", "ycm2cm1b", "ycm2bm1b",
              "ybm2cm1c", "ybm2bm1c", "ybm2cm1b", "ybm2bm1b")
}

# MODEL BASED ESTIMATES +------------------------------------------------------+
# model based answers for the combinations with answers
# simulation based answers for all combinations
# how to use each
f.model.based.est <- function(l.mo.est) {
    # setup +------------------------------------------------------------+
    ## user supplied input +-----------------------------------+
    s.ci <- 0.95                       # Confidence interval
    int.boot <- 1e1L                     # Number of simulations
    s.seed <- 8L                       # Starting seed
    b.parallel <- "auto"
    int.cores <- parallel::detectCores() # Number of cores to use for parallel computations.
    b.raw <- TRUE                      # Return raw results
    # +------------------------------------------------------------------+
    Map(function(l.mo1) mo.med(l.mo1, s.ci, int.boot, s.seed, b.parallel, int.cores), l.mo)
    ## save(l.est.model, file = paste0("l.est.model-", str.pad0(int.boot, 8), ".RData"))
    # +------------------------------------------------------------------+
}

# SIMULATION BASED ESTIMATES +-------------------------------------------------+
permutate.args <- function(...) {
    l.args <- list(...) |> rev()
    Map(function(l, v.ind) l[v.ind],
        l.args, mapply(function(v)
                       1:length(v),
                       l.args, SIMPLIFY = FALSE) |>
        expand.grid()) |>
        rev()
}
f.sim.based.est <- function(l.mo.est) {
    # setup +------------------------------------------------------------+
    s.ci <- 0.95                       # Confidence interval
    s.seed <- 8L                       # Starting seed
    b.parallel <- "auto"                   # Threshold that will trigger the use of multiple cores.
    int.cores <- parallel::detectCores() # Number of cores to use for parallel computations.
    b.raw <- TRUE                      # Return raw

    s.mode <- "ce"                     # Mode of operation: "ce", "sa"
    s.cof.mth <- "boot"                 # Method to draw coefficients: "mvn", "boot"
    int.sims <- 1e1L                     # Number of simulations
    s.fn.base <- c("l.est.sim.ce-", "-", ".RData")
    v.b <- "auto"
    # +------------------------------------------------------------------+
    Map(function(l.mo) sim.med(l.mo, s.mode, v.b, s.cof.mth, s.ci, int.sims, s.seed, b.parallel, int.cores, b.raw), l.mo.est)
#    save(l.est.sim, file = paste0(append(append(s.fn.base, str.pad0(int.sims, 8), 2), s.cof.mth, 1), collapse = ""))
}

# SENSITIVTY ANALYSIS +--------------------------------------------------------+
f.sim.med.sa <- function() {
    s.mode <- "sa"                     # Mode of operation: "ce", "sa"
    s.cof.mth <- "mvn"                 # Method to draw coefficients: "mvn", "boot"
    s.ci <- 0.95                       # Confidence interval
    int.sims <- 1e4L                     # Number of simulations
    s.seed <- 8L                       # Starting seed
    b.parallel <- "auto"
    int.cores <- parallel::detectCores() # Number of cores to use for parallel computations.
    b.raw <- TRUE                      # Return raw
    # +++--+ +++--+ +++--+ +++--+ +++--+ +++--+ +++--+ +++--+ +--------------------+
    iter <- 1e1L
    l.med.sa <- Map(function(l.mo1) sim.med.sa(l.mo1, int.sims = int.sims), l.mo)
}

# APPLICATION +----------------------------------------------------------------+
    application <- function() {
    ## Data +------------------------------------------------------------+
    process_d <- function() {
        Map(function(fn) load(fn, envir = .GlobalEnv),
            list.files(pattern = "\\.RData$")) |>
            ut.silent()
        d.original <- local({
            d <- merge(list.meta[["master"]][,c("subid", "group")], df.data.ext,
                       all.x = FALSE, all.y = TRUE, by = "subid") |>
                 subset(item == "total" &
                        (role == "cg1" | role == "pt") &
                        instrument != "bmi",
                    select = -c(subid, subscale, item))
            d.proc <-
                within(d,
                       {
                           instrument <- paste(time, role, instrument, sep = "-")
                           rm(time, role)
                       })
            l.proc <- split(d.proc, d.proc[,"instrument"])
            l.proc <- Map(function(d)
                          within(d,
                                 {
                                     assign(instrument[1], score)
                                     rm(score, instrument)
                                 }), l.proc)
            d.meta <-
                subset(list.meta[["master"]],
                       role == "cg1" | role == "pt",
                       select = -c(subid, treatment, cg.count))
            l.rx <-
                subset(list.meta[["master"]],
                       role == "cg1" | role == "pt",
                       select = c(group, treatment)) |>
                unique() |> list()
            l.meta <-
                Map(function(d)
                    {
                        d.cnm <- colnames(d)
                        d.cnm.ext <- d.cnm[match(d.cnm, c("role", "group"), nomatch = 0) == 0]
                        d.cnm.new <- paste("demo", d[1, "role"], d.cnm.ext, sep = "-")
                        d[,d.cnm.ext] |>
                        cnms(d.cnm.new) |>
                        cbind(d["group"])
                    }, with(d.meta, split(d.meta, role)))
            l.meta <-
                Reduce(function(x,y) merge(x, y, by = "group", all = TRUE), l.meta) |>
                list()
            d.merged <-
                Reduce(function(x, y)
                   {
                       merge(x, y, by = "group", all = TRUE)
                   }, c(l.rx, l.meta, l.proc)) |>
                within(treatment <- treatment + 0)
            cnms(d.merged, gsub("-", ".", colnames(d.merged)))
        })
        d.replaced <- local({
            for(s.nm in (colnames(d.original) |> Swap(grep)("^t[1-9]", value = TRUE))) {
                v <- d.original[, s.nm]
                v.split <- strsplit(s.nm, "")[[1]]
                v.split[2] <- as.numeric(v.split[2]) - 1
                v.replace.nm <- paste0(v.split, collapse = "")
                if(any(grepl(v.replace.nm, colnames(d.original)))) {
                    v.missing <- is.na(v)
                    v[v.missing] <- d.original[v.missing, v.replace.nm]
                }
                d.original[, s.nm] <- v
            }
            d.original
        })
        d <- local({
            d.temp <- d.replaced[,c("group",
                 "treatment",
                 "t0.cg1.aesed",
                 "t0.cg1.fq",
                 "t0.pt.edeq",
                 "t1.cg1.fq",
                 "t2.cg1.aesed",
                 "t2.pt.edeq",
                 "t4.pt.edeq",
                 "t4.pt.relapse")] |>
            na.omit()
            d.replaced[match(d.replaced[,"group"],
                             d.temp[,"group"],
                             nomatch = 0) > 0,]
        })
        d
    }
    d <- process_d()
    ## Descriptive +-----------------------------------------------------+
    gen_summary <- function() {
        summ <- function(v, s.round) {
            summ.cat <- function(v, s.round) {
                tab.v <- table(v)
                d.vc <-  as.data.frame(tab.v, stringsAsFactors = FALSE)
                d.vp <- (as.data.frame(prop.table(tab.v), stringsAsFactors = FALSE)[,2] * 100) |>
                        round(s.round)
                if(is(v, "factor"))  d.vc[,1] <- factor(d.vc[,1], levels = levels(v))
                if(is(v, "logical")) d.vc[,1] <- as.logical(d.vc[,1])
                d <- cbind(d.vc, d.vp)
                colnames(d)[2:3] <- c("counts", "perc")
                d
            }
            summ.con <- function(v, s.round) {
                v.summ <- c(mean = mean(v, na.rm = TRUE),
                            sd   =   sd(v, na.rm = TRUE))
                data.frame(t(round(v.summ, s.round)))
            }
            if(sum(is.na(v)) != length(v)) {
                if(is.numeric(v)) {
                    f <- summ.con
                } else {
                    f <- summ.cat
                }
                f(v, s.round)
            }
        }
        fac.to.char <- function(v) {
            if(is.factor(v)) {
               v <-  levels(v)[v]
            }
            v
        }
        replaceNA.mean <- function(v) {
            v[is.na(v)] <- mean(v, na.rm = TRUE)
            v
        }
        replaceNA.levels <- function(v) {
            v.na <- is.na(v)
            v.na.sum <- sum(v.na)
            if(is.factor(v))
                v.uniq <- levels(v)
            if(is.character(v))
                v.uniq <- unique(v)
            if(is.logical(v))
                v.uniq <- c(TRUE, FALSE)
            suppressWarnings(v[v.na] <- v.uniq)
            v
        }
        summarise_d <- function(d) {
            v.nm <- grep("^(demo|t\\d|treatment)", colnames(d), value = TRUE)
            v.rx <- d[,"treatment"]
            s.round <- 2
            l <- Map(function(v.nm1, v)
                     {
                         d.total <- summ(v, s.round)
                         l.split <- split(v, v.rx)
                         l.subtt <- Map(summ, l.split, s.round)
                         v.nm.rx <- names(l.subtt)
                         if(!is.null(d.total)) {
                             if(ncol(d.total) == 3) {
                                 colnames(d.total)[1] <- v.nm1
                                 l.subtt <- Map(ss.b, l.subtt, -1)
                             }
                             do.call(cbind, c(d.total, l.subtt))
                         }
                     }, v.nm, d[,v.nm])
            l[!unlist(Map(is.null, l))]
        }

        d <- d[, -which(colnames(d ) == "demo.cg1.medication")]
        v.missing <- apply(is.na(d[,grep("^(demo|t\\d|treatment)", colnames(d), value = TRUE)]), 2, sum)
        v.missing <- v.missing[v.missing > 0]
        d <- within(d,
               {
                   demo.cg1.age <- replaceNA.mean(demo.cg1.age)
                   demo.cg1.ethnicity <- replaceNA.levels(demo.cg1.ethnicity)
                   demo.cg1.education <- replaceNA.levels(demo.cg1.education)
                   demo.pt.ethnicity <- replaceNA.levels(demo.pt.ethnicity)
                   demo.pt.marital <- replaceNA.levels(demo.pt.marital)
                   demo.pt.medication <- replaceNA.levels(demo.pt.medication)
                   t0.cg1.lee <- replaceNA.mean(t0.cg1.lee)
                   t0.cg1.pcsc <- replaceNA.mean(t0.cg1.pcsc)
                   t1.cg1.lee <- replaceNA.mean(t1.cg1.lee)
                   t1.cg1.pcsc <- replaceNA.mean(t1.cg1.pcsc)
                   t2.cg1.lee <- replaceNA.mean(t2.cg1.lee)
                   t2.cg1.pcsc <- replaceNA.mean(t2.cg1.pcsc)
                   t3.cg1.lee <- replaceNA.mean(t3.cg1.lee)
                   t3.cg1.pcsc <- replaceNA.mean(t3.cg1.pcsc)
                   t4.cg1.lee <- replaceNA.mean(t4.cg1.lee)
                   t4.cg1.pcsc <- replaceNA.mean(t4.cg1.pcsc)
               })

        l.demo.summary <- summarise_d(d)
        l.demo.summary <- Map(function(s.nm, d.data)
                              {
                                  v.nrow <- nrow(d.data)
                                  m.demo <- matrix(s.nm,
                                                   nrow = v.nrow,
                                                   ncol = 1,
                                                   dimnames = list(NULL, "demo"))
                                  d.summary <- cbind(m.demo, d.data)
                                  if(v.nrow != 1)
                                      colnames(d.summary)[2] <- "category"
                                  d.summary
                              }, names(l.demo.summary), l.demo.summary)

        b.contbin <- unlist(Map(function(d.summary) nrow(d.summary) == 1, l.demo.summary) )
        l.demo.summary <- Map(function(l) do.call(rbind, l), split(l.demo.summary, b.contbin))
        Map(function(d.summary, v.filename)
            {
                v.bracket <- grep('perc$', colnames(d.summary), value = TRUE)
                if(length(v.bracket) != 0) {
                    v.rearrange <- c(1,2,7,8,5,6,3,4)
                }
                if(length(v.bracket) == 0) {
                    v.bracket <- grep('sd$', colnames(d.summary), value = TRUE)
                    v.rearrange <- c(1,6,7,4,5,2,3)
                }
                d.summary <- d.summary[,v.rearrange]

                d.summary[,v.bracket] <- Map(function(v) sprintf("(%0.2f)", v), d.summary[,v.bracket])
                write.csv(d.summary, file = v.filename, row.names = FALSE)
            }, l.demo.summary, list("summ-cat.csv", "summ-con.csv"))
    }
    gen_summary_old <- function() {
        d.demo.formatted <- local({
            l <- d.demo.summary
            l <- l[!unlist(Map(is.null, l))]
            l.summary <- Map(function(s.nm, d)
                     {
                         v.title <-
                             s.nm.fmt <- gsub("^demo", "", s.nm)        |>
                             Swap(gsub, 3)("\\.cg1\\.", "(caregiver)#") |>
                             Swap(gsub, 3)("\\.pt\\.", "(patient)#")    |>
                             Swap(gsub, 3)("(.*?)#(.)(.*)", "\\U\\2\\L\\3 \\1",
                                           perl = TRUE)                 |>
                             Swap(gsub, 3)("(Marital)", "\\1 status")   |>
                             Swap(gsub, 3)("(Education)", "\\1 level")  |>
                             Swap(gsub, 3)("Medication", "On medication")
                         d <-
                             gsub("counts", "Frequency", colnames(d))  |>
                             Swap(gsub, 3)("perc", "%")                |>
                             Swap(gsub, 3)("mean", "Mean")             |>
                             Swap(gsub, 3)("sd", "SD")                 |>
                             Swap(cnms, 2)(d)

                         if(ncol(d) == 3)     colnames(d)[1] <- v.title
                         if(grepl("gender", s.nm))     d[,1] <- ifelse(d[,1], "male", "female")
                         if(grepl("medication", s.nm)) d[,1] <- ifelse(d[,1], "yes", "no")
                         if(grepl("ethnic", s.nm))     d[,1] <- as.character(d[,1])
                         if(grepl("education", s.nm))  d[,1] <- as.character(d[,1])
                         if(grepl("marital", s.nm))    d[,1] <- as.character(d[,1])

                         list(title = v.title, summary = d)
                     }, names(l), l)
            l.summary <-
                Map(function(v, l) l[["summary"]],
                    unname(unlist(Map(function(l)
                                      l[["title"]],
                                      l.summary))),
                    l.summary)

            v <- Map(function(v.title, d)
                     {
                         m <- rbind(colnames(d),
                                    Map(function(v)
                                        {
                                            v.char <- as.character(v)
                                            v.nchar <- nchar(v.char)
                                            v.pad <- max(v.nchar) - v.nchar
                                            Map(function(s.pad, s)
                                                paste0(c(rep(" ", s.pad), s), collapse = ""),
                                                v.pad, v.char) |> unlist()
                                        }, d) |>
                                    Swap(do.call, 2)(cbind))
                         m.cnm <- colnames(m)
                         if(any(grepl("Frequency", m.cnm))) {
                             v <- paste0(m[,1], " & ", paste0(m[,"Frequency"], " (", m[,"%"], ")")) |>
                                  Swap(gsub, 3)("%", "\\\\%")
                         }
                         if(any(grepl("SD", m.cnm))) {
                             v <- paste0(m[,"Mean"], " ", "(", m[,"SD"], ")")

                         }
                         v.tabalign <- paste0(c("l", rep("r", length(m.cnm) - 2)), collapse = " ")

                         s.label <-
                             gsub("(.*)", "\\L\\1", v.title, perl = TRUE) |>
                             Swap(gsub, 3)("\\(|\\)", "")                 |>
                             Swap(gsub, 3)(" ", "-")
                        c("\\begin{table}[!htbp]",
                                 "    \\centering",
                          paste0("    \\caption{"       , v.title   , "}"),
                          paste0("    \\label{tab-5-"   , s.label   , "}"),
                          paste0("    \\begin{tabular}{", v.tabalign, "}"),
                                 "        \\toprule",
                          paste0("        ", v[1]),
                                 "        \\\\ \\midrule",
                          paste0("           ", v[2]),
                          paste0("        \\\\ ", v[-c(1:2)]),
                                 "        \\\\ \\bottomrule",
                          paste0("    \\end{tabular}"),
                          "\\end{table}"
                        )
                     }, names(l.summary), l.summary)
            v |> unlist()
        })
        with(d, data.frame(treatment, t4.pt.edeq, t4.pt.relapse, t1.cg1.fq, t2.pt.edeq))
        Map(function(v) c(mean(v), sd(v)), with(d, data.frame(t0.cg1.fq, t0.cg1.aesed, t0.pt.edeq)))
        cat(d.demo.summary, file = "../../04-thesis/parts/casis-descriptives-autogen.tex", sep = "\n")
    }
    ## Models +----------------------------------------------------------+
    estimate_models <- function() {
        mo <- local({
            b <- binomial(link = "logit")
            c <- gaussian(link = "identity")
            d <-
                with(d, data.frame(t4.pt.relapse, treatment, t0.cg1.fq,  t0.pt.edeq,
                                   t1.cg1.fq, t2.pt.edeq)) |>
                na.omit()
            mo.b <- Map(function(fml, fam)
                        glm(fml, family = fam, data = d),
                        list(formula(t4.pt.relapse ~
                                     treatment + t0.cg1.fq + t0.pt.edeq + t1.cg1.fq + t2.pt.edeq),
                             formula(t2.pt.edeq ~
                                     treatment + t0.cg1.fq + t0.pt.edeq + t1.cg1.fq),
                             formula(t1.cg1.fq ~
                                     treatment + t0.cg1.fq + t0.pt.edeq)),
                        list(b, c, c))
            rm(d)
            d <-
                with(d, data.frame(t4.pt.edeq, treatment, t0.cg1.fq, t0.cg1.aesed,
                                    t1.cg1.fq, t2.cg1.aesed)) |>
                na.omit()
            mo.c <- Map(function(fml, fam)
                        glm(fml, family = fam, data = d),
                        list(formula(t4.pt.edeq ~
                                     treatment + t0.cg1.fq + t0.cg1.aesed + t1.cg1.fq + t2.cg1.aesed),
                             formula(t2.cg1.aesed ~
                                     treatment + t0.cg1.fq + t0.cg1.aesed + t1.cg1.fq),
                             formula(t1.cg1.fq ~
                                     treatment + t0.cg1.fq + t0.cg1.aesed)),
                        list(c, c, c))
            list(b = mo.b, c = mo.c)
        })
        library("ggplot2")
        reshape.results <- function(results) {
            reshape.results1 <- function(m) {
                b.ie <- grepl("^ie", colnames(m))
                b.te <- grepl("^te$", colnames(m))
                b.de <- grepl("^de$", colnames(m))
                v.nm <- character()
                v.nm[b.te] <- "Total"
                v.nm[b.de] <- "Direct"
                v.nm[b.ie] <- switch(sum(b.ie) |> as.character(),
                                     "2" = c("Indirect (M1)",
                                             "Indirect (M2)"),
                                     "3" = c("Indirect (M1)",
                                             "Indirect (M2)",
                                             "Indirect (M1, M2)"))
                v.nm <- factor(v.nm,
                               levels = v.nm,
                               labels = c(v.nm[b.te],
                                          v.nm[b.de],
                                          v.nm[b.ie]))
                data.frame(effect = v.nm, t(m))
            }
            switch(is.list(results) + 1,
                   "1" = {
                       reshape.results1(results)
                   },
                   "2" = {
                       do.call(rbind, Map(function(x, x.nm)
                                          data.frame(confound = x.nm,
                                                     reshape.results1(x)),
                                          results,
                                          sprintf("%.2f",
                                                  attributes(results)[["cor"]])))
                   })
        }

        # Model 1 (Odds ratio) +-------------------------------------------------------+
        ## Model +-----------------------------------------------------------+
        # intervention -> reduce cg stress -> improve initial response -> improve outcome
        # treatment -> t1.cg1.fq -> t2.pt.edeq -> t4.pt.relapse
        # treatment -> t1.cg1.fq -> t2.pt.edeq
        # treatment -> t1.cg1.fq
        model1 <- mo[[1]]
        ## Estimates +-------------------------------------------------------+
        model1.med <- mo.med(model1)
        model1.sim <- sim.med(model1)
        ## Sensitivity analysis +--------------------------------------------+
        model1.sim.sa <- sim.med.sa(model1)
        ## Graphs +----------------------------------------------------------+
        d1 <- do.call(rbind,
                      list(data.frame(confound = "0.00 (est)", reshape.results(model1.sim)),
                           reshape.results(model1.sim.sa)))
        d1 <- do.call(rbind, split(d1, d1[,"effect"]))
        d1 <- within(d1, confound <- factor(confound, levels = confound |> unique()))

        model1.graph.data <- d1

        # Model 2 (Difference) +-------------------------------------------------------+
        ## Model +-----------------------------------------------------------+
        # treatment -> t1.cg1.fq + t2.cg1.aesed + t4.pt.edeq
        # treatment -> t1.cg1.fq + t2.cg1.aesed
        # treatment -> t1.cg1.fq
        model2 <- mo[[2]]
        ## Estimates +-------------------------------------------------------+
        model2.med <- mo.med(model2)  # 18s
        model2.sim <- sim.med(model2) # 10s
        ## Sensitivity analysis +--------------------------------------------+
        model2.sim.sa <- sim.med.sa(model2)
        ## Graphs +----------------------------------------------------------+
        d2 <- do.call(rbind,
                      list(data.frame(confound = "0.00 (est)", reshape.results(model2.sim)),
                           reshape.results(model2.sim.sa)))
        d2 <- do.call(rbind, split(d2, d2[,"effect"]))
        d2 <- within(d2, confound <- factor(confound, levels = confound |> unique()))

        model2.graph.data <- d2

        # Save +-----------------------------------------------------------------------+
        save(list = c("d", "mo",
                      "model1", "model1.med", "model1.sim", "model1.sim.sa", "model1.graph.data",
                      "model2", "model2.med", "model2.sim", "model2.sim.sa", "model2.graph.data",),
             file = "casis.RData")
    }
    draw_graphs_sa <- function() {
        require(ggplot2)
        model1.graph <-
            ggplot(model1.graph.data, aes(x = confound , y= est, group = confound)) +
            xlab("level of confounding") +
            ylab(if.t(attributes(model1.sim.sa[[1]])[["efftype"]] == "difference",
                      "estimate (difference)",
                      "estimate (odds ratio)")) +
            geom_point(shape = 4) +
            geom_line(aes(group = effect)) +
            geom_hline(yintercept = 1, linetype = "dashed") +
            geom_errorbar(aes(ymin = ci.0025, ymax = ci.0975, width = 0.1)) +
            facet_grid(rows = vars(effect))

        model2.graph <-
            ggplot(model2.graph.data, aes(x = confound , y= est, group = confound)) +
            xlab("Level of confounding") +
            ylab("Estimate (Difference)") +
            geom_point(shape = 4) +
            geom_line(aes(group = effect)) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_errorbar(aes(ymin = ci.0025, ymax = ci.0975, width = 0.1)) +
            facet_grid(rows = vars(effect))
        list(model1.graph, model2.graph)
    }
    draw_graphs <- function() {
        require(ggplot2)
        model1.graph <-
            ggplot(model1.graph.data, aes(x = effect , y= est)) +
            xlab("Effect") +
            ylab("Estimate (Odds Ratio)") +
            geom_point(shape = 4) +
            geom_hline(yintercept = 1, linetype = "dashed") +
            geom_errorbar(aes(ymin = ci.0025, ymax = ci.0975, width = 0.1))

        model2.graph <-
            ggplot(model2.graph.data, aes(x = effect , y= est)) +
            xlab("Effect") +
            ylab("Estimate (Difference)") +
            geom_point(shape = 4) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_errorbar(aes(ymin = ci.0025, ymax = ci.0975, width = 0.1))
        list(model1.graph, model2.graph)
    }
    load("casis.RData")
    write.csv(model1.graph.data, file = "model1.results.csv", row.names = FALSE)
    write.csv(model2.graph.data, file = "model2.results.csv", row.names = FALSE)
    gh.sens <- draw_graphs_sa()
    Map(function(format)
        Map(function(g, g.nm)
            ggsave(filename = g.nm,
                   g, device = format,
                   width = 1000, height = 1500,
                   units = "px", dpi = 300, scale = 2.0)
            , gh.sens, paste0(c("model1.sa.", "model2.sa."), format)),
        c("eps", "png", "pdf"))
    model1.graph.data <- model1.graph.data[with(model1.graph.data, grep("est", confound)),]
    model2.graph.data <- model2.graph.data[with(model2.graph.data, grep("est", confound)),]
    gh.est <- draw_graphs()
    Map(function(format)
        Map(function(g, g.nm)
            ggsave(filename = g.nm,
                   g, device = format,
                   width = 1500, height = 1000,
                   units = "px", dpi = 300, scale = 2.0)
            , gh.est, paste0(c("model1.est.", "model2.est."), format)),
        c("eps", "png", "pdf"))
    mod.sa <- function(d)  {
        mod.v <- function(v) {
            c(v[1], (v[-c(1:2)] / (v[-1] |> head(-1)))  * (v |> head(-2)))
        }
        v.est <- d[,"est"]
        v.confound <- with(d, grep("est", confound, invert = TRUE, value = TRUE))
        data.frame(effect = d[1,"effect"], confound = v.confound, Map(mod.v, d[,c("est", "ci.0025", "ci.0975")]))

        d <- d[!with(d, confound == "0.00"), ]

    }
    tmp <- model1.graph.data[with(model1.graph.data, grep("Total", effect)),]
    model1.graph.data[with(model1.graph.data, grep("est", confound)),]
    # Graphs +----------------------------------------------------------+
    model1.sim.df <- reshape.results(model1.sim)
    ggplot(model1.sim.df, aes(x = effect, y= est, lower = ci.0025, upper = ci.0975)) +
        xlab("Effect") +
        ylab(if.t(attributes(model1.sim)[["efftype"]] == "difference",
                  "Estimate (Difference)",
                  "Estimate (Odds Ratio)")) +
    geom_point()

    load("l.mo1.results.RData")
    d3.sim.sa <- l.mo1.results[["sa"]]
    d3.sim <- l.mo1.results[["sim"]]
    reshape.results <- function(results) {
        reshape.results1 <- function(m) {
            b.ie <- grepl("^ie", colnames(m))
            b.te <- grepl("^te$", colnames(m))
            b.de <- grepl("^de$", colnames(m))
            v.nm <- character()
            v.nm[b.te] <- "Total"
            v.nm[b.de] <- "Direct"
            v.nm[b.ie] <- switch(sum(b.ie) |> as.character(),
                                 "2" = c("Indirect (M1)",
                                         "Indirect (M2)"),
                                 "3" = c("Indirect (M1)",
                                         "Indirect (M2)",
                                         "Indirect (M1, M2)"))
            v.nm <- factor(v.nm,
                           levels = v.nm,
                           labels = c(v.nm[b.te],
                                      v.nm[b.de],
                                      v.nm[b.ie]))
            data.frame(effect = v.nm, t(m))
        }
        switch(is.list(results) + 1,
               "1" = {
                   reshape.results1(results)
               },
               "2" = {
                   do.call(rbind, Map(function(x, x.nm)
                                      data.frame(confound = x.nm,
                                                 reshape.results1(x)),
                                      results,
                                      sprintf("%.2f",
                                              attributes(results)[["cor"]])))
               })
    }
    d3 <- do.call(rbind,
                 list(data.frame(confound = "0.00 (est)", reshape.results(d3.sim)),
                      reshape.results(d3.sim.sa)))
    d3 <- do.call(rbind, split(d3, d3[,"effect"]))
    d3 <- within(d3, confound <- factor(confound, levels = confound |> unique()))
    ggplot(d3, aes(x = confound , y= est, group = confound)) +
        xlab("Effect") +
        ylab("Estimate (Difference)") +
        geom_point(shape = 4) +
        geom_line(aes(group = effect)) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_errorbar(aes(ymin = ci.0025, ymax = ci.0975, width = 0.1)) +
        facet_grid(rows = vars(effect))
    # +----------------------------------------------------------------------------+
}

# VALIDATION CHECKS +----------------------------------------------------------+
# A total of 96 combinations
# For each combination, 100 datasets were geneerated.
# Each dataset contains y, m2, m1, covariates, and treatment indicator.
# 9600 datasets were generated and each dataset was put through the estimating algorithim for the model-based estimator and simulator based estimator.
# 9600 x 2 estimating procedures were run.
# The bootstrap and number of simulations per procedure was kept at a modest 10000 to makes this comparison complete in a reasonable amount of time.
# This is the only aspect that differs from the everyday use case where at least 100000 simulations or bootstrap is recommended particularly when binary outcomes are involved.

# 9600 x 2 estimations mean that 19200 estimations need to be made.
# About 6 minutes was needed for each estimation resulting in a total of 115200 minutes or about 80 days.
# If the recommended number of bootstraps and simulations, i.e. 100000, was used, the amount of time will scale linearly to 800 days.
# At 10000 bootstraps and simulations and using parallelisation and multiple computers, the time to produce a set of results was reduced to about 5 days and 50 days if 100000 bootstraps and simulations were used.
# Using 2 computers and parallelising as much of the operations as was possible, this reduced the number of days required to about 5 days.

## functions +-------------------------------------------------------+
gen.check.combn <- function(v.var, v.vartype, v.ss, v.es) {
    v.ef <- c("te", "de", "ie1", "ie2", "ie12")
    d.varcomb <- expand.grid(rep(list(v.vartype), length(v.var)))
    v.comb <- apply(as.matrix(d.varcomb), 1, paste0, collapse = "")
    d <- merge(data.frame(comb = v.comb, `colnames<-`(d.varcomb, v.var)),
               `colnames<-`(expand.grid(v.comb, v.ss), c("comb", "ss")), all.y = TRUE) |>
         merge(`colnames<-`(expand.grid(v.comb, v.es), c("comb", "es")), all.y = TRUE) |>
         merge(`colnames<-`(expand.grid(v.comb, v.ef), c("comb", "ef")), all.y = TRUE)
    rbind(d[-with(d, grep("bc", comb)),],
          d[with(d, intersect(grep("te", ef), grep("bc", comb))),])
}
cohend <- function(v1, v2) {
    s.corr <- 1
    n <- length(v1)
    if(length(v2) != n) stop("Sample sizes unequal!")

    if(n <= 50) {
        s.corr <- ((n - 3)/(n - 2.25)) * sqrt((n - 2) / n)
    }
    d <- (mean(v1) - mean(v2))/sqrt((var(v1) + var(v2))/2)
    d * s.corr
}
cohend.b <- function(m1, m2 = 0.5) {
    v1 <- m1 * (1 - m1)
    v2 <- m2 * (1 - m2)
# cohend.b.chk <- function(dec, v.es) {
#     mkdec <- function(n) (1:n)/n
#     v.dec <- mkdec(dec)
#     v.cohend.b <- abs(unlist(Map(cohend.b, v.dec)))
#     m <- matrix(c(v.cohend.b, v.dec), ncol = 2)
#     m.es.diff <- `rownames<-`(t(mapply(function(d, v) abs(d - v), m[,1], list(v.es))),
#                               sprintf("%0.3f", v.dec))
#     l.es.diff.min <- apply(m.es.diff, 2, function(x) as.numeric(names(which(x == min(x)))), simplify = FALSE)
#     Map(function(v)
#         {
#             if(length(v) < 2)
#                 v <- sort(c(1 - v, v))
#             v
#         }, l.es.diff.min)
# }
# m <- cohend.b.chk(1e7, v.es)
# > m
# [[1]]
# [1] 0.152 0.848
# #
# [[2]]
# [1] 0.264 0.736
# #
# [[3]]
# [1] 0.401 0.599
# odr [1] 5.578947
# odr [1] 2.787879
# odr [1] 1.493766
    (m1 - m2)/sqrt((v1 + v2) / 2)
}
rlogis.acc <- function(n, p = 0.5, acc = 1e-3, type = "q") {
    b.cont <- TRUE
    while(b.cont) {
        b.cont <- FALSE
        v <- rlogis(n, location = 0, scale = 1)
        s.diff <- abs(plogis(mean(v), location = 0, scale = 1) - 0.5)
        if(s.diff >= acc) b.cont <- TRUE
    }
    v.adj <- v - qlogis(p, location = 0, scale = 1)
    switch(type,
           q = v.adj,
           p = plogis(v.adj, location = 0, scale = 1))
}
rbin.acc <- function(n, p) {
    n.p <- round(p * n, 0)
    c(rep(1, n.p), rep(0, n - n.p))
}
rnorm.acc <- function(n, m = 0, s = 1, acc.m = (1e-4) * s, acc.s = acc.m) {
    v <- rnorm(n, m, s)
    v <- v - mean(v)
    v <- v/sd(v)
    v
}
rbinp.acc <- function(n, p = 0.5, acc = 1/(1e2*n),
                       v.spread = min(abs(c(0.5 - p,  1.0 - p, 0.0 - p)))) {
    v.limit <- c(p - v.spread, p + v.spread)
    b.cont <- TRUE
    while(b.cont) {
        b.cont <- FALSE
        v <- runif(n, min = v.limit[1], max = v.limit[2])
        if(abs(mean(v) - p) >= acc) b.cont <- TRUE
    }
    v <- attr.set(list(v), list(spread = v.spread))
    qlogis(v)
}
gen.cont <- function(n, m, ef, s) {
    m1 <- m + (ef * s)
    m2 <- m
    v1 <- rnorm.acc(n, m1, s)
    v2 <- rnorm.acc(n, m2, s)
    message(paste0("Cohen's D is ", cohend(v1, v2), "."))
    list(v1, v2)
}
cor.m <- function(m, v.rho, s.dist = "normal") {
    s.n <- nrow(m)
    if(is.null(s.n)) {
        s.n <- length(m)
        if(is.null(s.n)) {
            stop("Error")
        } else {
            m <- matrix(m, ncol = 1)
        }
    }
    v.rand <- switch(s.dist,
                     normal = rnorm(s.n),
                     logistic = rlogis(s.n))
    m.sc <- scale(m)
    l.m.svd <- svd(m.sc)
    svd.u <- l.m.svd[["u"]]
    svd.v <- l.m.svd[["v"]]
    svd.d <- l.m.svd[["d"]]
    v.svd.d.dual <- ifelse(svd.d > 0, 1/svd.d, 0)
    if(length(svd.d) == 1) {
        m.svd.d.dual <- matrix(v.svd.d.dual, ncol = 1, nrow = 1)
    } else {
        m.svd.d.dual <- diag(v.svd.d.dual)
    }
    m.sc.dual <- (s.n - 1) * svd.u %*% m.svd.d.dual %*% t(svd.v)
    v.res <- residuals(lm(rnorm(s.n) ~ m.sc))
    s.sigma.sq <- c((1 - (v.rho %*% cov(m.sc.dual) %*% v.rho)) / var(v.res))
    if(s.sigma.sq < 0) {
        stop("Correlations provided not possible.")
    }
    m.sc.dual %*% v.rho + sqrt(s.sigma.sq) * v.res
}
fac.to.char <- function(v.fac) {
    levels(v.fac)[v.fac]
}
gen.env <- function(v.var, v.vartype, v.ss, v.es, s.err) {
    v.ss.h <- v.ss/2
    d.combn <- within(gen.check.combn(v.var, v.vartype, v.ss, v.es),
                      {

                          # initialise so that each have the same length
                          es.bc.ct <- es.bc.rx <- es

                          es.bc.ct[y == "b"] <- 0.5
                          es.bc.ct[y == "c"] <- 0

                          # setup binary rx P(Y=1)
                          es.bc.rx[y == "b"] <- (`names<-`(c(0.848, 0.736, 0.599),
                                                           c("large", "medium", "small")))[names(es.bc.rx[y == "b"])]

                          # convert y, m1, m2 to character
                          y  <- fac.to.char(y)
                          m1 <- fac.to.char(m1)
                          m2 <- fac.to.char(m2)
                      })
    l.combn <- unname(split(d.combn, 1:nrow(d.combn)))
    list(v.var = v.var,
         v.vartype = v.vartype,
         v.ss = v.ss,
         v.es = v.es,
         l.combn = l.combn)
}
gen.est <- function() {
    v.datafiles <- list.files(pattern = "^model-.*RData")
    s.iter <- 1e4L
    s.ci  <- 0.95

    for(i in v.datafiles) {
        load(i)
        cl.data <- make.cl(env = environment())
        l.est <- clusterMap(cl.data,
                            function(l.mo.1)
                            {
                                list(mo =   mo.med(l.mo = l.mo.1, s.ci = s.ci, int.boot = s.iter,
                                                   b.parallel = FALSE),
                                     sim = sim.med(l.mo = l.mo.1, s.ci = s.ci, int.sims = s.iter,
                                                   b.parallel = FALSE))
                            }, l.models)
        save(l.est, file = paste0("est-", i), compress = "xz")
        rm(l.est, l.models)
    }
}
rm.names <- function(d) {
    `colnames<-`(`rownames<-`(d, NULL),NULL)
}
gen.c.y <- function(d.combn.1, s.err.prop) {
    s.ss <- unlist(d.combn.1[,"ss"])
    s.es <- unlist(d.combn.1[,"es"])
    r <- sort(rep(c(0, 1), s.ss/2))
    cv <- cor.m(r, 0)
    y.ne <- cv + (s.es * r)
    y.ne <- predict(glm(y.ne ~ cv + r))
    y.e <- cor.m(y.ne, 0) * sqrt(var(y.ne) * s.err.prop)

    data.frame(y = y.ne + y.e, y.e = y.e, r = r, cv = cv)
}
gen.b.y <- function(d.combn.1, s.err) {
    v.y.p <- unlist(d.combn.1[, c("ct.mn", "rx.mn")])
    s.n <- d.combn.1[,"ss"]
    v.y.rx <- rbinp.acc(n = s.n/2, p = v.y.p[2])
    v.y.ct <- rbinp.acc(n = s.n/2, p = v.y.p[1], v.spread = attr(v.y.rx, "spread"))
    v.y  <- c(v.y.ct, v.y.rx)
    v.r <- sort(rep(c(0, 1), s.n/2))
    v.r.coef <- odr(v.y.p[2], v.y.p[1]) * v.r
    v.cv <- residuals(glm(v.y ~ v.r.coef))
    v.cv <- v.cv + rnorm.acc(length(v.cv), s = sqrt(var(v.cv) * s.err))
    v.y.b <- unname(unlist(Map(function(x, p)
                               {
                                   x.len <- length(x)
                                   n1 <- round(p * x.len)
                                   n0 <- x.len - n1
                                   v.y.sorted <- sort(`names<-`(x, as.character(1:x.len)))
                                   v.y.b.sorted <- `names<-`(unlist(Map(rep, c(0, 1), c(n0, n1))), names(v.y.sorted))
                                   v.y.b.sorted[as.character(sort(as.integer(names(v.y.b.sorted))))]
                               }, split(v.y, v.r), v.y.p)))
    data.frame(y = v.y.b, y.e = 0, r = v.r, cv = v.cv)
}
gen.data <- function(d.combn.uniq) {
    s.err.prop <- 0.1
    s.m.coef <- 4
    v.b.m.p <- c(0.5, 0.8)

    Map(function(s.ind)
        {
            d.combn.1 <- d.combn.uniq[s.ind,]
            set.seed(d.combn.1[,"seed"])
            v.var <- unlist(d.combn.1[, c("y", "m2", "m1")])
            v.var.c <- names(v.var)[v.var == "c"]
            v.var.b <- names(v.var)[v.var == "b"]

            if(any(v.var.b == "y")) {
                d <- gen.b.y(d.combn.1, s.err.prop)
            }
            if(any(v.var.c == "y")) {
                d <- gen.c.y(d.combn.1, s.err.prop)
            }

            b.c.m <- grepl("^m", v.var.c)
            b.b.m <- grepl("^m", v.var.b)
            if(any(b.c.m)) {
                m.nocor <- cbind(d[,"y.e"], d[,"r"], d[,"cv"])
                b.const <- unlist(Map(length, apply(m.nocor, 2, unique))) == 1
                if(any(b.const)) {
                    m.nocor <- m.nocor[,-which(b.const)]
                }
                v.m.e <- cor.m(m.nocor, rep(0, ncol(m.nocor)))
                if(sum(b.c.m) >= 1) {
                    v.m <- v.m.e + s.m.coef * d[,"r"] + d[,"cv"]
                    d <- data.frame(d, v.m)
                    colnames(d)[ncol(d)] <- (v.var.c[b.c.m])[1]
                }
                if(sum(b.c.m) == 2) {
                    m.nocor <- cbind(m.nocor, v.m.e)
                    v.m.e <- cor.m(m.nocor, rep(0, ncol(m.nocor)))
                    v.m <- v.m.e + s.m.coef * d[,"r"] + d[,"cv"]
                    d <- data.frame(d, v.m)
                    s.ncol.d <- ncol(d)
                    colnames(d)[(s.ncol.d - 1):s.ncol.d] <- v.var.c[b.c.m]
                }
            }
            if(any(b.b.m)) {
                s.n <- nrow(d)
                n.r1.p1 <- round(v.b.m.p[2] * (s.n/2))
                n.r1.p0 <- (s.n/2) - n.r1.p1
                n.r0.p1 <- round(v.b.m.p[1] * (s.n/2))
                n.r0.p0 <- (s.n/2) - n.r0.p1
                v.b.m.unsorted <- unlist(Map(rep, c(0, 1), c(n.r0.p0, n.r0.p1, n.r1.p0, n.r1.p1)))
                v.b.m <- v.b.m.unsorted[with(d, order(r, y))]
                d <- data.frame(d, v.b.m)
                colnames(d)[ncol(d)] <- (v.var.b[b.b.m])[1]
                if(sum(b.b.m) == 2) {
                    v.b.m2 <- unlist(Map(function(v)
                                         {
                                             v.n <- length(v)
                                             v.replace.ind <- sample(1:v.n, round(v.n/2), replace = FALSE)
                                             v[v.replace.ind] <- 1 - v[v.replace.ind]
                                             v
                                         }, split(v.b.m, with(d, r))))
                    d <- data.frame(d, v.b.m2)
                    colnames(d)[ncol(d)] <- (v.var.b[b.b.m])[2]
                }
            }
            d <- d[c("y", "r", "cv", "m2", "m1")]
        }, 1:nrow(d.combn.uniq))
}
gen.models.1 <- function(d.combn.1, m) {
    d.y  <- as.data.frame(m)
    d.m2 <-  d.y[, -which(colnames(d.y)  == "y")]
    d.m1 <- d.m2[, -which(colnames(d.m2) == "m2")]

    fam.b <- binomial(link = "logit")
    fam.c <- gaussian(link = "identity")

    l.fam <- Map(function(s) `if`(s == "b", fam.b, fam.c),
                 unlist(d.combn.1[, c("y", "m2", "m1")]))
    l.fml <- Map(paste0, c("y", "m2", "m1"), " ~ .")
    l.d <- list(d.y, d.m2, d.m1)

    l.models <- Map(function(fml, fam, d) glm(fml, data = d, family = fam),
                    l.fml, l.fam, l.d)
    l.models
}
gen.models <- function(d.combn, l.d) {
    Map(function(s.ind) gen.models.1(d.combn[s.ind,], l.d[[s.ind]]), 1:nrow(d.combn))
}
## setup +-----------------------------------------------------------+
v.var <- c("y", "m2", "m1")
v.vartype <- c("c", "b")
v.ss <- sort(mapply(function(n) `if`(n%%2 == 1, n - 1, n), 125 * 2^(0:3)))
v.es <- c(large = 0.8, medium = 0.5, small = 0.2)
s.err <- 0.2

ut.silent(list2env(gen.env(v.var, v.vartype, v.ss, v.es, s.err), environment()))
d.combn <- do.call(rbind, l.combn)
d.combn.uniq <- `rownames<-`(unique(d.combn[,!grepl("^ef$", colnames(d.combn))]), NULL)
d.combn.uniq <- within(d.combn.uniq,
                       {
                           ct.mn <- es.bc.ct
                           rx.mn <- es.bc.rx

                           ct.sd <- ct.mn
                           rx.sd <- rx.mn

                           mn <- (cbind(ss, ct.mn, rx.mn))[y == "b",]
                           ct.sd.b <- mn[,"ct.mn"] * (1 - mn[,"ct.mn"]) * (mn[,"ss"] / (mn[,"ss"] - 1))
                           rx.sd.b <- mn[,"rx.mn"] * (1 - mn[,"rx.mn"]) * (mn[,"ss"] / (mn[,"ss"] - 1))

                           ct.sd[y == "b"] <- ct.sd.b
                           rx.sd[y == "b"] <- rx.sd.b
                           ct.sd[y == "c"] <- 1
                           rx.sd[y == "c"] <- 1

                           rm(es.bc.ct, es.bc.rx, ct.sd.b, rx.sd.b, mn)
                       })
d.combn.uniq <- local({
    n.root.seeds <- 100
    n.combn <- nrow(d.combn.uniq)
    d.seeds <- do.call(rbind,
                       Map(function(x)
                           {
                               set.seed(x)
                               m <- `colnames<-`(cbind(1:n.combn, x, gen.seeds(n.combn)), c("combn", "root.seed", "seed"))
                               d <- as.data.frame(m)
                               with(d, data.frame(combn = sprintf("%02d", combn),
                                                  root.seed = sprintf("%03d", root.seed),
                                                  seed = seed))
                           }, 1:n.root.seeds))
    l.seeds <- Map(function(d) `rownames<-`(d, NULL), split(d.seeds, with(d.seeds, combn)))
    l.combn <- split(d.combn.uniq, 1:nrow(d.combn.uniq))
    d <- do.call(rbind,
                 Map(function(d, d.combn.1)
                     {
                         `rownames<-`(cbind(d.combn.1[rep(1, nrow(d)),], d), NULL)
                     }, l.seeds, l.combn))
    `rownames<-`(d[with(d, order(root.seed, combn)),], NULL)
})

## data generation and model fitting +-------------------------------+
# l.data.seeded <- gen.data(d.combn.uniq)
# save(l.data.seeded, file = "data-simulations/data-seed-combn.RData")
# Map(function(d, fn)
#     save(d, file = fn, compress = "xz"),
#     l.data.seeded,
#     paste0("data-simulations/data-",
#            with(d.combn.uniq,
#                 apply(cbind(root.seed, combn), 1,
#                       paste0, collapse = "-")),
#            ".RData"))

# load("data-simulations/data-seed-combn.RData")
# l.models <- gen.models(d.combn.uniq, l.data.seeded)
# save(l.models, file = "data-simulations/models-seed-combn.RData", compress = "xz")
# Map(function(mo, fn)
#     save(mo, file = fn, compress = "xz"),
#     l.models,
#     paste0("data-simulations/model-",
#            with(d.combn.uniq,
#                 apply(cbind(root.seed, combn), 1,
#                       paste0, collapse = "-")),
#            ".RData"))
#

## estimation of mediated causal effects +---------------------------+
s.dir <- "data-simulations/"
# v.model.files <- list.files(paste0(s.dir, "models/"), pattern = "^model-\\d+-\\d+\\.RData")
# v.est.files <- list.files(s.dir, pattern = "^est-")
# v.model.files <- v.model.files[!v.model.files %in% gsub("^est", "model", v.est.files)]
# v.seq <- 1:length(v.model.files)
# cl <- make.cl(env = environment())
# # Map(function(i)
# clusterMap(cl, function(i)
#            {
#                s.dir <- "data-simulations/"
#                s.file <- v.model.files[i]
#                s.file.in  <- paste0(s.dir, "models/",  s.file)
#                s.file.out <- gsub("model", "est", s.file)
#                s.seed <- as.integer(gsub("^model|\\.RData$|-", "", s.file))
#                load(s.file.in)
#                s.ci <- 0.95
#                s.iter <- 1e4L
#                b.parallel <- FALSE
#                l.est <- list(mo  =  mo.med(mo, s.ci = s.ci, int.boot = s.iter, s.seed = s.seed, b.parallel = b.parallel),
#                              sim = sim.med(mo, s.ci = s.ci, int.sims = s.iter, s.seed = s.seed, b.parallel = b.parallel))
#                d <- `rownames<-`(do.call(rbind,
#                                          Map(function(m, m.nm)
#                                              {
#                                                  d <- as.data.frame(t(m))
#                                                  data.frame(rownames(d), m.nm, d)
#                                              }, l.est, names(l.est))), NULL)
#                d.est <- data.frame(index = i, `colnames<-`(d, c("effect", "est.type", "est", "ll", "ul")))
#                save(d.est, file = s.file.out, compress = "xz")
#            }, v.seq)

## graphs +----------------------------------------------------------+
## functions & constants +---------------------------------+
s.offset <- 25
plot.effects <- function(d, s.offset, fn, s.height, s.width) {
    require(extrafont)
    v.ss <- sort(unique(with(d, ss)))
    d <- within(d,
                {
                    ss[est.type == "mo"]  <- ss[est.type == "mo"]  - s.offset
                    ss[est.type == "sim"] <- ss[est.type == "sim"] + s.offset
                })

    if(d[1, "y"] == "c") {
        v.y.axis <- "Causal effects (Difference)"
        v.y.hline <- 0
    }
    if(d[1, "y"] == "b") {
        v.y.axis <- "Causal effects (Odds ratio)"
        v.y.hline <- 1
    }

    v.height <- length(unique(d[,"effect"])) * s.height
    gph <-
        ggplot(d, aes(x = ss , y = est, shape = factor(est.type))) +
        geom_point(size = 2) +
        scale_shape_discrete(name = "Estimate type",
                             labels = c("Model-based",
                                        "Simulation-based")) +
        scale_x_continuous(name = "Sample size",
                           breaks = c(0, v.ss),
                           labels = waiver(),
                           limits = c(0, 1050)) +
        scale_y_continuous(name = v.y.axis) +
        geom_errorbar(aes(ymin = ll, ymax = ul, width = 0.1)) +
        geom_hline(yintercept = v.y.hline, linetype = "dashed", color = "black") +
        facet_grid(rows = vars(effect), cols = vars(es)) +
        theme(text = element_text(family = "PragmataPro Mono Liga"),
              axis.text.x = element_text(angle = -45, vjust = -0.5, hjust = 0.8))

    fn <- paste0(gsub("\\.","-", gsub("^(FALSE|TRUE)", "fig-5", fn)), ".")
    fn.path <- "../../04-thesis/parts"
    ggsave(filename  = paste0(fn, "pdf"),
           plot      = gph,
           device    = "pdf",
           path      = fn.path,
           scale     = 1,
           width     = s.width,
           height    = v.height,
           units     = "in",
           dpi       = 300,
           limitsize = FALSE,
           bg        = NULL,
           family    = "PragmataPro Mono Liga")

    ggsave(filename  = paste0(fn, "eps"),
           plot      = gph,
           device    = "eps",
           path      = fn.path,
           scale     = 1,
           width     = s.width,
           height    = v.height,
           units     = "in",
           dpi       = 300,
           limitsize = FALSE,
           bg        = NULL,
           family    = "PragmataPro Mono Liga")

}
## data processing +---------------------------------------+
### test read +----------------------------------+
# l.est.files <- local({
#     v.est.files <- list.files(s.dir, pattern = "^data-")
#     m.est.files <- `colnames<-`(apply(do.call(rbind,
#                                               strsplit(gsub("^est-(.*)\\.RData", "\\1",
#                                                             v.est.files), "-")),
#                            2, as.integer),
#                      c("seed", "combn"))
#     d.est.files <- data.frame(filename = v.est.files, m.est.files)
#     split(d.est.files, with(d.est.files, combn))
# })

# for(i in sort(unname(unlist(do.call(rbind, Map(`[`, l.est.files, 1)))))) {
#     message(i)
#     load(paste0(s.dir, i))
# }

### generate summary +---------------------------+
# l.summary <-
#     Map(function(d)
#         {
#             l.combined.ind <-
#                 do.call(rbind,
#                         Map(function(r)
#                             {
#                                 load(paste0(s.dir, r[1,1]))
#                                 d.est
#                             }, split(d, 1:nrow(d))))
#             l.summary <-
#                 Map(function(d.esttype)
#                     {
#                         l.effect <- split(d.esttype, with(d.esttype, effect))
#                         l.effect.summary <-
#                             Map(function(d)
#                                 {
#                                     with(d, data.frame(est.type = est.type[1],
#                                                        effect = effect[1],
#                                                        t(apply(cbind(est, ll, ul), 2, mean, na.rm = TRUE))))
#                                 }, l.effect)
#                         do.call(rbind, l.effect.summary)
#                     }, split(l.combined.ind, with(l.combined.ind, est.type)))
#             `rownames<-`(do.call(rbind, l.summary), NULL)
#         }, l.est.files)
load(paste0(s.dir, "l.summary.RData"))

d.summary <-
    `rownames<-`(do.call(rbind,
                         Map(function(d, ind)
                             data.frame(combn.ind = ind, d),
                             l.summary, as.integer(names(l.summary)))),
      NULL)
d.combn.noseed <-
    unique(within(d.combn.uniq,
                  {
                      combn.ind <- as.integer(combn)
                      rm(seed, root.seed)
                  }))
d <- merge(within(d.combn.noseed,
                  combn.ind <- as.integer(combn)),
           d.summary,
           by = "combn.ind",
           all = TRUE)[,-1]
d <- d[with(d, !is.nan(est)),]
d.te <- d[with(d, grepl("^te", effect)),]
d <- within(d, effect <- gsub("^te.*", "te", effect))
d[, "effect"] <- factor(d[, "effect"],
                        levels = c("te", "de", "ie1", "ie2", "ie3"),
                        labels = c("Total",
                                   "Direct",
                                   "Indirect (through M1)",
                                   "Indirect (through M2)",
                                   "Indirect (through M1 & M2)"))

l <- split(d, with(d, comb))
l <- split(l, grepl("bc", names(l)))
l[["FALSE"]] <- Map(function(d)
                    `names<-`(split(d, grepl("^Indirect", d[,"effect"])),
                              c("total.direct", "indirect")),
                   l[["FALSE"]])
l[["FALSE"]] <- unlist(l[["FALSE"]], recursive = FALSE)
l[["TRUE"]] <- Map(function(d)
                   d[with(d, effect == "Total"),],
                   l[["TRUE"]])
l <- unlist(l, recursive = FALSE)

### generate graphs +----------------------------+
Map(plot.effects, l, s.offset, names(l), 2, 8.5)

