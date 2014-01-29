### Copyright (c) 2012 Christopher G. Green <christopher.g.green@gmail.com>
###
### This file is part of the cggmisc package for R.
###
### This program is free software; you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2 of the License, or
### (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### Based on qqmath.formula from the lattice package written by 
### Deepayan Sarkar and the lmfmResQQPlot function from the robust
### package maintained by Kjell Konis. 
### 
### See the COPYRIGHTS file in the top-level package directory for 
### a full list of copyright and authorship information.
###
"qqmathFitted" <- function(formula, 
    data = NULL,
    allow.multiple = is.null(groups) || outer,
    outer = !is.null(groups),
    distribution = qnorm, 
    f.value = NULL, 
    auto.key = FALSE,
    aspect = "fill",
    panel = panel.qqmath.fitted, 
    prepanel = prepanel.qqmath.fitted, 
    scales = list(),
    strip = TRUE,
    groups = NULL, 
    xlab,
    xlim,
    ylab,
    ylim,
    drop.unused.levels = lattice.getOption("drop.unused.levels"),
    distribution.fit = function(y) list(mean=0, sd=1),
    quantile.type = 7,
    qqstyle = NULL,
    add.qqline = c("none", "lm", "lmRob"),
    add.qqline.par = if ( !is.null(groups) ) 
      trellis.par.get("panel.superpose") else 
         trellis.par.get("add.line"),
    envelope = TRUE,
    rdist = rnorm,
    rdist.args = NULL,
    sig.level = 0.95,
    mc.samples = 100,
    seed = .Random.seed,
    envstyle = NULL,
    id.n = 3,
    labelargs = FALSE,
    verbose = FALSE,
    ...,
    lattice.options = NULL,
    default.scales = list(),
    subscripts = !is.null(groups),
    subset = TRUE
    )
{
    #formula <- x
    dots <- list(...)
    groups <- eval(substitute(groups), data, environment(formula))
    subset <- eval(substitute(subset), data, environment(formula))

    if (!is.null(lattice.options))
    {
        oopt <- lattice.options(lattice.options)
        on.exit(lattice.options(oopt), add = TRUE)
    }

    ## Step 1: Evaluate x, y, etc. and do some preprocessing
    
    form <-
        latticeParseFormula(formula, data, subset = subset,
            groups = groups, multiple = allow.multiple,
            outer = outer, subscripts = TRUE,
            drop = drop.unused.levels)
    groups <- form$groups

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    if ("subscripts" %in% names(formals(panel))) subscripts <- TRUE
    if (subscripts) subscr <- form$subscr

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    cond <- form$condition
    ## number.of.cond <- length(cond)
    x <- form$right
    if (length(cond) == 0)
    {
        strip <- FALSE
        cond <- list(gl(1, length(x)))
        ## number.of.cond <- 1
    }

    dist.name <- paste(deparse(substitute(distribution)), collapse = "")
    if (missing(xlab)) xlab <- dist.name
    if (missing(ylab)) ylab <- form$right.name

    ## create a skeleton trellis object with the
    ## less complicated components:
    # cgg trellis.skeleton is private to lattice
    trellis.skeleton <- lattice:::trellis.skeleton

    foo <-
        do.call("trellis.skeleton",
            c(list(formula = formula, 
                cond = cond,
                aspect = aspect,
                strip = strip,
                panel = panel,
                xlab = xlab,
                ylab = ylab,
                xlab.default = dist.name,
                ylab.default = form$right.name,
                lattice.options = lattice.options),
                dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- sys.call(sys.parent()); foo$call[[1]] <- quote(qqmath)

    ## Step 2: Compute scales.common (leaving out limits for now)

    if (is.character(scales)) scales <- list(relation = scales)
    scales <- lattice:::updateList(default.scales, scales)
    construct.scales <- lattice:::construct.scales
    foo <- c(foo, do.call("construct.scales", scales))

    ## Step 3: Decide if limits were specified in call:
    
    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limit))
    {
        have.xlim <- TRUE
        xlim <- foo$x.scales$limit
    }
    have.ylim <- !missing(ylim)
    if (!is.null(foo$y.scales$limit))
    {
        have.ylim <- TRUE
        ylim <- foo$y.scales$limit
    }

    ## Step 4: Decide if log scales are being used:

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log

    ## This is slightly weird because 'x' is eventually plotted in the
    ## Y-axis
    if (have.xlog)
    {
        warning("Can't have log X-scale")
        have.xlog <- FALSE
        foo$x.scales$log <- FALSE
    }
    if (have.ylog)
    {
        ylog <- foo$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)
        
        x <- log(x, ybase)
        if (have.ylim) ylim <- lattice:::logLimits(ylim, ybase)
    }


    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    ## Step 6: Determine packets

    # cgg force argument matching for add.qqline
    add.qqline <- match.arg(add.qqline)

    # cgg modify this to include all panel function arguments
    foo$panel.args.common <-
        c(list(distribution = distribution,
            f.value = f.value,
            distribution.fit = distribution.fit,
            quantile.type = quantile.type,
            qqstyle = qqstyle,
            add.qqline = add.qqline,
            add.qqline.par = add.qqline.par,
            envelope = envelope,
            rdist = rdist,
            rdist.args = rdist.args,
            sig.level = sig.level,
            mc.samples = mc.samples,
            seed = seed,
            envstyle = envstyle,
            id.n = id.n,
            labelargs = labelargs,
            storeframe = sys.frame(sys.nframe()),
            verbose = verbose
         ), dots)

    if (subscripts)
    {
        foo$panel.args.common$groups <- groups
    }

    npackets <- prod(cond.max.level)
    if (npackets != prod(sapply(foo$condlevels, length))) 
        stop("mismatch in number of packets")

    # setting up arguments for each panel
    foo$panel.args <- vector(mode = "list", length = npackets)

    foo$packet.sizes <- numeric(npackets)
    if (npackets > 1)
    {
        dim(foo$packet.sizes) <- sapply(foo$condlevels, length)
        dimnames(foo$packet.sizes) <- lapply(foo$condlevels, as.character)
    }
    cond.current.level <- rep(1, length(cond))
    for (packet.number in seq_len(npackets))
    {
        id <- lattice:::compute.packet(cond, cond.current.level)
        foo$packet.sizes[packet.number] <- sum(id)

        foo$panel.args[[packet.number]] <-
            list(x = x[id])
        if (subscripts)
            foo$panel.args[[packet.number]]$subscripts <-
                subscr[id]

        cond.current.level <-
            lattice:::cupdate(cond.current.level,
                cond.max.level)
    }

    # fitted values for each panel
    fitted.args <- vector(mode = "list", length = npackets)

    more.comp <-
        c(lattice:::limits.and.aspect(prepanel, #prepanel.default.qqmath,
            prepanel = NULL, #prepanel, 
            have.xlim = have.xlim, xlim = xlim, 
            have.ylim = have.ylim, ylim = ylim, 
            x.relation = foo$x.scales$relation,
            y.relation = foo$y.scales$relation,
            panel.args.common = foo$panel.args.common,
            panel.args = foo$panel.args,
            aspect = aspect,
            npackets = npackets,
            x.axs = foo$x.scales$axs,
            y.axs = foo$y.scales$axs,
            count = quote(count)),
        lattice:::cond.orders(foo))

    foo[names(more.comp)] <- more.comp

    # prepanel will set up fitted.args properly.
    # now modify distribution.fit to use the fitted values
    # in the panel function, thereby eliminating the need
    # to refit the distribution parameters

    for (packet.number in seq_len(npackets)) {
       foo$panel.args[[packet.number]]$fitted.args <- 
          fitted.args[[packet.number]]
    }

    # clear distribution.fit in the common args
    foo$panel.args.common$distribution.fit <- NULL
    foo$panel.args.common$storeframe       <- NULL

    if (is.null(foo$legend) && !is.null(groups) &&
        (is.list(auto.key) || (is.logical(auto.key) && auto.key)))
    {
        foo$legend <-
            list(list(fun = "drawSimpleKey",
                args = lattice:::updateList(list(text = levels(as.factor(groups)),
                    points = TRUE,
                    rectangles = FALSE,
                    lines = FALSE),
                    if (is.list(auto.key)) auto.key else list())))
        foo$legend[[1]]$x <- foo$legend[[1]]$args$x
        foo$legend[[1]]$y <- foo$legend[[1]]$args$y
        foo$legend[[1]]$corner <- foo$legend[[1]]$args$corner

        names(foo$legend) <- 
            if (any(c("x", "y", "corner") %in% names(foo$legend[[1]]$args)))
                "inside"
            else
                "top"
        if (!is.null(foo$legend[[1]]$args$space))
            names(foo$legend) <- foo$legend[[1]]$args$space
    }
    class(foo) <- "trellis"
    foo
}
