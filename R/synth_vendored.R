# =====================================================================
# synth_vendored.R : copie du code R de Synth::synth (Hainmueller et Diamond,
# package Synth 1.1.10, GPL >= 2) et de kernlab::ipop (Karatzoglou et al.,
# package kernlab 0.9.33, GPL-2). Copie faite pour que l'appli tourne dans le
# navigateur (webR / Shinylive) sans dépendre de la chaîne Synth > optimx >
# nloptr, dont les binaires WebAssembly ne sont pas garantis.
# Avec un seul prédicteur (moyenne pré du résultat), synth() n'appelle pas
# optimx : seul ipop() sert, d'où cette copie minimale. Code non modifié.
# =====================================================================
setClass("ipop", representation(primal = "vector", dual = "numeric", how = "character"))
primal <- function(object) object@primal
`primal<-` <- function(object, value) { object@primal <- value; object }
`dual<-`   <- function(object, value) { object@dual <- value; object }
`how<-`    <- function(object, value) { object@how <- value; object }
optimx <- function(...) stop("synth_vendored : un seul prédicteur est pris en charge (pas d'optimx).")

synth_vendored <-
function (data.prep.obj = NULL, X1 = NULL, X0 = NULL, Z0 = NULL, 
    Z1 = NULL, custom.v = NULL, optimxmethod = c("Nelder-Mead", 
        "BFGS"), genoud = FALSE, quadopt = "ipop", Margin.ipop = 0.00050000000000000001, 
    Sigf.ipop = 5, Bound.ipop = 10, verbose = FALSE, ...) 
{
    if (!is.null(data.prep.obj)) {
        if (verbose) 
            cat("\nX1, X0, Z1, Z0 all come directly from dataprep object.\n\n")
        X1 <- data.prep.obj$X1
        Z1 <- data.prep.obj$Z1
        X0 <- data.prep.obj$X0
        Z0 <- data.prep.obj$Z0
    }
    else {
        if (verbose) 
            cat("X1, X0, Z1, Z0 were individually input (not dataprep object).\n\n")
    }
    store <- list(X1 = X1, X0 = X0, Z1 = Z1, Z0 = Z0)
    for (i in 1:4) {
        if (is.null(store[[i]])) {
            stop(paste("\n", names(store)[i], "is missing \n"))
        }
        if (sum(is.na(store[[i]])) > 0) {
            stop(paste("\n NAs in", names(store)[i], "\n"))
        }
        if (is.matrix(store[[i]]) == FALSE) {
            stop(paste("\n", names(store)[i], "is not a matrix object\n"))
        }
    }
    if (ncol(X1) != 1) {
        stop("\n Please specify only one treated unit: X1 has to have ncol= 1")
    }
    if (ncol(Z1) != 1) {
        stop("\n Please specify only one treated unit: Z1 has to have ncol= 1")
    }
    if (ncol(X0) < 2) {
        stop("\n Please specify at least two control units: X0 has to have ncol >= 2 ")
    }
    if (ncol(Z0) < 2) {
        stop("\n Please specify at least two control units: Z0 has to have ncol >= 2")
    }
    if (nrow(Z0) != nrow(Z1)) {
        stop("\n Different number of periods for treated and controls: nrow(Z0) unequal nrow(Z1)")
    }
    if (nrow(X0) != nrow(X1)) {
        stop("\n Different number of predictors for treated and controls: nrow(X0) unequal nrow(X1)")
    }
    if (nrow(X0) == 0) {
        stop("No predictors specified. Please specify at least one predictor")
    }
    if (nrow(Z0) == 0) {
        stop("No periods specified for Z1 and Z0. Please specify at least one period")
    }
    if (0 %in% apply(X0, 1, sd)) {
        stop("\n At least one predictor in X0 has no variation across control units. Please remove this predictor.")
    }
    nvarsV <- dim(X0)[1]
    big.dataframe <- cbind(X0, X1)
    divisor <- sqrt(apply(big.dataframe, 1, var))
    scaled.matrix <- t(t(big.dataframe) %*% (1/(divisor) * diag(rep(dim(big.dataframe)[1], 
        1))))
    X0.scaled <- scaled.matrix[, 1:ncol(X0), drop = FALSE]
    X1.scaled <- scaled.matrix[, ncol(scaled.matrix)]
    if (is.null(custom.v) & nrow(X0) != 1) {
        if (verbose) 
            cat("\n****************", "\n searching for synthetic control unit  \n", 
                "\n")
        if (genoud == TRUE) {
            if (verbose) 
                cat("\n****************", "\n genoud() requested for optimization\n", 
                  "\n")
            rgV.genoud <- genoud(fn.V, nvarsV, X0.scaled = X0.scaled, 
                X1.scaled = X1.scaled, Z0 = Z0, Z1 = Z1, quadopt = quadopt, 
                margin.ipop = Margin.ipop, sigf.ipop = Sigf.ipop, 
                bound.ipop = Bound.ipop)
            SV1 <- rgV.genoud$par
            if (verbose) 
                cat("\n****************", "\n genoud() finished, now running local optimization using optim()\n", 
                  "\n")
        }
        else {
            SV1 <- rep(1/nvarsV, nvarsV)
        }
        all.methods <- FALSE
        if (sum(optimxmethod %in% c("All")) == 1) {
            all.methods <- TRUE
        }
        rgV.optim.1 <- optimx(par = SV1, fn = fn.V, gr = NULL, 
            hess = NULL, method = optimxmethod, itnmax = NULL, 
            hessian = FALSE, control = list(kkt = FALSE, starttests = FALSE, 
                dowarn = FALSE, all.methods = all.methods), X0.scaled = X0.scaled, 
            X1.scaled = X1.scaled, Z0 = Z0, Z1 = Z1, quadopt = quadopt, 
            margin.ipop = Margin.ipop, sigf.ipop = Sigf.ipop, 
            bound.ipop = Bound.ipop)
        if (verbose == TRUE) {
            print(rgV.optim.1)
        }
        rgV.optim.1 <- collect.optimx(rgV.optim.1, "min")
        Xall <- cbind(X1.scaled, X0.scaled)
        Xall <- cbind(rep(1, ncol(Xall)), t(Xall))
        Zall <- cbind(Z1, Z0)
        Beta <- try(solve(t(Xall) %*% Xall) %*% t(Xall) %*% t(Zall), 
            silent = TRUE)
        if (inherits(Beta, "try-error")) {
            rgV.optim <- rgV.optim.1
        }
        else {
            Beta <- Beta[-1, ]
            V <- Beta %*% t(Beta)
            SV2 <- diag(V)
            SV2 <- SV2/sum(SV2)
            rgV.optim.2 <- optimx(par = SV2, fn = fn.V, gr = NULL, 
                hess = NULL, method = optimxmethod, itnmax = NULL, 
                hessian = FALSE, control = list(kkt = FALSE, 
                  starttests = FALSE, dowarn = FALSE, all.methods = all.methods), 
                X0.scaled = X0.scaled, X1.scaled = X1.scaled, 
                Z0 = Z0, Z1 = Z1, quadopt = quadopt, margin.ipop = Margin.ipop, 
                sigf.ipop = Sigf.ipop, bound.ipop = Bound.ipop)
            if (verbose == TRUE) {
                print(rgV.optim.2)
            }
            rgV.optim.2 <- collect.optimx(rgV.optim.2, "min")
            if (verbose == TRUE) {
                cat("\n Equal weight loss is:", rgV.optim.1$value, 
                  "\n")
                cat("\n Regression Loss is:", rgV.optim.2$value, 
                  "\n")
            }
            if (rgV.optim.1$value < rgV.optim.2$value) {
                rgV.optim <- rgV.optim.1
            }
            else {
                rgV.optim <- rgV.optim.2
            }
        }
        solution.v <- abs(rgV.optim$par)/sum(abs(rgV.optim$par))
    }
    else {
        if (verbose) 
            cat("\n****************", "\n optimization over w weights: computing synthetic control unit \n", 
                "\n\n")
        if (nrow(X0) == 1) {
            custom.v <- 1
        }
        else {
            if (verbose) 
                cat("\n****************", "\n v weights supplied manually: computing synthetic control unit \n", 
                  "\n\n")
            if (length(custom.v) != nvarsV) {
                stop("custom.v misspecified: length(custom.v) != nrow(X0)")
            }
            if (mode(custom.v) != "numeric") {
                stop("custom.v must be numeric")
            }
        }
        rgV.optim <- NULL
        solution.v <- abs(custom.v)/sum(custom.v)
    }
    V <- diag(x = as.numeric(solution.v), nrow = nvarsV, ncol = nvarsV)
    H <- t(X0.scaled) %*% V %*% (X0.scaled)
    a <- X1.scaled
    c <- -1 * c(t(a) %*% V %*% (X0.scaled))
    A <- t(rep(1, length(c)))
    b <- 1
    l <- rep(0, length(c))
    u <- rep(1, length(c))
    r <- 0
    if (quadopt == "ipop") {
        res <- ipop(c = c, H = H, A = A, b = b, l = l, u = u, 
            r = r, margin = Margin.ipop, maxiter = 1000, sigf = Sigf.ipop, 
            bound = Bound.ipop)
        solution.w <- as.matrix(primal(res))
    }
    else if (quadopt == "LowRankQP") {
        stop("LowRankQP is no longer supported; please use quadopt = \"ipop\" instead")
    }
    else {
        stop(sprintf("Unknown quadopt = \"%s\"; supported value is \"ipop\"", 
            quadopt))
    }
    rownames(solution.w) <- colnames(X0)
    colnames(solution.w) <- "w.weight"
    names(solution.v) <- rownames(X0)
    loss.w <- t(X1.scaled - X0.scaled %*% solution.w) %*% V %*% 
        (X1.scaled - X0.scaled %*% solution.w)
    loss.v <- t(Z1 - Z0 %*% as.matrix(solution.w)) %*% (Z1 - 
        Z0 %*% as.matrix(solution.w))
    loss.v <- loss.v/nrow(Z0)
    if (verbose) {
        cat("\n****************", "\n****************", "\n****************", 
            "\n\nMSPE (LOSS V):", loss.v, "\n\nsolution.v:\n", 
            round(as.numeric(solution.v), 10), "\n\nsolution.w:\n", 
            round(as.numeric(solution.w), 10), "\n\n")
    }
    optimize.out <- list(solution.v = solution.v, solution.w = solution.w, 
        loss.v = loss.v, loss.w = loss.w, custom.v = custom.v, 
        rgV.optim = rgV.optim)
    return(invisible(optimize.out))
}
fn.V <-
function (variables.v = stop("variables.v missing"), X0.scaled = stop("X0.scaled missing"), 
    X1.scaled = stop("X1.scaled missing"), Z0 = stop("Z0 missing"), 
    Z1 = stop("Z1 missing"), margin.ipop = 0.00050000000000000001, 
    sigf.ipop = 5, bound.ipop = 10, quadopt = "ipop") 
{
    if (!(quadopt %in% c("ipop", "LowRankQP"))) {
        stop("option quadopt must be \"ipop\" (\"LowRankQP\" is no longer supported)")
    }
    V <- diag(x = as.numeric(abs(variables.v)/sum(abs(variables.v))), 
        nrow = length(variables.v), ncol = length(variables.v))
    H <- t(X0.scaled) %*% V %*% (X0.scaled)
    a <- X1.scaled
    c <- -1 * c(t(a) %*% V %*% (X0.scaled))
    A <- t(rep(1, length(c)))
    b <- 1
    l <- rep(0, length(c))
    u <- rep(1, length(c))
    r <- 0
    if (quadopt == "ipop") {
        res <- ipop(c = c, H = H, A = A, b = b, l = l, u = u, 
            r = r, bound = bound.ipop, margin = margin.ipop, 
            maxiter = 1000, sigf = sigf.ipop)
        solution.w <- as.matrix(primal(res))
    }
    else if (quadopt == "LowRankQP") {
        stop("LowRankQP is no longer supported; please use quadopt = \"ipop\" instead")
    }
    loss.w <- as.numeric(t(X1.scaled - X0.scaled %*% solution.w) %*% 
        (V) %*% (X1.scaled - X0.scaled %*% solution.w))
    loss.v <- as.numeric(t(Z1 - Z0 %*% solution.w) %*% (Z1 - 
        Z0 %*% solution.w))
    loss.v <- loss.v/nrow(Z0)
    return(invisible(loss.v))
}
ipop <-
function (c, H, A, b, l, u, r, sigf = 7, maxiter = 40, margin = 0.050000000000000003, 
    bound = 10, verb = 0) 
{
    if (!is.matrix(H)) 
        stop("H must be a matrix")
    if (!is.matrix(A) && !is.vector(A)) 
        stop("A must be a matrix or a vector")
    if (!is.matrix(c) && !is.vector(c)) 
        stop("c must be a matrix or a vector")
    if (!is.matrix(l) && !is.vector(l)) 
        stop("l must be a matrix or a vector")
    if (!is.matrix(u) && !is.vector(u)) 
        stop("u must be a matrix or a vector")
    n <- dim(H)[1]
    if (n == dim(H)[2]) 
        smw <- 0
    if (n > dim(H)[2]) 
        smw <- 1
    if (n < dim(H)[2]) {
        smw <- 1
        n <- dim(H)[2]
        H <- t(H)
    }
    if (is.vector(A)) 
        A <- matrix(A, 1)
    m <- dim(A)[1]
    primal <- rep(0, n)
    if (missing(b)) 
        bvec <- rep(0, m)
    if (n != length(c)) 
        stop("H and c are incompatible!")
    if (n != ncol(A)) 
        stop("A and c are incompatible!")
    if (m != length(b)) 
        stop("A and b are incompatible!")
    if (n != length(u)) 
        stop("u is incopatible with H")
    if (n != length(l)) 
        stop("l is incopatible with H")
    c <- matrix(c)
    l <- matrix(l)
    u <- matrix(u)
    m <- nrow(A)
    n <- ncol(A)
    H.diag <- diag(H)
    if (smw == 0) 
        H.x <- H
    else if (smw == 1) 
        H.x <- t(H)
    b.plus.1 <- max(svd(b)$d) + 1
    c.plus.1 <- max(svd(c)$d) + 1
    one.x <- -matrix(1, n, 1)
    one.y <- -matrix(1, m, 1)
    if (smw == 0) 
        diag(H.x) <- H.diag + 1
    else smwn <- dim(H)[2]
    H.y <- diag(1, m)
    c.x <- c
    c.y <- b
    if (smw == 0) {
        AP <- matrix(0, m + n, m + n)
        xp <- 1:(m + n) <= n
        AP[xp, xp] <- -H.x
        AP[xp == FALSE, xp] <- A
        AP[xp, xp == FALSE] <- t(A)
        AP[xp == FALSE, xp == FALSE] <- H.y
        s.tmp <- solve(AP, c(c.x, c.y))
        x <- s.tmp[1:n]
        y <- s.tmp[-(1:n)]
    }
    else {
        V <- diag(smwn)
        smwinner <- chol(V + crossprod(H))
        smwa1 <- t(A)
        smwc1 <- c.x
        smwa2 <- smwa1 - (H %*% solve(smwinner, solve(t(smwinner), 
            crossprod(H, smwa1))))
        smwc2 <- smwc1 - (H %*% solve(smwinner, solve(t(smwinner), 
            crossprod(H, smwc1))))
        y <- solve(A %*% smwa2 + H.y, c.y + A %*% smwc2)
        x <- smwa2 %*% y - smwc2
    }
    g <- pmax(abs(x - l), bound)
    z <- pmax(abs(x), bound)
    t <- pmax(abs(u - x), bound)
    s <- pmax(abs(x), bound)
    v <- pmax(abs(y), bound)
    w <- pmax(abs(y), bound)
    p <- pmax(abs(r - w), bound)
    q <- pmax(abs(y), bound)
    mu <- as.vector(crossprod(z, g) + crossprod(v, w) + crossprod(s, 
        t) + crossprod(p, q))/(2 * (m + n))
    sigfig <- 0
    counter <- 0
    alfa <- 1
    if (verb > 0) 
        cat("Iter    PrimalInf  DualInf  SigFigs  Rescale  PrimalObj  DualObj", 
            "\n")
    while (counter < maxiter) {
        counter <- counter + 1
        if (smw == 0) 
            H.dot.x <- H %*% x
        else if (smw == 1) 
            H.dot.x <- H %*% crossprod(H, x)
        rho <- b - A %*% x + w
        nu <- l - x + g
        tau <- u - x - t
        alpha <- r - w - p
        sigma <- c - crossprod(A, y) - z + s + H.dot.x
        beta <- y + q - v
        gamma.z <- -z
        gamma.w <- -w
        gamma.s <- -s
        gamma.q <- -q
        x.dot.H.dot.x <- crossprod(x, H.dot.x)
        primal.infeasibility <- max(svd(rbind(rho, tau, matrix(alpha), 
            nu))$d)/b.plus.1
        dual.infeasibility <- max(svd(rbind(sigma, t(t(beta))))$d)/c.plus.1
        primal.obj <- crossprod(c, x) + 0.5 * x.dot.H.dot.x
        dual.obj <- crossprod(b, y) - 0.5 * x.dot.H.dot.x + crossprod(l, 
            z) - crossprod(u, s) - crossprod(r, q)
        old.sigfig <- sigfig
        sigfig <- max(-log10(abs(primal.obj - dual.obj)/(abs(primal.obj) + 
            1)), 0)
        if (sigfig >= sigf) 
            break
        if (verb > 0) 
            cat(counter, "\t", signif(primal.infeasibility, 6), 
                signif(dual.infeasibility, 6), sigfig, alfa, 
                primal.obj, dual.obj, "\n")
        hat.beta <- beta - v * gamma.w/w
        hat.alpha <- alpha - p * gamma.q/q
        hat.nu <- nu + g * gamma.z/z
        hat.tau <- tau - t * gamma.s/s
        d <- z/g + s/t
        e <- 1/(v/w + q/p)
        if (smw == 0) 
            diag(H.x) <- H.diag + d
        diag(H.y) <- e
        c.x <- sigma - z * hat.nu/g - s * hat.tau/t
        c.y <- rho - e * (hat.beta - q * hat.alpha/p)
        if (smw == 0) {
            AP[xp, xp] <- -H.x
            AP[xp == FALSE, xp == FALSE] <- H.y
            s1.tmp <- solve(AP, c(c.x, c.y))
            delta.x <- s1.tmp[1:n]
            delta.y <- s1.tmp[-(1:n)]
        }
        else {
            V <- diag(smwn)
            smwinner <- chol(V + chunkmult(t(H), 2000, d))
            smwa1 <- t(A)
            smwa1 <- smwa1/d
            smwc1 <- c.x/d
            smwa2 <- t(A) - (H %*% solve(smwinner, solve(t(smwinner), 
                crossprod(H, smwa1))))
            smwa2 <- smwa2/d
            smwc2 <- (c.x - (H %*% solve(smwinner, solve(t(smwinner), 
                crossprod(H, smwc1)))))/d
            delta.y <- solve(A %*% smwa2 + H.y, c.y + A %*% smwc2)
            delta.x <- smwa2 %*% delta.y - smwc2
        }
        delta.w <- -e * (hat.beta - q * hat.alpha/p + delta.y)
        delta.s <- s * (delta.x - hat.tau)/t
        delta.z <- z * (hat.nu - delta.x)/g
        delta.q <- q * (delta.w - hat.alpha)/p
        delta.v <- v * (gamma.w - delta.w)/w
        delta.p <- p * (gamma.q - delta.q)/q
        delta.g <- g * (gamma.z - delta.z)/z
        delta.t <- t * (gamma.s - delta.s)/s
        alfa <- -(1 - margin)/min(c(delta.g/g, delta.w/w, delta.t/t, 
            delta.p/p, delta.z/z, delta.v/v, delta.s/s, delta.q/q, 
            -1))
        newmu <- (crossprod(z, g) + crossprod(v, w) + crossprod(s, 
            t) + crossprod(p, q))/(2 * (m + n))
        newmu <- mu * ((alfa - 1)/(alfa + 10))^2
        gamma.z <- mu/g - z - delta.z * delta.g/g
        gamma.w <- mu/v - w - delta.w * delta.v/v
        gamma.s <- mu/t - s - delta.s * delta.t/t
        gamma.q <- mu/p - q - delta.q * delta.p/p
        hat.beta <- beta - v * gamma.w/w
        hat.alpha <- alpha - p * gamma.q/q
        hat.nu <- nu + g * gamma.z/z
        hat.tau <- tau - t * gamma.s/s
        c.x <- sigma - z * hat.nu/g - s * hat.tau/t
        c.y <- rho - e * (hat.beta - q * hat.alpha/p)
        if (smw == 0) {
            AP[xp, xp] <- -H.x
            AP[xp == FALSE, xp == FALSE] <- H.y
            s1.tmp <- solve(AP, c(c.x, c.y))
            delta.x <- s1.tmp[1:n]
            delta.y <- s1.tmp[-(1:n)]
        }
        else if (smw == 1) {
            smwc1 <- c.x/d
            smwc2 <- (c.x - (H %*% solve(smwinner, solve(t(smwinner), 
                crossprod(H, smwc1)))))/d
            delta.y <- solve(A %*% smwa2 + H.y, c.y + A %*% smwc2)
            delta.x <- smwa2 %*% delta.y - smwc2
        }
        delta.w <- -e * (hat.beta - q * hat.alpha/p + delta.y)
        delta.s <- s * (delta.x - hat.tau)/t
        delta.z <- z * (hat.nu - delta.x)/g
        delta.q <- q * (delta.w - hat.alpha)/p
        delta.v <- v * (gamma.w - delta.w)/w
        delta.p <- p * (gamma.q - delta.q)/q
        delta.g <- g * (gamma.z - delta.z)/z
        delta.t <- t * (gamma.s - delta.s)/s
        alfa <- -(1 - margin)/min(c(delta.g/g, delta.w/w, delta.t/t, 
            delta.p/p, delta.z/z, delta.v/v, delta.s/s, delta.q/q, 
            -1))
        x <- x + delta.x * alfa
        g <- g + delta.g * alfa
        w <- w + delta.w * alfa
        t <- t + delta.t * alfa
        p <- p + delta.p * alfa
        y <- y + delta.y * alfa
        z <- z + delta.z * alfa
        v <- v + delta.v * alfa
        s <- s + delta.s * alfa
        q <- q + delta.q * alfa
        mu <- newmu
    }
    if (verb > 0) 
        cat(counter, primal.infeasibility, dual.infeasibility, 
            sigfig, alfa, primal.obj, dual.obj)
    ret <- new("ipop")
    primal(ret) <- x
    dual(ret) <- drop(y)
    if ((sigfig > sigf) & (counter < maxiter)) 
        how(ret) <- "converged"
    else {
        if ((primal.infeasibility > 1000000) & (dual.infeasibility > 
            1000000)) 
            how(ret) <- "primal and dual infeasible"
        if (primal.infeasibility > 1000000) 
            how(ret) <- "primal infeasible"
        if (dual.infeasibility > 1000000) 
            how(ret) <- "dual infeasible"
        else how(ret) <- "slow convergence, change bound?"
    }
    ret
}
chunkmult <-
function (Z, csize, colscale) 
{
    n <- dim(Z)[1]
    m <- dim(Z)[2]
    d <- sqrt(colscale)
    nchunks <- ceiling(m/csize)
    res <- matrix(0, n, n)
    for (i in 1:nchunks) {
        lowerb <- (i - 1) * csize + 1
        upperb <- min(i * csize, m)
        buffer <- t(Z[, lowerb:upperb, drop = FALSE])
        bufferd <- d[lowerb:upperb]
        buffer <- buffer/bufferd
        res <- res + crossprod(buffer)
    }
    return(res)
}
