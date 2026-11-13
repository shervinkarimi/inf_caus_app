# =====================================================================
# scm_core.R : contrôle synthétique (Abadie, Diamond, Hainmueller 2010)
# Dépendances : base R + quadprog (disponible aussi pour webR, à vérifier
# à l'étape GitHub). Ce fichier est le seul à remplacer si tu veux
# brancher ton propre code : garder la signature de scm_fit().
# =====================================================================

# --- Poids W pour un V donné -----------------------------------------
# X1 : vecteur K (prédicteurs du traité, standardisés)
# X0 : matrice K x J (prédicteurs des donneurs, standardisés)
# v  : vecteur K de poids >= 0 sur les prédicteurs
scm_weights <- function(X1, X0, v) {
  J  <- ncol(X0)
  sv <- sqrt(v)
  Xv <- X0 * sv                       # chaque ligne k multipliée par sqrt(v_k)
  xv <- X1 * sv
  D  <- crossprod(Xv)
  D  <- D + diag(1e-8 * max(1, mean(diag(D))), J)   # régularisation numérique
  d  <- drop(crossprod(Xv, xv))
  A  <- cbind(rep(1, J), diag(J))     # somme = 1, puis w >= 0
  b  <- c(1, rep(0, J))
  sol <- quadprog::solve.QP(D, d, A, b, meq = 1)
  w <- pmax(sol$solution, 0)
  w <- w / sum(w)
  names(w) <- colnames(X0)
  w
}

# --- Construction des prédicteurs ------------------------------------
# Y      : matrice T x N (lignes = années, colonnes = unités)
# pre    : indices (lignes) des années pré-traitement
# covs   : liste nommée de matrices T x N (covariables), facultatif
# lags   : "all" = chaque année pré est un prédicteur ; "mean" = moyenne pré
build_predictors <- function(Y, pre, covs = NULL, lags = c("all", "mean")) {
  lags <- match.arg(lags)
  P <- if (lags == "all") {
    m <- Y[pre, , drop = FALSE]
    rownames(m) <- paste0("Y_", rownames(Y)[pre]); m
  } else {
    m <- matrix(colMeans(Y[pre, , drop = FALSE]), nrow = 1,
                dimnames = list("Y_moy_pre", colnames(Y))); m
  }
  if (length(covs)) {
    cm <- t(sapply(covs, function(C) colMeans(C[pre, , drop = FALSE], na.rm = TRUE)))
    if (is.null(dim(cm))) cm <- matrix(cm, nrow = 1)
    rownames(cm) <- paste0(names(covs), "_moy_pre")
    colnames(cm) <- colnames(Y)
    P <- rbind(P, cm)
  }
  P
}

# --- Estimation principale ------------------------------------------
# treated : nom de la colonne traitée ; donors : noms des colonnes donneuses
# method : "synth" = package Synth (ta spécification : synth() avec ses
#                    options par défaut, Sigf.ipop = 5, V optimisée par Synth)
#          "optim" = moteur rapide (quadprog), V optimisée sur le MSPE pré
#          "equal" = moteur rapide, V égale sur prédicteurs standardisés
scm_fit <- function(Y, treated, donors, pre, covs = NULL,
                    lags = "mean", method = c("synth", "optim", "equal")) {
  method <- match.arg(method)
  P  <- build_predictors(Y, pre, covs, lags)
  P  <- P[, c(treated, donors), drop = FALSE]
  Y1 <- Y[, treated];  Y0 <- Y[, donors, drop = FALSE]

  if (method == "synth") {
    # Même objet que dataprep() : X = prédicteurs, Z = résultat sur les années pré
    Z  <- Y[pre, , drop = FALSE]
    keepP <- apply(P[, donors, drop = FALSE], 1, sd) > 0   # Synth refuse un prédicteur constant
    invisible(utils::capture.output(
      s <- synth_vendored(X1 = P[keepP, treated, drop = FALSE], X0 = P[keepP, donors, drop = FALSE],
                        Z1 = Z[, treated, drop = FALSE],     Z0 = Z[, donors, drop = FALSE],
                        Sigf.ipop = 5, verbose = FALSE)))
    w <- setNames(as.numeric(s$solution.w), donors)
    v <- setNames(rep(NA_real_, nrow(P)), rownames(P)); v[keepP] <- as.numeric(s$solution.v)
  } else {
    sdv <- apply(P, 1, sd); sdv[!is.finite(sdv) | sdv == 0] <- 1
    Ps <- P / sdv
    X1 <- Ps[, treated]; X0 <- Ps[, donors, drop = FALSE]
    K  <- nrow(Ps)
    mspe_pre <- function(w) mean((Y1[pre] - Y0[pre, , drop = FALSE] %*% w)^2)
    v <- rep(1 / K, K)
    if (method == "optim" && K > 1) {
      obj <- function(th) { vv <- exp(th) / sum(exp(th)); mspe_pre(scm_weights(X1, X0, vv)) }
      op  <- optim(rep(0, K), obj, method = "Nelder-Mead",
                   control = list(maxit = 100 * K, reltol = 1e-8))
      v   <- exp(op$par) / sum(exp(op$par))
      if (op$value > mspe_pre(scm_weights(X1, X0, rep(1 / K, K)))) v <- rep(1 / K, K)
    }
    names(v) <- rownames(Ps)
    w <- scm_weights(X1, X0, v)
  }

  synth <- drop(Y0 %*% w)
  gap   <- Y1 - synth
  post  <- setdiff(seq_len(nrow(Y)), pre)
  mspe_pre  <- mean(gap[pre]^2)                        # = loss.v de Synth
  mspe_post <- if (length(post)) mean(gap[post]^2) else NA
  list(
    years = as.integer(rownames(Y)), pre = pre, post = post, method = method,
    treated = treated, y = Y1, synth = synth, gap = gap, w = w, v = v,
    balance = data.frame(predicteur = rownames(P), traite = P[, treated],
                         synthetique = drop(P[, donors, drop = FALSE] %*% w),
                         moyenne_donneurs = rowMeans(P[, donors, drop = FALSE]),
                         poids_V = v, row.names = NULL),
    mspe_pre = mspe_pre, mspe_post = mspe_post,
    rmspe_pre = sqrt(mspe_pre), rmspe_post = sqrt(mspe_post),
    ratio = mspe_post / mspe_pre,                      # comme mspe.test (SCtools)
    att_post = if (length(post)) mean(gap[post]) else NA
  )
}

# --- Placebos dans l'espace (comme SCtools::generate.placebos) --------
# Chaque donneur est traité fictivement ; son pool = les autres donneurs,
# le vrai traité exclu.
scm_placebos <- function(Y, treated, donors, pre, covs = NULL, lags = "mean",
                         method = "synth", progress = NULL) {
  res <- vector("list", length(donors)); names(res) <- donors
  for (i in seq_along(donors)) {
    j <- donors[i]
    f <- tryCatch(scm_fit(Y, j, setdiff(donors, j), pre, covs, lags, method),
                  error = function(e) NULL)
    if (!is.null(f)) res[[j]] <- list(gap = f$gap, mspe_pre = f$mspe_pre, ratio = f$ratio)
    if (!is.null(progress)) progress(i / length(donors))
  }
  res[!vapply(res, is.null, logical(1))]
}

# --- p-value (comme SCtools::mspe.test) -------------------------------
# ratio = MSPE post / MSPE pré ; p = part des unités (traité inclus) dont le
# ratio est >= celui du traité. discard : retire les placebos dont le MSPE
# pré est >= mspe_limit fois celui du traité.
scm_pvalue <- function(fit, placebos, mspe_limit = Inf) {
  r  <- vapply(placebos, `[[`, numeric(1), "ratio")
  mp <- vapply(placebos, `[[`, numeric(1), "mspe_pre")
  keep <- if (is.finite(mspe_limit)) mp / fit$mspe_pre < mspe_limit else rep(TRUE, length(r))
  ratios <- c(fit$ratio, r[keep]); ratios <- ratios[is.finite(ratios)]
  list(p = mean(ratios >= fit$ratio), rang = sum(ratios >= fit$ratio), n = length(ratios),
       exclus = sum(!keep))
}

# --- Diagnostics automatiques ----------------------------------------
scm_diagnostics <- function(fit, Y, donors) {
  msg <- character(0)
  pre <- fit$pre
  lvl <- mean(abs(fit$y[pre]))
  if (length(pre) < 4)
    msg <- c(msg, sprintf("Seulement %d années pré-traitement : ajustement peu informatif.", length(pre)))
  if (lvl > 0 && fit$rmspe_pre / lvl > 0.10)
    msg <- c(msg, sprintf("Ajustement pré-traitement médiocre : RMSPE pré = %.1f %% du niveau moyen du traité.",
                          100 * fit$rmspe_pre / lvl))
  Y0 <- Y[pre, donors, drop = FALSE]
  out <- mean(fit$y[pre] > apply(Y0, 1, max) | fit$y[pre] < apply(Y0, 1, min))
  if (out > 0.5)
    msg <- c(msg, "Le traité est hors de l'enveloppe des donneurs sur la plupart des années pré : aucune combinaison convexe ne peut le reproduire (envisager l'échelle en indice).")
  if (max(fit$w) > 0.8)
    msg <- c(msg, sprintf("Un donneur concentre %.0f %% du poids (%s) : le contrefactuel repose sur une seule unité.",
                          100 * max(fit$w), names(which.max(fit$w))))
  n_act <- sum(fit$w > 0.001)
  if (lvl > 0 && fit$rmspe_pre / lvl < 0.002 && n_act >= length(pre))
    msg <- c(msg, sprintf("Ajustement pré quasi parfait avec %d donneurs actifs pour %d années pré : risque de surajustement (le synthétique interpole aussi le bruit). Vérifier le leave-one-out, essayer « Moyenne pré » ou réduire le pool.",
                          n_act, length(pre)))
  if (length(fit$post) == 0)
    msg <- c(msg, "Aucune année post-traitement disponible.")
  msg
}
