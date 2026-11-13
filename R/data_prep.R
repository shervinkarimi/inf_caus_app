# =====================================================================
# data_prep.R : de la table SAE longue (AN x FI) aux matrices du SCM
# Colonnes attendues : AN, FI, dept + variables numériques.
# =====================================================================

ID_COLS <- c("AN", "FI", "FI_EJ", "dept", "Manche")

DEPT_NOMS <- c("01"="Ain","02"="Aisne","03"="Allier","04"="Alpes-de-Haute-Provence","05"="Hautes-Alpes",
  "06"="Alpes-Maritimes","07"="Ardèche","08"="Ardennes","09"="Ariège","10"="Aube","11"="Aude","12"="Aveyron",
  "13"="Bouches-du-Rhône","14"="Calvados","15"="Cantal","16"="Charente","17"="Charente-Maritime","18"="Cher",
  "19"="Corrèze","2A"="Corse-du-Sud","2B"="Haute-Corse","21"="Côte-d'Or","22"="Côtes-d'Armor","23"="Creuse",
  "24"="Dordogne","25"="Doubs","26"="Drôme","27"="Eure","28"="Eure-et-Loir","29"="Finistère","30"="Gard",
  "31"="Haute-Garonne","32"="Gers","33"="Gironde","34"="Hérault","35"="Ille-et-Vilaine","36"="Indre",
  "37"="Indre-et-Loire","38"="Isère","39"="Jura","40"="Landes","41"="Loir-et-Cher","42"="Loire",
  "43"="Haute-Loire","44"="Loire-Atlantique","45"="Loiret","46"="Lot","47"="Lot-et-Garonne","48"="Lozère",
  "49"="Maine-et-Loire","50"="Manche","51"="Marne","52"="Haute-Marne","53"="Mayenne","54"="Meurthe-et-Moselle",
  "55"="Meuse","56"="Morbihan","57"="Moselle","58"="Nièvre","59"="Nord","60"="Oise","61"="Orne",
  "62"="Pas-de-Calais","63"="Puy-de-Dôme","64"="Pyrénées-Atlantiques","65"="Hautes-Pyrénées",
  "66"="Pyrénées-Orientales","67"="Bas-Rhin","68"="Haut-Rhin","69"="Rhône","70"="Haute-Saône",
  "71"="Saône-et-Loire","72"="Sarthe","73"="Savoie","74"="Haute-Savoie","75"="Paris","76"="Seine-Maritime",
  "77"="Seine-et-Marne","78"="Yvelines","79"="Deux-Sèvres","80"="Somme","81"="Tarn","82"="Tarn-et-Garonne",
  "83"="Var","84"="Vaucluse","85"="Vendée","86"="Vienne","87"="Haute-Vienne","88"="Vosges","89"="Yonne",
  "90"="Territoire de Belfort","91"="Essonne","92"="Hauts-de-Seine","93"="Seine-Saint-Denis",
  "94"="Val-de-Marne","95"="Val-d'Oise","97"="Outre-mer","98"="Outre-mer")

dept_label <- function(code) {
  nom <- DEPT_NOMS[code]
  ifelse(is.na(nom), paste("Dép.", code), paste0(nom, " (", code, ")"))
}

load_sae <- function(path) {
  ext <- tolower(tools::file_ext(path))
  df <- if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(path, guess_max = 1e5))
  } else {
    utils::read.csv(path, colClasses = c(FI = "character", FI_EJ = "character",
                                         dept = "character"), check.names = FALSE)
  }
  stopifnot(all(c("AN", "FI", "dept") %in% names(df)))
  df$AN   <- as.integer(df$AN)
  df$FI   <- sprintf("%09s", as.character(df$FI))
  df$dept <- as.character(df$dept)
  df$dept[nchar(df$dept) == 1] <- paste0("0", df$dept[nchar(df$dept) == 1])
  if (anyDuplicated(df[, c("FI", "AN")])) stop("Doublons FI x AN dans les données.")
  df
}

numeric_vars <- function(df) {
  setdiff(names(df)[vapply(df, is.numeric, logical(1))], ID_COLS)
}

# long -> large (T x N)
to_wide <- function(df, id, var, years) {
  ids <- sort(unique(df[[id]]))
  M <- matrix(NA_real_, length(years), length(ids), dimnames = list(years, ids))
  r <- match(df$AN, years); c <- match(df[[id]], ids)
  ok <- !is.na(r)
  M[cbind(r[ok], c[ok])] <- df[[var]][ok]
  M
}

prepare_panel <- function(df, level = c("etab", "dept"), outcome, covariates = character(0),
                          treated, t0, scale = c("level", "index"),
                          exclude_same_dept = TRUE, exclusions = character(0),
                          k_pool = 150, min_coverage = 0.5) {
  level <- match.arg(level); scale <- match.arg(scale)
  vars  <- c(outcome, covariates)
  notes <- character(0)

  # 1. Années où la variable résultat est massivement absente (ex. ETP en 2020)
  cov_year <- tapply(!is.na(df[[outcome]]), df$AN, mean)
  years <- as.integer(names(cov_year)[cov_year >= min_coverage])
  dropped <- setdiff(as.integer(names(cov_year)), years)
  if (length(dropped))
    notes <- c(notes, sprintf("Années exclues (%s renseigné pour moins de %.0f %% des lignes) : %s.",
                              outcome, 100 * min_coverage, paste(dropped, collapse = ", ")))
  d <- df[df$AN %in% years, ]

  # 2. Construction des matrices selon le niveau
  if (level == "etab") {
    mats <- lapply(vars, function(v) to_wide(d, "FI", v, years)); names(mats) <- vars
    dept_of <- tapply(d$dept, d$FI, function(x) x[length(x)])
  } else {
    # Panel cylindré : établissements complets sur toutes les années retenues
    W <- lapply(vars, function(v) to_wide(d, "FI", v, years))
    complete <- Reduce(`&`, lapply(W, function(M) colSums(is.na(M)) == 0))
    fi_keep <- names(complete)[complete]
    notes <- c(notes, sprintf("Agrégation départementale sur %d établissements présents et renseignés toutes les années retenues (panel cylindré), sur %d au total.",
                              length(fi_keep), length(complete)))
    dd <- d[d$FI %in% fi_keep, ]
    agg <- stats::aggregate(dd[, vars, drop = FALSE], by = list(dept = dd$dept, AN = dd$AN), FUN = sum)
    mats <- lapply(vars, function(v) to_wide(agg, "dept", v, years)); names(mats) <- vars
    n_etab <- tapply(dd$FI, dd$dept, function(x) length(unique(x)))
    if (treated %in% names(n_etab))
      notes <- c(notes, sprintf("Département traité %s : %d établissements agrégés.", treated, n_etab[[treated]]))
  }

  Y <- mats[[outcome]]
  if (!treated %in% colnames(Y)) stop("Unité traitée absente des données retenues.")

  # 3. Années où le traité n'est pas renseigné
  miss_t <- is.na(Y[, treated])
  if (any(miss_t)) {
    notes <- c(notes, sprintf("Années sans donnée pour le traité, exclues : %s.",
                              paste(rownames(Y)[miss_t], collapse = ", ")))
    mats <- lapply(mats, function(M) M[!miss_t, , drop = FALSE]); Y <- mats[[outcome]]
  }
  yrs <- as.integer(rownames(Y))
  pre <- which(yrs < t0)
  if (length(pre) < 2) stop("Moins de 2 années pré-traitement disponibles.")
  if (!any(yrs >= t0)) stop("Aucune année post-traitement disponible.")

  # 4. Donneurs complets sur toutes les années (résultat et covariables)
  complete <- Reduce(`&`, lapply(mats, function(M) colSums(is.na(M)) == 0))
  if (!complete[[treated]]) stop("Covariables manquantes pour le traité : retirer la covariable.")
  donors <- setdiff(names(complete)[complete], treated)
  n0 <- length(donors)

  # 5. Exclusions
  if (level == "etab" && exclude_same_dept) {
    donors <- donors[dept_of[donors] != dept_of[[treated]]]
    notes <- c(notes, sprintf("Établissements du département %s exclus du pool (risque de contamination).", dept_of[[treated]]))
  }
  donors <- setdiff(donors, exclusions)

  # 6. Échelle
  if (scale == "index") {
    m <- colMeans(Y[pre, , drop = FALSE])
    okm <- names(m)[which(is.finite(m) & m > 0)]
    if (!treated %in% okm) stop("Moyenne pré nulle pour le traité : échelle en indice impossible.")
    Y <- sweep(Y[, okm, drop = FALSE], 2, m[okm], "/") * 100
    donors <- intersect(donors, okm)
  }

  # 7. Pool restreint aux k voisins les plus proches sur la trajectoire pré
  if (length(donors) > k_pool) {
    dist <- sqrt(colSums((Y[pre, donors, drop = FALSE] - Y[pre, treated])^2))
    donors <- names(sort(dist))[seq_len(k_pool)]
    notes <- c(notes, sprintf("Pool restreint aux %d donneurs les plus proches sur la trajectoire pré (sur %d éligibles).", k_pool, n0))
  }
  if (length(donors) < 2) stop("Moins de 2 donneurs disponibles.")

  keep <- c(treated, donors)
  covs <- if (length(covariates)) lapply(mats[covariates], function(M) M[, keep, drop = FALSE]) else NULL
  list(Y = Y[, keep, drop = FALSE], covs = covs, treated = treated, donors = donors,
       pre = pre, years = yrs, t0 = t0, notes = notes)
}
