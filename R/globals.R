Sys.setlocale("LC_TIME", "French")  # Pour définir le format de la date en français
options(shiny.maxRequestSize = 10 * 1024^2)  # Limite de la taille des fichiers uploadés

utils::globalVariables(c(
  "ltm", "masse", "sexe", "wr", "moyenne", "classe_brute",
  "classe", "intervalle", "fit", "lwr", "upr", "groupe", "ic95",
  "age", "methode", "t0_ic", "k_ic", "categorie", "sp", "number",
  "no_station", "gcat", "aicc", "modele_id", "convergence", "commentaire",
  "biomasse", "mf_ratio", "delta_aicc", "fill", "maturite", "lim_inf", "lim_sup",
  "h_pose", "date_leve", "h_leve", "no_lac", "typ_pech", "nom_lac",
  "comments", "annee", "nb_capture", "nb_pese", "marquage", "comments_specimen",
  "lat_dd.dec", "long_dd.dec", "prof_deb", "prof_fin", "min_pose", "heure_pose",
  "min_leve", "heure_leve", "date_pose", "leve", "pose", "count","aiccwt", "delta_aic", 
  
  "poisson", "hdi", "jags", "pred", "leve", "pose", "count",
  "Modnames", "Delta_AICc", "AICcWt", "aicc_sort", "aicc_num", "Q", "ltm_nb","masse_nb","age_nb",#"ltm_nb","ltm_nb",
  
  
  "st_valide", "superficie_ha", "t0", "st_hasard", "l_inf_ic", "l_inf", "ltm_interval", "ajustement_hnp", "no_specimen",
  "k", "cpue_moyenne", "bpue", "percent","ic_95", "freq", "abondance", "group", "a", "z", "se", "biomasse_g", "proportion", "cpue", "quantile"    
))

myspinner <- 6
