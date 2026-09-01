test_hnp <- function(simuler,
                     ...,
                     n_init = 2,
                     n_suppl = 3,
                     seuil_inf = 10,
                     seuil_sup = 15) {
  
  set.seed(2023)
  
  # Première série de simulations
  res_hnp <- tryCatch(
    {
      simuler(..., n_iter = n_init)
    },
    error = function(e) NULL
  )
  
  if (is.null(res_hnp)) {
    return(list(
      ajustement_hnp = NA_real_,
      nb_iterations_hnp = NA_real_,
      graph_hnp = NULL
    ))
  }
  
  
  hnp_valeurs <- res_hnp$pct
  graph_hnp <- list(
      initial = res_hnp$hnp
      ) 
  
  ajustement_hnp <- round(mean(hnp_valeurs), 2)
  nb_iterations_hnp <- n_init
  
  
  # Si le résultat est limite, on ajoute des simulations
  if (!is.na(ajustement_hnp) &&
      ajustement_hnp >= seuil_inf &&
      ajustement_hnp < seuil_sup) {
    
    
    res_hnp_suppl <- tryCatch(
      {
        simuler(..., n_iter = n_suppl)
      },
      error = function(e) NULL
    )
    
    
    if (!is.null(res_hnp_suppl)) {
      
      hnp_valeurs <- c(
        hnp_valeurs,
        res_hnp_suppl$pct
      )
      
      graph_hnp$supplementaire <- res_hnp_suppl$hnp
      
      ajustement_hnp <- round(mean(hnp_valeurs), 2)
      nb_iterations_hnp <- n_init + n_suppl
    }
  }
  
  
  list(
    ajustement_hnp = ajustement_hnp,
    nb_iterations_hnp = nb_iterations_hnp,
    graph_hnp = graph_hnp
  )
}