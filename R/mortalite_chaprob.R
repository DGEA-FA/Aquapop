mortalite_chaprob <- function(pp, agemax_val, data) {
  # Calculer la mortalité en utilisant la méthode Chapman-Robson
  mortalite <- fishmethods::agesurv(
    type = 1,
    age = data$age,
    full = pp,
    last = agemax_val,
    estimate = "z",
    method = "cr"
  )
  
  final_table <- mortalite$results %>%
    rename(
      methode = Method,
      z = Estimate,
      se = SE
    ) %>%
    dplyr::select(-Parameter) %>%
    mutate(
      a = round((1 - exp(-z)) * 100, digits = 1),
      lower_z = z - se,
      high_z = z + se,
      lower_a = round((1 - exp(-lower_z)) * 100, digits = 1),
      high_a = round((1 - exp(-high_z)) * 100, digits = 1),
      ic_95 = glue("[{lower_a}-{high_a}]")
    ) %>%
    dplyr::select(-c(lower_a, high_a, lower_z, high_z))
  
  # Ajouter des labels aux colonnes
  final_table <- final_table %>%
    set_variable_labels(
      methode = "Méthode utilisée",
      z = "Estimation de Z",
      se = "Erreur standard",
      a = "Taux de mortalité (A%)",
      ic_95 = "Intervalle de confiance à 95% pour A%"
    )
  
  return(final_table)
}