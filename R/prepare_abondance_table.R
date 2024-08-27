prepare_abondance_table <- function(abundance_table, CPUE_tous, CPUEic_tous, CPUE_Fmature, CPUEic_Fmature) {
  
  # Mise à jour des colonnes CPUE et IC95
  abundance_table <- abundance_table %>%
    mutate(
      cpue = case_when(
        group == "Tous" ~ CPUE_tous,
        group == "Repro. actifs femelles" ~ CPUE_Fmature,
        TRUE ~ NA_character_
      ),
      ic95 = case_when(
        group == "Tous" ~ CPUEic_tous,
        group == "Repro. actifs femelles" ~ CPUEic_Fmature,
        TRUE ~ NA_character_
      )
    )
  
  # Formater les colonnes
  abundance_table <- abundance_table %>%
    mutate(
      proportion = format(round(as.numeric(proportion), digits = 0), nsmall = 0),
      cpue = ifelse(is.na(cpue), "-", cpue),
      ic95 = ifelse(is.na(ic95), "-", ic95),
      mf_ratio = ifelse(is.na(mf_ratio), "-", mf_ratio)
    )
  
  # Renommer les colonnes pour l'affichage final
  abundance_table <- abundance_table %>%
    rename(
      `IC 95%` = ic95,
      `Ratio ♂:♀` = mf_ratio
    ) %>% as.data.frame()
  
  return(abundance_table)
}