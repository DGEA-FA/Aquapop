create_initcroissance <- function(data, sp_pen) {
  # Selectionner seulement les données nécessaires
  x <- data %>% filter(sp == sp_pen) # Prendre seulement l'espèce PEN
  x <- subset(x, !is.na(ltm)) # Supprimer tous les enregistrements où les mesures LTM sont manquantes
  x <- subset(x, !is.na(age)) # Supprimer tous les enregistrements où les mesures AGE sont manquantes
 
  x <- x %>% select(c(ltm, age, no_specimen))

    # Rename the row names sequentially from 1 to the total number of rows
  rownames(x) <- seq(nrow(x))
  
  
   return(x)
}

