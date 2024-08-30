# Load necessary libraries
library(dplyr)

# Simulate the 'sp_pen' variable
sp_pen <- "SANA"

# Simulate a subset of the 'specimen' dataframe
specimen <- data.frame(
  no_lac = factor(rep("01480", 10)),
  nom_lac = factor(rep("Memphrémagog, Lac", 10)),
  typ_pech = factor(rep("PENT", 10)),
  annee = rep(2020, 10),
  no_station = factor(sample(c("04", "08", "16", "18", "19", "20", "24", "25", "26", "27"), 10, replace = TRUE)),
  no_specimen = factor(1:10),
  sp = factor(rep("SANA", 10)),
  ltm = runif(10, 250, 700),
  age = sample(2:15, 10, replace = TRUE),
  masse = runif(10, 1000, 3000),
  sexe = factor(sample(c("F", "M", "IND"), 10, replace = TRUE)),
  maturite = factor(sample(c("IND", "N", "O"), 10, replace = TRUE)),
  date_pose = as.Date(rep("2020-08-17", 10)),
  date_leve = as.Date(rep("2020-08-18", 10))
)

# Inspect the structure of the variables
str(sp_pen)
str(specimen)

# Perform operations (as needed in your context)
result <- specimen %>%
  filter(sp == sp_pen) %>%  # Filter by species
  filter(maturite != "IND") %>%  # Remove rows where maturite is "IND"
  droplevels() %>%  # Drop unused factor levels
  filter(sexe != "IND") %>%  # Remove rows where sexe is "IND"
  droplevels()  # Drop unused factor levels again

# Order the maturite factor levels
result$maturite <- factor(result$maturite, levels = c("N", "O"), ordered = TRUE)

# Display the result (for the REPREX)
result
