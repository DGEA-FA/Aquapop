death <- function(data, espece) {
  death <- data %>%  dplyr::filter(sp == espece)  %>% droplevels()
  death <- subset(death,!is.na(age))
  death
}