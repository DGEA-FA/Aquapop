dispersiontest <- function(data) {
  #Mainguy et Moral (2021) ont appliqué l’idée d’avoir recours à des extensions de la distribution de Poisson pour tenir
  #compte de la sur-dispersion plutôt que d’appliquer un facteur de correction comme le font les estimateurs CRCB (Smith et al. 2012) et le PM adapté de Nelson (2019).
  #si les données sont sur-dispersées, un modèle s’appuyant sur une distribution de Poisson, soit un GLMPoisson, ne s’ajustera pas suffisamment bien aux données observées car l’équidispersion
  #requise ne sera pas respectée et ainsi, la SE calculée sera biaisée à la baisse, ce qui aura des incidences sur les inférences statistiques.
  m.data.p <- glm(number ~ age, family = poisson, data = data)
  
  #il faut tester la sur-dispersion sur les données originale et non celles avec extensions de zéros
  dispersiontest <-
    AER::dispersiontest(m.data.p , alternative = "greater")
  dispersiontest[["estimate"]][["dispersion"]]  #sur-dispersion si val >> 1
}
