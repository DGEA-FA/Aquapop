psd_indice <- function(data, sp) {
  df  <- data %>% filter(sp == sp) %>% droplevels()
  
  if (unique(df$sp %>% droplevels()) %>% as.character() == "SANA") {
    breakClass <- c(0, 300, 500, 650, 800, 1000)
  } else if (unique(df$sp %>% droplevels()) %>% as.character() == "SAFO") {
    breakClass <- c(0, 150, 250, 325, 400, 500)
  } else if (unique(df$sp %>% droplevels()) %>% as.character() == "SAVI") {
    breakClass <- c(0, 250, 380, 510, 630, 760)
  }
  
  limInfStock <-
    breakClass [2] # limite inférieure de la classe Stock pour cette sp.
  
  bunch <-  df  %>%
    filter(ltm >= limInfStock) %>%
    mutate(gcat = FSA::lencat(
      ltm,
      breaks = breakClass,
      use.names = TRUE,
      droplevels = TRUE
    ))
  
  gfreq <-  xtabs(~ gcat, data = bunch)
  psdtable <- prop.table(gfreq) * 100
  
  psdQ <- FSA::rcumsum(psdtable)
  temp <- length(psdQ) %>% as.numeric()
  if (temp == 4) {
    PSDresult  <-
      FSA::psdCI(
        c(0, 1, 1, 1),
        ptbl = psdtable ,
        n = sum(gfreq),
        method = "binomial",
        label = "PSD Q"
      ) %>% as.data.frame()
  }
  if (temp == 5) {
    PSDresult <-
      FSA::psdCI(
        c(0, 1, 1, 1, 1),
        ptbl = psdtable ,
        n = sum(gfreq),
        method = "binomial",
        label = "PSD Q"
      ) %>% as.data.frame()
  }
  if (temp == 3) {
    PSDresult <-
      FSA::psdCI(
        c(0, 1, 1),
        ptbl = psdtable ,
        n = sum(gfreq),
        method = "binomial",
        label = "PSD Q"
      ) %>% as.data.frame()
  }
  if (temp == 2) {
    PSDresult <-
      FSA::psdCI(
        c(0, 1),
        ptbl = psdtable,
        n = sum(gfreq),
        method = "binomial",
        label = "PSD Q"
      ) %>% as.data.frame()
  }
  
  LCI <- PSDresult[2]
  UCI <- PSDresult[3]
  PSDresult <- PSDresult %>% mutate(IC95 = glue("[{LCI}-{UCI}]"))
  colnames(PSDresult)[1] <- "PSD"
  colnames(PSDresult)[4] <- "IC 95%"
  PSDresult <- PSDresult %>% dplyr::select(1, 4)
}