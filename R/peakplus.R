peakplus <- function(data) {
  #Largement inspiré de Guide de normalisation et manuel JMainguy
  PeakPlus <- function() {
    uniqv <- unique(data$age)
    Peak <- uniqv[which.max(tabulate(match(data$age, uniqv)))]
    Peak + 1
  }
  PP <- PeakPlus()
  PP
}