# More user-friendly y axes on plots

addUnits <- function(n) {
  labels <- ifelse(n < 1000, round(n, 1),  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3, 1), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6, 1), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9, 1), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12, 1), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}