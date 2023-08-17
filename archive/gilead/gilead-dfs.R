dfT <- fread("df-TRUE.csv")
paramsT <- fread("df-params-TRUE.csv")

dfF <- fread("df-FALSE.csv")
paramsF <- fread("df-params-FALSE.csv")

dfT[, mech2 := T]
dfF[, mech2 := F]
dfF[, ti2 := NA]
dfs <- rbind(dfT, dfF)

dfs <- dfs[(di == 0 & mech2 == FALSE) | di == 1, ]
dfs[, exclude := !no.exclude]
dfs[, excluded := ifelse(exclude, "0. Yes", "1. No")]

dfs[, xaxis := ifelse(di == 0, "Negative", ifelse(mech2 == FALSE, "Positive, Background Testing",
                                                  "Positive, Infection-Based Testing"))]

library(ggplot2)
ggplot(data=dfs, aes(x=xaxis, fill=excluded)) + geom_bar()
