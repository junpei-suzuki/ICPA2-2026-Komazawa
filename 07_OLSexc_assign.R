dta <- readRDS("07_USPresident.RDS")

# 問１：与党候補の得票率の記述統計
sum(is.finite(dta$Vote100))
mean(dta$Vote100, na.rm = TRUE)
sd(dta$Vote100, na.rm = TRUE)
min(dta$Vote100, na.rm = TRUE)
max(dta$Vote100, na.rm = TRUE)

# 問２：実質可処分所得の変化の記述統計
sum(is.finite(dta$RDI))
mean(dta$RDI, na.rm = TRUE)
sd(dta$RDI, na.rm = TRUE)
min(dta$RDI, na.rm = TRUE)
max(dta$RDI, na.rm = TRUE)

# 問３・４：散布図
plot(dta$RDI, dta$Vote100)

# 問５・６：二変量回帰分析（全体）
OLSResult <- lm(Vote100 ~ RDI, data = dta)
summary(OLSResult)

# 問７・８：二変量回帰分析（現職出馬or不出馬）
# 現職出馬
OLSResult_Incum <- lm(Vote100 ~ RDI, data = dta, subset = (Reelection == 1))
summary(OLSResult_Incum)

# 現職不出馬
OLSResult_NoIncum <- lm(Vote100 ~ RDI, data = dta, subset = (Reelection == 0))
summary(OLSResult_NoIncum)
