## Coefficienti da utilizzare per l'admission rate nella simulazione

#1
coefHE = c(0.41, 2.51 , 0.95, 0.13)
coefHU = c(0.60, 1.81, 1.22, 0.37)
coefDE = c(rep(1.316, 5), rep(0.21, 2))
coefDU = c(rep(1.160, 5), rep(0.60, 2))
coefME = rep(1, 12)
coefMU = rep(1,12)
ar = 0.19
missed.coef = 1.25
election = 0.73
error = FALSE
error.perc = NA
coef = 1.00
tabella1 = c(ar, missed.coef, ar*missed.coef, election, 1-election, coefHE, coefHU, coefDE, coefDU, coefME, coefMU, error, error.perc, coef)

#2

coefHE = c(0.41, 2.51 , 0.95, 0.13)
coefHU = c(0.60, 1.81, 1.22, 0.37)
coefDE = c(rep(1.316, 5), rep(0.21, 2))
coefDU = c(rep(1.160, 5), rep(0.60, 2))
coefME = c(0.9904762, 1.1428571, 1.1238095, 1.1619048, 1.2952381, 1.1238095, 0.9523810, 0.2476190, 1.0666667, 1.4285714, 0.9523810, 0.5142857)
coefMU = c(1.2200557, 0.7688022, 1.2200557, 1.1866295, 1.1364903, 1.2033426, 1.0027855, 0.9526462, 0.9025070, 1.0529248, 1.1699164, 0.1838440)
ar = 0.19
missed.coef = 1.25
election = 0.73
error = FALSE
error.perc = NA
coef = 1.00
tabella2 = c(ar, missed.coef, ar*missed.coef, election, 1-election, coefHE, coefHU, coefDE, coefDU, coefME, coefMU, error, error.perc, coef)

#3

coefHE = c(0.41, 2.51 , 0.95, 0.13)
coefHU = c(0.60, 1.81, 1.22, 0.37)
coefDE = c(rep(1.316, 5), rep(0.21, 2))
coefDU = c(rep(1.160, 5), rep(0.60, 2))
coefME = c(0.9904762, 1.1428571, 1.1238095, 1.1619048, 1.2952381, 1.1238095, 0.9523810, 0.2476190, 1.0666667, 1.4285714, 0.9523810, 0.5142857)
coefMU = c(1.2200557, 0.7688022, 1.2200557, 1.1866295, 1.1364903, 1.2033426, 1.0027855, 0.9526462, 0.9025070, 1.0529248, 1.1699164, 0.1838440)
ar = 0.19
missed.coef = 1.25
election = 0.73
error = FALSE
error.perc = NA
coef = 1.5
tabella3 = c(ar, missed.coef, ar*missed.coef, election, 1-election, coefHE, coefHU, coefDE, coefDU, coefME, coefMU, error, error.perc, coef)

#4

coefHE = c(0.41, 2.51 , 0.95, 0.13)
coefHU = c(0.60, 1.81, 1.22, 0.37)
coefDE = c(rep(1.316, 5), rep(0.21, 2))
coefDU = c(rep(1.160, 5), rep(0.60, 2))
coefME = c(0.9904762, 1.1428571, 1.1238095, 1.1619048, 1.2952381, 1.1238095, 0.9523810, 0.2476190, 1.0666667, 1.4285714, 0.9523810, 0.5142857)
coefMU = c(1.2200557, 0.7688022, 1.2200557, 1.1866295, 1.1364903, 1.2033426, 1.0027855, 0.9526462, 0.9025070, 1.0529248, 1.1699164, 0.1838440)
ar = 0.19
missed.coef = 1.25
election = 0.73
error = TRUE
error.perc = 0.15
coef = 1.00
tabella4 = c(ar, missed.coef, ar*missed.coef, election, 1-election, coefHE, coefHU, coefDE, coefDU, coefME, coefMU, error, error.perc, coef)

#5

coefHE = c(0.16, 2.87 , 0.94, 0.03)
coefHU = c(0.48, 1.69, 1.32, 0.51)
coefDE = c(rep(1.352, 5), rep(0.12, 2))
coefDU = c(rep(1.204, 5), rep(0.49, 2))
coefME = c(1.03, 0.98, 0.91, 1.14, 1.10, 1.01, 0.60, 0.97, 1.34, 1.24, 0.89, 1.52)
coefMU = c(1.52, 1.01, 1.01, 1.01, 0.51, 0.34, 1.18, 0.59, 0.68, 1.69, 0.76, 1.69)
ar = 0.13
missed.coef = 1.25
election = 0.85
error = FALSE
error.perc = NA
coef = 1.00
tabella5 = c(ar, missed.coef, ar*missed.coef, election, 1-election, coefHE, coefHU, coefDE, coefDU, coefME, coefMU, error, error.perc, coef)

###### Tabella ######

tabella = data.frame(tabella1, tabella2, tabella3, tabella4, tabella5)

tabella = cbind(c("Cumulative Admission Ratio", "Missed Orig. Rejected", "Sim. Admission Ratio", "Prop. Election", "Prop. Urgency",
paste0("Election Hour ", c("6-11", "12-17", "18-23", "00-05")), paste0("Urgency Hour ", c("6-11", "12-17", "18-23", "00-05")),
paste0("Election Day ", c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), paste0("Urgency Day ", c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
paste0("Election Month ", c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), paste0("Urgency Month ", c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
"Lambda error", "SD Lambda error", "Multiplier coef."), tabella)