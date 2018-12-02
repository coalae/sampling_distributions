# A1 
# Cordula Eggerth

rm(list=ls())

#-----------------------------------------------------------------------------------------------
### AUFGABE 1 ### ------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# 1. Gegeben ist eine Grundgesamtheit von 6 Elementen mit jeweils einem Merkmal y: 
#    U={1,2,3,4,5,6}; y1=2.6, y2=3.4, y3=5.7, y4=1.3, y5=0.9, y6=4.1 .
#    Es werden Stichproben vom Umfang 3 ohne Zurücklegen (anm.: reihenfolge nicht wichtig) 
#    gezogen.

# a) Wie viele Stichproben gibt es?

N <- 6
y <- c(2.6,3.4, 5.7, 1.3, 0.9, 4.1)
n <- 3

# möglichkeit 1: formel N!/((N-n!)*n!)
anzahl_stichproben <- factorial(N)/(factorial(N-n)*factorial(n))  
# möglichkeit 2: direkt binomialkoeffizient
anzahl_stichproben_2 <- choose(N,n)
# ANTWORT: 20 mögliche Stichproben (gemäß Daten aus der Angabe)

# b) Berechne für jede mögliche Stichprobe den Mittelwert und die Varianz der (Mittelwerte?) 
#    und bestimme die Verteilung der Mittelwerte und die Verteilung der Varianzen der 
#    geschätzten Mittelwerte von y.

stichprobenmatrix <- (combn(y, m=3))
mittelwerte_allerStichproben <- colSums(stichprobenmatrix)/n 
varianz_derMittelwerte <- sd(mittelwerte_allerStichproben)^2

varianz_stichproben <- function(sample=stichprobenmatrix, means=mittelwerte_allerStichproben){
    varianzen <- rep(0,ncol(stichprobenmatrix))
    for(i in 1:ncol(stichprobenmatrix)){
      varianzen[i] <- sd(stichprobenmatrix[ ,i])^2
    }
    varianzen
}

varianz_derStichproben <- varianz_stichproben()

par(mfrow=c(1,1))
hist(mittelwerte_allerStichproben, breaks=10, col = "cornsilk3",  
     main = "Verteilung der Mittelwerte aller Stichproben",
     xlim = c(0,5), ylim = c(0,5),
     xlab = "Geschätzte Stichprobenmittelwerte", ylab="Häufigkeit")

hist(varianz_derStichproben, breaks=10, col = "darkseagreen2", 
     main = "Verteilung der Varianz der geschätzten Mittelwerte",
     xlim = c(0,9), ylim = c(0,5),
     xlab = "Geschätzte Varianzen", ylab="Häufigkeit")


# c) Welche Relation besteht zwischen dem Mittelwert der Grundgesamtheit und
#    dem Mittelwert der Verteilung der Schätzungen des Mittelwerts und der Verteilung der 
#    Schätzungen der Varianzen.

mittelwert_derGrundgesamtheit <- sum(y)/N
varianz_derGrundgesamtheit <- sd(y)^2
mittelwert_derSchaetzungen <- sum(mittelwerte_allerStichproben)/length(mittelwerte_allerStichproben)
varianz_derSchaetzungen <- sd(varianz_derStichproben)^2

#-----------------------------------------------------------------------------------------------
### AUFGABE 2 ### ------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# 2. Eine oft verwendete Möglichkeit zum Ziehen einer Stichprobe ist das sogenannte Bernoulli 
#    Sampling: 
#    Eine Grundgesamtheit besteht aus insgesamt N Elementen. 
#    Will man eine Stichprobe realisieren, die einen Anteil p der Elemente der Grundgesamtheit 
#    hat, so erzeugt man N gleichverteilte Zufallszahlen u_i und entscheidet nach folgender 
#    Regel, ob das Element zur Stichprobe gehört:
#    Falls (u_i < p), gehört das Element i zur Stichprobe, ansonsten nicht.
#    Man zeige, dass die möglichen Umfänge der Stichprobe bei dieser Methode nach einer 
#    Binomialverteilung B(N,p) verteilt sind.

stichprobe_generieren <- function(N=10000, p=0.3){
  
  u <- runif(N, min=0, max=1) 
  
  bool_u_kleiner_p <- u<p
  
  stichprobe <- u[bool_u_kleiner_p]
  
  stichprobe
}

# stichproben damit generieren:
anzahl_zuGenerierendeStichproben <- 10000
stichprobenumfaenge_p0.8 <- rep(0, anzahl_zuGenerierendeStichproben)
stichprobenumfaenge_p0.5 <- rep(0, anzahl_zuGenerierendeStichproben)
stichprobenumfaenge_p0.3 <- rep(0, anzahl_zuGenerierendeStichproben)

for(j in 1:anzahl_zuGenerierendeStichproben){
  stichprobenumfaenge_p0.8[j] <- length(stichprobe_generieren(10000,0.8))
  stichprobenumfaenge_p0.5[j] <- length(stichprobe_generieren(10000,0.5))
  stichprobenumfaenge_p0.3[j] <- length(stichprobe_generieren())
}

liste_stichprobenumfaenge <- list(stichprobenumfaenge_p0.8, 
                                  stichprobenumfaenge_p0.5,
                                  stichprobenumfaenge_p0.3)

# verteilung der moeglichen stichprobenumfaenge
p_vec <- c(0.8, 0.5, 0.3)
for(i in 1:length(liste_stichprobenumfaenge)){
  hist(liste_stichprobenumfaenge[[i]], breaks=40, col = "lightblue3",  
       main = paste("Verteilung der Stichprobenumfänge, N=10000, p=", p_vec[i]), 
       xlab = "Stichprobenumfänge", ylab="Häufigkeit") 
}

#-----------------------------------------------------------------------------------------------
### AUFGABE 3 ### ------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# Eine praktisch wichtige Verteilungsfamilie, die keine Exponentialfamilie ist, 
# ist die Familie der Weibullverteilungen deren Dichte durch
#   f(x;lambda,k) = k/lambda * (x/lambda)^(k-1) * e^((-x/lambda)^k) mit x>=0
# gegeben ist.
# Die praktische Anwendung in der Zuverlässigkeitstheorie ergibt sich daraus, dass die Verteilung
# für unterschiedliche Wahl der Parameter flexible Formen hat.
# Man veranschauliche sich das durch Visualisierung der Dichten für die folgenden Parameter werte:
# 1.:
# lambda=1,   k=0.3;
# lambda=2,   k=0.3;
# lambda=0.5, k=0.3;
# 2.:
# lambda=1,   k=1; (welcher Verteilung entspricht das? >> entspricht der Exponentialverteilung,
# quelle: http://math.bme.hu/~nandori/Virtual_lab/stat/special/Weibull.pdf)
# 3.:
# lambda=1,   k=2;
# lambda=2,   k=2;
# lambda=0.5, k=2;
# 4.:
# lambda=1,   k=5;
# lambda=2,   k=5;
# lambda=0.5, k=5;
#
# Was bedeuten die Parameter (d.h. scale bzw. lambda, shape bzw. k) inhaltlich? 

par(mfrow=c(2,2))

x <- seq(0, 3, length=200)

# ad 2.: lambda=1,   k=1
dichte_weibull <- dweibull(x, shape=1, scale=1, log=FALSE)
plot(x, dichte_weibull, type = "l", col="mediumslateblue", lwd=2, 
     main="Weibull-Dichte mit lambda=1, k=1", xlab="x", ylab="")

# ad 1.: lambda=1,k=0.3;  lambda=2,k=0.3;  lambda=0.5,k=0.3
dichte_weibull_l1 <- dweibull(x, shape=0.3, scale=1, log=FALSE)
dichte_weibull_l2 <- dweibull(x, shape=0.3, scale=2, log=FALSE)
dichte_weibull_l3 <- dweibull(x, shape=0.3, scale=0.5, log=FALSE)

plot(x=x, y=dichte_weibull_l1, type = "l", 
     col=c("mediumslateblue"), lwd=2, 
     main="Weibull-Dichte für k=0.3", xlab="x", ylab="")
lines(x, dichte_weibull_l2, type = "l", lty = 1, col = "violetred4")
lines(x, dichte_weibull_l3, type = "l", lty = 1, col = "turquoise")

legend("topright", legend=c("lambda=1, k=0.3","lambda=2, k=0.3", "lambda=0.5, k=0.3"), 
       col = c("mediumslateblue","violetred4", "turquoise"),
       border = "black", lwd=1, cex = 0.8)


# ad 3.: lambda=1,k=2;  lambda=2,k=2;  lambda=0.5,k=2
dichte_weibull_31 <- dweibull(x, shape=2, scale=1, log=FALSE)
dichte_weibull_32 <- dweibull(x, shape=2, scale=2, log=FALSE)
dichte_weibull_33 <- dweibull(x, shape=2, scale=0.5, log=FALSE)

plot(x=x, y=dichte_weibull_31, type = "l", 
     col=c("mediumslateblue"), lwd=2, 
     main="Weibull-Dichte für k=2", xlab="x", ylab="")
lines(x, dichte_weibull_32, type = "l", lty = 1, col = "violetred4")
lines(x, dichte_weibull_33, type = "l", lty = 1, col = "turquoise")

legend("topright", legend=c("lambda=1, k=2","lambda=2, k=2", "lambda=0.5, k=2"), 
       col = c("mediumslateblue","violetred4", "turquoise"),
       border = "black", lwd=1, cex = 0.8)


# ad 4.: lambda=1,k=5;  lambda=2,k=5;  lambda=0.5,k=5
dichte_weibull_41 <- dweibull(x, shape=5, scale=1, log=FALSE)
dichte_weibull_42 <- dweibull(x, shape=5, scale=2, log=FALSE)
dichte_weibull_43 <- dweibull(x, shape=5, scale=0.5, log=FALSE)

plot(x=x, y=dichte_weibull_41, type = "l", 
     col=c("mediumslateblue"), lwd=2, 
     main="Weibull-Dichte für k=5", xlab="x", ylab="")
lines(x, dichte_weibull_42, type = "l", lty = 1, col = "violetred4")
lines(x, dichte_weibull_43, type = "l", lty = 1, col = "turquoise")

legend("topright", legend=c("lambda=1, k=5","lambda=2, k=5", "lambda=0.5, k=5"), 
       col = c("mediumslateblue","violetred4", "turquoise"),
       border = "black", lwd=1, cex = 0.8)

#-----------------------------------------------------------------------------------------------











