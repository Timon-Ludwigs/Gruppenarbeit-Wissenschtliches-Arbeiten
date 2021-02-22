###Funktionen

# laden der R Datei, mit den noetigen Hilfsfunktionen
source("Skript 2")

#a)
a <- function(X){  #X ist ein Vektor an Variablen aus dem Datensatz
  min <- min(X)      #berechnet Minimum von X
  max <- max(X)      #berechnet Maximum von X
  ari <- mean(X)     #berechnet das arithmetisches Mittel von X  
  var <- var(X)      #berechnet die Varians von X
  med <- median(X)   #berechnet den Median von X
  mod <- Mode(X)     #berechnet Modus von X
  ra <- range(X)     #berechnet Spannweite von X
  sd <- sd(X)        #berechnet Standardabweichnung von X
  u_quartil<- quantile(X, 0.25, names= F) #berechent das untere Quartil von X
  o_quartil<- quantile(X, 0.75, names= F) #berechnet das obere Quartil von X
  return(data.frame("Minimum"=min,"Maximum"=max,"Mittelwert"=ari,"Median"=med,"Varianz"=var, "Standardabweichung"=sd, "Modus"=mod, "unteres Quartil"=u_quartil, "oberes Quartil"=o_quartil)) #erstellt Dataframe mit berechneten Werten und gibt diesen aus
}

#######################################################################

#b)

# desk_kategorical - Diese Funktion berechnet zwei 
#verschiedene deskriptive Statistiken
#für kategoriale Variablen.

######################Input########################### 

#x - Unsere kategorialen Daten

######################Output########################## 

#Eine Liste mit eine Modus, eine Frequency Tabelle,
#die die Anzahl für jede Kombination von kategorialem Variable 
#berechnet und eine Proportionstabelle, 
#die die Anteile (Prozentsätze) jeder Kategorie berechnet.

######################################################

desk_kategorical <- function(x){
  
  frequency <- table(x)
  proportion <- prop.table(frequency)
  Modus <- Mode(X)
  
  return(list("Modus" = Modus,
              "Frequency" = frequency, 
              "Proportion" = proportion))
}


#######################################################################

#c)

#desk_biv_categorical - Diese Funktion berechnet
#deskriptive bivariate Statistiken für
#den Zusammenhang zwischen zwei kategorialen Variablen.

####################Input######################### 

#x - x beinhaltet die zwei kategorialen Variablen

###################Output######################### 

#Eine bennante Liste mit :

#Frequency - eine Kontingenztafel mit frequencies und margins

#Proportions - eine Kontingenztafel mit Wahrscheinlichkeiten
#und margins

#Chi_sq_Test - Eine Chi-Quadrat-Test der testet, ob zwischen zwei 
#kategorialen Variablen ein Zusammenhang besteht.

##################################################

desk_biv_categorical <- function(x){
  
  frequencies <- addmargins(table(x[,1],x[,2]))
  
  proportions <- addmargins(prop.table(table(x[,1],x[,2])))
  
  chi_squared <- summary(table(x[,1], x[,2]))
  
  return(list(frequencies = frequencies,
              Proportions = proportions,
              Chi_sq_Test = chi_squared))
}



#######################################################################

#d)

func_d <- function(){
  # Gruppen erstellen mit Mathe LK ja und nein:
  null <- Datensatz$Alter[Datensatz$MatheLK == 0]
  eins <- Datensatz$Alter[Datensatz$MatheLK == 1]
  
  # Minimum und Maximum der beiden Gruppen berechenen:
  min_null <- min(null)
  max_null <- max(null)
  
  min_eins <- min(eins)
  max_eins <- max(eins)
  
  # Arithmetisches Mittel von den Gruppen:
  ari_null <- mean(null)
  ari_eins <- mean(eins)
  
  # Spannweite der Gruppen:
  ra_null <- range(null)
  ra_eins <- range(eins)
  
  # Standardabweichung der Gruppen:
  sd_null <- sd(null)
  sd_eins <- sd(eins)
  
  # oberes und unteres Quantil der Gruppen:
  u_quantil_null <- quantile(null, 0.25)
  o_quantil_null <- quantile(null, 0.75)
  
  u_quantil_eins <- quantile(eins, 0.25)
  o_quantil_eins <- quantile(eins, 0.75)
  
  # Rueckgabe ist eine Liste der zuvor berechneten Werte:
  return(list("Minimum von Gruppe 0" = min_null,
              "Maximum von Gruppe 0" = max_null,
              "Minimum von Gruppe 1" = min_eins,
              "Maximum von Gruppe 1" = max_eins,
              "Arithm. Mittel von Gruppe 0" = ari_null,
              "Arithm. Mittel von Gruppe 1" = ari_eins,
              "Spannweite Gruppe 0" = sd_null,
              "Spannweite Gruppe 1" = sd_eins,
              "Unteres Quantil Gruppe 0" = u_quantil_null,
              "0beres Quantil Gruppe 0" = o_quantil_null,
              "Unteres Quantil Gruppe 1" = u_quantil_eins,
              "0beres Quantil Gruppe 1" = o_quantil_eins
  ))
}

#######################################################################

#e)

# mit count als Hilfsfunktion in Skript2

func_e <- function(x){ # x ist ein numerischer Vektor 
  res <- list() # erzeugt eine Liste mit dem Namen res
  res$niedrig <- count(x, quantile(x, 0.25)) # berechnet die Anzahl der 25% kleinsten Werte
  res$mittel <- count(x, quantile(x, 0.75)) - count(x, quantile(x, 0.25)) # berechnet die Anzahl der Werte zwischen 25% und 75%
  res$hoch <- count(x, quantile(x, 1)) - count(x, quantile(x, 0.75)) # berechnet die Anzahl der groessten Werte, also alle ueber 75%
  return(res) # gibt die Liste zurueck
}


#######################################################################

#f)

func_f <- function(x, y, z, v = 0){ # alle Parameter sind Variablen aus dem Datensatz. v wird als default auf 0 gesetzt. Falls nun 4 Variablen genutzt werden sollen, wird v auf die entsprechende Variable gesetzt
  if(v == 0){ # prueft, ob fuer v eine Variable uebergeben wurde und fuehrt die nachfolgenden Zeilen aus, falls dies nicht so ist
    par(mfrow=c(3,1)) # Erzeugt drei barplots, die untereinander stehen
    # im folgenden werden die Barplots fuer die Variablen berechnet
    barplot(table(x))
    barplot(table(y))
    barplot(table(z))
  }
  else{ # falls fuer v ein anderer Wert als 0, also eine Variable uebergeben wurde werden die Nachfolgenden Zeilen ausgefuehrt
    par(mfrow=c(2,2)) # Erzeugt vier barplots, die im 2 x 2 angeordnet sind
    # im folgenden werden die Barplots fuer die Variablen berechnet
    barplot(table(x))
    barplot(table(y))
    barplot(table(z))
    barplot(table(v))
  }
}


#######################################################################

####freiwilliger Zusatz