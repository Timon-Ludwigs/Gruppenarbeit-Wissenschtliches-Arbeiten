###Funktionen

# laden der R Datei, mit den noetigen Hilfsfunktionen
source("Skript2.R")

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

#x - Unsere kategorialen Daten als Data Frame

######################Output########################## 

#Eine Liste mit eine Modus, eine Frequency Tabelle,
#die die Anzahl für jede Kombination von kategorialem Variable 
#berechnet und eine Proportionstabelle, 
#die die Anteile (Prozentsätze) jeder Kategorie berechnet.

######################################################

desk_kategorical <- function(X){
  
  frequency <- table(X)
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
  
  frequencies_marigin <- addmargins(table(x[,1],x[,2]))
  frequencies_no_margin <- table(x[,1],x[,2])
  proportions <- addmargins(prop.table(table(x[,1],x[,2])))
  
  chi_squared <- summary(table(x[,1], x[,2]))
  
  return(list(frequencies = frequencies_no_margin,
              freq_with_margin = frequencies_marigin,
              Proportions = proportions,
              Chi_sq_Test = chi_squared))
}



#######################################################################

#d)

func_d <- function(x){
  # zwei Gruppen der bivariaten Variable erstellen 
  #mit Mathe LK ja und nein:
  
  null <- data$Alter[data$MatheLK == 0]
  eins <- data$Alter[data$MatheLK == 1]
  
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
  
  #####################################################################
  #Sei x[1] - eine metrische Variable
  #Sei x[2] - eine dichotome Variable
  
  frequencies_no_margin <- table(x[,1],x[,2])
  
  #####################################################################
  
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
              "0beres Quantil Gruppe 1" = o_quantil_eins,
              "Frequency_zw_Met.Dichotom" = frequencies_no_margin
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

func_f <- function(x){
  
  #Es wird hier vorausgesetzt, dass eine Data Frame
  #als Parameter angegeben wird.
  
  namess <- names(x)
  
  #Wir erzeugen eine Frequency Tabelle hier
  List <- apply(x,2,table)
  
  #1.Variable 
  
  d.f_first <- as.data.frame(List[1])
  #Die erste Variable(eine Tabelle) wird dann zu einem
  #Data Frame gemacht
  
  #Hier wird unsere erste Grafik erzeugt und es wird
  #analog auch bei den anderen Variablen gemacht.
  p <- ggplot(data = d.f_first, aes(x = d.f_first[,1])) + 
    geom_bar(aes(y = d.f_first[,2],
                 fill = d.f_first[,1]),
             show.legend = FALSE,
             stat = 'identity',
             position = 'dodge') + 
    xlab(names(x)[1]) +
    ylab("Freq")
  
  #2.Variable
  
  d.f_second <- as.data.frame(List[2])
  
  q <- ggplot(data = d.f_second, aes(x = d.f_second[,1])) + 
    geom_bar(aes(y = d.f_second[,2],
                 fill = d.f_second[,1]),
             show.legend = FALSE,
             stat = 'identity',
             position = 'dodge') +
    xlab(names(x)[2]) +
    ylab("Freq")  
  
  #3.Variable
  d.f_third <- as.data.frame(List[3])
  
  r <- ggplot(data = d.f_third, aes(x = d.f_third[,1])) + 
    geom_bar(aes(y = d.f_third[,2],
                 fill = d.f_third[,1]),
             show.legend = FALSE,
             stat = 'identity',
             position = 'dodge')  +
    xlab(names(x)[3]) +
    ylab("Freq")
  
  combine <- ggarrange(p,q,r,
                       ncol = 2, nrow = 2)
  
  if(length(x) == 4){
    
    #4.Variable
    
    d.f_fourth <- as.data.frame(List[4])
    
    s <- ggplot(data = d.f_fourth, aes(x = d.f_fourth[,1])) + 
      geom_bar(aes(y = d.f_fourth[,2],
                   fill = d.f_fourth[,1]),
               show.legend = FALSE,
               stat = 'identity',
               position = 'dodge')  + 
      xlab(names(x)[4]) + 
      ylab("Freq")
    
    combine <- ggarrange(p,q,r,s,
                         ncol = 2, 
                         nrow = 2)
  }
  
  return(combine)
  
}
#######################################################################

####freiwilliger Zusatz

func_e_besser <- function(x){
  #Zunächst teilen wir die Daten in 3 gleiche Intervalle
  Factorise <- factor(cut(x,3,include.lowest = TRUE),
                      labels = c("Gering","Mittel","Hoch"))
  #Hier erfahren wir, wie viele Werte unter jede Kategorie fallen.
  table_category <- table("Frequency" = Factorise)

  
  return(list("kategorisierte Data" = Factorise,
              "Table" = table_category))
}

  