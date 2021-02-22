######Helferfunktionen:

####interne Funktionen für Skript 1:

# fuer Aufgabe e) gibt es die Funktion count. Sie zählt die Werte, die groesser oder gleich dem Eingabeparameter z sind

count <- function(x, z){ # x ist ein numerischer Vektor und z eine numerische Zahl
  counter <- 0
  for(i in 1:length(x)){ # for schleife, zum durchlaufen des x Vektors
    if(x[i] <= z){ # falls der betrachtete Wert kleiner oder gleich dem z ist, wird der counter hochgezaehlt
      counter <- counter + 1
    }
  }
  return(counter) # der counter wird zureuckgegeben
}

########################################################################

#In a) verwendet (Mittelwert)
mean <- function(X){   #erstellt die Funktion mean, welche das arithmetische Mittel berechnen soll
  #X ist ein Vektor an Variablen aus dem Datensatz
  return(sum(X)/length(X))  #addiert alle Werte aus X und teilt diese durch die Anzahl der Variablen und gibt den berechneten Wert aus
}

########################################################################

#For b.) und eventuell auch a.) 

#Mode - Diese Funktion berechnet den häufigsten Wert, 
#der in kategorialen Daten vorkommt.

###################### Input #########################

#x - die erfordelichen kategorialen Daten  

###################### Output #########################

#Eine bennante Liste mit:
#Modus - Der berechnete Modus
#Anzahl - dies sagt, wie oft der Modus vorkommt

#######################################################



Mode = function(X) {
  ta = table(X)
  
  anzahl = max(ta)
  #Es gibt möglicherweise keinen Modus, wenn kein
  #Wert mehr als jeder andere angezeigt wird
  
  if (all(ta == anzahl)){
    mod = NA
  }else{
    
    #Darüber hinaus können die zurückgegebenen Werte 
    #entweder Zeichen oder Zahlen sein
    
    if (is.numeric(X)) {
      
      mod = as.numeric(names(ta)[ta == anzahl])
      
    } else{
      
      mod = names(ta)[ta == anzahl]
    }
  }
  return(list(Modus = mod, Anzahl = anzahl))
}

########################################################################