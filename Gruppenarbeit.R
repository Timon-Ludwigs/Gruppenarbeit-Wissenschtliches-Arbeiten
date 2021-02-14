# GitHub Gruppenarbeit
##

##Aufgabe 1:

# Datensatz erstellen:


###ID für Person 1-100
ID <- 1:100

### Alter
set.seed(2411)  #Ergebnisse reproduzierbar, gleiche Zufallszahlen werden gezogen
Alter <- rnorm(100,25,2)
# Alter gerundet auf ganze Zahlen?
Alter <- round(Alter)

#### Studienfach
Studienfach <- c("Statistik", "DataScience", "Mathe", "Informatik")
# Wahrscheinlichkeiten für Studiengänge
prob = c(0.35, 0.35, 0.1, 0.2)
set.seed(2411)  #Ergebnisse reproduzierbar, gleiche Zufallszahlen werden gezogen
Fach <- sample(Studienfach,100, prob=prob, replace=TRUE)


###Interesse an Mathe
set.seed(2411) #Reproduzierbarkeit
Interesse_an_Mathematik <- sample(1:7, 100, replace = TRUE)
###Interesse Programmieren
set.seed(2411) #Reproduzierbarkeit
Interesse_an_Programmieren <- sample(1:7, 100, replace = TRUE)
###Mathe LK ja/nein
set.seed(2411) #Reproduzierbarkeit
Mathe_LK <- sample(c(0,1),100,prob=c(0.3,0.7), replace=TRUE) #Vektor mit 0 und 1 zufällig mit P(X=1)=0.7



### Variablen als Datensatz erstellen
data <- data.frame(ID, Alter, Fach, Interesse_an_Mathematik, Interesse_an_Programmieren, Mathe_LK)

### Working Directory ermitteln
getwd()

### Datensatz als .csv speichern
write.csv(Datensatz,"Datensatz.csv", row.names = FALSE)



