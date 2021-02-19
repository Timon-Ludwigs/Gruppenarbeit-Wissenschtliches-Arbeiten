############### Datensatz erstellen ###########################################

#ID für Person
ID <- 1:100

#Alter
set.seed(2411) #Ergebnisse reproduzierbar, gleiche Zufallszahlen werden gezogen
Alter <- rnorm(100,25,2)

#Alter gerundet auf ganze Zahlen?
  Alter <- round(Alter)

#Studienfach
Studienfach <- c("Statistik", "DataScience", "Mathe", "Informatik")

#Wahrscheinlichkeiten für Studiengänge
prob <- c(0.4,0.4,0.05,0.15)
set.seed(2411) #Ergebnisse reproduzierbar, gleiche Zufallszahlen werden gezogen
Fach <- sample(Studienfach,100, prob=prob, replace=TRUE) 

#Interesse an Mathematik
MatheInteresse <- numeric(100) #numerischer leerer Vektor mit 100 Einträgen erstellen

#W'keit Interesse an Mathe wenn Fach="Mathe"
pMathe <- c(0.025,0.025,0.025,0.025,0.2,0.3,0.4)

#W'keit Interesse an Mathe wenn Fach="Statistik"
pStatistik <- c(0.1,0.1,0.1,0.1,0.1,0.2,0.3)

#W'keit Interesse an Mathe wenn Fach="Informatik"
pInformatik <- c(0.45/4,0.45/4,0.45/4,0.45/4,0.1,0.2,0.25)

#W'keit Interesse an Mathe wenn Fach="DataScience"
pDataScience <- c(0.5/4,0.5/4,0.5/4,0.5/4,0.1,0.2,0.2)

#Vektor mit 1 bis 7 für das Interesse erstellen
Interesse <- 1:7

#leeren Vektor mit zufälligem Interesse für jeden Studiengang
set.seed(2411)
MatheInteresse[which(Fach=="Statistik")] <- sample(Interesse,1,prob=pStatistik)
MatheInteresse[which(Fach=="DataScience")] <- sample(Interesse,1,prob=pDataScience)
MatheInteresse[which(Fach=="Informatik")] <- sample(Interesse,1,prob=pInformatik)
MatheInteresse[which(Fach=="Mathe")] <- sample(Interesse,1,prob=pMathe)

#Interesse an Informatik
InfoInteresse <- numeric(100) #numerischer leerer Vektor mit 100 Einträgen erstellen

#W'keit Interesse an Informatik wenn Fach="Informatik"
pIInformatik <- c(0.025,0.025,0.025,0.025,0.2,0.3,0.4)

#W'keit Interesse an Informatik wenn Fach="DataScience"
pIDataScience <- c(0.1,0.1,0.1,0.1,0.1,0.2,0.3)

#W'keit Interesse an Informatik wenn Fach="Mathe"
pIMathe <- c(0.45/4,0.45/4,0.45/4,0.45/4,0.1,0.2,0.25)

#W'keit Interesse an Mathe wenn Fach="Statistik"
pIStatistik <- c(0.5/4,0.5/4,0.5/4,0.5/4,0.1,0.2,0.2)

#leerer Vektor mit zufälligem Interesse für jeden Studiengang
set.seed(2411)
InfoInteresse[which(Fach=="Statistik")] <- sample(Interesse,1,prob=pIStatistik)
InfoInteresse[which(Fach=="DataScience")] <- sample(Interesse,1,prob=pIDataScience)
InfoInteresse[which(Fach=="Informatik")] <- sample(Interesse,1,prob=pIInformatik)
InfoInteresse[which(Fach=="Mathe")] <- sample(Interesse,1,prob=pIMathe)

#Mathe LK
set.seed(2411)
MatheLK <- sample(c(0,1),100,prob=c(0.3,0.7), replace=TRUE) #Vektor mit 0 und 1 zufällig mit P(X=1)=0.7

#alle mit Fach Mathe ODER Interesse an Mathe=7 hatten Mathe LK
for(i in 1:100)
{
  if (MatheInteresse[i]==7|Fach[i]=="Mathe") MatheLK[i] <- 1
}
#Variablen als Datensatz erstellen
Datensatz <- data.frame(ID, Alter, Fach, MatheInteresse, InfoInteresse, MatheLK)

write.csv(Datensatz, file = "Datensatz.csv", row.names=FALSE)
