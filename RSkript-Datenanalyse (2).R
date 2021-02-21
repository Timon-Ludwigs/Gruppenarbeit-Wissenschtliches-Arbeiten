#Daten Analyse (Deskription und Visualisierung)

library(ggplot2)

#Deskription und Analyse von metrische Variablen

#einlesen des data frames

data <- read.csv(file = 'Datensatz.csv')
source("Skript 1")
source("Skript 2")

#a)
#Alter

data_alter <- merge(a(data$Alter), data$Alter)
names(data_alter)[11] <- "Alter"
data_alter


Analysis_alter_hist <- ggplot(data = data_alter) + 
  geom_histogram( aes(x = Alter), binwidth = 1) + 
  geom_vline(xintercept = data_alter$Mittelwert, colour = "blue") 
Analysis_alter_hist
#das histogramm zeigt, dass die hoehste Anzahl an Studierenden im Alter von 23 und 25 Jahren sind. 

Analysis_alter_boxplot <- ggplot(data = data_alter) + 
  geom_boxplot(aes(x = Alter))
Analysis_alter_boxplot 
#auch das boxplot zeigt, der groeßte Teil die Studierednen sind 
#23 bis 27 jahren alt.Der Median liegt bei 25.





