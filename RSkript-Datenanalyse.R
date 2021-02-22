#Daten Analyse (Deskription und Visualisierung)

library(ggplot2)

#Deskription und Analyse von metrische Variablen

#einlesen des data frames

data <- read.csv(file = 'Datensatz.csv', sep = ";")
source("Skript1.R")
source("Skript2.R")

#a)
#Alter

a(data$Alter) #Funktion aus a
#     Minimum Maximum Mittelwert Median  Varianz Standardabweichung Modus.Modus Modus.Anzahl unteres.Quartil oberes.Quartil
#1      19      31      25.01     25 4.919091           2.217902          23           18              23             27
#2      19      31      25.01     25 4.919091           2.217902          25           18              23             27

#Das Minimum des Alter ist 19, das Maximum 31. Das arithmetische Mittel ist 25,01 und die Varianz 4.919091. Die Standartabweichung beträgt 2.217902.
#Der Modus zeigt uns, dass das häuigste Alter 23 und 25 sind. Es sind jeweils 18 Personen 23 und 25. Das untere Quartil liegt bei 23 und das obere bei 27.
#Wir sehen also, dass die Personen 19-31 Jahre alt und im Mittel 25 sind. Das sehen wir sowohl durch das arithmetische Mittel sowohl als auch durch den Median.
#Durch die Standardabweichung erkennen wir, dass die durchschnittliche Abweichung vom Mittelwert bei ca. 2,218 Jahren liegt.

data_alter <- merge(a(data$Alter), data$Alter)
names(data_alter)[11] <- "Alter"
data_alter

#Um genauer zu sehen wie das Alter veteilt ist, visualisieren wir dieses.

Analysis_alter_hist <- ggplot(data = data_alter) + 
  geom_histogram( aes(x = Alter), binwidth = 1) + 
  geom_vline(xintercept = data_alter$Mittelwert, colour = "blue") 
Analysis_alter_hist
#das histogramm zeigt, dass die hoehste Anzahl an Studierenden im Alter von 23 und 25 Jahren sind. 

Analysis_alter_boxplot <- ggplot(data = data_alter) + 
  geom_boxplot(aes(x = Alter))
Analysis_alter_boxplot 
#auch das boxplot zeigt, der groeßte Teil die Studierednen sind 
#23 bis 27 jahren alt.

#b) Für Fach

freq_bar_Fach <- as.data.frame(desk_kategorical(data$Fach)$Frequency)

ggplot_freq <- ggplot(freq_bar_Fach, aes(X,Freq)) +
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 42, colour = "blue") + 
  geom_text(aes(x = "Statistik", y=45), label = "Modus",
            colour = "Black")
ggplot_freq

#c)


#d)


#e)


#f)

