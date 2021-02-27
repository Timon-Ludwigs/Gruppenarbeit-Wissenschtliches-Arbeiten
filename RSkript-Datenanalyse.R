#Daten Analyse (Deskription und Visualisierung)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggmosaic)
library(reshape2)
library(ggpubr)

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

############################################################################

#b) Für Fach

desk_kategorical(data$Fach)
#$Modus
#$Modus$Modus
#[1] "Statistik"
#
#$Modus$Anzahl
#[1] 42

#DataScience  Informatik       Mathe   Statistik 
#        33          18           7          42 
#$Proportion
#X
#DataScience  Informatik       Mathe   Statistik 
#      0.33        0.18        0.07        0.42 

#Die Funktion zeigt uns, dass in dem Datensatz am meisten Statistik
#Studenten vorhanden sind (siehe Modus und Tabelle). 42% der 
#Studierenden studieren Statistik. Danach folgen die 
#Date Science Studierenden mit 33% (33 Studierende). 
#Informatik studieren 18% (18 Studierende). Am wenigsten 
#vetreten sind Mathe Studenten mit 7% (7 Studierende).

#Zur Visualisierung:

freq_bar_Fach <- as.data.frame(desk_kategorical(data$Fach)$Frequency)

ggplot_freq <- ggplot(freq_bar_Fach, aes(X,Freq)) +
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 42, colour = "blue") + 
  geom_text(aes(x = "Statistik", y=45), label = "Modus",
            colour = "Black")
ggplot_freq

############################################################################

#c) Deskriptive bivariante Statistiken zw. Mathe Interesse
#   und Informatik Interesse

#Wir müssen zuerst die Daten entsprechend anpassen & ordnen, 
#damit unsere Grafiken sie lesen können

M_I <- factor(data$MatheInteresse, levels = c(4,5,6,7),
                labels = c("Mittel","Hoch",
                           "Sehr_Hoch","Höchste"))
I_I <- factor(data$InfoInteresse, levels = c(1,5,7),
              labels = c("Sehr Gering","Hoch",
                         "Höchste"))

table_Math_Info <- 
  desk_biv_categorical(data.frame(M_I,I_I))$frequencies

#Wir machen es zu einem Data frame, damit ggplot
#sie annehmen kann

table_Math_Info_df <- as.data.frame(table_Math_Info)
names(table_Math_Info_df) <- c("Math_Interesse",
                               "Info_Interesse",
                               "Frequency")

#Wir erstellen erstmal ein Mosaik plot
Mosaicplot_Math_Inf <- 
  ggplot(table_Math_Info_df) +
  geom_mosaic(
    aes( weight = Frequency, x = product(Math_Interesse), 
         fill = Info_Interesse)) + xlab("Math Interesse") + 
  ylab("Informatik Interesse") + 
  annotate(geom="text",x=0.55,y=-0.03,
           label=" Mittel           Hoch         Sehr Hoch            Höchste",
           color="black",size=3)
Mosaicplot_Math_Inf
#das mosaikplot soll den zusammenhang zwischen der interesse an mathe und informatik messen und wiedergeben.
#es stellt sich heraus, dass die wenigsten studierenedne hohe interesse an mathe und zugleich sehr geringe
#interesse an informatik haben. studierenden mit mittlmäßiger interesse an mathe haben hohe interesse an info.
#bei sehr hohen interesse an mathe ist auch die informatik affinität am größten.außerdem ist bei
#höchster interesse an mathe auch die höchste interesse an info zu sehen.


############################################################

#Da die Frequenz bei einem Mosaikplot nicht genau sichtbar ist, 
#erstellen wir ein Balkendiagramm

Barplot_Math_Inf <- ggplot(table_Math_Info_df, 
                           aes(x=Math_Interesse, 
                               y=Frequency,
                               fill = factor(Info_Interesse))) +
  geom_bar(stat="identity", position="dodge", colour="black") +
  scale_fill_brewer(type="qual", palette=1) + 
  geom_text(aes(label=Frequency), 
            position=position_dodge(width=0.9), vjust=-0.25)+
  scale_y_continuous(limits = c(0,45)) + 
  guides(fill=guide_legend(title="Info_Interesse"))
  
Barplot_Math_Inf
#dieses barplot drückt in relativen zaheln die intresse der studierenden im fach informatik in
#abhängikeit der interesse in mathe aus. 
#dabei haben 33 Studierenden mit mittlmäßiges Intreresse an mathe eine hohe interesse an info.
#nur 7 studierende mit hoher begeisterung für mathe haben ein geringes interesse für info.
#42 studierende haben ein sehr hohe interesse für mathe und info.der rest (18 studierende) weist die höchste
#intresse in beiden fächern auf.
#es lässt sich also sagen, dass die meisten studierenden die mittelmäßiges bis hohe interesse 
#an mathe haben, auch eine hohe interesse in info haben.

################################################################################

#d)

#Wir passen zuerst die Daten entsprechend an, 
#damit unsere Grafiken sie lesen können

Mathe_LK <- factor(data$MatheLK, levels = c(0,1),
              labels = c("Nein","Ja"))
d.f <- data.frame("Age" = data[,3],Mathe_LK)


#Um eine Idee zu haben, wie die Daten verteilt sind: 

data_met_dichotom <- melt(func_d(d.f)$
                        Frequency_zw_Met.Dichotom)
names(data_met_dichotom) <- c("Age", "Mathe_LK", "Frequency")


#Boxplot erstellen:

box_plot <- ggplot(data = data,
                   aes(x=Mathe_LK,y = Alter)) + 
  geom_boxplot()
box_plot

##################################################################

#Weil wir mit einen Boxplot nichts über die Frequency wissen,
#erstellen wir auch nich einen Lineplot


Lineplot_MathLK_Age <- ggplot(data_met_dichotom, 
                           aes(x=Age, 
                               y=Frequency,
                               group = Mathe_LK)) +
  geom_line(aes(color=Mathe_LK)) 
 
Lineplot_MathLK_Age

#e)

func_e(data$InfoInteresse)

#Result:

#$niedrig  $mittel  $hoch 
#[1] 40    [1] 60   [1] 0

#Wir sehen hier, dass die Liste nicht wirklich die 
#richtige Werte zurückgeben, weil die Quantile Funktionen
#von Skript1.R es anders berechnet als gewünscht.

#Daher ist eine bessere Variante(func_e_besser) mit der 
#cut(), table() und factor() Funktion erstellt und gezeigt hier.

func_e_besser(data$InfoInteresse)
func_e_besser(data$InfoInteresse)$Table

#Result:
#Gering Mittel   Hoch 
#     7     33     60 

#Analyse: Wir sehen hier nämlich, dass die Meisten
#Studierende in unserem Datensatz(60%) generall ein
#Hohes Interesse an Informatik haben. 33 Studierende
#haben eine mittlere Interesse und nur 7 von 100 
#Studenten haben kein großes Interesse an Informatik.

#f)

#Drei Variablen
#Wir werden zunächst Fach, Interesse an Mathematik
#und Interesse an Informatik visualisieren. Wir geben
#Input als eine Data Frame

#######################Data vorbereiten#########################################
data_three_variables <- data[c(4,5,6)]
data_three_variables$Fach <- as.character(data_three_variables$Fach)
data_three_variables[data_three_variables == "DataScience"]<-"D.S."
data_three_variables[data_three_variables == "Informatik"] <- "Inf."
data_three_variables[data_three_variables == "Statistik"] <- "Stat."
data_three_variables$Fach <- as.factor(data_three_variables$Fach)
#All dies musste ich zuerst machen, damit die Labels auf der
#x-Achse sich nicht überschneiden. 

#############################################################################

#Visualisierung
func_f(data_three_variables)

#Vier Variablen
#Wir fügen hier auch MatheLK hinzu:

########################Data vorbereiten#######################################

MatheLK <- data$MatheLK
data_four_variables <- cbind(data_three_variables,MatheLK)

###############################################################################
#Visualisierung

func_f(data_four_variables)

#Analyse:










