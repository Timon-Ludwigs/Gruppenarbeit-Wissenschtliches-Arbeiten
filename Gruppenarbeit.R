# GitHub Gruppenarbeit

# Datensatz:

ID <- 1:100

Alter <- rnorm(100, mean = 25, sd = 2)

Studienfach <- c("Statistik", "Data Science", "Mathe", "Informatik")

Studienfach <- sample(Studienfach, size = 100, replace = TRUE, 
                      prob = c(0.35, 0.35, 0.2, 0.1))

Interesse_an_Mathematik <- sample(1:7, 100, replace = TRUE)

Interesse_an_Programmieren <- sample(1:7, 100, replace = TRUE)

Mathe_LK <- sample(c(0, 1), 100, replace = TRUE)

data <- data.frame(ID, Alter, Studienfach, Interesse_an_Mathematik, 
                   Interesse_an_Programmieren, Mathe_LK)

write.csv2(data, file = "data_Gruppenarbeit.csv")
