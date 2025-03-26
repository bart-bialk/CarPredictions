setwd("C:/Users/wikto/Desktop/CAR_PREDICTIVE")



install.packages("dplyr")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("ranger")
library(dplyr)
library(ggplot2)
library(randomForest)
library(ranger)






#Ładowanie datasetu treningowego do "standard_df" i testowego do "test_df"
standard_df <- read.csv("sales_ads_train.csv")
test_df <- read.csv("sales_ads_test.csv")


#Wstępne podsumowanie datasetu, sprawdzanie typu danych dla kolumny.
summary(standard_df)


#Zamiana pustych pól na nulle
standard_df <- standard_df %>%
  mutate(across(everything(), ~ ifelse(trimws(.) == "", NA, .)))

test_df <- test_df %>%
  mutate(across(everything(), ~ ifelse(trimws(.) == "", NA, .)))










#Wstępna wizualizacja przewidywanej kolumny

#Cena

summary(standard_df$Cena)



#Histogram

ggplot(standard_df, aes(x = Cena)) +
  geom_histogram(bins = 100, fill = "red") +
  labs(title = "Histogram:Cena", x = "Cena", y = "Ilość")

#Boxplot
ggplot(standard_df, aes(y = Cena)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot: Cena", y = "Cena")

#Violinplot
ggplot(standard_df, aes(x = "", y = Cena)) +
  geom_violin(fill = "blue") +
  labs(title = "Violin Plot: Cena", y = "Cena")















#Podsumowanie kolumn z wartościami numerycznymi.

summary(standard_df)
summary(standard_df$Cena)
summary(standard_df$Rok_produkcji)
summary(standard_df$Przebieg_km)
summary(standard_df$Moc_KM)
summary(standard_df$Pojemnosc_cm3)
summary(standard_df$Emisja_CO2)
summary(standard_df$Liczba_drzwi)




#Wizualizacja i profiling poszczególnych kolumn


#rok produkcji

summary(standard_df$Rok_produkcji)

ggplot(standard_df, aes(x = Rok_produkcji)) +
  geom_histogram(binwidth = 1, fill = "red") +
  labs(title = "Histogram: Rok Produkcji", x = "Rok Produkcji", y = "Ilość")

ggplot(standard_df, aes(y = Rok_produkcji)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot: Rok Produkcji", y = "Rok Produkcji")

ggplot(standard_df, aes(x = "", y = Rok_produkcji)) +
  geom_violin(fill = "blue") +
  labs(title = "Violin Plot: Rok Produkcji", y = "Rok Produkcji")


#Wizualizacja procentowych granice wartości

quantile(standard_df$Rok_produkcji, probs = seq(0, 0.1, by = 0.01), na.rm = TRUE)

#Odcięcie przy 1% wzwyż = 1991
standard_df <- standard_df %>%
  filter(Rok_produkcji >= 1991 | is.na(Rok_produkcji))






#Przebieg_km

summary(standard_df$Przebieg_km)

ggplot(standard_df, aes(x = Przebieg_km)) +
  geom_histogram(bins = 50, fill = "red") +
  labs(title = "Histogram: Przebieg_km", x = "Przebieg_km", y = "Ilość")

ggplot(standard_df, aes(y = Przebieg_km)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot: Przebieg_km", y = "Przebieg_km")

ggplot(standard_df, aes(x = "", y = Przebieg_km)) +
  geom_violin(fill = "blue") +
  labs(title = "Violin Plot: Przebieg_km", y = "Przebieg_km")


#Obliczanie IQR do usunięcia outlierów
iqr_limit <- quantile(standard_df$Przebieg_km, 0.75, na.rm = TRUE) +
  1.5 * IQR(standard_df$Przebieg_km, na.rm = TRUE)

iqr_limit

standard_df <- standard_df %>%
  filter(Przebieg_km <= iqr_limit | is.na(Przebieg_km))





#Pojemnosc_cm3
summary(standard_df$Pojemnosc_cm3)

ggplot(standard_df, aes(x = Pojemnosc_cm3)) +
  geom_histogram(bins = 50, fill = "red") +
  labs(title = "Histogram: Pojemnosc_cm3", x = "Pojemnosc_cm3", y = "Ilość")

ggplot(standard_df, aes(y = Pojemnosc_cm3)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot: Pojemnosc_cm3", y = "Pojemnosc_cm3")

ggplot(standard_df, aes(x = "", y = Pojemnosc_cm3)) +
  geom_violin(fill = "blue") +
  labs(title = "Violin Plot: Pojemnosc_cm3", y = "Pojemnosc_cm3")


#Usuwanie outlierów z 1% najwyższych wartości
percentile_99 <- quantile(standard_df$Pojemnosc_cm3, 0.99, na.rm = TRUE)
percentile_99


standard_df <- standard_df %>%
  filter(Pojemnosc_cm3 <= percentile_99 | is.na(Pojemnosc_cm3))





#Emisja_CO2
summary(standard_df$Emisja_CO2)

ggplot(standard_df, aes(x = Emisja_CO2)) +
  geom_histogram(bins = 50, fill = "red") +
  labs(title = "Histogram: Emisja CO2", x = "Emisja_CO2 (g/km)", y = "Ilość")

ggplot(standard_df, aes(y = Emisja_CO2)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot: Emisja_CO2", y = "Emisja_CO2")


ggplot(standard_df, aes(x = "", y = Emisja_CO2)) +
  geom_violin(fill = "blue") +
  labs(title = "Violin Plot: Emisja_CO2", y = "Emisja_CO2")

#Emisja została zignorowana, z powodu zbyt dużej ilości nulli


#Liczba drzwi

summary(standard_df$Liczba_drzwi)


ggplot(standard_df, aes(x = as.factor(Liczba_drzwi))) +
  geom_bar(fill = "red") +
  labs(title = "Barplot: Liczba Drzwi", x = "Liczba_drzwi", y = "Ilość")


#Usuwanie nierealistycznych wartości
table(standard_df$Liczba_drzwi)

standard_df <- standard_df %>%
  filter(Liczba_drzwi >= 2 & Liczba_drzwi <= 6 | is.na(Liczba_drzwi))




#Punkt kontrolny


#write.csv(standard_df, file = "standard_df_checkpoint_1.csv", row.names = FALSE)

#standard_df <- read.csv("standard__df_checkpoint_1.csv")








#Wizualizacje kolumn z wartościami kategorycznymi. Sprawdzanie dystrybucji pomiędzy kategoriami.
#Podstawianie median za nulle dla kolumn z wartościami numerycznymi.


#Waluta
unique(standard_df$Waluta) 
sum(is.na(standard_df$Waluta))

table(standard_df$Stan)

ggplot(standard_df, aes(x = Waluta)) +
  geom_bar(fill = "red") +
  labs(title = "Histogram: Waluta", x = "Waluta", y = "Ilość")


#Stan
unique(standard_df$Stan) 
sum(is.na(standard_df$Stan))

table(standard_df$Stan)

ggplot(standard_df, aes(x = Stan)) +
  geom_bar(fill = "red") +
  labs(title = "Histogram: Stan", x = "Stan", y = "Ilość")



#Marka_pojazdu
length(unique(standard_df$Marka_pojazdu))
sum(is.na(standard_df$Marka_pojazdu))

table(standard_df$Marka_pojazdu)

#Model_pojazdu
length(unique(standard_df$Model_pojazdu))
sum(is.na(standard_df$Model_pojazdu))


#Wersja_pojazdu
length(unique(standard_df$Wersja_pojazdu))
sum(is.na(standard_df$Wersja_pojazdu))      


#Generacja_pojazdu
length(unique(standard_df$Generacja_pojazdu))
sum(is.na(standard_df$Generacja_pojazdu))


#Rok_produkcji - 
length(unique(standard_df$Rok_produkcji))
sum(is.na(standard_df$Rok_produkcji))

table(standard_df$Rok_produkcji)

#Przebieg_km
sum(is.na(standard_df$Przebieg_km))

standard_df$Przebieg_km[is.na(standard_df$Przebieg_km)] <- median(standard_df$Przebieg_km, na.rm = TRUE)


#Moc_KM
sum(is.na(standard_df$Moc_KM)) 
standard_df$Moc_KM[is.na(standard_df$Moc_KM)] <- median(standard_df$Moc_KM, na.rm = TRUE)


#Pojemnosc_cm3
sum(is.na(standard_df$Pojemnosc_cm3))


standard_df$Pojemnosc_cm3[is.na(standard_df$Pojemnosc_cm3)] <- median(standard_df$Pojemnosc_cm3, na.rm = TRUE)


#Rodzaj_paliwa
unique(standard_df$Rodzaj_paliwa)
sum(is.na(standard_df$Rodzaj_paliwa))

table(standard_df$Rodzaj_paliwa)

ggplot(standard_df, aes(x = Rodzaj_paliwa)) +
  geom_bar(fill = "red") +
  labs(title = "Histogram: Rodzaj Paliwa", x = "Rodzaj_paliwa", y = "Ilość")


#Emisja_CO2
sum(is.na(standard_df$Emisja_CO2))


#Naped
unique(standard_df$Naped)
sum(is.na(standard_df$Naped))

table(standard_df$Naped)

ggplot(standard_df, aes(x = Naped)) +
  geom_bar(fill = "red") +
  labs(title = "Histogram: Naped", x = "Naped", y = "Ilość")


#Skrzynia_biegow
unique(standard_df$Skrzynia_biegow)
sum(is.na(standard_df$Skrzynia_biegow))

table(standard_df$Skrzynia_biegow)

ggplot(standard_df, aes(x = Skrzynia_biegow)) +
  geom_bar(fill = "red") +
  labs(title = "Histogram: Skrzynia Biegow", x = "Skrzynia_biegow", y = "Ilość")


#Typ_Nadwozia
unique(standard_df$Typ_nadwozia)
sum(is.na(standard_df$Typ_nadwozia))

table(standard_df$Typ_nadwozia)


ggplot(standard_df, aes(x = Typ_nadwozia)) +
  geom_bar(fill = "red") +
  labs(title = "Histogram: Typ Nadwozia", x = "Typ_nadwozia", y = "Ilość")


#Liczba_drzwi
unique(standard_df$Liczba_drzwi)
sum(is.na(standard_df$Liczba_drzwi))

table(standard_df$Liczba_drzwi)

#Kolor
unique(standard_df$Kolor)
sum(is.na(standard_df$Kolor))

table(standard_df$Kolor)

#Kraj_pochodzenia
length(unique(standard_df$Kraj_pochodzenia))
sum(is.na(standard_df$Kraj_pochodzenia))


#Pierwszy_wlasciciel
unique(standard_df$Pierwszy_wlasciciel)
sum(is.na(standard_df$Pierwszy_wlasciciel))

ggplot(standard_df, aes(x = Pierwszy_wlasciciel)) +
  geom_bar(fill = "red") +
  labs(title = "Histogram: Pierwszy Wlasciciel", x = "Pierwszy_wlasciciel", y = "Ilość")



#Data_pierwszej_rejestracji
length(unique(standard_df$Data_pierwszej_rejestracji))
sum(is.na(standard_df$Data_pierwszej_rejestracji))

#Data_publikacji_oferty
length(unique(standard_df$Data_publikacji_oferty)) 
sum(is.na(standard_df$Data_publikacji_oferty))

#Lokalizacja_oferty
length(unique(standard_df$Lokalizacja_oferty))
sum(is.na(standard_df$Lokalizacja_oferty))

#Wyposażenie
length(unique(standard_df$Wyposazenie))
sum(is.na(standard_df$Wyposazenie))








#Cena
sum(is.na(standard_df$Cena))
#Waluta
sum(is.na(standard_df$Waluta))
#Stan
sum(is.na(standard_df$Stan)) #1
#Marka_pojazdu
sum(is.na(standard_df$Marka_pojazdu)) #2
#Model_pojazdu
sum(is.na(standard_df$Model_pojazdu)) #3
#Wersja_pojazdu
sum(is.na(standard_df$Wersja_pojazdu))
#Generacja_pojazdu
sum(is.na(standard_df$Generacja_pojazdu))
#Rok_produkcji
sum(is.na(standard_df$Rok_produkcji)) #4
#Przebieg_km
sum(is.na(standard_df$Przebieg_km))
#Moc_KM
sum(is.na(standard_df$Moc_KM))
#Pojemnosc_cm3
sum(is.na(standard_df$Pojemnosc_cm3))
#Rodzaj_paliwa
sum(is.na(standard_df$Rodzaj_paliwa)) #5
#Emisja_CO2
sum(is.na(standard_df$Emisja_CO2))
#Naped
sum(is.na(standard_df$Naped)) #6
#Skrzynia_biegow
sum(is.na(standard_df$Skrzynia_biegow)) #7
#Typ_nadwozia
sum(is.na(standard_df$Typ_nadwozia)) #8
#Liczba_drzwi
sum(is.na(standard_df$Liczba_drzwi)) #9
#Kolor
sum(is.na(standard_df$Kolor)) #0
#Kraj_pochodzenia
sum(is.na(standard_df$Kraj_pochodzenia))
#Pierwszy_wlasciciel
sum(is.na(standard_df$Pierwszy_wlasciciel))
#Data_pierwszej_rejestracji
sum(is.na(standard_df$Data_pierwszej_rejestracji))
#Data_publikacji_oferty
sum(is.na(standard_df$Data_publikacji_oferty))
#Lokalizacja_oferty
sum(is.na(standard_df$Lokalizacja_oferty))
#Wyposazenie
sum(is.na(standard_df$Wyposazenie))












#Punkt kontrolny
#write.csv(standard_df, file = "standard_df_checkpoint_2.csv", row.names = FALSE)
#standard_df <- read.csv("standard__df_checkpoint_2.csv")





#Ustalanie wpływu kolumn do ceny. Metody: random forest, regresja liniowa, korelacja. 



rf_data <- standard_df


#Waluta

rf_data$Waluta <- as.factor(rf_data$Waluta)

rf_model_waluta <- randomForest(Cena ~ Waluta, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)

randomForest::importance(rf_model_waluta)

#Stan
rf_data$Stan <- as.factor(rf_data$Stan)


rf_model_stan <- randomForest(Cena ~ Stan, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)

randomForest::importance(rf_model_stan)


#Marka_pojazdu

rf_data$Marka_pojazdu <- as.factor(rf_data$Marka_pojazdu)

rf_marka <- ranger(Cena ~ Marka_pojazdu, data = rf_data, importance = "impurity", na.action = "na.omit")

rf_marka$variable.importance

#Model_pojazdu

rf_data$Model_pojazdu <- as.factor(rf_data$Model_pojazdu)

ranger_model_model <- ranger(formula = Cena ~ Model_pojazdu,data = rf_data,importance = "impurity",num.trees = 100,na.action = "na.omit")

ranger_model_model$variable.importance




#Wersja_pojazdu

rf_data$Wersja_pojazdu <- as.factor(rf_data$Wersja_pojazdu)

ranger_model_wersja <- ranger(formula = Cena ~ Wersja_pojazdu,data = rf_data,importance = "impurity",num.trees = 100,na.action = "na.omit")

ranger_model_wersja$variable.importance


#Generacja_pojazdu

rf_data$Generacja_pojazdu <- as.factor(rf_data$Generacja_pojazdu)


ranger_generacja <- ranger(Cena ~ Generacja_pojazdu,data = rf_data,importance = "impurity",num.trees = 100,na.action = "na.omit")

ranger_generacja$variable.importance


#Rok_produkcji


rf_model_rok <- randomForest(Cena ~ Rok_produkcji, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_rok)



summary(lm(Cena ~ Rok_produkcji, data = rf_data))



#Przebieg_km


cor(rf_data$Cena, rf_data$Przebieg_km)


rf_model_przebieg <- randomForest(Cena ~ Przebieg_km, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_przebieg)


#Moc_KM

cor(rf_data$Cena, rf_data$Moc_KM)


rf_model_moc <- randomForest(Cena ~ Moc_KM, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_moc)


#Pojemnosc_cm3

cor(rf_data$Cena, rf_data$Pojemnosc_cm3)


rf_model_pojemnosc <- randomForest(Cena ~ Pojemnosc_cm3, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_pojemnosc)



#Rodzaj_paliwa


rf_data$Rodzaj_paliwa <- as.factor(rf_data$Rodzaj_paliwa)


rf_model_paliwo <- randomForest(Cena ~ Rodzaj_paliwa, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_paliwo)


#Emisja_CO2

cor(rf_data$Cena, rf_data$Emisja_CO2)


rf_model_emisja <- randomForest(Cena ~ Emisja_CO2, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_emisja)


#Naped

rf_data$Naped <- as.factor(rf_data$Naped)


rf_model_naped <- randomForest(Cena ~ Naped, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_naped)


#Skrzynia_biegow

rf_data$Skrzynia_biegow <- as.factor(rf_data$Skrzynia_biegow)

rf_model_skrzynia <- randomForest(Cena ~ Skrzynia_biegow, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_skrzynia)


#Typ_nadwozia


rf_data$Typ_nadwozia <- as.factor(rf_data$Typ_nadwozia)


rf_model_nadwozie <- randomForest(Cena ~ Typ_nadwozia, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_nadwozie)


#Liczba_drzwi


rf_data$Liczba_drzwi <- as.factor(rf_data$Liczba_drzwi)


rf_model_drzwi <- randomForest(Cena ~ Liczba_drzwi, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_drzwi)



#Kolor


rf_data$Kolor <- as.factor(rf_data$Kolor)


rf_model_kolor <- randomForest(Cena ~ Kolor, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_kolor)


#Kraj_pochodzenia

rf_data$Kraj_pochodzenia <- as.factor(rf_data$Kraj_pochodzenia)

rf_model_kraj <- randomForest(Cena ~ Kraj_pochodzenia, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_kraj)



#Pierwszy_wlasciciel

rf_data$Pierwszy_wlasciciel <- as.factor(rf_data$Pierwszy_wlasciciel)

rf_model_wlasciciel <- randomForest(Cena ~ Pierwszy_wlasciciel, data = rf_data, ntree = 100, importance = TRUE, na.action = na.omit)
randomForest::importance(rf_model_wlasciciel)




#Usuwanie nieistotnych kolumn, czyszcenie pozostałych nulli


standard_df <- standard_df[!is.na(standard_df$Stan), ]
standard_df <- standard_df[!is.na(standard_df$Marka_pojazdu), ]
standard_df <- standard_df[!is.na(standard_df$Model_pojazdu), ]
standard_df <- standard_df[!is.na(standard_df$Rok_produkcji), ]
standard_df <- standard_df[!is.na(standard_df$Rodzaj_paliwa), ]
standard_df <- standard_df[!is.na(standard_df$Naped), ]
standard_df <- standard_df[!is.na(standard_df$Skrzynia_biegow), ]
standard_df <- standard_df[!is.na(standard_df$Typ_nadwozia), ]
standard_df <- standard_df[!is.na(standard_df$Liczba_drzwi), ]
standard_df <- standard_df[!is.na(standard_df$Kolor), ]


standard_df$ID <- NULL
standard_df$Waluta <- NULL
standard_df$Wersja_pojazdu <- NULL
standard_df$Generacja_pojazdu <- NULL
standard_df$Kraj_pochodzenia <- NULL
standard_df$Pierwszy_wlasciciel <- NULL
standard_df$Data_pierwszej_rejestracji <- NULL
standard_df$Data_publikacji_oferty <- NULL
standard_df$Lokalizacja_oferty <- NULL
standard_df$Wyposazenie <- NULL
standard_df$Emisja_CO2 <- NULL





test_df$Moc_KM[is.na(test_df$Moc_KM)] <- median(test_df$Moc_KM, na.rm = TRUE)
test_df$Przebieg_km[is.na(test_df$Przebieg_km)] <- median(test_df$Przebieg_km, na.rm = TRUE)
test_df$Pojemnosc_cm3[is.na(test_df$Pojemnosc_cm3)] <- median(test_df$Pojemnosc_cm3, na.rm = TRUE)



test_df$Waluta <- NULL
test_df$Wersja_pojazdu <- NULL
test_df$Generacja_pojazdu <- NULL
test_df$Kraj_pochodzenia <- NULL
test_df$Pierwszy_wlasciciel <- NULL
test_df$Data_pierwszej_rejestracji <- NULL
test_df$Data_publikacji_oferty <- NULL
test_df$Lokalizacja_oferty <- NULL
test_df$Wyposazenie <- NULL
test_df$Emisja_CO2 <- NULL





write.csv(standard_df, file = "standard_df_final.csv", row.names = FALSE)
write.csv(test_df, file = "test_df_final.csv", row.names = FALSE)







































































































































































































































































































































































































































































