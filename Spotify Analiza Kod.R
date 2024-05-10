# Spotify - analiza

# Analiza najpopularniejszych piosenek na Spotify w 2023.

#Celem mojej analizy jest odpowiedź na następujące pytania:
#1. Kto jest  najpopularniejszym(ą) artyst(k)ą w 2023 wedle liczby odtworzeń ich piosenek? --> The Weeknd
#2. Kto jest  najpopularniejszym(ą) artyst(k)ą w 2023 wedle liczby piosenek, któr  znalazły się w danych? --> Taylor Swift
#3. Jak korelują ze sobą poszczególne zmienne w danych m.in. pozycja w rankingach sewisów streamingowych: Spotify, Apple Music i Deezer? --> Pozytywnie, wysoka korelacja.
#4. Piosenki w której tonacji mają największą liczbę odtworzeń? --> C# 

## OCZYSZCZANIE I EKSPLORACJA

#Wczytanie zbioru danych o piosenkach Spotify. 


spotify <- read.csv("/Users/kacperwodiczko/Downloads/spotify-2023.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)


#Przegląd danych. 
#Użycie funkcji View() na zbiorze danych.


View(spotify)


### Podsumowanie:

#head() i tail() - początek i koniec, alfa i omega


head(spotify, 5)
tail(spotify, 5)


#Podsumowanie z summary().


summary(spotify)


#Suma miejsc z brakującymi wartościami, tzn. NA.


sum(is.na(spotify))


#Usunięcie pustych miejsc w zbiorze danych. 


czyste_spotify <- spotify[complete.cases(spotify), ]


#Instalowanie skimr, w celu lepszego podsumowania, oraz tidyverse dla dalszej analizy.


install.packages("skimr")
install.packages("tidyverse")


#Podsumowanie danych z pakietem skimr.


library(skimr)
skim(czyste_spotify)


#Struktura danych za pomocą funkcji "glimpse()".


library(tidyverse)
glimpse(czyste_spotify)


#Sprawdzam, czy są duplikaty.


print(paste("Liczba duplikatów wynosi:", duplicated(czyste_spotify) %>% sum()))


## Wizualizacja

#Wykres słupkowy pokazujący, że większość najpopularniejszych piosenek w 2023 została opublikowana w 2022. 


czyste_spotify %>% 
  ggplot(aes(as.factor(released_year)))+
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Rok wypuszczenia piosenki", y = "Liczba piosenek")


#Liczba unikalnych artystów.


print(paste("Liczba artystów to", czyste_spotify$`artist.s._name` %>% unique() %>% length()))


#Lista piosenek poszczególnych artystów w najpopularniejszych piosenkach 2023.


czyste_spotify %>% group_by(`artist.s._name`) %>% summarise(liczba = n()) %>% 
  arrange(desc(liczba)) %>% print()


#Tworzenie wykresu, pokazującego ile piosenek danego artysty znalazło się w zbiorze danych, co pokazuje jeden ze sposoów mierzenia popularności artysty.


najwiekszych20 <- czyste_spotify %>% group_by(`artist.s._name`) %>% summarise(liczba = n()) %>% 
  arrange(desc(liczba)) %>% filter(!is.na(`artist.s._name`)) %>% head(20)

najwiekszych20 <- najwiekszych20 %>% rename(artysta = `artist.s._name`) %>% arrange(liczba)

najwiekszych20 <- najwiekszych20[order(-najwiekszych20$liczba),]


#Zmiana kolejności słupków. 


najwiekszych20$artysta <- factor(najwiekszych20$artysta, levels = rev(najwiekszych20$artysta))


#Wykres pokazjący liczbę najpopularniejszych piosenek poszczeglnych artystów w 2023 roku. Taylor Swift miała najwięcej najpopularniejszych piosenek w 2023, aż 34.


najwiekszych20 %>% ggplot(aes(artysta, liczba)) + 
  geom_bar(stat = "identity", fill = "skyblue", width = .9) +
  geom_text(aes(label = liczba), vjust = 0, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Artysta", y = "Liczba", title = "Liczba piosenek poszczególnych artystów")


## Badanie korelacji pomiędzy zmiennymi liczbowymi. 

#Wybór wyłącznie zmiennych numerycznych


spotify_liczbowe <- czyste_spotify[sapply(czyste_spotify, is.numeric)]


#Wyrzucenie wartości NA


spotify_liczbowe <- na.omit(spotify_liczbowe)


#Obliczenie macierzy korelacji z dokładnością do dwóch miejsc po przecinku.


macierz_korelacji <- round(cor(spotify_liczbowe), digits = 2)


#Konwersja macierzy korelacji do dataframe.


korelacja_spotify <- as.data.frame(as.table(macierz_korelacji))


#Utworzenie wykresu korelacji z podpisami.


ggplot(korelacja_spotify, aes(x = Var1, y = Var2, fill = Freq, label = Freq)) +
  geom_tile(color = "white") +
  geom_text(size = 3) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1,1), name= "Korelacja") +
  labs(x = "Zmienne", y = "Zmienne", title = "Mapa termiczna korelacji") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))



## Wykres słupkowy pokazujący który artysta był odtwarzany największą ilość razy w 2023.

#Usunięcie wadliwego rzędu w dataframe. 


spotify_bez_wadliwego_rzędu <- czyste_spotify[-575,]


#Konwersja kolumny "streams" do liczb.


spotify_bez_wadliwego_rzędu$streams <- as.numeric(as.character(spotify_bez_wadliwego_rzędu$streams))


#Agregacja danych wedle artystów i obliczenie łącznej liczby odtworzeń. 


artist_streams <- aggregate(streams ~ artist.s._name, data = spotify_bez_wadliwego_rzędu, sum)


#ortowanie zagregowanych danych po sumie wszystkich streamów w malejącym porządku.


artist_streams <- artist_streams[order(-artist_streams$streams), ]


#Wybranie 20 najpopularniejszych artystów.



top_20 <- head(artist_streams, 20)
top_20


#Stworzenie wykresu słupkowego dla 20 najpopularniejszych artystów. Na podstawe wykresu można stwierdzić, że muzyka The Weeknd jest popularniejsza od muzyki Taylor Swift. 
#Suma odtworzeń piosenek jest alternatywną metodą mierzenia popularności, dla sprawdzenia ile piosenek danego artysty pojawiło sie w zbiorze danych top piosenek 2023.


barplot(height = top_20$streams/1000000000, #dzielenie przez miliard (duża liczba streamów)
        names.arg = top_20$artist.s._name,
        main = "Top 20 artystów według ilości odtworzeń ich piosenek, \n tj. muzyka którego artysty została odtworzona najwięcej razy?",
        xlab = "",
        ylab = "Odtworzenia (w miliardach)",
        col = "skyblue",
        cex.names = 0.6,
        las = 2)
top_20
```

## Wykres streamy a tonacja utworów.

#Wyrzucanie komórek bez wartości z kolumny tonacji.


spotify_data_clean <- spotify_bez_wadliwego_rzędu[!is.na(spotify_bez_wadliwego_rzędu$key), ]


#Agregacja


artist_streams <- aggregate(streams ~ key, data = spotify_data_clean, sum)


#Sortowanie


artist_streams <- artist_streams[order(-artist_streams$streams), ]


#Usuniecie niepokornego rzędu w danych, bez tonacji. 


artist_streams <- artist_streams[-2,]

head(artist_streams, 10)



#Wykres słupkowy tonacje a odtworzenia. 

#Wykres pokazuje, że piosenki  tonacji C# mają największą liczbę odtworzęń, nie doprecyzowano w zbiorze danych, czy są to piosenki w tonacji C# dur, czy C# moll.


barplot(height = artist_streams$streams/1000000000, 
        names.arg = artist_streams$key,
        main = "Top 10 tonacji według odtworzeń",
        ylab = "Odtworzenia (w miliardach)",
        col = "skyblue",
        cex.names = 0.6,
        las = 1)







