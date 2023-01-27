#Projekt z Języków skryptowych

install.packages("treemap")
install.packages("tidyverse")
install.packages("ggrepel")

library(tidyverse) # metapackage with lots of helpful functions
library(ggrepel)
library(treemap)


getwd()
# ustawić ścieżkę z folderu
setwd("C:/Users/Scuro Guardiano/Desktop/hentai/JezykiSkryptoweAD/Project")
df = data.frame(read.csv("netflix_titles.csv", header=TRUE, sep=",", dec="."), stringsAsFactors = FALSE)

summary(df)
glimpse(df)

# Sprawdzenie w których kolumnach znajdują się puste komórki
dfx = which(df == "", arr.ind = T)
dfx = table(dfx[, 2])

dfBlanks <- data.frame(NumerKolumny = colnames(df[as.numeric(names(dfx))]), IloscPowtorzen = as.numeric(dfx))

# Wykres znalezionych pustych pól
ggplot(dfBlanks, aes(x = reorder(NumerKolumny, IloscPowtorzen), y = IloscPowtorzen, fill = NumerKolumny)) + 
  geom_col() +  
  labs(x = "Kolumny", y = "IloscPowtorzen") + 
  ggtitle("Ilość braków w kolumnach") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(rep("#3366CC", 6))) +
  geom_text_repel(aes(label = stat(y), group = factor(NumerKolumny)), position = position_nudge_repel(y = 1))

# można pozbywać się pustych danych na kilka sposobów
# 1. wszystkie wiersze zawierające - usunąć
# 2. zamienić je na coś spodziewanego np "Brak informacji"
# 3. przewidzieć wartość - np na najczęściej występujący

# w przypadku kraju, obsady, daty dodania, reżysera jak i długości możemy użyć punktu 2
# w przypadku kategorii można użyć 3 opcji

# uzupełnianie pustych pól, aby nie musieć usuwać całych wierszy
# Po dokładniejszym zastanowieniu: w naszej analizie nie wykorzystamy w żaden sposób kolumn cast oraz director
# Więc zamiast uzupełniać dane w tych kolumnach, można się ich po prostu pozbyć

df = df[, -4]
df = df[, -4]
df$duration[df$duration == ""] = "Brak danych"
df$country[df$country == ""] = "Brak danych"
df$date_added[df$date_added == ""] = "Brak danych"
df$date_added = trimws(df$date_added)


names(sort(table(df$rating)))
# Podczas przygotowania danych do uzupełnienia braków, zauważamy
# że w kolumnie znajdują się niepoprawne dane "66 min", "74min" oraz "84 min"
# Sprawdzamy czy wiersze z tymi danymi posiadają rekordy w duration
checking <- df[is.element(df$rating, c('66 min','74 min', '84 min')),]
checking[c("duration","type")]
# Jak widzimy były to puste pola, dodatkowo typ każdego z nich to film
# stąd wniosek, że ten błąd wynika z pomyłki ludzkiej
# zamieniamy dane miejscami
df$duration[df$rating == "66 min"] <- "66 min"
df$duration[df$rating == "74 min"] <- "74 min"
df$duration[df$rating == "84 min"] <- "84 min"
# a następnie przypiszemy do pustych jak i błędnych danych najczęściej występujący typ
df$rating[is.element(df$rating, c('66 min','74 min', '84 min'))] = names(sort(-table(df$rating)))[1]
df$rating[df$rating == ""] = names(sort(-table(df$rating)))[1]

# jeszcze w przypadku country znaleziono złe wartości, z przecinkiem na początku
# usuwamy je by nie liczyły się jako unikalne wartości


dfx = which(df == "", arr.ind = T)
dfx = table(dfx[, 2])
dfx
# Pozbyliśmy się wszystkich braków danych

# Sprawdzanie liczby unikalnych wartości, by wiedzieć co warto wziąć na tapetę

uniques <- apply(df, MARGIN = 2, FUN = function(x) length(unique(x)))
# Dołożenie do liczb odpowiadające im nazwy kolumn
uniques <- data.frame(Kolumny = names(uniques), Unikalne = uniques, stringsAsFactors =  F)


ggplot(uniques, aes(y = reorder(Kolumny, Unikalne), x = Unikalne, fill = Kolumny)) +
  geom_col() +
  ggtitle("Ilość unikalnych wartości w kolumnach") +
  theme(plot.title = element_text(size=20, face="bold"),) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c(rep("#3366CC",2), "#0000AA", rep("#3366CC",4), "#0000AA","#0000AA", "#3366CC")) + 
  labs(x = "Kolumny", y = "Unikalne wartości") + 
  geom_text_repel(aes(label = stat(x), group = factor(Unikalne)), position = position_nudge_repel(x = 4))

# Jak widać kolumny:
# title, id oraz description (zaznaczone innym kolorem)
# posiadają taką samą lub prawie taką samą liczbę unikalnych wartości, co rekordów
# dlatego tymi wartościami nie będziemy się zajmować

# najmniej natomiast ma kolumna typ
unique(df$type)

# Podział bazy na filmy i seriale

tvShows <- df[df$type == "TV Show", ]
movies <- df[df$type == "Movie", ]
types <- data.frame(Typ = c("TV Show", "Movie"), Ilosc = c(nrow(tvShows), nrow(movies)))
# stosunek ilości filmów do ilości seriali

ggplot(types, aes(x = Typ, y = Ilosc, fill = Typ)) + 
  geom_col() + 
  ggtitle("Porównanie ilości filmów i seriali") + 
  theme(plot.title = element_text(size=20, face="bold"),) +
  scale_fill_manual(values = c("#0000AA", "#3366CC")) +
  geom_text_repel(aes(label = stat(y), group = factor(Typ)), position = position_nudge_repel(y = -2), color = "white")
types$Ilosc[2] - types$Ilosc[1] 
# zdecydowanie więcej jest filmów - różnica: 3455 pozycji

# Następna kolumna: rating
# Przy rozpoznaniu oznaczeń pomogła strona https://help.netflix.com/pl/node/2064/us
# Dodajemy nową kolumnę z prostszym podziałem: dzieci, nastolatki, dorośli

rating = c('TV-Y', 'TV-Y7', 'TV-G', 'PG', 'TV-PG',
           'PG-13', 'TV-14',
           'R', 'TV-MA', 'NC-17', 'NR', 'UR',
           'TV-Y7-FV', 'G'
           )
age_rating = c('Kids', 'Kids', 'Kids', 'Kids', 'Kids', 
               'Teens', 'Teens', 
               'Adults', 'Adults', 'Adults', 'Adults', 'Adults' 
               ,'Kids', 'Kids'
               )
df$age_rating = as.character(
  factor(
    df$rating,
    levels = rating,
    labels = age_rating
  )
)

treeAge <- count(df, age_rating)
treeAge$labels = paste(treeAge$age_rating,  treeAge$n, sep = "\n ")


treemap(treeAge,
        index = c("labels"), vSize = "n", title = "Porównanie Kategorii wiekowych",
        fontsize.labels=c(15,12), fontcolor.labels=c("white"), palette = c("#0000AA", "#3366CC", "#6633FF"))

# Jak widać zdecydowana większość zawartości netflixa jest przeznaczona dla dorosłych

# Jak się to ma do podziału na seriale i filmy?

tvShows <- df[df$type == "TV Show", ]
movies <- df[df$type == "Movie", ]

tvShowsRating <- count(tvShows, age_rating)
moviesRating <- count(movies, age_rating)

tvShowsRating$labels = paste(tvShowsRating$age_rating,  tvShowsRating$n, sep = "\n ")
moviesRating$labels = paste(moviesRating$age_rating,  moviesRating$n, sep = "\n ")


treemap(tvShowsRating,
        index = c("labels"), vSize = "n", title = "Porównanie Kategorii wiekowych w serialach",
        fontsize.labels=c(15,12), fontcolor.labels=c("white"), palette = c("#0000AA", "#3366CC", "#6633FF"))

# W serialach jest mniej tych dla nastolatków niż dla dzieci

treemap(moviesRating,
        index = c("labels"), vSize = "n", title = "Porównanie Kategorii wiekowych w filmach",
        fontsize.labels=c(15,12), fontcolor.labels=c("white"), palette = c("#0000AA", "#3366CC", "#6633FF"))


# przygotowanie danych do wykresu z dodawania treści na przestrzeni lat


splitedDate <- sub('.*, ', '', df$date_added)
plotYear <- data.frame(type = "All", splitedDate)
plotYear <- count(plotYear, splitedDate, type)

plotYear <- slice(plotYear, 1:(n() - 1)) 

splitedDate <- sub('.*, ', '', tvShows$date_added)
plotTvYear <- data.frame(type = "TV Show", splitedDate)
plotTvYear <- count(plotTvYear, splitedDate, type)

plotMissing <- data.frame(type = "TV Show", splitedDate = c("2009", "2010", "2011", "2012"), n = 0)
plotTvYear <- rbind(plotTvYear[1,], plotMissing, plotTvYear[-(1),])


plotTvYear <- slice(plotTvYear, 1:(n() - 1))

splitedDate <- sub('.*, ', '', movies$date_added)
plotMoviesYear <- data.frame(type = "Movie", splitedDate)
plotMoviesYear <- count(plotMoviesYear, splitedDate, type)
plotYearAll <- rbind(plotYear, plotMoviesYear, plotTvYear)

ggplot(plotYearAll, aes(x = splitedDate, y = n, color = type, label = n))  +
  geom_point() + 
  geom_line(data = plotYear, aes(group = 1)) +
  geom_line(data = plotMoviesYear, aes(group = 2)) +
  geom_line(data = plotTvYear, aes(group = 3)) +
  geom_text(data = plotYearAll[plotYearAll$splitedDate == c("", "2016", "2017", "2018", "2019", "2020", "2021"),], color = "black", vjust = -0.5) +
  xlab("Rok")+ylab("Ilość") +
  ggtitle("Dodawanie treści na przestrzeni lat") +
  theme(legend.position = c(0.1,0.9)) +
  scale_color_manual(values = c("red", "blue", "green"))
  


#plot(plotYear$splitedDate, plotYear$n,  type = "b", col = 2 , lwd = 3, pch = 1, xlab = "Year", ylab = "Amount")
#lines(plotMoviesYear$splitedDate, plotMoviesYear$n, type = "b", col = 4 , lwd = 3, pch = 1)
#lines(plotTvYear$splitedDate, plotTvYear$n, type = "b", col = 3 , lwd = 3, pch = 1)

#legend(x = "topleft",legend = c("All", "Movies", "Tv Shows"), lty = 1, col = c(2, 4, 3), lwd = 3)
#title("Dodawanie treści na przestrzeni lat")
#text(plotYear$splitedDate[-8:0], plotYear$n[-8:0] + 55, labels=plotYear$n[-8:0], col = 1)
#text(plotMoviesYear$splitedDate[-8:0], plotMoviesYear$n[-8:0] + 60, labels=plotMoviesYear$n[-8:0], col = 1)
#text(plotTvYear$splitedDate[-4:0], plotTvYear$n[-4:0] -60, labels=plotTvYear$n[-4:0], col = 1)

# Zauważalny wzrost zawartości rozpoczął się w 2015 roku
# Wzrost liczby filmów w serwisie Netflix jest znacznie wyższy niż w przypadku programów telewizyjnych
# W latach 2018: 2020 co roku dodano ponad 1200 nowych filmów

# A w którym miesiącu najczęściej były te treści dodawane?
splitedMonth <- sub(' .*', '', df$date_added)
plotMonth <- data.frame(splitedMonth)
plotMonth <- count(plotMonth, splitedMonth)
plotMonth <- slice(plotMonth, which(plotMonth$splitedMonth != "Brak")) 

plotMonth$num = as.character(
  factor(
    plotMonth$splitedMonth,
    levels = month.name,
    labels = c(1:12)
  )
)

plotMonth <- plotMonth[order(as.numeric(plotMonth$num)),]

ggplot(data=plotMonth ,aes(x = reorder(splitedMonth, n), y = n, fill = n)) +
  geom_bar(stat = "identity",width = 1,colour = "white",size = 0.7) +
  coord_polar()+
  scale_fill_gradient(low = "white", high = "dodgerblue3") +
  xlab("")+ylab("") +
  ggtitle("Dodawanie treści na miesiącach") + 
  theme(plot.title = element_text(size=20, face="bold"),) + 
  guides(fill=guide_legend(title="Amount"))

# Jak widać najczęściej treść na Netflixie pojawiała się w Grudniu oraz Lipcu

# Podziały na gatunki

dfCategories <- df %>% 
  select(c('show_id','type','listed_in')) %>% 
  separate_rows(listed_in, sep = ',') %>%
  rename(Category = listed_in)
dfCategories$Category <- trimws(dfCategories$Category)
head(dfCategories)

dfUniqueCategories <- dfCategories %>% group_by(type,Category) %>%  summarise()

# Korelacja gatunków w filmach
Category1 = subset(dfUniqueCategories, type == 'Movie')$Category
Category2 = subset(dfUniqueCategories, type == 'Movie')$Category
dfCategoryCorrelationsMovies <- data.frame(expand_grid(type = 'Movie', Category1, Category2))

# Korelacja gatunków w serialach
Category1 = subset(dfUniqueCategories, type == 'TV Show')$Category
Category2 = subset(dfUniqueCategories, type == 'TV Show')$Category
dfCategoryCorrelationsTv <- data.frame(expand_grid(type = 'TV Show', Category1, Category2))

# Połączenie obu data.frame'ów
dfCategoryCorrelations <- rbind(dfCategoryCorrelationsMovies,dfCategoryCorrelationsTv)

# Zliczamy wszystkie połączenia gatunków
dfCategoryCorrelations$matchedCount <- apply(dfCategoryCorrelations, MARGIN = 1,FUN = function(x) {
  length(intersect(subset(dfCategories, type == x['type'] & Category == x['Category1'])$show_id,
                   subset(dfCategories, type == x['type'] & Category == x['Category2'])$show_id))})

# Usuwamy korelacje gatunku samego ze sobą oraz wszystkich tych których była zerowa
dfCategoryCorrelations <- subset(dfCategoryCorrelations, (as.character(Category1) < as.character(Category2)) & (matchedCount > 0))

ggplot(subset(dfCategoryCorrelations, type == 'Movie'), aes(x = Category1, y = Category2, fill = matchedCount)) + 
  geom_tile() + 
  ggtitle("Korelacja Gatunków Filmów") + 
  theme(plot.title = element_text(size=20, face="bold"),) +
  scale_fill_distiller(palette = "Spectral") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))

# Najwięcej filmów posiada gatunki "International Dramas"
# Można zauważyć, że "Wiara i duchowość" nie pokrywają się z filmami "LGBTQ".

ggplot(subset(dfCategoryCorrelations, type == 'TV Show'), aes(x = Category1, y = Category2, fill = matchedCount)) + 
  geom_tile() + 
  ggtitle("Korelacja Gatunków Seriali") + 
  theme(plot.title = element_text(size=20, face="bold"),) +
  scale_fill_distiller(palette = "Spectral") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))

# Tutaj tak samo najwięcej połączeń gatunków w serialach to "International Dramas"




# Wyznaczmy top 15 dla krajów

topCountries <- df %>%  
  separate_rows(country,sep="(, |,)") %>%
  group_by(country) %>%  
  summarise(count=n()) %>% 
  arrange(desc(count))


# Widzimy że jest sporo braków, ale mimo wszystko spróbujemy bez tych danych
topCountries <- slice(topCountries, which(topCountries$country != "Brak danych"))

topCountriesNames <- as.vector(topCountries$country[1:15])

toPlot <- df %>%  
  separate_rows(country, sep = ", ") %>%
  filter(country %in% topCountriesNames) %>% 
  mutate(country=factor(country, levels = topCountriesNames)) %>% 
  group_by(country, type) %>% 
  summarise(count = n()) 

ggplot(toPlot, aes(x = reorder(country, count), y = count, fill = type)) +
  scale_fill_manual(values = c("#3366CC", "#0000AA")) + 
  geom_bar(stat = 'identity') +
  coord_flip()

# Jak widać Stany zjednoczone górują ilością.
# W większości przypadków widać przewagę Filmów nad serialami
# Jednak widać, że nie w każdym przypadku tak jest.
# Które kraje mają więcej seriali niż filmów na tej platformie?

ggplot(toPlot, aes(x = country, y = count, fill = type)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_manual(values = c("#89ABE3", "#3366CC")) + 
  coord_flip()

# Jak widać na wykresie procentowym krajami które mają na netflixie więcej seriali niż filmów to Japonia oraz Korea Południowa

toCategory <- df %>% 
  filter(type=='Movie') %>% 
  separate_rows(listed_in,sep = ', ') %>% 
  group_by(listed_in) %>% 
  count()

ggplot(toCategory, aes(x = reorder(listed_in, n), y = n, fill = n)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient(low = "grey", high = "dodgerblue3") +
  xlab("Kategorie") + ylab("Ilość") +
  coord_flip()

