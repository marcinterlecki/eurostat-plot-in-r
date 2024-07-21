# Marcin Terlecki 122943


# pobranie bibliotek

library(ggplot2)
library(dplyr)
library(eurostat)


# pobranie danych

eurostat_raw <- get_eurostat(id = "prc_hicp_manr") %>% 
  filter(
    coicop == "CP00" 
    & time > "2000-01-31" 
    & time < "2022-10-01"
  )

# wybranie krajow

lista_krajow <- eu_countries %>%
  select(code, name) %>% 
  filter(name != "United Kingdom") %>%
  rename(geo = code, kraj = name)

eurostat <- eurostat_raw %>% 
  left_join(lista_krajow,by="geo") %>% 
  select(kraj, time, values) %>% 
  na.omit() %>%
  arrange(time)

# wykres liniowy

x11(width= 700, height= 400)
ggplot(eurostat, aes(x = time, y = values, group= kraj, color = kraj)) +
  geom_line() +
  ylab("Wartosc") +
  xlab("Czas") +
  ggtitle("Figure 1: Przebiegi HICP dla krajow EU") +
  labs(color = "Kolory krajow") +
  theme_test()


# utworzenie listy krajow
kraje <- c("Austria", "Belgium", "Bulgaria", "Czechia", "Denmark", 
           "Germany", "Estonia", "Ireland", "Greece", "Spain", "France", 
           "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania", "Luxembourg", 
           "Hungary", "Malta", "Netherlands", "Sweden", "Poland", "Portugal", 
           "Slovenia", "Slovakia", "Finland", "Romania")

# stworzenie df i przekrecenie wierszy
x <- data.frame(lapply(kraje, function(c) eurostat$values[eurostat$kraj == c]))
colnames(x) <- kraje

x <- t(x) 

# obliczenia

mat <- dist(x, method = "minkowski", p = 1.5)
hc1 <- hclust(mat)

# wykres klastrowy

x11(width = 700, height = 400)
plot(hc1, 
     main = "Clustering countries based on HICP", 
     ylab = "", 
     xlab= "")
rect.hclust(hc1, k = 4)
