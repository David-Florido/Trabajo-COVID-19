library(dplyr)
library(ggplot2)
library(lubridate)  #Trabajo con fechas
library(scales) #Formato
library(gridExtra)


casos_internacionales <- read.csv("D:/Dave/UMA/Working on it/LCC/COVID/datasets/worldwide_cases")
str(casos_internacionales)

casos_internacionales <- casos_internacionales %>% 
  select(-c(day, month, year, geoId, countryterritoryCode))

names(casos_internacionales) <- c("fecha", "casos", "muertes", "pais", "población (2018)", "continente")

casos_internacionales <- casos_internacionales %>% filter(casos > 0)

casos_internacionales$fecha <- casos_internacionales$fecha %>%
  as.Date(format = "%d/%m/%Y") 

str(casos_internacionales)
casos_internacionales <- casos_internacionales %>%  arrange(fecha)

casos_internacionales <- casos_internacionales %>%
  mutate(pais = ifelse(continente == "Other", "Japan", pais), 
         continente = ifelse(continente == "Other", "Asia", continente))

casos_internacionales %>% filter(continente == "Other")
casos_internacionales %>% filter(pais == "Japan")



#library(translateR) #Traductor
#casos_internacionales <- translate(dataset = casos_internacionales,
#                                content.field = pais,
#                               google.api.key = my.api.key,
#                               source.lang = 'en',
#                               target.lang = 'es')



fechaInicial <- format(casos_internacionales$fecha[1], "%d/%m/%Y")
fechaFinal <- format(casos_internacionales$fecha[nrow(casos_internacionales)], "%d/%m/%Y")


#dias_en_meses <- days_in_month(unique(month(casos_internacionales$fecha)))
#seq_vector <- Vectorize(seq.default, vectorize.args = c("from", "to"))  #Definicion funcion seq pero para vectores
#dias_por_mes <- c(seq_vector(from = c(1, 1, 1, 1, 1, 1, 1), to = dias_en_meses, by = 1))


Sys.setlocale(locale = "Spanish") #Usar lenguaje español
plot <- ggplot(casos_internacionales, aes(x=fecha, y=casos), color=vore) +
  coord_cartesian() +
  geom_col() +
  theme_linedraw() + 
  labs(x = "fecha", y = "número de casos") +
  scale_x_date(labels = date_format("%d-%B"));
plot


casos_internacionales <- filter(casos_internacionales, fecha > as.Date("2020/01/15"))
plot2 <- ggplot(casos_internacionales, aes(x=fecha, y=casos), color=vore) +
  coord_cartesian() +
  geom_col() +
  theme_linedraw() + 
  labs(x = "fecha", y = "número de casos") +
  scale_x_date(labels = date_format("%d-%B"));
plot2


casos_Africa <- filter(casos_internacionales, continente == "Africa")
casos_America <- filter(casos_internacionales, continente == "America")
casos_Asia <- filter(casos_internacionales, continente == "Asia")
casos_Europa <- filter(casos_internacionales, continente == "Europe")
casos_Oceania <- filter(casos_internacionales, continente == "Oceania")

#show_col(hue_pal()(5))
pAf <- ggplot(casos_Africa, aes(x=fecha, y=casos), color=vore) +
  coord_cartesian() +
  geom_col(fill = "#F8766D") +
  theme_linedraw() + 
  scale_x_date(labels = date_format("%d-%B")) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank());


pAm <- ggplot(casos_America, aes(x=fecha, y=casos), color=vore) +
  coord_cartesian() +
  geom_col(fill = "#A3A500") +
  theme_linedraw() + 
  scale_x_date(labels = date_format("%d-%B")) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank());


pAs <- ggplot(casos_Asia, aes(x=fecha, y=casos), color=vore) +
  coord_cartesian() +
  geom_col(fill = "#00BF7D") +
  theme_linedraw() + 
  scale_x_date(labels = date_format("%d-%B")) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank());


pEu <- ggplot(casos_Europa, aes(x=fecha, y=casos), color=vore) +
  coord_cartesian() +
  geom_col(fill = "#00B0F6") +
  theme_linedraw() + 
  scale_x_date(labels = date_format("%d-%B")) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank());


pOc <- ggplot(casos_Oceania, aes(x=fecha, y=casos), color=vore) +
  coord_cartesian() +
  geom_col(fill = "#E76BF3") +
  theme_linedraw() + 
  scale_x_date(labels = date_format("%d-%B")) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank());


pie <- ggplot(casos_internacionales, aes(x="", y=casos, fill=continente))+
  geom_bar(width = 1, stat = "identity") +
  labs(y = "total de casos") +
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) + 
  coord_polar("y");


grid.arrange(pAf, pAm, pAs, pEu, pOc, pie, nrow=3, ncol = 2)

