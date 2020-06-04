library(tidyverse)
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



############################################################################
############################################################################
############################################################################

library(arules)

casos_sintomas <- read.csv("D:/Dave/UMA/Working on it/LCC/COVID/novel-corona-virus-2019-dataset/COVID19_line_list_data.csv")


casos_sintomas <- casos_sintomas %>%
  filter(!(death==0 & recovered==0)) %>%
  select(c(gender, age, symptom, recovered)) %>%
  drop_na() %>%
  mutate(recovered = factor(ifelse(recovered == 0, 0, 1))) %>%
  mutate(symptom = factor(ifelse(symptom == "", NA, symptom))) %>%
  separate(
    symptom,
    into = c("symptom1","symptom2","symptom3"),
    sep = ",",
    remove = TRUE,
    convert = FALSE
  )

casos_sintomas$gender <- as.factor(casos_sintomas$gender)
casos_sintomas$age <- discretize(casos_sintomas$age, method = "interval", breaks = 4, dig.lab = 0, ordered_result = TRUE)
casos_sintomas$symptom1 <- as.factor(casos_sintomas$symptom1)
casos_sintomas$symptom2 <- as.factor(casos_sintomas$symptom2)
casos_sintomas$symptom3 <- as.factor(casos_sintomas$symptom3)
casos_sintomas$recovered <- as.factor(casos_sintomas$recovered)
str(casos_sintomas)

  

reglas <- apriori(casos_sintomas,
                  parameter = list(supp=0.1,conf=0.8, minlen=2, target="rules"),#minlen = 2 para que lhs no sea vacio
                  appearance = list(rhs=c("recovered=0","recovered=1")))#reglas para ver recuperación
inspect(reglas)
reglas.pruned <- reglas[!is.redundant(reglas)]

inspect(reglas.pruned)



############################################################################
############################################################################
############################################################################


casos_internacionales <- read.csv("D:/Dave/UMA/Working on it/LCC/COVID/datasets/worldwide_cases")

casos_internacionales <- casos_internacionales %>% 
  select(-c(day, month, year, geoId, countryterritoryCode))

names(casos_internacionales) <- c("fecha", "casos", "muertes", "pais", "poblacion", "continente")

casos_internacionales <- casos_internacionales %>% filter(casos > 0)

casos_internacionales$fecha <- casos_internacionales$fecha %>%
  as.Date(format = "%d/%m/%Y") 

casos_internacionales <- casos_internacionales %>%  arrange(fecha)

casos_internacionales <- casos_internacionales %>%
  mutate(pais = ifelse(continente == "Other", "Japan", pais), 
         continente = ifelse(continente == "Other", "Asia", continente))

#casos_internacionales <- filter(casos_internacionales, fecha > as.Date("2020/03/01"))

casos_internacionales_por_fecha <- casos_internacionales %>%
  drop_na() %>%
  group_by(fecha) %>%
  summarise(total_casos=sum(casos), poblacion_total=sum(log(poblacion)))

y <- casos_internacionales_por_fecha$total_casos
x <- as.integer(casos_internacionales_por_fecha$fecha) + casos_internacionales_por_fecha$poblacion_total

casos_fecha <- lm(y~x+I(x^2), data=casos_internacionales_por_fecha)
summary(casos_fecha)


dates <- as.integer(casos_internacionales_por_fecha$fecha)
as.Date(x, origin)

p <- casos_fecha %>%
  ggplot(aes(x, y)) +
  geom_point() +
  stat_smooth(method="lm", formula="y~(x+poly(x,2))") +
  labs(y="total de casos", x = "fecha codificada + poblacion total")
p
