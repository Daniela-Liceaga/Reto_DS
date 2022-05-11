
# Reto --------------------------------------------------------------------

places_details <-read.csv("places_details.csv")
places_review <- read.csv("places_reviews.csv")
reto_precios <- read.csv("reto_precios.csv")
library(dplyr)
library(psych)
library(GGally)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(car)
library(tidytext)
library(wordcloud)
library(RColorBrewer)


# Pregunta 1 --------------------------------------------------------------

#Pregunta 1 (Obligatoria)
#El archivo reto_precios.csv contiene información sobre precios de departamentos nuevos dentro de la CDMX, 
#junto con diversas características del inmueble. Para esta pregunta, requerimos que nos puedas explicar los factores que influyen 
#en el precio por metro cuadrado de cada vivienda (Hint: Existe información externa que te podría ser útil para resolver esta pregunta).

reto_precios$colonia <- ifelse(reto_precios$location %like% 'Roma Norte', 0,ifelse(reto_precios$location %like% 'Roma Sur', 1,2))
reto_precios$amenities <- ifelse(is.na(reto_precios$amenities), 0,reto_precios$amenities)
reto_precios$cellars <- ifelse(is.na(reto_precios$cellars), 0,reto_precios$cellars)

#es el unico depto que sobresale por esos m2, además de las colonias que no sean la roma por la data las 
reto_precios_limpia <- reto_precios %>% filter(m2 != 7210, colonia != 2) %>% 
  select (vendor,final_price,since_value,amenities,bathrooms,cellars,parking_lots,num_bedrooms,m2,price_square_meter,colonia)


#regresion lineal sobre m2 


datos <- reto_precios_limpia

datos %>% group_by(colonia) %>% summarize(promedio =mean(price_square_meter),mediana = median(price_square_meter))


ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

modelo1 <- lm(final_price ~ m2 + parking_lots + num_bedrooms, data = datos )
summary(modelo1)

modelo2 <- lm(final_price ~ m2 + parking_lots + num_bedrooms +  colonia, data = datos )
summary(modelo2)


plot1 <- ggplot(data = datos, aes(m2, modelo1$residuals)) +
  geom_point() + geom_smooth(color = "darkblue") + geom_hline(yintercept = 0) +
  theme_bw()

plot2 <- ggplot(data = datos, aes(parking_lots , modelo1$residuals)) +
  geom_point() + geom_smooth(color = "red") + geom_hline(yintercept = 0) +
  theme_bw()

plot3 <- ggplot(data = datos, aes(num_bedrooms, modelo1$residuals)) +
  geom_point() + geom_smooth(color = "green") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1,plot2,plot3)


plot4 <-ggplot(data = datos, aes(m2 + parking_lots + num_bedrooms, final_price )) +
  geom_point() + geom_smooth(method = "lm", colour = "red") 
grid.arrange(plot4)


#como estaban las ubicaciones
mapa_mundo <- map_data("world")

mapa_mundo%>%
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group),
               fill = "grey",
               color = "white") +
  geom_point(data= reto_precios, 
             aes(x=lon, y = lat, size = m2)) +
  scale_size_continuous(name = "price_square_meter") +
  ggtitle( "ubicaciones") +
  coord_fixed (xlim= c(-120, -98),
               ylim= c(16,33))


# Pregunta 2 --------------------------------------------------------------


#Pregunta 2 (Opcional)
#Los archivos places_details.csv y places_reviews.csv contienen información sobre reseñas que han dejado diversos usuarios
#a múltiples restaurantes. Para este ejercicio, solicitamos que resumas los principales temas que mencionan los comensales 
#al momento de dejar una reseña (Hint: Sabor, variedad, servicio, etc)

places_review<- places_review %>% filter(!is.na(rating))

places_general <- merge( places_review,places_details, by = 'place_id' ,all.x = TRUE)

tipo_lugar<-places_general %>% group_by(business_status) %>% 
  summarise(count=n()) %>% arrange(desc(count))

languages<-places_general %>% group_by(language) %>% 
  summarise(count=n()) %>% arrange(desc(count))

cor.test(places_general$rating.x,places_general$rating.y)

rating <-places_general %>%group_by(rating.x) %>% 
       summarise(count=n()) %>% arrange(desc(rating.x))

hist(places_general$rating.x)

#places best rating
pl_b_rat <-places_general %>% filter (rating.x %in% c(5,4)) %>% group_by(place_id,rating.x) %>% 
  summarise(count=n()) %>% arrange(desc(rating.x))

ggplot(data =pl_b_rat[1:30,]) + geom_col(mapping = aes(x=place_id, y =count), fill ='blue') + coord_flip()

pl_b_rat_positivo_es<- places_general %>% filter (rating.x %in% c(5), language =='es',business_status =='OPERATIONAL')

texto_analisis_positivo <- tibble(texto =pl_b_rat_positivo_es$text)

vacias <- get_stopwords("es", "snowball")


analisis_positivo<- texto_analisis_positivo %>%
  unnest_tokens(palabra, texto) %>%
  count(palabra, sort = T) 

analisis_positivo_1<- analisis_positivo[is.na(match(analisis_positivo$palabra,vacias$word)),] %>%
  with(wordcloud(palabra,
                 n,
                 max.words = 50,
                 color = brewer.pal(8, "Dark2")))


#worst rating
pl_w_rat <-places_general %>% filter (rating.x %in% c(1,2)) %>% group_by(place_id,rating.x) %>% 
  summarise(count=n()) %>% arrange(rating.x)

ggplot(data =pl_w_rat [1:30,]) + geom_col(mapping = aes(x=place_id, y =count), fill ='darkblue') + coord_flip()
pl_w_rat_negativo <- places_general %>% filter (rating.x %in% c(1,2), language =='es',business_status =='OPERATIONAL')

texto_analisis_negativo <- tibble(texto =pl_w_rat_negativo$text)


analisis_negativo<- texto_analisis_negativo %>%
  unnest_tokens(palabra, texto) %>%
  count(palabra, sort = T) 

analisis_negativo_1<- analisis_negativo[is.na(match(analisis_negativo$palabra,vacias$word)),] %>%
  with(wordcloud(palabra,
                 n,
                 max.words = 50,
                 color = brewer.pal(8, "Dark2")))










