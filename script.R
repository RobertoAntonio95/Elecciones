# Importación de librerias 
library(rtweet)
library(tidyverse)
library(lubridate)
library(tm)
library(dplyr)

# Llaves de acceso a la API
consumer_key = 'CyecHKWtEtGyxFWvWirv96KVq'
consumer_secret = 'koFsmD60E3TvXNd6w2BtAHHprj8AAF7HUgivLTJIVvMagVhfdD'
access_token = '1382170144978436099-mWK5IgIlqoaVKTexksaFlWFgeIbNAk'
access_secret = '8J7fgbaNoacrUPjqMoi5ZtGWxugEdBA8ZA2EunPFwbdXD'

# Creación de token de acceso API Twitter
token <- create_token(
  app = "elecciones_presidenciales",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)
rm(access_secret, access_token, consumer_secret, consumer_key)

# Extracción de timelines
kast <- get_timeline(user = "@joseantoniokast", n = 50000, parse = TRUE,
                            check = TRUE, include_rts = FALSE)

boric <- get_timeline(user = "@gabrielboric", n = 50000, parse = TRUE,
                     check = TRUE, include_rts = FALSE)

# Ajustamos la fecha a horario santiago de Chile
kast <- kast %>% mutate(created_at = with_tz(created_at, tz = "America/Santiago"))
boric <- boric %>% mutate(created_at = with_tz(created_at, tz = "America/Santiago"))

saveRDS(boric,"boric.rds")
saveRDS(kast,"kast.rds")

tweets <- bind_rows(boric, kast)

# ------ analisis -------

# Cantidad de tweets por cada candidato
tweets %>% group_by(screen_name) %>% summarise(n())

#Columnas disponibles
colnames(tweets) %>% view()

#Se renombran las variables con nombres más prácticos
tweets <- tweets %>% rename(autor = screen_name, fecha = created_at,
                            texto = text, tweet_id = status_id)
#Selección de variables
tweets <- tweets %>% select(autor,fecha,texto,tweet_id)
head(tweets, 20)

rm(limpiar_tokenizar)

limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # removemos emojis
  # nuevo_texto <- iconv(nuevo_texto, to = "ASCII", sub = "")
  # Removemos stopwords
  nuevo_texto <- removeWords(nuevo_texto, words = stopwords("spanish"))
  #quitamos las tildes 
  nuevo_texto<- stri_trans_general(nuevo_texto,"Latin-ASCII")
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Removemos stopwords
  nuevo_texto <- removeWords(nuevo_texto, words = stopwords("spanish"))
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  nuevo_texto <- str_trim(nuevo_texto)
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto) 
}


# Se aplica la función de limpieza y tokenización a cada tweet
lexico_nrc <- lexico_nrc %>% dplyr::mutate(texto_tokenizado = map(.x = palabra,
                                                          .f = limpiar_tokenizar))


lexico <- lexico_nrc %>% select(-word, -palabra) %>% unnest(cols = c(texto_tokenizado))
lexico <- lexico %>% rename(palabra = texto_tokenizado)
head(lexico) 


names (lexico)[2] = "words"




tuits_nrc <-  tweets_tidy %>%
  unnest_tokens(input = "token", output = "words") %>%
  inner_join(lexico, ., by = "words") %>%
  mutate(Tipo = sentimiento)

write.csv(tuits_nrc, file="C:/Users/ramc_/Desktop/Analisis politico/datos.csv")













# Se aplica la función de limpieza y tokenización a cada tweet
tweets <- tweets %>% dplyr::mutate(texto_tokenizado = map(.x = texto,
                                                          .f = limpiar_tokenizar))






tweets_tidy <- tweets %>% select(-texto) %>% unnest(cols = c(texto_tokenizado))
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy) 

tweets_tidy <- tweets %>% select(-texto) %>% unnest(cols = c(texto_tokenizado))
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy) 

## Total de palabras escritas por cada usuario ##
tweets_tidy %>% group_by(autor) %>% summarise(n = n()) 
tweets_tidy %>%  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw() 

### Palabras distintas utilizadas por cada usuario ###
tweets_tidy %>% select(autor, token) %>% distinct() %>%  group_by(autor) %>%
  summarise(palabras_distintas = n()) 

tweets_tidy %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()                        

### Palabras más utilizadas por usuario ###
palabras_kast <- select(as_data_frame(tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
                                   filter(autor == "joseantoniokast") %>%
                                   top_n(100, n) %>% arrange(autor, desc(n))), -autor)

wordcloud2(data=palabras_kast, size=1.3)


palabras_boric <- select(as_data_frame(tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
                                        filter(autor == "gabrielboric") %>%
                                        top_n(100, n) %>% arrange(autor, desc(n))), -autor)

wordcloud2(data=palabras_boric, size=1.3)


install.packages("ggthemes")
library(ggthemes)

### Representación gráfica de las frecuencias ###
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>% #filter(autor == "joseantoniokast")%>%
  top_n(30, n) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_base()+
  labs(y = "Frecuencia", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  geom_text(
    aes(label = n, y = n + 0.05),
    position = position_dodge(1), hjust = 1 , vjust = 0.4 , size= 4.7
  )+
  facet_wrap(~autor,scales = "free", ncol = , drop = TRUE)+
  labs (title= "Palabras más utilizadas por cada candidato", 
        subtitle="al hacer una publicación en Twitter", 
        x="Frecuencia", 
        y="Palabras", 
        caption= "Datos extraidos desde las timeline de cada candidato, hasta 18/12/2021."
  )





# ----- correlaciones entre palabras

install.packages(gridExtra)
install.packages(scales)

library(gridExtra)
library(scales)

tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)


cor.test(~ joseantoniokast + gabrielboric, method = "pearson", data = tweets_spread)


p1 <- ggplot(tweets_spread, aes(joseantoniokast, gabrielboric)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5, size=3) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red", size = 1) +
  theme_base() +
  labs (title= "Similaridad entre palabras publicadas escritas por los candidatos", 
        subtitle="al hacer una publicación en Twitter", 
        y="Gabriel Boric Font", 
        x="José Antonio Kast", 
        caption= "Datos extraidos desde las timeline de cada candidato, hasta 18/12/2021.\n Autor: Roberto Muñoz Campos"
  )


grid.arrange(p1, nrow = 1)


palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor=="joseantoniokast") %>%
                                       select(token), tweets_tidy %>% filter(autor=="gabrielboric") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre jose antonio kast y gabriel boric", palabras_comunes)







#Se crea un DF con las cuentas que más se mencionan por los usuarios
menciones_kast <- boric %>%
  mutate(menciones=purrr::map(.x=text,
                              pattern='@\\w+',
                              .f=str_extract_all)) %>% 
  dplyr::select(created_at,screen_name,menciones) %>% 
  unnest(menciones) %>% unnest(menciones) %>%  
  mutate(menciones=str_to_lower(menciones))

#DF de 2 variables que cuenta cuantas veces se ha mensionado a la persona
menciones <- menciones_kast %>%  group_by(menciones) %>% 
  summarise(total = n()) %>% arrange(desc(total))

#eliminamos las variables que más se repiten (están gigantes en la nube de palabras)
menciones <- filter(menciones, menciones != '@s')
  
#Nube de palabras con las cuentas + mencionadas
wordcloud2(menciones, size = 0.5, shape = 'diamond') %>% 
  htmlwidgets::prependContent(htmltools::tags$h1("Cuentas más mencionadas por el candidato Gabriel Boric")) 




















# Número de veces que aparece cada término por tweet
tweets_tf <- tweets_tidy %>% group_by(tweet_id, token) %>% summarise(n = n())

# Se añade una columna con el total de términos por tweet
tweets_tf <- tweets_tf %>% mutate(total_n = sum(n))

# Se calcula el tf
tweets_tf <- tweets_tf %>% mutate(tf = n / total_n )
head(tweets_tf)

### Inverse Document Frequency ###
total_documentos <- tweets_tidy$tweet_id %>% unique() %>% length()
total_documentos

# Número de documentos en los que aparece cada término
tweets_idf <- tweets_tidy %>% distinct(token, tweet_id) %>% group_by(token) %>%
  summarise(n_documentos = n())

# Cálculo del idf
tweets_idf <- tweets_idf %>% mutate(idf = n_documentos/ total_documentos) %>%
  arrange(desc(idf))
head(tweets_idf)

### Term Frequency - Inverse Document Frequency ###
tweets_tf_idf <- left_join(x = tweets_tf, y = tweets_idf, by = "token") %>% ungroup()
tweets_tf_idf <- tweets_tf_idf %>% mutate(tf_idf = tf * idf)
tweets_tf_idf %>% head(20) %>% view()
#### FIN PARTE 5 ####

### INICIO PARTE 6 ####
#### Separación de los datos en entrenamiento y test ####
tweets_joseantonio_daniel<- tweets %>% filter(autor %in% c("joseantoniokast", "danieljadue"))

set.seed(123)
train <- sample(x = 1:nrow(tweets_joseantonio_daniel), size = 0.8 * nrow(tweets_joseantonio_daniel))
tweets_train <- tweets_joseantonio_daniel[train, ]
tweets_test  <- tweets_joseantonio_daniel[-train, ]

#Es importante verificar que la proporción de cada grupo es similar en el set de 
#entrenamiento y en el de test.
table(tweets_train$autor) / length(tweets_train$autor)
table(tweets_test$autor) / length(tweets_test$autor)


rm(palabras_comunes,total_documentos,limpiar_tokenizar)







install.packages("syuzhet")
library(syuzhet)














#### INICIO PARTE 7 ####
### Sentimiento ####
install.packages("tidytext")
library(tidytext)
sentimientos <- get_sentiments(lexicon = "bing")
head(sentimientos)

sentimientos <- sentimientos %>%
  mutate(valor = if_else(sentiment == "negative", -1, 1))

#### Sentimiento promedio de cada tweet ####

tweets_sent <- inner_join(x = tweets_tidy, y = sentimientos,
                          by = c("token" = "word"))

### Se suman los sentimientos de las palabras que forman cada tweet. ###
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  head()

#### Porcentaje de tweets positivos, negativos y neutros por autor ####

porcentaje_sentimiento <- tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(autor) %>%
  summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
            neutros = 100 * sum(sentimiento_promedio == 0) / n(),
            negativos = 100 * sum(sentimiento_promedio  < 0) / n())

porcentaje_sentimiento %>%
  ungroup() %>%
  gather(key = "sentimiento", value = "valor", -autor) %>%
  ggplot(aes(x = autor, y = valor, fill = sentimiento)) + 
  geom_col(position = "dodge", color = "black") + coord_flip() +
  theme_bw() +
  labs(title="Analizando la personalidad de personajes políticos",
       subtitle="",
       caption="Gráfico hecho por @botquehabla",
       x="",
       y="")

textostop <- tm::stopwords(kind="es") #stopwords de la librer?a TM para tener en espa?ol
textostop <- as_tibble(textostop) # lo convierto a tibble

## cambio de nombre a words la columna ##
colnames(textostop)
colnames(textostop)[1] <- "token"

dic_nrc <- get_sentiments("nrc")
colnames(dic_nrc)
colnames(dic_nrc)[1] <- "token"

sentiment_nrc_tbl <- tweets_tidy %>%
  inner_join(dic_nrc, by = "token")

library("ggjoy")
## analizando todo el tiempo disponible ##
sentiment_nrc_tbl %>% 
  ggplot() +
  geom_joy(aes(
    x = fecha,
    y = sentiment, 
    fill = sentiment),
    rel_min_height = 0.01,
    alpha = 0.7,
    scale = 3) +
  theme_joy() +
  labs(title = "Sentimiento en los posteos en el tiempo",
       x = "Fecha",
       y = "Sentimento") + 
  scale_fill_discrete(guide=FALSE)

## filtramos 2020

sentiment_nrc_tbl %>% filter(fecha > '2021/01/1') %>% 
  ggplot() +
  geom_joy(aes(
    x = fecha,
    y = sentiment, 
    fill = sentiment),
    rel_min_height = 0.01,
    alpha = 0.7,
    scale = 3) +
  theme_joy() +
  labs(title = "Sentimiento en los posteos del año 2021",
       x = "Fecha",
       y = "Sentimento") + 
  scale_fill_discrete(guide=FALSE)

#Crear la función coord_radar()
#Código cortesía de Erwan Le Pennec
#http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html

coord_radar <- function (theta = "x", start = 0, direction = 1)
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x")
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

sentiment_nrc_tbl %>%
  group_by(autor, sentiment) %>%
  tally %>% 
  ggplot(aes(x=sentiment, y=n, group=autor)) +
  geom_polygon(aes(color = autor),        #geom_polygon para que cierre las líneas.
               fill = NA, size = 1.1) +  #fill=NA para relleno del polígono sea transparente.
  theme(axis.ticks.y = element_blank(),  #Elimino marcas de ejes
        axis.text.y = element_blank()) + #Elimino nombres de ejes.
  labs(title="Analizando personalidad de personajes políticos",
       subtitle="Utilizando NRC",
       caption="analiza_pln_app #PLN",
       x="",
       y="") +                           #Elimino etiquetas de ejes
  coord_radar()    

#Hacer gráfico por usuario analalizado joseantonio
sentiment_nrc_tbl %>%
  group_by(autor, sentiment) %>%
  tally %>% filter(autor=="joseantoniokast") %>% 
  ggplot(aes(x=sentiment, y=n, group=autor)) +
  geom_polygon(aes(color = autor),        #geom_polygon para que cierre las líneas.
               fill = NA, size = 1.1) +  #fill=NA para relleno del polígono sea transparente.
  theme(axis.ticks.y = element_blank(),  #Elimino marcas de ejes
        axis.text.y = element_blank()) + #Elimino nombres de ejes.
  labs(title="Analizando ersonalidad de personajes políticos",
       subtitle="Utilizando NRC",
       caption="analiza_pln_app #PLN",
       x="",
       y="") +                           #Elimino etiquetas de ejes
  coord_radar()    

#Hacer gráfico por usuario analalizado danieljadue
sentiment_nrc_tbl %>%
  group_by(autor, sentiment) %>%
  tally %>% filter(autor=="danieljadue") %>% 
  ggplot(aes(x=sentiment, y=n, group=autor)) +
  geom_polygon(aes(color = autor),        #geom_polygon para que cierre las líneas.
               fill = NA, size = 1.1) +  #fill=NA para relleno del polígono sea transparente.
  theme(axis.ticks.y = element_blank(),  #Elimino marcas de ejes
        axis.text.y = element_blank()) + #Elimino nombres de ejes.
  labs(title="Analizando ersonalidad de personajes políticos",
       subtitle="Utilizando NRC",
       caption="analiza_pln_app #PLN",
       x="",
       y="") +                           #Elimino etiquetas de ejes
  coord_radar() 

#Hacer gráfico por usuario analalizado evelynmatthei
sentiment_nrc_tbl %>%
  group_by(autor, sentiment) %>%
  tally %>% filter(autor=="evelynmatthei") %>% 
  ggplot(aes(x=sentiment, y=n, group=autor)) +
  geom_polygon(aes(color = autor),        #geom_polygon para que cierre las líneas.
               fill = NA, size = 1.1) +  #fill=NA para relleno del polígono sea transparente.
  theme(axis.ticks.y = element_blank(),  #Elimino marcas de ejes
        axis.text.y = element_blank()) + #Elimino nombres de ejes.
  labs(title="Analizando ersonalidad de personajes políticos",
       subtitle="Utilizando NRC",
       caption="analiza_pln_app #PLN",
       x="",
       y="") +                           #Elimino etiquetas de ejes
  coord_radar() 

#### FIN PARTE 7 ####