# Importaci�n de librerias 
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

# Creaci�n de token de acceso API Twitter
token <- create_token(
  app = "elecciones_presidenciales",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)
rm(access_secret, access_token, consumer_secret, consumer_key)

# Extracci�n de timelines
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

#Se renombran las variables con nombres m�s pr�cticos
tweets <- tweets %>% rename(autor = screen_name, fecha = created_at,
                            texto = text, tweet_id = status_id)
#Selecci�n de variables
tweets <- tweets %>% select(autor,fecha,texto,tweet_id)
head(tweets, 20)

rm(limpiar_tokenizar)

limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a min�sculas
  nuevo_texto <- tolower(texto)
  # removemos emojis
  # nuevo_texto <- iconv(nuevo_texto, to = "ASCII", sub = "")
  # Removemos stopwords
  nuevo_texto <- removeWords(nuevo_texto, words = stopwords("spanish"))
  #quitamos las tildes 
  nuevo_texto<- stri_trans_general(nuevo_texto,"Latin-ASCII")
  # Eliminaci�n de p�ginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminaci�n de signos de puntuaci�n
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Removemos stopwords
  nuevo_texto <- removeWords(nuevo_texto, words = stopwords("spanish"))
  # Eliminaci�n de n�meros
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminaci�n de espacios en blanco m�ltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  nuevo_texto <- str_trim(nuevo_texto)
  # Tokenizaci�n por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminaci�n de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto) 
}


# Se aplica la funci�n de limpieza y tokenizaci�n a cada tweet
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













# Se aplica la funci�n de limpieza y tokenizaci�n a cada tweet
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

### Palabras m�s utilizadas por usuario ###
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

### Representaci�n gr�fica de las frecuencias ###
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
  labs (title= "Palabras m�s utilizadas por cada candidato", 
        subtitle="al hacer una publicaci�n en Twitter", 
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
        subtitle="al hacer una publicaci�n en Twitter", 
        y="Gabriel Boric Font", 
        x="Jos� Antonio Kast", 
        caption= "Datos extraidos desde las timeline de cada candidato, hasta 18/12/2021.\n Autor: Roberto Mu�oz Campos"
  )

grid.arrange(p1, nrow = 1)

palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor=="joseantoniokast") %>%
                                       select(token), tweets_tidy %>% filter(autor=="gabrielboric") %>%
                                       select(token)) %>% nrow()
paste("N�mero de palabras comunes entre jose antonio kast y gabriel boric", palabras_comunes)


#Se crea un DF con las cuentas que m�s se mencionan por los usuarios
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

#eliminamos las variables que m�s se repiten (est�n gigantes en la nube de palabras)
menciones <- filter(menciones, menciones != '@s')
  
#Nube de palabras con las cuentas + mencionadas
wordcloud2(menciones, size = 0.5, shape = 'diamond') %>% 
  htmlwidgets::prependContent(htmltools::tags$h1("Cuentas m�s mencionadas por el candidato Gabriel Boric")) 

