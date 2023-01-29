library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)
library(dplyr)
library(ggplot2)


# Leer el chat
myChat <- rwa_read("chat.txt")

#Preparación de datos para análisis de fecha
myChat <- myChat %>%
  mutate(day = date(time)) %>%
  mutate(
    #Segmentación por mes
    season = case_when(
      day >= dmy(21032022) & day <= dmy(21062022) ~ "Primavera 2022",
      day >= dmy(22062022) & day <= dmy(23092022) ~ "Verano 2022",
      day >= dmy(23092022) & day <= dmy(20122022) ~ "Otoño 2022",
      day >= dmy(21122022) & day <= dmy(20032023) ~ "Invierno 2023",
      day >= dmy(21032023) ~ "Primavera 2020",
      T ~ "Fuera de rango"
    )
  ) %>%
  mutate(season = factor(season)) %>%
  filter(!is.na(author))

#Paleta de colores
pallete.season <- brewer.pal(8, "Set1")[c(7,5,1,3,4,2,6,8)]

#Verificar cuantos mensajes se enviaron durante el periodo de tiempo
myChat %>%
  group_by(season) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=season)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=pallete.season) + 
  ylab("Numero de mensajes") + xlab("Fecha") +
  ggtitle("Mensajes por día", "Frecuencia por estación del año") +
  theme(legend.title = element_blank(), legend.position = "Bottom")

#Mensajes por día de la semana
myChat %>%
  mutate(wday.num = wday(day), wday.name = weekdays(day)) %>%
  group_by(season, wday.num, wday.name) %>%
  count() %>%
  ggplot(aes(x = reorder(wday.name, -wday.num), y = n, fill=season)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=pallete.season) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Número de mensajes por día de la semana", "Frecuencia por estación del año") +
  theme_minimal() + 
  theme(legend.title = element_blank(), legend.position = "Bottom")

# Mantener el orden de días de la semana
weekDay <- c("domingo, lunes, martes, miercoles, jueves, viernes, sábado, domingo")
names(weekDay) <- 1:7

#Mensajes por hora al día
myChat %>% 
 mutate(hour = hour(time), wday.num = wday(day), wday.name = weekdays(day)) %>%
 count(season, wday.num, wday.name, hour) %>%
 ggplot(aes(x = hour, y = n, fill = season)) +
 geom_bar(stat = "identity") +
 scale_fill_manual(values = pallete.season) +
 ylab("Número de mesnajes") + xlab("Horario") +
 ggtitle("Número de mensajes por hora al día", "Frecuencía por estación del año") +
 facet_wrap(~wday.num, ncol=7, labeller = labeller(wda.num=weekDay)) +
 theme_minimal() +
 theme(legend.title = element_blank(), legend.position = "Bottom", panel.spacing.x = unit(0.0, "lines"))

#Nombre de los usuarios
levels(myChat$author)[2] <- "Second User"
levels(myChat$author)[1] <- "1st User"

#Mensajes por usuario
myChat %>%
  mutate(day = date(time)) %>%
  group_by(season) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n, fill = season)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pallete.season) +
  ylab("Número total de mensajes") + xlab("Usuario") +
  coord_flip() +
  ggtitle("Número total de mensajes por usuario", "¿Quién es más comunicativo? Frecuencia por estación del año") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "Bottom")

#Emoji Library
library(ggimage)

#Emoji Ranking
plotEmojis <- myChat %>% 
  unnest(c(emoji, emoji_name)) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% 
  mutate( emoji_name = str_remove(emoji_name, ":.*")) %>% 
  count(emoji, emoji_name) %>% 

#Top 30 Emojis
top_n(30, n) %>%
arrange(desc(n)) %>%

# URL imagen con el UNICODE del emoji
mutate( emoji_url = map_chr(emoji, ~paste0("https://abs.twimg.com/emoji/v2/72x72/", as.hexmode(utf8ToInt(.x)), ".png"))) 

#Plot del ranking de los emojis más usados
plotEmojis %>% 
  ggplot(aes(x=reorder(emoji_name, n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .2) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  geom_image(aes(image=emoji_url), size=.045) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
  scale_color_gradient(low="#2b83ba",high="#d7191c") +
  ylab("Número de veces que el emoji fue usado") + xlab("Emoji y significado") +
  ggtitle("Emojis más utilizados de manera general", "Emojis más usados por todos") +
  coord_flip() +
  theme_minimal() +
  theme()
  

#Emoji Rank por usuario
plotEmojis <- myChat %>%
  unnest(c(emoji, emoji_name)) %>%
  mutate( emoji = str_sub(emoji, end = 1)) %>% 
  count(author, emoji, emoji_name, sort = TRUE) %>%
  
# Plot del top 8 emojis por usuario
group_by(author) %>%
top_n(n = 8, n) %>%
slice(1:8) %>% 

#URL de imagen con el UNICODE del emoji
mutate( emoji_url = map_chr(emoji, ~paste0("https://abs.twimg.com/emoji/v2/72x72/", as.hexmode(utf8ToInt(.x)), ".png")))

#Plot de los datos
plotEmojis %>% 
ggplot(aes(x = reorder(emoji, -n), y = n)) +
geom_col(aes(fill = author, group=author), show.legend = FALSE, width = .20) +
  
#Fetch de la imagén PNG del emoji
geom_image(aes(image=emoji_url), size=.13) +
ylab("Número de veces que se usó el emoji") +
xlab("Emoji") +
facet_wrap(~author, ncol = 5, scales = "free") +
ggtitle("Emojis más usados en la conversación, por usuario") +
theme_minimal() +
theme(axis.text.x = element_blank())

#Text library
library(tidytext)
library(stopwords)

#Remover palabras sin significado relevante, como artículos o pronombres
remove_words <- c(stopwords(language = "pt"), "0", "y", "es", "sé" ,"si", "en","no", "a ver", "el", "la", "lo", "ya", "pero", "ese", "los","yo","mi", "un", "con", "las", "omitido", "más","eso", "al", "una", "del", "qué", "todo", "así", "le", "su", "va", "porque", "todos", "hay", "les", "pue", "ese", "son", "está", "pues", "ahí", "sí","ver", "estás", "algo", "vas", "ir","voy", "creo","fue","solo", "ni","sólo","nada", "aqui", "q", "tú", "multimedia")

#Conteo de palabras
myChat %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% remove_words) %>% 
  count(word) %>% 

#Plot de las 30 palabras más usadas en la conversación
  top_n(30,n) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(word,n), y=n, fill=n, color=n)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
  scale_color_gradient(low="#2b83ba",high="#d7191c") +
  ggtitle("Palabras más usadas en la conversación de manera general") +
  xlab("Palabras") +
  ylab("Número de veces que se usó la palabra") +
  coord_flip() +
  theme_minimal()

#Conteo de palabras por usuario
myChat %>%
  unnest_tokens(input = text,output = word) %>%
  filter(!word %in% remove_words) %>%
  count(author, word, sort = TRUE) %>%

#Top 20 palabras más usadas por usuario
  group_by(author) %>%
  top_n(n = 20, n) %>%
  slice(1:20) %>%
  ungroup() %>% 
  arrange(author, desc(n)) %>% 
  mutate(order=row_number()) %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = author, color = author)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  xlab("Palabras") +
  ylab("Número de veces que se usó la palabra") +
  coord_flip() +
  facet_wrap(~author, ncol = 3, scales = "free") +
  ggtitle("Palabras más usadas por usuario en la conversación") +
  theme_minimal()

#Diversidad de Léxico
myChat %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% remove_words) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity), y = lex_diversity, fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Diversidad léxica") + xlab("Usuario") +
  ggtitle("Diversidad de léxico en la conversación") +
  coord_flip()
  
