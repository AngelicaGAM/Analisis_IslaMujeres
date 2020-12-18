
# -------------- PRUEBAS TIDY --------------- #
library(ggplot2)
library(tidyr)
library(wordcloud)
library(reshape2)
library(tidytext)
library(stringr)
library(plyr)
library(igraph)
library(ggraph)



library(janitor)
library(dplyr)
  source("limpieza.R")

sentiment <- sentiment[1:461]
sentiment1 <- sentiment1[1:461]

# ------------------- ELIMINAR LO NECESARIO ----------------------

biblioteca <- function(text_word_df, stop_df){
  # Eliminar palabras sin valor.
  text_word_df <- text_word_df %>%
    anti_join(stop_df)
  # Eliminar espacios
  text_word_df$word <- str_trim(text_word_df$word, side = "both")
  # Quitar mayusculas
  text_word_df$word <- str_to_lower(text_word_df$word, locale = "es")
  # Elimina acentos
  text_word_df$word <- chartr('αινσϊρ','aeioun',text_word_df$word)
  # Separa por palabra
  text_word_df <- left_join(text_word_df, word_lib, key = "word")
  text_word_df <- distinct(text_word_df, FOLIO, word, .keep_all = TRUE)
  text_word_df <- drop_na(text_word_df, edicion)
  return(text_word_df)
}

# ------------------- MOSTRAR RESULTADOS 1 ----------------------

show_result_1 <- function(sentiment, text_word_df,sent_df){
  bing_word_counts <- text_word_df  %>%
    inner_join(sent_df) %>%
    count(edicion, sentiment, sort = TRUE) %>%
    ungroup()
  
  bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(edicion, n)) %>%
    ggplot(aes(edicion, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()
} 
# ------------------- MOSTRAR RESULTADOS 2 ----------------------


show_result_2 <- function(sentiment,text_word_df,sent_df){
  sentiment <- sentiment1
  
  sent_df <- data.frame(word_lib, sentiment)
  
  #text_word_df  %>%
   # anti_join(stop_df) %>%
    #count(edicion) %>%
    #with(wordcloud(edicion, n, max.words = 100))
  
  text_word_df %>%
    inner_join(sent_df) %>%
    count(edicion, sentiment, sort = TRUE) %>%
    acast(edicion ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray10", "gray60"),
                     max.words = 100)
}





# -------------- PRUEBAS TIDY PREGUNTA 1 --------------- #
Fi_df <- Final_df
Fi_df$RESPUESTA <- as.character(Fi_df$RESPUESTA)

# Tokenisacion de las palabras.
text_word_df <- Fi_df[1:55,] %>%
  unnest_tokens(word, RESPUESTA)

# Biblioteca para eliminar palabras sin valor.
lexicon <- stop_words$lexicon[1:146]
stop_df <- data.frame(word, lexicon)

text_word_df <- biblioteca(text_word_df,stop_df)

# Mostrar veces que se repite cada palabra.
#text_word_df %>%
# count(word, sort = TRUE)

# Biblioteca de sentimientos.
sent_df <- data.frame(word_lib, sentiment)

# ------------------- MOSTRAR RESULTADOS ---------------------------- #
show_result_1(sentiment, text_word_df,sent_df)
show_result_2(sentiment, text_word_df,sent_df)






# -------------- NUEVAS PRUEBAS DE ASOCIACION CON TIDY --------------- #


text_word_df1 <- Final_df %>%
  unnest_tokens(bigram, RESPUESTA, token = "ngrams", n = 2)
text_word_df1 <- text_word_df1[-c(1),]


text_word_df1 %>%
  count(bigram, sort = TRUE)


bigrams_separated <- text_word_df1 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)



bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")


# EJEMPLO DE FILTRADO
bigrams_filtered %>%
  filter(word2 == "conocimiento") %>%
  count(FOLIO, word1, sort = TRUE)

bigrams_separated %>%
  filter(word1 == "no") %>%
  count(word1, word2, sort = TRUE)


# INTERESANTE QUE ES?
bigram_tf_idf <- bigrams_united %>%
  count(FOLIO, bigram) %>%
  bind_tf_idf(bigram, FOLIO, n) %>%
  arrange(desc(tf_idf))


prueba_pal <- sent_df[,c(2,3)]
prueba_pal$sentiment<- as.numeric(prueba_pal$sentiment)

prueba_pal[prueba_pal$sentiment == 1, "sentiment"] <- -3
prueba_pal[prueba_pal$sentiment == 2, "sentiment"] <- -2
prueba_pal[prueba_pal$sentiment == 3, "sentiment"] <- -1
prueba_pal[prueba_pal$sentiment == 4, "sentiment"] <- 1
prueba_pal[prueba_pal$sentiment == 5, "sentiment"] <- 2
prueba_pal[prueba_pal$sentiment == 6, "sentiment"] <- 3

not_words <- bigrams_separated %>%
  filter(word1 == "no") %>%
  inner_join(prueba_pal, by = c(word2 = "word")) %>%
  count(word2, sentiment, sort = TRUE) %>%
  ungroup()

not_words %>%
  mutate(contribution = n * sentiment) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * sentiment, fill = n * sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()


check_words <- c("no", "terreno", "personas", "actividad")

check_words <- bigrams_separated %>%
  filter(word1 %in% check_words) %>%
  inner_join(prueba_pal, by = c(word2 = "word")) %>%
  count(word1, word2, sentiment, sort = TRUE) %>%
  ungroup()

check_words %>%
  mutate(contribution = n * sentiment) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * sentiment, fill = n * sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~word1, ncol = 2, scales = "free_x")+
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()


bigram_graph <- bigram_counts %>%
  filter(n > 1) %>%
  graph_from_data_frame()


set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



