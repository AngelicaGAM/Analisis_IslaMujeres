
library(plyr)
library(dplyr)
library(janitor)

# -------------- FINAL --------------- #

Final_df <- read.csv("UC/Proyecto Terminal/Analisis_IslaMujeres/dataset/CSV/Final.csv", header = FALSE, sep=",", strip.white = TRUE, na.strings = "EMPTY")

Final_df_2 <- Final_df[118:173,1:6]
Final_df_2 <- Final_df_2 %>%
  row_to_names(row_number = 1)
row.names(Final_df_2) <- NULL


Final_df <- Final_df[2:289,1:2]
Final_df <- Final_df[-c(58,59,116,117,174,175,232,233,c(118:173)),]
Final_df <- Final_df %>%
  row_to_names(row_number = 1)
row.names(Final_df) <- NULL

# -------------- AMBIENTAL --------------- #

Ambiental_df <- read.csv("UC/Proyecto Terminal/Analisis_IslaMujeres/dataset/CSV/Ambiental.csv", header = FALSE, sep=",", strip.white = TRUE, na.strings = "EMPTY")
Ambiental_df <- Ambiental_df[2:347,1:3] 
Ambiental_df <- Ambiental_df[-c(58,59,116,117,174,175,232,233,290,291),]
Ambiental_df <- Ambiental_df %>%
  row_to_names(row_number = 1)
row.names(Ambiental_df) <- NULL

# -------------- ECONOMICO --------------- #

Economico_df <- read.csv("UC/Proyecto Terminal/Analisis_IslaMujeres/dataset/CSV/Economico.csv", header = FALSE, sep=",", strip.white = TRUE, na.strings = "EMPTY")
Economico_df <- Economico_df[2:289,1:3] 
Economico_df <- Economico_df[-c(58,59,116,117,174,175,232,233),]
Economico_df <- Economico_df %>%
  row_to_names(row_number = 1)
row.names(Economico_df) <- NULL

# -------------- SOCIAL --------------- #

Social_df <- read.csv("UC/Proyecto Terminal/Analisis_IslaMujeres/dataset/CSV/Salinas1.csv", header = FALSE, sep=",", strip.white = TRUE, na.strings = "EMPTY")
Social_df_2 <- Social_df[2:57,1:5] 
Social_df_2 <- Social_df_2 %>%
  row_to_names(row_number = 1)
row.names(Social_df_2) <- NULL

Social_df <- Social_df[60:347,1:3]
Social_df <- Social_df %>%
  row_to_names(row_number = 1)
row.names(Social_df) <- NULL 
Social_df <- Social_df[-c(57,58,115,116,173,174,231,232),]



# -------------- Bibliotecas --------------- #


word_lib <- read.csv("UC/Proyecto Terminal/Analisis_IslaMujeres/dataset/CSV/word_library2.csv", header = FALSE, sep=",", strip.white = TRUE, na.strings = "EMPTY")
word_lib <- word_lib[,1:2] %>%
  row_to_names(row_number = 1)
row.names(word_lib) <- NULL

word <-c("es", "me","se", "los","más","mas", "que", "el", "le", "la", "al", "de","por", "en", "ya", "aquá", "aqui", "porque", "donde", "habÃ?a", "habia","ni", "queria", "querÃ?a", "un", "las", "los", "ellos", "ellas", "les","unos", "una", "unas", "eran", "era", "son", "seran", "su", "sus", "y","ya", "o", "u", "a", "ha", "e", "eh", "del", "sido", "estaban", "estaba", "estan","estarian", "estaran", "den", "con", "como", "para", "pueda", "puedo","ser", "puede","ella", "hacer", "hacr", "no se", "estarÃ?a", "cuando","vienen", "mÃ©rida","ver","hay", "ahi", "ay", "esta", "tiene", "acerca", "serÃ?a", "dice", "muy","podido", "sobre", "tienen", "hizo", "pero", "van", "nos", "dio", "ante", "misma","mismo", "seÃ±ora", "seÃ±or", "sr", "sra", "sr.", "sra.", "quiso", "dar", "solÃ?a", "ahora","porto", "demas", "pudimos", "mucha", "ido", "fue", "sin", "embargo", "aca", "usaban", "estuviera","puesto", "ell", "palabra", "llego", "llegÃ³", "asi", "asÃ?", "poner", "solamente","n", "tan", "algÃºn", "algun", "algo", "tambien", "tambiÃ©n", "usa", "uso", "cuestiones", "poque", "algunas","usan", "bronquitis", "serie","nortes", "pues", "parte", "van","tener","gilberto", "maneja", "fueron", "todavia","dano", "todavÃ?a")

sentiment1 <- read.csv("UC/Proyecto Terminal/Analisis_IslaMujeres/dataset/CSV/word_library.csv", header = FALSE, sep=",", strip.white = TRUE, na.strings = "EMPTY")
sentiment <- sentiment1[2:462,3]
sentiment1 <- sentiment1[2:462,4]

