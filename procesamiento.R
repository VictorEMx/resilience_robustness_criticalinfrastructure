library(pacman)
p_load(tidyverse)
p_load(clue)
p_load(tm)
p_load(pdftools)
p_load(SnowballC)
p_load(wordcloud)
p_load(RColorBrewer)

#Directorio
dir.model<-"C:/Users/User.HPPROBOOK640G1/Google Drive/DPP/Tesis/Paper 1/Doc para procesar/"

#Creaci?n de Corpus, entidad conceptual similar a una base de datos que contenga y gestione documentos de texto de una manera gen?rica
corpus <- VCorpus(DirSource(dir.model, pattern = ".pdf"), 
                     readerControl = list(reader = readPDF))

#Conversi?n a min?sculas
corpus<-tm_map(corpus,content_transformer(tolower))

#Eliminar stopwords, palabras que no aportan informaci?n relevante
corpus<-tm_map(corpus, removeWords, stopwords("SMART"))

#Eliminar signos de puntuaci?n
corpus<-tm_map(corpus, removePunctuation)

#Eliminar los n?meros
corpus<-tm_map(corpus,removeNumbers)

#Lematizaci?n de las palabras
corpus<-tm_map(corpus,stemDocument)

inspect(corpus)

#Plain Text Document
ptd <- tm_map(corpus, PlainTextDocument)

#Nube de palabras
wordcloud(ptd, max.words = 80, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))

#Term Document Matrix
tdm<-TermDocumentMatrix(corpus)
tdm

#Frecuencia de palabras
matrix_palabras<-as.matrix(tdm)
dim(matrix_palabras)
matrix_palabras<-matrix_palabras%>%rowSums()%>%sort(decreasing = TRUE)
matrix_palabras<-data.frame(palabra=names(matrix_palabras),frec=matrix_palabras)
wordcloud(
  words = matrix_palabras$palabra, 
  freq = matrix_palabras$frec, 
  max.words = 80, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)

#Las 20 palabras m?s frecuentes
matrix_palabras[1:20,]

#Gr?ficas de frecuencia
matrix_palabras[1:9, ] %>%
  ggplot(aes(palabra, frec)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = frec)) + 
  coord_flip() + 
  labs(title = "Nueve palabras m?s frecuentes",  x = "Palabras", y = "N?mero de usos")

#Asociaci?n entre palabras
findAssocs(tdm, terms = c("system"), corlimit = 0.65)
findAssocs(tdm, terms = c("model"), corlimit = 0.70)
findAssocs(tdm, terms = c("network"), corlimit = 0.60)
findAssocs(tdm, terms = c("infrastructur"), corlimit = 0.75)
findAssocs(tdm, terms = c("power"), corlimit = 0.75)
findAssocs(tdm, terms = c("risk"), corlimit = 0.85)
findAssocs(tdm, terms = c("node"), corlimit = 0.55)
findAssocs(tdm, terms = c("robust"), corlimit = 0.90)
findAssocs(tdm, terms = c("resili"), corlimit = 0.55)

#Eliminar t?rminos dispersos
tdm_new <- removeSparseTerms(tdm, sparse = .30)

tdm
tdm_new

#transformamos ahora esta lista en matriz
tdm_new <- tdm_new %>% as.matrix()

#crearemos una matriz de distancia, pero antes es necesario estandarizar
tdm_new <- tdm_new / rowSums(tdm_new)

#Procedemos a obtener una matriz de distancia a partir de ella, con el m?todo de distancias euclidianas y la asignamos al objeto tdm_dist.
tdm_dist<-dist(tdm_new, method = "euclidian")

#Realizaremos nuestro agrupamiento jer?rquico usando la funci?n hclust
tdm_hclust <-  hclust(tdm_dist, method = "ward.D")

#Graficamos el dendrograma, diagrama de datos en forma de ?rbol que organiza los datos en subcategor?as que se van dividiendo en otros hasta llegar al nivel de detalle deseado.
plot(tdm_hclust, main = "Dendrograma de Niebla - hclust", sub = "", xlab = "")

#Agrupando, quedar?an:
plot(tdm_hclust, main = "Dendrograma de Niebla - hclust - agrupados", sub = "", xlab = "")
rect.hclust(tdm_hclust, k = 20, border="blue")
