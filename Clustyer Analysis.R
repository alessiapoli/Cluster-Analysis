#####################################
########## DATASET METRO ############
#####################################

metro<-file.choose()
metro<-read.csv2(metro,header=TRUE,sep=";") 

dim(metro) # dimensioni del dataset
str(metro) # struttura del dataset
colnames(metro) #  nomi delle variabili del dataset
summary(metro) # statistiche di sintesi delle variabili coinvolte

# rappresentazione grafica delle distribuzioni relative ai pezzi acquistati in
# base ai comportamenti dei clienti
par(mfrow=c(3,3))
for(i in 1:9)
  hist(metro[,i],breaks=75,main=colnames(metro)[i],xlab="Numero di pezzi
acquistati",ylab="Numero di
clienti",col=c("green","yellow","red","lightblue","orange","gray","violet","b
lue","pink")[i])

# matrice di correlazione delle variabili del dataset metro.csv
library(corrplot) # per la visulizzazione grafica della matrice di  correlazione 
# tra le variabili quantitative
cormetro<-cor(metro) # matrice di correlazione delle variabili
# rappresentazione grafica della matrice di correlazione
corrplot(cormetro,method="number") 


##################################################
###### METODI DI CLASSIFICAZIONE GERARCHICI ######
##################################################
library(cluster) # per l'implementazione dei metodi di classificazione gerarchici

# calcolo delle matrici delle distanze per ambedue i dataset (standardizzato e
# non standardizzato)
metro.dist<-daisy(metro,metric="euclidean",stand=TRUE) 
# utilizzo la distanza euclidea come misura di distanza
as.matrix(metro.dist) 

# applicazione dei diversi metodi di classificazione gerarchica che utilizzano
# congiuntamente come input dei dati la matrice delle distanze e la matrice dei
# dati

metro.hc.cen<-hclust(metro.dist,method="centroid") # metodo del centroide
plot(metro.hc.cen) # visualizzazione del dendrogramma

metro.hc.ward<-hclust(metro.dist,method="ward") # metodo di Ward
plot(metro.hc.ward) # visualizzazione del dendrogramma
rect.hclust(metro.hc.ward,k=4,border=c("red","blue","green","orange")) 
# suddivisione del dendrogramma in quattro gruppi sulla base del metodo di Ward
metro.hc.ward.segment<-cutree(metro.hc.ward,k=4) 
# allocazione delle unità statistiche nei quattro gruppi
table(metro.hc.ward.segment) # dimensione di ogni gruppo
# funzione per la determinazione delle medie di gruppo
seg.mean<-function(data,groups){
  aggregate(data,list(groups),FUN=mean)
}
seg.mean(metro,metro.hc.ward.segment) # medie delle variabili nei quattro clusters


#########################################################################
#### METODI DI CLASSIFICAZIONE NON GERARCHICA (METODO DELLE k-MEDIE) ####
#########################################################################

# standardizzazione del dataset per il calcolo della distanza euclidea
# utilizzata nel metodo basato sulle k-medie
metrostan<-scale(metro)

# determinazione del numero di gruppi iniziale entro i quali classificare le
# osservazioni
set.seed(200) # fisso i seeds iniziali
k.max<-20 # definisco come numero massimo di gruppi k=20

# applicazione del metodo delle k-medie con numero di iterazioni pari a 20 e
# numero di volte in cui i semi iniziali vengono ricampionati pari a 50 e
# restituzione della misura di devianza within
withinss<-sapply(1:k.max,function(k)
{kmeans(metrostan,k,nstart=50,iter.max=20)$tot.withinss})
plot(1:k.max,withinss,type="b",pch=20,xlab="Numero di gruppi",ylab="Devianza
within",col="red") # grafico basato sul criterio di elbow
kmeans5<-kmeans(metro,5) # numero di cluster scelti in base al grafico pari a 5

library(factoextra) # per la valutazione della configurazione di cluster 

ris5<-eclust(metro,"kmeans",k=5) 
# dimensione dei gruppi e valore medio di silhoutte in ogni gruppo
fviz_silhouette(ris5)
sil5<-ris5$silinfo$widths # misure di silhoutte per ogni osservazione
neg_sil_index5<-which(sil5[,'sil_width']<0) # posizione delle osservazioni con
silhoutte<0

# individuazione delle osservazioni con misura di silhoutte<0, del cluster 
# di appartenenza e del cluster più vicino
sil5[neg_sil_index5,] 