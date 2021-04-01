# Carregar pacotes

library(tidyverse)
library(readtext)
library(quanteda)


#Trabalhar no data.frame da fase1 e no grárico

fase1$categoria_nome<-factor(fase1$categoria_nome)
fase1<-select(fase1, - categoria)
fase1<-select(fase1, categoria_nome:temer_1)
fase1$fhc<-prop.table(fase1$fhc)*100
fase1$fhc<-round(fase1$fhc, 1)
fase1$lula<-prop.table(fase1$lula)*100
fase1$lula<-round(fase1$lula, 1)
fase1$dilma<-prop.table(fase1$dilma)*100
fase1$dilma<-round(fase1$dilma, 1)
fase1$temer_1<-prop.table(fase1$temer_1)*100
fase1$temer_1<-round(fase1$temer_1, 1)
fase1$temer<-fase1$temer_1
fase1$temer_1<-NULL
fase1<-edit(fase1)
write.csv(fase1, "fase1.csv")

fase1junto<-gather(fase1, key = "emissor", value = "valor", -categoria_nome)
fase1junto$emissor<-factor(fase1junto$emissor, labels = c("Dilma", "FHC", "Lula", "Temer"))

ggplot(fase1junto, aes(x=emissor, y = valor)) +
  geom_point(size = 2) +
  facet_wrap(~categoria_nome, nrow = 4) + 
  theme(axis.text.x = element_text(colour = "black", size = rel(1.3)), 
        axis.text.y = element_text(colour = "black", size = rel(1.3)),
        axis.title = element_blank(), 
        strip.text = element_text(size = rel(1.1), colour = "black"),
        panel.background = element_rect(colour = "black", fill = "white"),
        strip.background = element_rect(colour = "black", fill = "white")) + 
  geom_segment(aes(xend = emissor), yend = 0) + coord_flip() 



# Trabalhar com a fase 2, em que selecionei apenas RI e CE

fase2$categoria_nome<-factor(fase2$categoria_nome)
fase2<-select(fase2, - categoria)
fase2<-select(fase2, -FHC1, -FHC2, -Dilma1, -Dilma2)
fase2$Lampreia<-prop.table(fase2$Lampreia)*100
fase2$Lampreia<-round(fase2$Lampreia, 1)
fase2$Lafer<-prop.table(fase2$Lafer)*100
fase2$Lafer<-round(fase2$Lafer, 1)
fase2$LULA<-prop.table(fase2$LULA)*100
fase2$LULA<-round(fase2$LULA, 1)
fase2$Amorim<-prop.table(fase2$Amorim)*100
fase2$Amorim<-round(fase2$Amorim, 1)
fase2$Patriota<-prop.table(fase2$Patriota)*100
fase2$Patriota<-round(fase2$Patriota, 1)
fase2$Figueiredo<-prop.table(fase2$Figueiredo)*100
fase2$Figueiredo<-round(fase2$Figueiredo, 1)
fase2$Vieira<-prop.table(fase2$Vieira)*100
fase2$Vieira<-round(fase2$Vieira, 1)
fase2$Temer<-prop.table(fase2$Temer)*100
fase2$Temer<-round(fase2$Temer, 1)
fase2$Serra<-prop.table(fase2$Serra)*100
fase2$Serra<-round(fase2$Serra, 1)
fase2$Nunes<-prop.table(fase2$Nunes)*100
fase2$Nunes<-round(fase2$Nunes, 1)
fase2$FHC<-prop.table(fase2$FHC)*100
fase2$FHC<-round(fase2$FHC, 1)
fase2$DILMA<-prop.table(fase2$DILMA)*100
fase2$DILMA<-round(fase2$DILMA, 1)
fase2<-edit(fase2)
write.csv(fase2, "fase2.csv")

fase2junto<-gather(fase2, key = "emissor", value = "valor", -categoria_nome)
fase2junto$emissor<-factor(fase2junto$emissor)
fase2junto$emissor<-factor(fase2junto$emissor, labels = c("Amorim", "Dilma", "FHC", "Figueiredo", "Lafer", "Lampreia", "Lula", "Nunes", "Patriota", "Serra", "Temer", "Vieira"))

ggplot(fase2junto, aes(x=emissor, y = valor)) +
  geom_point(size = 2) +
  facet_wrap(~categoria_nome, ncol =7) + 
  theme(axis.text.x = element_text(colour = "black", size = rel(1.5)), 
        axis.text.y = element_text(colour = "black", size = rel(1.5)),
        axis.title = element_blank(), 
        strip.text = element_text(size = rel(1), colour = "black"),
        panel.background = element_rect(colour = "black", fill = "white"),
        strip.background = element_rect(colour = "black", fill = "white")) +
  geom_segment(aes(xend = emissor), yend = 0)  + scale_fill_manual(values = c( "white", "black")) + 
  guides(fill=FALSE) + coord_flip()


# Para medir associação entre Presidentes e MRE

discursos<-readtext("*.txt", docvarsfrom = "filenames", docvarnames = c("emissor", "cargo"), encoding = "UTF-8")
discursos.corpus<-corpus(discursos)
discursos.dfm.emissor<-dfm(discursos.corpus, tolower = TRUE,
                         remove = c(stopwords("portuguese"), 
                                    "ã", "brasil", "é", "sob", "onde", "país"),
                         remove_numbers = TRUE,
                         remove_punct = TRUE, groups = "emissor")

discursos.dfm.cargo<-dfm(discursos.corpus, tolower = TRUE,
                   remove = c(stopwords("portuguese"), 
                              "ã", "brasil", "é", "sob", "onde", "país"),
                   remove_numbers = TRUE,
                   remove_punct = TRUE, groups = "cargo")

textplot_wordcloud(discursos.dfm.cargo, comparison = TRUE, 
                   colors = c("black", "chocolate4"), rot.per = .2)


discursos_emissor_trim<-dfm_trim(discursos.dfm.emissor,
                                 min_count = 4, min_docfreq = 3)

distancia<-textstat_dist(dfm_weight(discursos_emissor_trim, "relfreq"))
cluster<-hclust(distancia)
cluster$labels<-docnames(discursos_emissor_trim)
plot(cluster, xlab = "", sub = "", main = "", axes = F, ylab = "", )


discursos.simil<-textstat_simil(dfm_weight(discursos_emissor_trim,
                                           "relfreq"), method = "cosine")

discursos.simil.matrix<-as.matrix(discursos.simil)
corrplot(discursos.simil.matrix, diag = F, tl.col = "black")


# Categoria Outros

f$Lampreia<-prop.table(f$Lampreia)*100
f$Lampreia<-round(f$Lampreia, 1)
f$Lafer<-prop.table(f$Lafer)*100
f$Lafer<-round(f$Lafer, 1)
f$Lula<-prop.table(f$Lula)*100
f$Lula<-round(f$Lula, 1)
f$Amorim<-prop.table(f$Amorim)*100
f$Amorim<-round(f$Amorim, 1)
f$Patriota<-prop.table(f$Patriota)*100
f$Patriota<-round(f$Patriota, 1)
f$Figueiredo<-prop.table(f$Figueiredo)*100
f$Figueiredo<-round(f$Figueiredo, 1)
f$Vieira<-prop.table(f$Vieira)*100
f$Vieira<-round(f$Vieira, 1)
f$Temer<-prop.table(f$Temer)*100
f$Temer<-round(f$Temer, 1)
f$Serra<-prop.table(f$Serra)*100
f$Serra<-round(f$Serra, 1)
f$Nunes<-prop.table(f$Nunes)*100
f$Nunes<-round(f$Nunes, 1)
f$FHC<-prop.table(f$FHC)*100
f$FHC<-round(f$FHC, 1)
f$Dilma<-prop.table(f$Dilma)*100
f$Dilma<-round(f$Dilma, 1)
f<-edit(f)
write.csv(f, "outros.emissor.csv")
fd<-gather(f, key= "emissor", value = "valor", -categoria)

ggplot(fd, aes(x=emissor, y=valor)) + 
  geom_point() + facet_wrap(~categoria, ncol = 6) + 
  coord_flip() + geom_segment(aes(xend = emissor), yend = 0) + 
theme(axis.text.x = element_text(colour = "black", size = rel(1.5)), 
      axis.text.y = element_text(colour = "black", size = rel(1.3)),
      axis.title = element_blank(), 
      strip.text = element_text(size = rel(1), colour = "black"),
      panel.background = element_rect(colour = "black", fill = "white"),
      strip.background = element_rect(colour = "black", fill = "white")) +
  geom_segment(aes(xend = emissor), yend = 0)  + scale_fill_manual(values = c( "white", "black")) + 
  guides(fill=FALSE) + coord_flip()
