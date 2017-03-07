library(rJava)
library(tidyverse)
library(xlsx)
library(cluster)
library(ape)
library(dendextend) 
library(dplyr)
library(tidyr)
library(tibble)
library(splitstackshape)
movie_metadata_csv <- read_csv("~/Downloads/movie_metadata.csv.zip")
library(ggplot2)
library(ggthemes)
library(party)
library(earth)

movies <- movie_metadata_csv %>%
  filter(gross > 0)  %>%
  select(plot_keywords, genres, movie_title, imdb_score)

movies <- as.data.frame(movies)

movies <-  movies[!duplicated(movies$movie_title), ]

rownames(movies) <- movies$movie_title

movies <- movies %>%
  select(-movie_title) %>%
  na.omit()

movies$type <- paste(movies$plot_keywords, movies$genres, sep='|') 



m_matrix <- cSplit_e(select(movies,imdb_score,type), "type", sep = "|", mode = "binary", 
         type = "character", fill = 0, drop = TRUE)

marsModel <- earth(imdb_score ~ . , data= m_matrix) # build model
ev <- evimp (marsModel)


base.mod <- lm(imdb_score ~ . , data= m_matrix)  # base intercept only model
all.mod <- lm(imdb_score ~ . , data= m_matrix) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(shortlistedVars)

cf1 <- cforest(imdb_score ~ . , data= m_matrix, control=cforest_unbiased(mtry=2,ntree=20))
df_imp <- varimp(cf1) 
summary(cf1)

fit = manova(m_matrix,formula = imdb_score ~ .)
fit = lm(m_matrix,formula = imdb_score ~ .)
books_pc <- prcomp(m_matrix, scale = TRUE)

df <- data.frame(books_pc$rotation)
list <- c()
list <-rownames(df[which(df$PC1 > .1),])
list <- c(list,rownames(df[which(df$PC2 > .1),]))
list <- c(list,rownames(df[which(df$PC3 > .1),]))

m_matrix <- m_matrix[,colSums(m_matrix) > 10]
m_matrix <- m_matrix[rowSums(m_matrix) > 0,]
m_matrix[,2:24] <- lapply(m_matrix[,2:24] , as.factor) 

boruta_output <- Boruta(imdb_score ~ ., data=m_matrix, doTrace=2) 
boruta.df <- attStats(boruta_output)
plot(boruta_output)
importantFeatures <- data.frame(
   feature=labels(tail(boruta_output$ImpHistory, 1)),
   weight=c(tail(boruta_output$ImpHistory, 1)))[,2:3]

names(importantFeatures) <- c("feature", "weight")

importantFeatures$feature <- factor(
   importantFeatures$feature,
   levels=importantFeatures$feature[order(importantFeatures$weight, decreasing=T)])

ggplot(importantFeatures[importantFeatures$weight >= 10,], aes(x=feature, y=weight)) +
  geom_bar(stat="identity", position="dodge") +
  theme_fivethirtyeight()

movies_d <- daisy(m_matrix)

cld <- as.dendrogram(diana(movies_d))

# Three versions of the dendrogram:

# need to use hang.dendrogram to keep the hanging raggedness
x
par(mar = c(5,4,4,7), font.main = 1)
par(bg = "grey99")
plot(hang.dendrogram(cld), horiz = TRUE, yaxt = "n", type = "triangle",
     xlim = c(4.5, -1), 
     edgePar = list(col = "steelblue"),
     main = "Selected childrens books or series, clustered by thematic elements")

par(mar = c(1,1,1,1))
par(bg = "grey99")
plot(as.phylo(cld), type = "unrooted", cex = 0.6) # maybe

svg("movies_good.svg", width=30, height=30)
par(mar=c(5,3,2,2)+0.1)
plot(as.phylo(cld), type = "fan", cex = 0.6) # maybe
dev.off()

#-----------principle components-------------
books_pc <- prcomp(m_matrix, scale = TRUE)
View(books_pc$rotation)
##par(family = "Source Sans Pro")
pdf("movies_pca.pdf", width=40, height=15)
par(bg = "grey99")
biplot(books_pc, choices = 1:2, col = c("darkblue",  "grey75"), pc.biplot = TRUE)
dev.off()