# Import libarys

library(dplyr)
library(ggplot2)
library(gridExtra)
library(forcats)
library(lubridate)
library(caret)
library(ROCR)
library(FSelector)

# Import DataFrame

df <- read.csv("./data.csv", header = T)

df$action_type <- as.factor(df$action_type)
df$combined_shot_type <- as.factor(df$combined_shot_type)
df$season <- as.factor(df$season)
df$shot_type <- as.factor(df$shot_type)
df$shot_zone_area <- as.factor(df$shot_zone_area)
df$shot_zone_basic <- as.factor(df$shot_zone_basic)
df$shot_zone_range <- as.factor(df$shot_zone_range)
df$team_name <- as.factor(df$team_name)
df$game_date <- as.factor(df$game_date)
df$matchup <- as.factor(df$matchup)
df$opponent <- as.factor(df$opponent)
df$shot_made_flag <- as.factor(df$shot_made_flag)

# Descripcion de los datos
str(df)
summary(df)

# Visualización gráfica de los datos
png("lanzamientos.png")

lanz_graf <- ggplot(df[!is.na(df$shot_made_flag),], aes(x=lon, y=lat)) +
  geom_point(size = 0.5, alpha = 0.5, aes(color=shot_made_flag)) +
  labs(title="Shot type") +
  ylim(c(33.7, 34.0883)) +
  theme_void() +
  theme(legend.title=element_blank(),
        plot.title=element_text(hjust=0.5))

lanz_graf
print(lanz_graf)
dev.off()


png("zonas lanzamientos.png")

g1 <- ggplot(df, aes(x=lon, y=lat)) +
  geom_point(size = 1, alpha = 0.5, aes(color=shot_zone_basic)) +
  labs(title="Básicas") +
  ylim(c(33.7, 34.0883)) +
  theme_void() +
  theme(legend.title=element_blank(),
        plot.title=element_text(hjust=0.5))
g2 <- ggplot(df, aes(x=lon, y=lat)) +
  geom_point(size = 1, alpha = 0.5, aes(color=shot_zone_range)) +
  labs(title="Por distancia") +
  ylim(c(33.7, 34.0883)) +
  theme_void() +
  theme(legend.title=element_blank(),
        plot.title=element_text(hjust=0.5))

g3 <- ggplot(df, aes(x=lon, y=lat)) +
  geom_point(size = 1, alpha = 0.5, aes(color=shot_zone_area)) +
  labs(title="Por áreas") +
  ylim(c(33.7, 34.0883)) +
  theme_void() +
  theme(legend.title=element_blank(),
        plot.title=element_text(hjust=0.5))

g4 <- ggplot(df, aes(x=fct_infreq(shot_zone_basic))) + 
  geom_bar(aes(fill=shot_zone_basic)) +
  labs(y="Frecuencia") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        axis.title.x=element_blank(), 
        legend.position="none")

g5 <- ggplot(df, aes(x=fct_infreq(shot_zone_range))) + 
  geom_bar(aes(fill=shot_zone_range)) +
  labs(y="Frecuencia") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        axis.title.x=element_blank(), 
        legend.position="none")

g6 <- ggplot(df, aes(x=fct_infreq(shot_zone_area))) + 
  geom_bar(aes(fill=shot_zone_area)) +
  labs(y="Frecuencia") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        axis.title.x=element_blank(), 
        legend.position="none")

print(grid.arrange(g1, g4, g2, g5, g3, g6,
             top = "Gráifco por zonas de lanzamiento",
             layout_matrix=cbind(rbind(c(1, 2), c(3, 4), c(5,6)))))
dev.off()


pplot <- function(feat) {
  feat <- substitute(feat)
  ggplot(data = df[!is.na(df$shot_made_flag),], aes_q(x = feat)) +
    geom_bar(aes(fill = shot_made_flag), stat = "count", position = "fill") +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    ggtitle(paste("accuracy by", feat))
}
# Función editada de la función de Alexandru Papiu (https://www.kaggle.com/apapiu/exploring-kobe-s-shots)


png("accuracy.png")

g7 <- pplot(minutes_remaining) +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5))

g8 <-pplot(seconds_remaining)+
  theme(plot.title=element_text(hjust=0.5))

g9 <- pplot(opponent) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        axis.title.x=element_blank(), 
        legend.position="none",
        plot.title=element_text(hjust=0.5))

g10 <- pplot(season) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        axis.title.x=element_blank(),
        plot.title=element_text(hjust=0.5))

print(grid.arrange(g7, g8, g9, g10,
             top = "Precisión del lanzamiento",
             layout_matrix=cbind(rbind(c(1, 2), c(3, 4))), widths =c(14,20)))
dev.off()


# Preprocesado de datos

# eliminamos "team_id" y "team_name"
df <- select(df, -team_id, -team_name)

# eliminamos "matchup"
df <- select(df, -matchup)

# eliminamos "lon" y "lat"
df <- select(df, -lon, -lat)

# Generamos una nueva variable de tiempos restante
df$time_remaining <- hms(with(df, paste(0, minutes_remaining, seconds_remaining)))
df <- select(df, -minutes_remaining, -seconds_remaining)

# Split DataFra
df_train <- df[!is.na(df$shot_made_flag),]
df_test <- df[is.na(df$shot_made_flag),]

#Eliminamos la variable "shot_id" del conjunto de entrenamiento dado que no proporcionara información para los modelos.
df_train <- select(df_train, -shot_id)

# Método ranking
weights <- gain.ratio(shot_made_flag~., df_train)
weights=weights[order(weights$attr_importance,decreasing=TRUE),drop=F]
print(weights)

df_train <- select(df_train, -game_id, -playoffs)

# Modelado
# Cross-validation 5 kflods
fitControl <- trainControl(method = "cv",
                           number = 5)

# Naive Bayes
set.seed(123)
naive_bayesFit <- train(shot_made_flag ~ ., data = df_train, 
                        method = "naive_bayes", 
                        trControl = fitControl,
                        verbose = FALSE)

# Árbol de decisión
set.seed(123)
DTFit <- train(shot_made_flag ~ ., data = df_train, 
               method = "C5.0", 
               trControl = fitControl,
               verbose = FALSE)

# Vecinos más cercanos
set.seed(123)
kknnFit <- train(shot_made_flag ~ ., data = df_train, 
                 method = "kknn", 
                 trControl = fitControl,
                 verbose = FALSE)

# Support Vecto Machina (linear kernel)
set.seed(123)
svmLinearFit <- train(shot_made_flag ~ ., data = df_train, 
                      method = "svmLinear", 
                      trControl = fitControl,
                      verbose = FALSE)

# RESULTADOS DE LOS MODELOS
tabla <- resamples(list(Naive_Bayes = naive_bayesFit,
                        Decision_Tree = DTFit,
                        Nearest_Neighbour = kknnFit,
                        SVM_Linear= svmLinearFit))
summary(tabla)


pred_DTFit_train <- predict(DTFit, df_train)
pred_svmLinearFit_train <- predict(svmLinearFit, df_train)
print("Árbol de decisión")
print(confusionMatrix(as.factor(pred_DTFit_train), as.factor(df_train$shot_made_flag)))
print("Support Vector Machine")
print(confusionMatrix(as.factor(pred_svmLinearFit_train), as.factor(df_train$shot_made_flag)))

# VISUALIZACIÓN DE LOS RESULTADOS
shot_made_flag <- df_train$shot_made_flag

pred_DTFit_train_GoodBad <-ifelse(pred_DTFit_train==LaMeteOno, TRUE, FALSE)
pred_svmLinearFit_train_GoodBad <-ifelse(pred_svmLinearFit_train==LaMeteOno, TRUE, FALSE)
pred_svmRadialFit_train_GoodBad <-ifelse(pred_svmRadialFit_train==LaMeteOno, TRUE, FALSE)

df_train <-cbind(df_train,pred_DTFit_train_GoodBad)
df_train <-cbind(df_train,pred_svmLinearFit_train_GoodBad)
df_train <-cbind(df_train,pred_svmRadialFit_train_GoodBad)

g11 <- ggplot(df_train, aes(x=lon, y=lat)) +
  geom_point(size = 1.5, alpha = 0.5, aes(color=pred_DTFit_train_GoodBad)) +
  labs(title="Predicción de arboles de decisión") +
  ylim(c(33.7, 34.0883)) +
  theme_void() +
  theme(legend.title=element_blank(), 
        legend.position="none",
        plot.title=element_text(hjust=0.5))
g12 <- ggplot(df_train, aes(x=lon, y=lat)) +
  geom_point(size = 1.5, alpha = 0.5, aes(color=pred_svmLinearFit_train_GoodBad)) +
  labs(title="Predicción SVM") +
  ylim(c(33.7, 34.0883)) +
  theme_void() +
  theme(legend.title=element_blank(),
        plot.title=element_text(hjust=0.5))

grid.arrange(g11, g12, ncol =2, widths = c(16, 20), heights = c(15,15),
             top = "Lanzamientos bien clasificados frente a mal clasificados.")

# Resultados sobre los datos de test
pred_DTFit <- predict(DTFit, df_test)
samples <- data.frame(shot_id = df_test$ shot_id, shot_made_flag = pred_DTFit)
write.csv(samples,file = "sumbissionDTF.csv", row.names = F)

pred_svmLinearFit <- predict(svmLinearFit, df_test)
samples <- data.frame(shot_id = df_test$ shot_id, shot_made_flag = pred_svmLinearFit)
write.csv(samples,file = "sumbissionSVMLinear.csv", row.names = F)