library(dplyr)
library(ggplot2)
library(caret)
library(Amelia)
library(pROC)
library(randomForest)
library(scales)
library(corrplot)

# Citirea datelor
data <- read.csv("C:/Users/User/Desktop/univ/anul 3/sem 1/AD/heardataset.csv")

# Vizualizarea primelor rânduri de date și verificarea valorilor lipsă
head(data)
colnames(data)
sum(is.na(data))
missmap(data, main = "Mapa valorilor lipsă în setul de date")

#######################################
#EDA
# Identificarea tipurilor de variabile
numeric_cols <- sum(sapply(data, is.numeric))
categorical_cols <- sum(sapply(data, is.factor))

# Crearea unui data frame pentru numărul de variabile numerice și categorice
var_counts <- data.frame(
  Type = c("Numeric", "Categorical"),
  Count = c(numeric_cols, categorical_cols)
)

# Crearea graficului pentru numărul de variabile numerice și categorice
ggplot(var_counts, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Tipul de Variabilă", y = "Numărul de Variabile", fill = "Tipul de Variabilă") +
  ggtitle("Distribuția Numărului de Variabile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print("Numărul de variabile numerice:")
print(sum(numeric_cols))
print("Variabile numerice:")
print(names(data[numeric_cols]))

print("Numărul de variabile categorice:")
print(sum(categorical_cols))
print("Variabile categorice:")
print(names(data[categorical_cols]))


numeric_data <- data[, sapply(data, is.numeric)]

data_long <- numeric_data %>%
  tidyr::gather(key = "Variable", value = "Value")

# Grafic de densitate pentru fiecare variabilă numerică
ggplot(data_long, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Distribuția Variabilelor Numerice", x = "Valoare", y = "Densitate") +
  theme_minimal()

#######################################
#Analizarea presupunerilor propuse
#1
data$age_group <- cut(data$age, 
                      breaks = c(0, 30, 40, 50, 60, 70, Inf), 
                      labels = c("Sub 30", "30-40", "40-50", "50-60", "60-70", "Peste 70"))

ggplot(data, aes(x = factor(target), fill = factor(age_group))) +
  geom_bar(position = "dodge") +
  labs(x = "Prezența Bolii Cardiace", y = "Numărul de Cazuri", fill = "Grupa de Vârstă") +
  ggtitle("Distribuția Bolii Cardiace în Funcție de Grupa de Vârstă")
    
#2
ggplot(data, aes(x = factor(target), fill = factor(sex))) +
  geom_bar(position = "dodge") +
  labs(x = "Prezența Bolii Cardiace", y = "Numărul de Cazuri", fill = "Sex") +
  ggtitle("Distribuția Bolii Cardiace în Funcție de Sex")

#3
ggplot(data, aes(x = factor(cp), fill = factor(target))) +
  geom_bar(position = "dodge") +
  labs(x = "Tipul Durerii Toracice", y = "Numărul de Cazuri", fill = "Prezența Bolii Cardiace") +
  ggtitle("Distribuția Tipului Durerii Toracice în Funcție de Boala Cardiacă")


#4
ggplot(data, aes(x = factor(target), y = trestbps)) +
  geom_boxplot() +
  labs(x = "Prezența Bolii Cardiace", y = "Tensiunea Arterială la Repaus") +
  ggtitle("Tensiunea Arterială la Repaus în Funcție de Boala Cardiacă")

ggplot(data, aes(x = factor(target), y = chol)) +
  geom_boxplot() +
  labs(x = "Prezența Bolii Cardiace", y = "Nivelul de Colesterol") +
  ggtitle("Nivelul de Colesterol în Funcție de Boala Cardiacă")

#5
ggplot(data, aes(x = factor(fbs), fill = factor(target))) +
  geom_bar(position = "dodge") +
  labs(x = "Glicemia în Repaus", y = "Numărul de Cazuri", fill = "Prezența Bolii Cardiace") +
  ggtitle("Distribuția Glicemiei în Repaus în Funcție de Boala Cardiacă")


#6
ggplot(data, aes(x = factor(target), y = thalach)) +
  geom_boxplot() +
  labs(x = "Prezența Bolii Cardiace", y = "Frecvența Cardiacă Maximă") +
  ggtitle("Frecvența Cardiacă Maximă în Funcție de Boala Cardiacă")

#7
ggplot(data, aes(x = factor(exang), fill = factor(target))) +
  geom_bar(position = "dodge") +
  labs(x = "Angina Indusă de Exercițiu", y = "Numărul de Cazuri", fill = "Prezența Bolii Cardiace") +
  ggtitle("Distribuția Anginei Induse de Exercițiu în Funcție de Boala Cardiacă")

#8
ggplot(data, aes(x = factor(target), y = oldpeak)) +
  geom_boxplot() +
  labs(x = "Prezența Bolii Cardiace", y = "Oldpeak") +
  ggtitle("Oldpeak în Funcție de Boala Cardiacă")

ggplot(data, aes(x = factor(target), y = slope)) +
  geom_boxplot() +
  labs(x = "Prezența Bolii Cardiace", y = "Slope") +
  ggtitle("Slope în Funcție de Boala Cardiacă")

ggplot(data, aes(x = factor(target), y = ca)) +
  geom_boxplot() +
  labs(x = "Prezența Bolii Cardiace", y = "CA") +
  ggtitle("CA în Funcție de Boala Cardiacă")

ggplot(data, aes(x = factor(target), y = thal)) +
  geom_boxplot() +
  labs(x = "Prezența Bolii Cardiace", y = "Thal") +
  ggtitle("Thal în Funcție de Boala Cardiacă")

numeric_data <- data %>% select_if(is.numeric)

# Calcularea matricei de corelație
correlation_matrix <- cor(numeric_data)

# Definim o paletă de culori
col <- colorRampPalette(c("#7F0000", "white", "#0022FF"))(200)

# Creăm o matrice de corelație estetică folosind pachetul corrplot
corrplot(correlation_matrix, method = "circle", type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         diag = FALSE, tl.cex = 0.8, cl.cex = 0.8, 
         col = col, title = "Correlation Plot of Features", mar = c(1,1,1,1))

#######################################
# Normalizarea datelor
data_norm <- as.data.frame(scale(data[, sapply(data, is.numeric)]))
data_norm$target <- as.factor(data$target)

# Împărțirea datelor pentru antrenament și test
set.seed(42) 
partition <- createDataPartition(data_norm$target, p = 0.8, list = FALSE)
training_set <- data_norm[partition, ]
testing_set <- data_norm[-partition, ]

# Construirea modelelor
model1 <- glm(target ~ 
                age + 
                sex + 
                chol + 
                trestbps + 
                fbs, 
              data = training_set, family = "binomial")

aic_model1 <- AIC(model1)

model2 <- glm(target ~ 
                thalach + 
                exang + 
                oldpeak + 
                slope, 
              data = training_set, family = "binomial")

aic_model2 <- AIC(model2)

model3 <- glm(target ~ 
                cp + 
                restecg + 
                ca + 
                thal, 
              data = training_set, family = "binomial")

aic_model3 <- AIC(model3)

print(paste("AIC pentru Modelul 1:", aic_model1))
print(paste("AIC pentru Modelul 2:", aic_model2))
print(paste("AIC pentru Modelul 3:", aic_model3))

model_names <- c("Model 1", "Model 2", "Model 3")

aic_df <- data.frame(Model = model_names, AIC = c(aic_model1, aic_model2, aic_model3))
ggplot(aic_df, aes(x = Model, y = AIC)) + 
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Compararea Valorilor AIC pentru Diferite Modele") +
  xlab("Model") +
  ylab("Valoarea AIC")


pred_model1 <- predict(model1, newdata = testing_set, type = "response")
pred_model2 <- predict(model2, newdata = testing_set, type = "response")
pred_model3 <- predict(model3, newdata = testing_set, type = "response")

# Calculează curbele ROC pentru fiecare model
roc_model1 <- roc(testing_set$target, pred_model1)
roc_model2 <- roc(testing_set$target, pred_model2)
roc_model3 <- roc(testing_set$target, pred_model3)


plot(roc_model1, col = "red", legacy.axes = TRUE)
plot(roc_model2, col = "blue", add = TRUE)
plot(roc_model3, col = "green", add = TRUE)
legend("bottomright", legend = c("Model 1", "Model 2", "Model 3"), col = c("red", "blue", "green"), lty = 1)

# Generarea predicțiilor pentru fiecare model
predictions_model1 <- predict(model1, newdata = testing_set, type = "response")
predictions_model2 <- predict(model2, newdata = testing_set, type = "response")
predictions_model3 <- predict(model3, newdata = testing_set, type = "response")

# Conversia predicțiilor în factori (binarizare folosind un prag de 0.5)
predictions_model1 <- as.factor(ifelse(predictions_model1 > 0.5, 1, 0))
predictions_model2 <- as.factor(ifelse(predictions_model2 > 0.5, 1, 0))
predictions_model3 <- as.factor(ifelse(predictions_model3 > 0.5, 1, 0))

# Calculul matricelor de confuzie pentru fiecare model
confusion_matrix_model1 <- confusionMatrix(predictions_model1, testing_set$target)
confusion_matrix_model2 <- confusionMatrix(predictions_model2, testing_set$target)
confusion_matrix_model3 <- confusionMatrix(predictions_model3, testing_set$target)

# Afișarea matricelor de confuzie
print("Matricea de confuzie pentru Modelul 1:")
print(confusion_matrix_model1)
print("Matricea de confuzie pentru Modelul 2:")
print(confusion_matrix_model2)
print("Matricea de confuzie pentru Modelul 3:")
print(confusion_matrix_model3)

#######################################
# Antrenarea modelului Random Forest
model_rf <- randomForest(target ~ ., data = data, importance = TRUE)

# Extrageți importanța și transformați-o într-un dataframe
importance_df <- as.data.frame(importance(model_rf))
importance_df$variable <- rownames(importance_df)

#Importanta variabilelor
importance_df <- importance_df[order(importance_df$IncNodePurity, decreasing = TRUE),]
importance_df$color <- rescale(importance_df$IncNodePurity, c(0, 1))  

green_to_red <- colorRampPalette(c("green", "red"))

ggplot(importance_df, aes(x = reorder(variable, IncNodePurity), y = IncNodePurity, fill = color)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Variabile") +
  ylab("Importanța (Creșterea Purității Nodului)") +
  ggtitle("Importanța Variabilelor") +
  scale_fill_gradientn(colors = green_to_red(100)) 
