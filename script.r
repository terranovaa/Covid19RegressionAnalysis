###### PREPROCESSING ######
# covid = read.csv("coronavirus-data-explorer.csv")
# Sposto la finestra temporale al 1' Settembre 2021, data per la quale sono presenti un minor numero di osservazioni mancanti
# covidReduced = covid[covid$date=="2021-09-01",]
# Seleziono i dati per i quali non ci sono valori mancanti per le colonne di interesse
# subsetCovid<-subset(covidReduced,  (!is.na(covidReduced$new_cases)) & (!is.na(covidReduced$reproduction_rate)) & (!is.na(covidReduced$stringency_index)) & (!is.na(covidReduced$people_fully_vaccinated)) & (!is.na(covidReduced$people_vaccinated)) & (!is.na(covidReduced$total_cases)) & (!is.na(covidReduced$total_deaths)))
# Seleziono le colonne di interesse
# tableCovid = subsetCovid[,c(3,5,6,8,17,36,37,46,47)]
###########################

table = read.csv("tabella.csv")
table <- data.frame(table[,-1], row.names = table[,1])

# Tabella con il logaritmo dei dati, da utilizzare successivamente
tablelog = log(table)
tablelog$new_cases[tablelog$new_cases == -Inf] = 0

# Standardizzazione della tabella del modello lineare
table = data.frame(scale(table))

# Esplorazione dei dati
summary(table)
plot(table)
round(cor(table),2)
library(corrplot)
corrplot(cor(table), "square")

# Modello di regressione lineare
r = matrix(ncol=2, nrow=4)
table.lm = lm(new_cases~., data=table)
summary(table.lm)
r[1,]=c(summary(table.lm)$r.squared, summary(table.lm)$adj.r.squared)

# Riduzione del modello di regressione lineare
table.lm2 = lm(new_cases~.-reproduction_rate, data=table)
summary(table.lm2)
r[2,]=c(summary(table.lm2)$r.squared, summary(table.lm2)$adj.r.squared)
table.lm3 = lm(new_cases~.-reproduction_rate-stringency_index, data=table)
summary(table.lm3)
r[3,]=c(summary(table.lm3)$r.squared, summary(table.lm3)$adj.r.squared)
table.lm4 = lm(new_cases~.-reproduction_rate-stringency_index-population, data=table)
summary(table.lm4)
r[4,]=c(summary(table.lm4)$r.squared, summary(table.lm4)$adj.r.squared)

# barplot dei p-value dei coefficienti
barplot(summary(table.lm)$coefficients[2:8,4], ylim=c(0,1), ylab = "p-value")
barplot(summary(table.lm4)$coefficients[2:5,4], ylim=c(0,0.0000001), ylab = "p-value")

ymin = min(r)
ymax = max(r)
plot(r[,1],pch=19,type="b",col="red",ylim=c(ymin,ymax))
lines(r[,2],pch=19,type="b",col="blue")
table.lm4.r = residuals(table.lm4)

#### Comandi utilizzati per mostrare l'effetto dell'eventuale ulteriore riduzione di fattori ####
# r = matrix(ncol=2, nrow=7)
# ... comandi precedenti per i vari modelli
# table.lm5 = lm(new_cases~.-reproduction_rate-stringency_index-population-total_deaths, data=table)
# r[5,]=c(summary(table.lm5)$r.squared, summary(table.lm5)$adj.r.squared)
# table.lm6 = lm(new_cases~.-reproduction_rate-stringency_index-population-total_deaths-people_fully_vaccinated, data=table)
# r[6,]=c(summary(table.lm6)$r.squared, summary(table.lm6)$adj.r.squared)
# table.lm7 = lm(new_cases~.-reproduction_rate-stringency_index-population-total_deaths-people_fully_vaccinated-people_vaccinated, data=table)
# r[7,]=c(summary(table.lm7)$r.squared, summary(table.lm7)$adj.r.squared)

# Modello di regressione non lineare
tablelog.lm=lm(new_cases~., data=tablelog)
summary(tablelog.lm)

# Analisi dei residui del modello di regressione lineare
plot(fitted(table.lm4),table.lm4.r)
which(fitted(table.lm4) == max(fitted(table.lm4))) # Anomalia in alto a destra
qqnorm(table.lm4.r)
qqline(table.lm4.r)
hist(table.lm4.r, 20, freq=F)
lines(density(table.lm4.r),col='red')
m = mean(table.lm4.r)
s = sd(table.lm4.r)
lines(sort(table.lm4.r),dnorm(sort(table.lm4.r),m,s))
shapiro.test(table.lm4.r)
mean(((table.lm4.r-mean(table.lm4.r))/sd(table.lm4.r))^3)
mean(((table.lm4.r-mean(table.lm4.r))/sd(table.lm4.r))^4)-3

# Rimozione degli outliers
outliers <- boxplot(table.lm4.r, plot=FALSE)$out
tableWithoutOutliers = table[-which(table.lm4.r %in% outliers),]
tableWithoutOutliers.lm = lm(new_cases~.-reproduction_rate-stringency_index-population, data=tableWithoutOutliers)
summary(tableWithoutOutliers.lm)
boxplot(table.lm4.r)
tableWithoutOutliers.lm.r = residuals(tableWithoutOutliers.lm)
boxplot(tableWithoutOutliers.lm.r)

#Analisi dei residui del modello che esclude gli outliers
plot(fitted(tableWithoutOutliers.lm),tableWithoutOutliers.lm.r)
qqnorm(tableWithoutOutliers.lm.r)
qqline(tableWithoutOutliers.lm.r)
hist(tableWithoutOutliers.lm.r, 20, freq=F)
lines(density(tableWithoutOutliers.lm.r),col='red')
m = mean(tableWithoutOutliers.lm.r)
s = sd(tableWithoutOutliers.lm.r)
lines(sort(tableWithoutOutliers.lm.r),dnorm(sort(tableWithoutOutliers.lm.r),m,s))
shapiro.test(tableWithoutOutliers.lm.r)
mean(((tableWithoutOutliers.lm.r-mean(tableWithoutOutliers.lm.r))/sd(tableWithoutOutliers.lm.r))^3)
mean(((tableWithoutOutliers.lm.r-mean(tableWithoutOutliers.lm.r))/sd(tableWithoutOutliers.lm.r))^4)-3

# Predizione e Autovalutazione
n = 20
err = rep(0,n)
errWithoutOutliers = rep(0,n)
for(i in 1:n){
  testset = sort(sample(96,10))
  table_train = table[-testset,]
  table_test = table[testset,]
  testsetWithoutOutliers = sort(sample(76,8))
  tableWithoutOutliers_train = table[-testsetWithoutOutliers,]
  tableWithoutOutliers_test = table[testsetWithoutOutliers,]
  table_train.lm = lm(new_cases~.-reproduction_rate-stringency_index-population, data=table_train)
  table_train.lm.p = predict(table_train.lm, table_test)
  err[i] = sqrt(mean((table_train.lm.p - table_test$new_cases)^2))
  tableWithoutOutliers_train.lm = lm(new_cases~.-reproduction_rate-stringency_index-population, data=tableWithoutOutliers_train)
  tableWithoutOutliers_train.lm.p = predict(tableWithoutOutliers_train.lm, tableWithoutOutliers_test)
  errWithoutOutliers[i] = sqrt(mean((tableWithoutOutliers_train.lm.p - tableWithoutOutliers_test$new_cases)^2))
}
mean(err)
median(err)
sd(err)
mean(errWithoutOutliers)
median(errWithoutOutliers)
sd(errWithoutOutliers)
gmin=min(err,errWithoutOutliers)
gmax=max(err,errWithoutOutliers)
plot(err, type="b", pch=20,col="blue", ylim=c(gmin,gmax))
points(errWithoutOutliers,type="b",pch=20,col="red")
