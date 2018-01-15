---
title: "5.0lr"
author: "Mironova Elena"
output: html_document
---
```{r}

#Пользуясь примером из лекции файл (5.0.R), проанализируйте данные
#о возрасте и физ. характеристиках молюсков
#https://archive.ics.uci.edu/ml/datasets/abalone
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=TRUE, sep=",")
summary(data)
colnames(data)
colnames(data) <- c("sex", "length", "diameter", "height", 
                "whole_weight", "shucked_weight",
                "viscera_weight", "shell_weight", "rings")

colnames(data)
data$sex <- factor(c("Female", "Infant", "Male")[data$sex])
summary(data)
par(mfrow=c(1,3)) 
hist(data$diameter, main = "Диаметр, мм")
hist(data$height, main = "Высота, мм")
hist(data$whole_weight, main = "Полный вес, гр")
```
```{r}
#Видим асимметрию https://en.wikipedia.org/wiki/Skewness
#и выбросы (от них нужно избавиться)

#Визулизируем возможные зависимости
par(mfrow=c(1,2)) 
plot(data$diameter, data$whole_weight,'p',main = "Зависимость веса от диаметра")
plot(data$height, data$whole_weight,'p',main = "Зависимость веса от высоты")
```
```{r}
#Хорошо видна зависимость, нужно её исследовать
#построить линейные модели при помощи функции lm, посмотреть их характеристики
#избавиться от выборосов, построить ещё модели и проверить их
#разделить массив данных на 2 случайные части
#подогнать модель по первой части
#спрогнозировать (функция predict) значения во второй части
#проверить качество прогноза
#Линейная модель зависимости веса от диаметра
linear.model.1<-lm(whole_weight~diameter,data=data)
linear.model.1
summary(linear.model.1)
plot(linear.model.1)
```
```{r}
#Линейная модель зависимости веса от высоты
linear.model.2<-lm(whole_weight~height,data=data)
linear.model.2
summary(linear.model.2)
plot(linear.model.2)
```
```{r}
#Определение отклонений для линейной модели Вес vs Высота
data.noout<-data[data$height<0.4&data$height>0.05,]
linear.model.3<-lm(whole_weight~height,data=data.noout)
linear.model.3
plot(linear.model.3)
```
```{r}
#Определение отклонений для линейной модели Вес vs Диаметр
data.noout<-data[data$height<0.4&data$height>0.03&data$diameter>0.1,]
linear.model.4<-lm(whole_weight~diameter,data=data.noout)
linear.model.4
plot(linear.model.4)
```
```{r}
#Линейная модель зависимости веса от высоты и диаметра
linear.model.4a<-lm(whole_weight~+height+diameter,data=data.noout)
linear.model.4a
summary(linear.model.4a)
plot(linear.model.4a)
```
```{r}
#Линейная модель "всё в одном" (кроме функций от веса)
linear.model.5<-lm(whole_weight~.-shucked_weight-viscera_weight-shell_weight,data=data.noout)
linear.model.5
summary(linear.model.5)
plot(linear.model.5)
```
```{r}
#Разделим массив данных и попробуем предсказать значения
odds <- seq(1, nrow(data.noout), by=2)
data.in <- data.noout[odds,]
data.out <- data.noout[-odds,]
data.in<-data.in
point<-round(runif(1,max=2078)); 
data.in<-data.in[-point,]; #удаление случайной точки, чтобы длины наборов данныхбыли одинаковы
```
```{r}
#Линейная модель "всё в одном" (кроме функций веса) с половинным набором данных
linear.model.5.half<-lm(whole_weight~.-shucked_weight-viscera_weight-shell_weight,data=data.in)
linear.model.5.half
summary(linear.model.5.half)
plot(linear.model.5.half)
```
```{r}
#Предсказание значений на известном наборе данных (in)
data.predict <- predict (linear.model.5.half)
cor (data.in$whole_weight, data.predict)
plot (data.in$whole_weight, data.predict)
#Корреляция: 0.9373871
```
```{r}
#Предсказание значений на "неизвестном" наборе данных (out)
data.predict.out <- predict (linear.model.5.half, data.out)
cor (data.out$whole_weight, data.predict.out)
plot (data.out$whole_weight, data.predict.out)
#Корреляция: 0.9393979

```