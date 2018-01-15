---
title: "classwork3"
author: "Mironova Elena"
output: html_document
---
```{r}
#Загрузите данные о землятресениях
anss <- readLines("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/earthquakes_2011.html", warn=FALSE)
```

```{r}
#Выберите строки, которые содержат данные, с помощью регулярных выражений и функции grep
str.position<-regexpr("^[^<>]+",anss); #нахождение позиций строк, содеражщих данные
str.value<-regmatches(anss,str.position); #создание вектора строк с данными

#str.value<-str.value[grep("[0-9]{4}/+",str.value)]; #исключение строки названий
#str.list<-strsplit(str.value,","); #разделение строк данных на отдельные элементы
#str.vector<-unlist(str.list); #представление в виде вектора
#str.matrix<-matrix(str.vector,ncol=12); #представление в виде матрицы: не совпадает количество элементов - 20253 вместо 20256, где-то пропущены элементы, поэтому не получается сформировать из вектора матрицу

#Проверьте, что все строки (all.equal) в результирующем векторе подходят под шаблон. 
anss.with.pattern<-grepl(pattern="^[^<>]+",x=anss)
all.equal(str.value,anss[which(anss.with.pattern)]);
```