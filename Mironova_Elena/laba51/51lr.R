---
title: "5.0lr"
author: "Mironova Elena"
output: html_document
---
```{r}
#download data
data = read.csv("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-
2017/master/data/diet.csv", row.names = 1)
summary(data)

#oznakomimsja so strukturoj i pereimenuem colonki kak nam udobno
colnames(data) <- c("gender", "age", "height", "initial.weight",
"diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])

#dobavim kolonku POHUDENIE
data$weight.loss = data$initial.weight - data$final.weight

#analiz na razlichie tipov diet
boxplot(weight.loss ~ diet.type, data = data, col = "light gray",
ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h = 0, col = "green")

#proverim sbalansirovany dannye ili net
table(data$diet.type) 
plotmeans(weight.loss ~ diet.type, data = data) #grafik dannih
aggregate(data$weight.loss, by = list(data$diet.type), FUN = sd) #srednja poterja wesa

#test na mezhgruppovye razlichija
fit <- aov(weight.loss ~ diet.type, data = data)
summary(fit)

#poparnye razlichija mezhdu srednimi znachenijami
TukeyHSD(fit)
#tablitsa
#grafik
par(mar = c(5, 4, 6, 2))
tuk <- glht(fit, linfct = mcp(diet.type = "Tukey"))
plot(cld(tuk, level = .05), col = "lightgrey")

#dobavim proverku na vybrosy
plot(data$weight.loss, data$diet.type)
#udalim ih
data.noout <- data[data$weight.loss > 0,]
plot(data.noout$weight.loss, data.noout$diet.type)

#novye testy bez vybrosov
table(data.noout$diet.type)

plotmeans(weight.loss ~ diet.type, data = data.noout)

fit <- aov(weight.loss ~ diet.type, data = data.noout)
summary(fit)
TukeyHSD(fit)
tuk <- glht(fit, linfct = mcp(diet.type = "Tukey"))
plot(cld(tuk, level = .05), col = "lightgrey")

#dieta C ostalas luchshej, no dieta B stala luchshe


#zadanie2
#Are there gender differences for weight lost?
data.noout$gender <- factor(c("Female", "Male")[as.ordered(data.noout$gender)])
boxplot(weight.loss ~ gender, data = data.noout, col = "light gray",
ylab = "Weight loss (kg)", xlab = "Gender")

plotmeans(weight.loss ~ gender, data = data.noout)
aggregate(data.noout$weight.loss, by = list(data.noout$gender), FUN = sd)

#Effect of diet and gender on weight lost? Means plot of weight lost by diet and gender
data.noout$typebygender <- interaction(data.noout$diet.type, data.noout$gender)
fit <- aov(weight.loss ~ typebygender, data = data.noout)
summary(fit)
TukeyHSD(fit)
tuk <- glht(fit, linfct = mcp(typebygender = "Tukey"))
plot(cld(tuk, level = .05), col = "lightgrey")
#Add height to either ANOVA
data.noout$height <- cut(data.noout$height, 3, labels = c('small', 'mid',
'tall'))
fit <- aov(weight.loss ~ typebygender * height, data = data.noout)
summary(fit)
TukeyHSD(fit)