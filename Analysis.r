
library(tidyverse)
library(readr)
criminalitate<-read.csv("D:/OneDrive/Desktop/Master/An 1/SEM 1/Introducere in R/PROIECT/databaseCriminalitate.csv")
#criminalitate<-criminalitate[-1]

#verificare daca exista valori lipsa
sum(is.na(criminalitate))

#selectie conditii min pt 2 variabile
criminalitate<-subset(criminalitate, prbarr<=1 & year==82)

criminalitate_v2<-subset(criminalitate, prbconv<=1)

#redenumire variabile
names(criminalitate_v2)[c(4,5,10,12,13,14)]<-c("criminalitate_loc","prob_arest", "densitate","regiune", "urban","minoritati")

#names(criminalitate_v2)[1:7]<-c("rata_criminalitate","prob_arest","prob_cond","densitate","minoritati","regiune","urban")

#toate var. categ sa aiba categoriile definite
criminalitate_v2$prob_arest.cat<-cut(criminalitate_v2$prob_arest, breaks= c(0,0.3,0.5,0.7,1), labels= c("mica", "medie", "mare", "foarte mare"))


#urban, regiunea din char in factor
levels(criminalitate_v2$urban)<-c("nu","da")
criminalitate_v2$urban<-as.factor(criminalitate_v2$urban)
levels(criminalitate_v2$urban)
levels(criminalitate_v2$regiune)<-c("centrala","vest","alta")
criminalitate_v2$regiune<-as.factor(criminalitate_v2$regiune)

#eliminam variabile care nu sunt necesare
criminalitate_v2 <- criminalitate_v2[, -c(15:25)]
criminalitate_v2 <- criminalitate_v2[, -c(11)]
criminalitate_v2 <- criminalitate_v2[, -c(6:9)]
criminalitate_v2 <- criminalitate_v2[, -c(1:3)]

#se elimina variabila prob_arest
criminalitate_v2<-criminalitate_v2[-2] 

# clasele variabilelor
sapply(criminalitate_v2,class)


#exportare baza de date
write.csv(criminalitate_v2,"C:/Users/alexa/Desktop/Master/Introducere in R/criminalitate.csv") 

#rezultat Data
# nr de obs si variabile
dim(criminalitate_v2)
sapply(criminalitate_v2,class)
names(criminalitate_v2)
sum(is.na(criminalitate_v2))
#tipul datelor
str(criminalitate_v2)
dim(criminalitate_v2)



#--------------------3--------------------
#Analiza descriptiva a variabilelor numerice si nenumerice
#Rata criminalitatii
rata_criminalitate<-criminalitate_v2[1]
summary(rata_criminalitate)
library(psych)
describe(rata_criminalitate)
#Probabilitatea de arest
prob_arest<-criminalitate[5]
summary(prob_arest)
describe(prob_arest)
#Minoritati
minoritati<-criminalitate_v2[5]
summary(minoritati)
describe(minoritati)
#Densitate
densitate<-criminalitate_v2[2]
summary(densitate)
describe(densitate)
# Toate variabilele
summary(criminalitate_v2)
describe(criminalitate_v2)


#statistici descriptive pe grupuri
library(pastecs) 
stat.desc(criminalitate_v2, basic=F)
summary(criminalitate_v2$criminalitate_loc)


#statistici descriptive pe grupuri .  intre o var numerica si una factor
describeBy(criminalitate_v2$criminalitate_loc,group=criminalitate_v2$urban,digits= 4) 
describeBy(criminalitate_v2$criminalitate_loc,group=criminalitate_v2$regiune,digits= 4) 
describeBy(criminalitate_v2$criminalitate_loc,group=criminalitate_v2$prob_arest.cat,digits= 4) 

tapply(criminalitate_v2$criminalitate_loc, list(criminalitate_v2$prob_arest.cat, criminalitate_v2$regiune), mean)

#analiza grafica variabile numerice
hist(criminalitate_v2$criminalitate_loc)
hist(criminalitate$prbarr)
hist(criminalitate_v2$minoritati)
hist(criminalitate_v2$densitate)


#graf var categ
barplot(table(criminalitate_v2$regiune),legend.text=T)
barplot(table(criminalitate_v2$urban),legend.text=T)
barplot(table(criminalitate_v2$prob_arest.cat),legend.text=T)

#boxplot variabile numerice
boxplot(criminalitate_v2$criminalitate_loc,horizontal = TRUE)
boxplot(criminalitate$prbarr,horizontal = TRUE)
boxplot(criminalitate_v2$minoritati,horizontal = TRUE)
boxplot(criminalitate_v2$densitate,horizontal = TRUE)


#plots pt doua variabile numerice

plot(criminalitate_v2$criminalitate_loc,criminalitate_v2$prob_arest) 
plot(criminalitate_v2$rata_criminalitate,criminalitate_v2$minoritati)
plot(criminalitate_v2$rata_criminalitate,criminalitate_v2$densitate)



#--------------------4. Analiza statistica a variabilelor categoriale--------------------
#4.1 Tabelarea datelor (obtinere frecvente marginale, conditionate, partiale)
contin<-table(criminalitate_v2$prob_arest.cat,criminalitate_v2$urban) 
mytable<-table(criminalitate_v2$prob_arest.cat,criminalitate_v2$urban)
table(criminalitate_v2$prob_arest.cat,criminalitate_v2$urban)
prop.table(contin)             #tabel de frecvente
margin.table(contin, 1)        # Frecvente marginale pentru prob_arest.cat
margin.table(contin, 2)        # Frecvente marginale pentru urban
prop.table(contin)             # frecvente partiale
prop.table(contin, 1)          # frecvente conditionate dupa prob_arest.cat(pe linie)
prop.table(contin, 2)          # frecvente conditionate dupa urban (pe coloana)
as.data.frame(contin)          # frecvente absolute la nivel de grup
addmargins(contin)             # frecvente absolute marginale
addmargins(prop.table(contin)) # frecvente relative partiale si marginale

# Table() poate genera si tabele multidimensionale, folosing 3 sau mai multe variabile categoriale
# In acest caz, folositi functia ftable() pentru a afisa rezultatele intr-o forma mai atractiva
mytable2 <- table(criminalitate_v2$regiune, criminalitate_v2$urban, criminalitate_v2$prob_arest.cat)
ftable(mytable2) 

#4.2. Analiza de asociere
summary(table(criminalitate_v2$regiune,criminalitate_v2$prob_arest.cat))
summary(table(criminalitate_v2$regiune,criminalitate_v2$urban, criminalitate_v2$prob_arest.cat))
summary(table(criminalitate_v2$urban,criminalitate_v2$prob_arest.cat))


#4.3. Analiza de concordanta
chisq.test(table(criminalitate_v2$prob_arest.cat))



#--------------------5. Analiza de regresie si corelatie--------------------
#criminalitate_v2$rata_criminalitate<-rata_criminalitate

# 5.1. Analiza de corelatie pentru variabilele numerice

#Pentru a crea matricea corelatiilor pentru tot setul de date: 
#selectie conditii min 2 variabile
criminalitate<-subset(criminalitate,prbarr<=1 & year==82)
criminalitate_v3<-subset(criminalitate,prbconv<=1)
names(criminalitate_v3)[c(4,5,10,12,13,14)]<-c("criminalitate_loc","prob_arest", "densitate","regiune", "urban","minoritati")

#eliminam variabile care nu sunt necesare
criminalitate_v3 <- criminalitate[, -c(1,2,3,6,7,8,9,11,12,13,15,16,17,18,19,20,21,22,23,24,25)]
# redenumim variabilele
names(criminalitate_v3)[c(1,2,3,4)]<-c("criminalitate_loc","prob_arest", "densitate","minoritati")

# coeficientul de corelatie Pearson
cor(criminalitate_v3)
# test de corelatie
cor.test(criminalitate_v3$criminalitate_loc, criminalitate_v3$densitate)

# 5.2. Analiza de regresie

# 5.2.1. Regresie liniara simpla

# Model de regresie liniara simpla
ls<-lm(criminalitate_loc~densitate, data=criminalitate_v2)
summary(ls)
# Reprezentati grafic legatura dintre cele doua variabile
# crea scatterplot-ul si linia de regresie pentru model de regresie liniar simplu
plot(criminalitate_loc~densitate, criminalitate_v2)
abline(coef(ls))
#salvare reziduuri standardizate pt model de regresie liniar simplu
ls.resid=rstandard(ls) 
#verificare normalitate erori
qqnorm(ls.resid,ylab="Standardized Residuals",xlab="Normal Scores")
qqline(ls.resid)
summary(ls.resid)

# 5.2.1. Regresie liniara multipla
lm<-lm(criminalitate_loc~densitate+prob_arest.cat+minoritati,data=criminalitate_v2)
#afisare, pentru model de regresie liniara multipla, coeficienti standardizati
library(lm.beta)
lm.beta(lm)
#salvare reziduuri standardizate pt model de regresie liniar multipla
lm.resid=rstandard(lm) 
#verificare normalitate erori
qqnorm(lm.resid,ylab="Standardized Residuals",xlab="Normal Scores")
qqline(lm.resid)
summary(lm.resid)


# 5.2.2. Regresia neliniara
modelparabolic<-lm(criminalitate_loc~densitate+I(densitate^2),data=criminalitate_v2)
summary(modelparabolic)
# Ecuatia de regresie: Y= valoare intercept+valoare X1*X1+ valoare X2*X2^2
criminalitate_loc=0.0172491+0.0137232*criminalitate_v2$densitate-0.0006780*criminalitate_v2$densitate^2

# 5.2.*. Model logaritmic (cu variabila independenta logaritmata)
ll<-lm(criminalitate_loc~log(densitate), data=criminalitate_v2)
summary(ll)
# Ecuatia de regresie: Y= valoare intercept+valoare X1*X1
criminalitate_loc=0.033623+0.017147*log(densitate)


######## NU ESTE BUN
#anova pt model simplu si cel polinomial
anova(ls,modelparabolic)
#H0: modelul parabolic (cu mai multi parametri) nu este semnificativ mai bun decat modelul linar simplu
#H1: modelul parabolic (cu mai multi parametri) este semnificativ mai bun decat modelul liniar simplu
#Interpretare: (Pr(>F)) adica p-value=0.07229 > 0,05 , nu se respinge H0, cu o probabilitate de 95%.
#Modelul parabolic nu este semnificativ mai bun decât modelul liniar simplu.

# reprezentam grafic legatura dintre cele doua variabile
plot(criminalitate_loc~densitate,criminalitate_v2)
# vom crea un scatterplot cu o ajustare parabolica.
# numerele din paranteze se iau din summary(modelparabolic)
# curve(val Intercept + val X1*x + val X2*I(x^2),add=T)
curve(0.017249+0.013723*x-0.000678*I(x^2),add=T)
#add linia de la model de regresie simplu
abline(coef(ls))

#afisare, pentru model de regresie polinomial, coeficienti standardizati
library(lm.beta)
lm.beta(modelparabolic)

#  Regresie liniara multipla
lm<-lm(criminalitate_loc~densitate+prob_arest.cat+minoritati,data=criminalitate_v2)
#Folositi metoda stepwise pentru a selecta cel mai potrivit model.
# metoda stepwise care ne ajuta sa selectam variabilele care influenteaza semnificativ variabila dependenta Y
model_final<-step(lm)
formula(model_final)
#Salvati reziduurile modelului ca obiect. Efectuati o analiza descriptiva a lor.  Reprezentati-le grafic.
lm.resid=resid(lm)
summary(lm.resid)
# Interpretare: Media erorilor este 0.
plot(criminalitate_v2$criminalitate_loc,lm.resid,ylab="Resid",xlab="rata criminalitate",main="Distributia reziduurilor in functie de rata de criminalitate")
abline(0,0) 

qqPlot(ls) #verificare normalitate reziduuri
leveragePlots(ls) #identificare grafica a punctelor de influenta

criminalitate_v2$rata_Crim[criminalitate_v2$criminalitate_loc==0.0579285]<-NA


# 5.2.3. Compararea a doua modele de regresie si alegerea celui mai bun model
anova(ls,lm) 



######## NU ESTE BUN
ls<-lm(crmrte~density+prbarr+prbconv+pctmin80,data=criminalitate)
summary(ls)
lm.beta(ls)
model2<-step(ls,  direction="backward")
summary(model2)

hist(criminalitate$prbarr)
lj<-lm(crmrte~density+pctmin80+prbarr,data=cm)
summary(lj)
l<-step(ls,  direction="backward")
#prbarr    1 0.00161222 0.0047941 -756.52
#pctmin80  1 0.00206525 0.0052471 -749.21
density

cr<-subset(criminalitate, anul==87& prob_arest<=1&prbconv<=1)
library(tidyverse)
cr<-criminalitate_v2%>%
  mutate(pr=west+central)

criminalitate_v2$prob_cond.Cat<-cut(criminalitate_v2$prob_cond, c(0,0.3,0.5,0.7,1), c("mica", "medie", "mare", "foarte mare"))

criminalitate<-subset(criminalitate,prbarr<=1 & year==82)
criminalitate_v2<-subset(criminalitate,prbconv<=1)

criminalitate_v2<-subset(criminalitate_v2, select = c("crmrte","prbarr","prbconv","density","pctmin","region","smsa"))
 
anova(lm,ln)


#-------------------- 6. Estimarea si testarea mediilor --------------------
# 6.1. Estimarea mediei prin interval de incredere
t.test(criminalitate_v2$criminalitate_loc,conf.level = 0.95)
# 6.2. Testarea mediilor popula??iei
#6.2.1. Testarea unei medii cu o valoare fixa
t.test(criminalitate_v2$criminalitate_loc,mu=0.04)
#6.2.2. Testarea diferentei dintre doua medii
bartlett.test(criminalitate_loc~urban,criminalitate_v2)
t.test(criminalitate_v2$criminalitate_loc~criminalitate_v2$urban,var.equal=TRUE)
#6.2.3. Testarea diferentei dintre trei sau mai multe medii
bartlett.test(criminalitate_loc~prob_arest.cat,criminalitate_v2)
anova1<-aov(criminalitate_loc~prob_arest.cat,criminalitate_v2)
anova(anova1)
