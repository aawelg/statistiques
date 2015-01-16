
### etude descriptive

temp.dat=tempsej #temp.dat=read.delim("tempsej.txt",header=T)
temp.dat$Ttemp=temp.dat$Ttemp/10 # degres celsius
temp.dat$Xlamb=temp.dat$Xlamb/100000
temp.dat$Ylamb=temp.dat$Ylamb/100000

# statistiques univariees
summary(temp.dat[,-1])
boxplot(temp.dat$Ttemp)

# statistiques bivariees
pairs(temp.dat[,-1])
cor(temp.dat[,-1]) # matrice de correlation

### modele lineaire

temp.lm=lm(Ttemp~Xlamb+Ylamb+mnt+rugo+pent+ori+rgl+en10+ind_veg+dmer+dFor+dvil, data=temp.dat)
summary(temp.lm)
# var. influent pour la temp. : mnt | pent | en10 | dmer
# R2 proche de 1 -> le modèle est valide

# analyse et normalite des residus d'estimation

# graphe de residus en fonction des valeures ajustees
plot(temp.lm$fitted,temp.lm$residuals,xlab="val ajustees",ylab="residus d'estimation")
# plot(fitted(temp.lm),residuals(temp.lm),xlab="val ajustees",ylab="residus d'estimation")
abline(h=0)

# visualisation graphique de la normalite
qqnorm(residuals(temp.lm))
qqline(residuals(temp.lm))
hist(residuals(temp.lm))

### detection des points influents et singuliers

inf.temp=influence(temp.lm) 
# graphe des points leviers Hii
plot(inf.temp$hat,ylab="Hii",xlab="individus")
abline(2*13/185,0) # comparaison avec les differents seuils 2(p+1) ou 3(p+1)
abline(3*13/185,0)
# test de bonferroni sur les residus de jackknife studentises
res.jack=rstudent(temp.lm)
plot(res.jack,ylim=c(-4,4))
# alpha du test de bonferroni (bilateral)
alpha=0.05/(2*185)
abline(-qt(alpha,185-13),0,lty=2)
abline(qt(alpha,185-13),0,lty=2)
# distance de cook
plot(cooks.distance(temp.lm),xlab="individus",ylab="distance de cook")
abline(1,0)

### selection des modeles les plus performants
temp.lm0=lm(Ttemp~mnt+pent+en10+dmer,data=temp.dat)
summary(temp.lm0)
anova(temp.lm0,temp.lm)
res.AIC=step(temp.lm)
res.BIC=step(temp.lm,k=log(185))
summary(res.AIC)
summary(res.BIC)
temp.lm2=lm(Ttemp~Xlamb+I(Xlamb^2)+Ylamb+I(Ylamb^2)+I(Xlamb*Ylamb)+mnt+pent+en10+dmer,data=temp.dat)
summary(temp.lm2)
### selection de var avec algo old school
res.leaps=regsubsets(Ttemp~., data=temp.dat)
res.sumleaps=summary(res.leaps)
# pour bic
res.sumleaps$bic
res.sumleaps$cp
# pour savoir tous les objets presents dans res.sumleaps
# attributes(res.sumleaps)

### intervalle de confiance et prediction
pred.temp=predict(temp.lm2,interval="prediction")
conf.temp=predict(temp.lm2,interval="confidence")

