Garcons = c(9.2,9.6,9.7,9.7,9.8,10.5,10.5,10.5,11.5,12.4,13.2,13.7)
Filles = c(10.4,10.5,10.8,11.1,11.8,12.9,13.2,13.4,14.2,14.5)

str(Filles);str(Garcons)

lf = length(Filles)
lg= length(Garcons) #nrow si tableau 

moyf = mean(Filles)
moyg= mean(Garcons)

varf = var(Filles)
varg = var(Garcons)

Alpha = 0.05
sigmag = sqrt(2.2)
sigmaf = sqrt(2.4)

#le quantile se calcul avec qnorm

bornegaucheg = moyg-qnorm(1-Alpha/2)*sigmag/sqrt(lg)
bornedroiteg = moyg+qnorm(1-Alpha/2)*sigmag/sqrt(lg)

bornegauchef = moyf-qnorm(1-Alpha/2)*sigmaf/sqrt(lf)
bornedroitef = moyf+qnorm(1-Alpha/2)*sigmaf/sqrt(lf)

bornegaucheg;bornedroiteg
bornegauchef;bornedroitef

#b
Alpha2 = 0.1
bornegauche = (moyg-moyf)-qnorm(1-Alpha2/2)*sqrt((sigmag)^2/lg+(sigmaf)^2/lf)
bornedroite = (moyg-moyf)+qnorm(1-Alpha2/2)*sqrt((sigmag)^2/lg+(sigmaf)^2/lf)

bornegauche;bornedroite

#c Non car l'intervalle des differences n'inclut pas 0

#3.t.test donne un intervalle de confiance d'une moyenne quand variance est inconnue 

t.test(Garcons, conf.level = 0.95)
t.test(Filles, conf.level = 0.95)

t.test(Garcons, conf.level = 0.9)
t.test(Filles, conf.level = 0.9)

t.test(Garcons,Filles,conf.level = 0.95)

#4.
install.packages("EnvStats")
library(EnvStats)

varTest(Garcons,conf.level = 0.95)$conf.int
varTest(Filles, conf.level = 0.95)$conf.int

#5.
#10 points compris entre 11 et 13

pg= (sum (Garcons >13) +sum(Garcons<11))/lg
pg


pf= (sum (Filles >13) +sum(Filles<11))/lf
pf

#b intervalle de confiance d'une proportion avec prop ou binom.test
install.packages("epitools")
library(epitools)

prop.test(lg*pg,lg,conf.level=0.95)$conf.int
binom.test(lg*pg,lg,conf.level=0.95)$conf.int

binom.test(lf*pf,lf,conf.level=0.95)$conf.int

#c

bornegauche_p= (pg-pf)-qnorm(1-0.05/2)*sqrt((pg*(1-pg)/lg)+(pf*(1-pf)/lf))
bornedroite_p= (pg-pf)+qnorm(1-0.05/2)*sqrt((pg*(1-pg)/lg)+(pf*(1-pf)/lf))
bornegauche_p;bornedroite_p

#oui car 0 dans intervalle

