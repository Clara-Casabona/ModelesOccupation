# Code R de l'atelier Modèles d'occupation

###Avis ###
                                                                            
# Ceci est un script afin de suivre les diferents codes présenters pendant
# l'atelier. 
#                                                                             #
# Il est minimalement annoté pour permettre aux participants de fournir leurs #
# commentaires : une pratique que nous encourageons vivement.                 #
#                                                                             #
# Notez que les solutions aux défis ne sont pas incluses dans ce script.     #
# Les solutions vont être révelés lors de l'atelier                           #
#                                                                             #
# Bon codage !  



#install.packages("unmarked", dependencies = TRUE)
# install.packages("AICcmodavg")
# install.packages("ggplot2")

library(unmarked)
library(AICcmodavg)
library(ggplot2)

# Chargez le jeu de données

borchi <- read.csv("borchi.csv", header = TRUE) 

# Cette ligne variera en fonction de l'endroit où vos données sont enregistrées. 
# Vérifiez votre répertoire de travail avec getwd(), et changez-le avec setwd() au besoin.

# Explorez les données 

str(borchi)
summary(borchi)



# Likelihood 

probDet <-seq(from=0, to=1, by=0.01)

LL = dbinom(x = 2, size = 3, prob = probDet)

plot(LL~probDet, type = "l", 
     ylab = "Likelihood", 
     xlab = "Probabilité de détection",
     main = "Probabilité de détection d'une mésange à tête brune (3 visites, 2 detections)"
     )

MLE = probDet[which(LL==max(LL))] 

abline(v = MLE, col="red", lwd=3, lty=2)


# Formatage des données

### Standardisé les variables

conifer_mean <- mean(borchi$conif)
conifer_sd <- sd(borchi$conif)
conifer_std <- as.data.frame((borchi$conif - conifer_mean)/conifer_sd)
names(conifer_std)[1] = paste("conifer_std")


temp =  borchi[, c(5:7)]
temp_mean <- mean(as.vector(as.matrix(temp)))
temp_sd <- sd(as.vector(as.matrix(temp)))
temp_std <- (temp - temp_mean)/temp_sd


jj = borchi[, c(8:10)]
jj_mean <- mean(as.vector(as.matrix(jj)))
jj_sd <- sd(as.vector(as.matrix(jj)))
jj_std <- (jj - jj_mean)/jj_sd


# Formater les données avec la fonction unmarkedFrameOccu() du package unmarked

borchi_data <- unmarkedFrameOccu(y = borchi[, c(1:3)],
                                 siteCovs =   conifer_std,
                                 obsCovs = list(temp_std =temp_std,
                                                jj_std=jj_std))
#  Explorer l'objet

detHist(borchi_data)

summary(borchi_data)


# Création des modèles

mod_null  <- occu( ~ 1 ~ 1,     data = borchi_data)

mod_occ   <- occu( ~ 1 ~ conifer_std, data = borchi_data)

mod_det   <- occu( ~ temp_std + jj_std  ~ 1, data = borchi_data)

mod_complet   <- occu(  ~ temp_std + jj_std ~ conifer_std, data = borchi_data)


# Inspecter les modèles

summary(mod_null)
summary(mod_occ)
summary(mod_det)
summary(mod_complet)


# Test d'ajustement

### Attention! Cette étape peut prendre quelques minutes à rouler (ou heures si le jeu de données et le nombre de simulations sont très grands)

gof <- mb.gof.test(mod_complet, nsim = 10, plot.hist = TRUE) # 10 simulations c'est pas assez! Mais si vous faites rouler cette ligne avec juste 10 cela va prendre seulement quelques secondes
gof

save(gof, file = "mod_complet_gof.Rdata")

gof$chisq.tab

# Sélection de modèles

Modeles <- list(mod_null, mod_occ, mod_det, mod_complet)

Names   <- c("mod_null","mod_occ","mod_det","mod_complet")

aictab(cand.set = Modeles, modnames = Names, c.hat = 1.11)

# Inférence multimodèle

modavgShrink(cand.set = Modeles, modnames = Names,
             c.hat = 1.11, parm = "conifer_std",
             parm.type = "psi")


# Inférence multimodèle  Défi

modavgShrink(cand.set = Modeles, modnames = Names,
             c.hat = 1.11, parm = "temp_std",
             parm.type = "detect")


# Predictions

## Pour la temperature

### Crée un nouveau jeu de données

new_dats <- data.frame(temp = 
                         seq(from = min(temp),
                             to = max(temp), 
                             length.out = 30),
                       jj_std = 0) 
####  Ajouter les valeurs standardisées pour faire les estimations

new_dats$temp_std <- (new_dats$temp - temp_mean)/temp_sd

###  Calculer les prédictions avec modavgPred()
preds <- modavgPred(cand.set = Modeles, modnames = Names,
                    c.hat = 1.11, newdata = new_dats, 
                    type = "response", parm.type = "detect")

new_dats$mod_avg_pred <- preds$mod.avg.pred
new_dats$uncond_se <- preds$uncond.se
new_dats$low95 <- preds$lower.CL
new_dats$upp95 <- preds$upper.CL

### Résumer les résultats dans un graphique

ggplot(new_dats, aes(temp)) + 
  geom_line(aes(y=mod_avg_pred), colour="blue") + 
  ylim(0,1) + 
  geom_ribbon(aes(ymin=low95, ymax=upp95), alpha=0.2) +
  labs(y =expression("Probabilité de détection" (p)), 
       x = expression(paste("Température de l'air (", 
                            degree, "C)"))) +
  theme_bw()

# Predictions - Défi


### Pourcentage de conifères

new_dats <- data.frame(conif = 
                         seq(from = min(borchi$conif),
                             to = max(borchi$conif), 
                             length.out = 30)) 
new_dats$conifer_std <- (new_dats$conif - conifer_mean)/conifer_sd

preds <- modavgPred(cand.set = Modeles,
                    modnames = Names,
                    c.hat = 1.11, 
                    newdata = new_dats, 
                    type = "response", 
                    parm.type = "psi")

new_dats$mod_avg_pred <- preds$mod.avg.pred
new_dats$uncond_se <- preds$uncond.se
new_dats$low95 <- preds$lower.CL 
new_dats$upp95 <- preds$upper.CL

## Plot predictions
ggplot(new_dats, aes(conif)) + 
  geom_line(aes(y=mod_avg_pred), colour="blue") + 
  geom_ribbon(aes(ymin=low95, ymax=upp95), alpha=0.2) +
  ylim(0,1) + 
  labs(y =expression("Probabilité d'occupation" (psi)), 
       x = "% Conifères dans la parcelle") +
  theme_bw()

