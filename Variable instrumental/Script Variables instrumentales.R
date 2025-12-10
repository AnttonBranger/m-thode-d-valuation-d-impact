
# Variables instrumentales
#############################

library(ggplot2)
library(dplyr)
library(stargazer)
library(AER)

load("Electrification_Inde.RData")
str(elec_In)
summary(elec_In)


treat <- elec_In |> group_split(treat)
for(i in seq_along(treat)) {
  stargazer(
    data.frame(treat[[i]]),
    type = "text", 
    title = paste("Statistiques descriptives pour le groupe", unique(treat[[i]]$treat)),
    summary = TRUE, 
    digits = 2
  )
}

ggplot(elec_In, aes(age, x = factor(treat), fill = factor(treat))) + 
  geom_boxplot() +
  labs(fill = "Traitement") + 
  ylab("age (en années)") + 
  xlab("")

plot(elec_In$distance, elec_In$total_expenditure,
     col = c("steelblue", "red")[factor(elec_In$treat)], 
     pch= 20, 
     cex = 0.5,
     xlim = c(20,60),
     ylim = c(0,30000),
     xlab = "distance",
     ylab = "total expenditure")
abline(v = 40, lty = 2)
legend("topleft",pch=20,col=c("steelblue","darkred"),
       legend=c("Do not receive treatment","Receive treatment"))


# modele sans exogène
mod_sansexo <- ivreg(total_expenditure ~ treat | distance, data=elec_In)
summary(mod_sansexo,diagnostics = TRUE)
mod_sansexo$coefficients

m1<- glm(treat ~ 1,data=elec_In)
m2<- glm(treat ~ distance, data=elec_In)
lmtest::waldtest(m1,m2) # ou anova(m1,m2)

# modele avec exogenes
mod_avecexo <- ivreg(total_expenditure ~ treat + gender + birthplace + age + religion + caste | distance + gender + birthplace + age + religion + caste , data=elec_In)
summary(mod_avecexo,diagnostics = TRUE)


# ajout d'un instrument 
mod1b <- ivreg(total_expenditure ~ treat + gender + birthplace + age + religion + caste | distance + education + gender + birthplace + age + religion + caste, data=elec_In)
summary(mod1b,diagnostics = TRUE)

# ajout d'un effet fixe 
mod_ef <- ivreg(total_expenditure ~ treat + gender + birthplace + age + religion + caste  + factor(Pole)| distance + gender + birthplace + age + religion + caste + factor(Pole), data=elec_In)
summary(mod_ef,diagnostics = TRUE)

# ecart-types robustes clusterisés : covariance des residus - corrélation au sein de chaque groupe (pole)
library(multiwayvcov)
e_mod_ef <- coeftest(mod_ef, cluster.vcov(mod_ef, as.factor(elec_In$Pole)))
padj <- p.adjust(e_mod_ef[,4], method="BH")
head(padj)
