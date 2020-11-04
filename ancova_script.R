#ANCOVA

install.packages("car")
install.packages("remotes")
remotes::install_github("ViniciusBRodrigues/GLMdiagnostic")

data <- read.csv(file.choose(), sep = ";", header = T)
attach(dados)

library(car)

leveneTest(Weight~Parasite, center = mean, data = data) #Homocedasticity test

ajus_A <- lm(Weight ~ Parasite, data = data) #Create a model for extract the residual deviance
summary(ajus_A)

shapiro.test(resid(ajus_A)) #Normality test

hist(Weight) #Check the distribution of the response variable

m.1= glm(Weight~Length*Parasite,family=gaussian) #Run glm with gaussian distribution
m.n = glm(Weight~1,family=gaussian) #Run a null model
anova(m.n,m.1,test="F") #Test significance between models

library(biodata)

modeldiagnostic1(m.1) #Check the distribution of the response variable

anova(m.1,test="F") #Run anova

summary(m.1)
