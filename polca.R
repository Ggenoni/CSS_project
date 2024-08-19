#   ========================================================
#    Gaudenzia Genoni
#    Computational Social Science - Research Project

#   Latent Class Analysis of Socio-Economic and Political Dynamics in Italy 
#   Before and During the COVID-19 Pandemic 
#   ========================================================
  
  
#load the package poLCA and emf images
library(poLCA)
library(devEMF)


### 2018-2020


#import the dataset
data9 <- read.csv("it_LCA9.csv")

#inspect the dataset
View(data9)
str(data9)
summary(data9)

#convert all variables into factors
data9[] <- lapply(data9, factor)
str(data9)

#Formula for variables in LCA
formula <- cbind(edlveit, hincfel, stflife, lrscale, vote, trstplt, psppsgva, polintr) ~ 1

#set a seed for reproducibility
set.seed(1234)

#fit LCA models (with specified number of latent classes)
lca9_model_2 <- poLCA(formula, data9, nclass = 2)
lca9_model_3 <- poLCA(formula, data9, nclass = 3)
lca9_model_4 <- poLCA(formula, data9, nclass = 4)


#compare models
aic_bic9 <- cbind(
  AIC = c(lca9_model_2$aic, lca9_model_3$aic, lca9_model_4$aic),
  BIC = c(lca9_model_2$bic, lca9_model_3$bic, lca9_model_4$bic)
)
print(aic_bic9)


#print the results
print(lca9_model_2)

#predicted class membership
predicted_classes9 <- lca9_model_2$predclass
table(predicted_classes9)  # Frequency of each class

#plotting and saving file
emf("LCA9.emf")
plot(lca9_model_2)
dev.off()


#__________________________________________________________________________



### 2020-2022

#import the dataset
data10 <- read.csv("it_LCA10.csv")

#inspect the dataset
View(data10)
str(data10)
summary(data10)

#convert all variables into factors
data10[] <- lapply(data10, factor)
str(data10)

#Formula for variables in LCA
formula <- cbind(edlveit, hincfel, stflife, lrscale, vote, trstplt, psppsgva, polintr) ~ 1

#set a seed for reproducibility
set.seed(1234)

#fit LCA models (with specified number of latent classes)
lca10_model_2 <- poLCA(formula, data10, nclass = 2)
lca10_model_3 <- poLCA(formula, data10, nclass = 3)
lca10_model_4 <- poLCA(formula, data10, nclass = 4)


# Compare models
aic_bic10 <- cbind(
  AIC = c(lca10_model_2$aic, lca10_model_3$aic, lca10_model_4$aic),
  BIC = c(lca10_model_2$bic, lca10_model_3$bic, lca10_model_4$bic)
)
print(aic_bic10)


# Print the results
print(lca10_model_2)

# Predicted class membership
predicted_classes10 <- lca10_model_2$predclass
table(predicted_classes10)  # Frequency of each class

# Plotting and saving file
png("LCA10.png")
plot(lca10_model_2)
dev.off()



