# Early Childhood Longitudinal Study - Kindergarten cohort of 1998-1999 (ECLSK)
# https://nces.ed.gov/ecls/kindergarten.asp


DATA = read.table("/Users/kjgrimm/Google Drive/Personal/Presentations/Data Science 2018.10.25/Materials/ECLSK.dat", 
                  header = TRUE)

# Math scores measured in 8th grade.
# All other variables measured at kindergarten.

# GENDER: 1 = MALE
# RACE: 1 = White; 2 = Black/AfAm; 3 = Hisp; 4 = All other

DATA$RACE = as.factor(DATA$RACE)
  
summary(DATA)

# Split into training and testing sets

N = nrow(DATA)
n_training = floor(N*3/4)
training_rows = 1:n_training

TRAINING = DATA[training_rows,]
TESTING = DATA[-training_rows,]


## Regression Tree

# install.packages("rpart")
# install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

Tree1 = rpart(MATH7 ~  GENDER + POVRTY + RACE + KINDER1 +
             FMOTOR + GMOTOR + SCONTROL + INTERP + EXTERN + 
             INTERN + GENERAL + MOM_ED,
           data = TRAINING)

rpart.plot(Tree1)

pred = predict(Tree1, TESTING)

# R^2 = cor(pred,y)^2

cor(pred, TESTING$MATH7)^2

### Cross-validated Tree

Tree2 = rpart(MATH7 ~  GENDER + POVRTY + RACE + KINDER1 +
               FMOTOR + GMOTOR + SCONTROL + INTERP + EXTERN + 
               INTERN + GENERAL + MOM_ED,
             data = TRAINING,
             control = rpart.control(cp = .0005))

Tree2$cptable

plot(Tree2$cptable[,2], Tree2$cptable[,3], type = "l",col="red", lwd = 5)
lines(Tree2$cptable[,2], Tree2$cptable[,4],col="blue", lwd = 5)

minCP = Tree2$cptable[which.min(Tree2$cptable[,4]),1]

Tree.pruned = prune.rpart(Tree2, cp = minCP)

pred = predict(Tree.pruned, TESTING)

#R^2
cor(pred, TESTING$MATH7)^2

rpart.plot(Tree.pruned)

Tree.pruned



### RANDOM FOREST

# install.packages("randomForest")

library(randomForest)

RF.1 = randomForest(MATH7 ~  GENDER + POVRTY + RACE + KINDER1 +
               FMOTOR + GMOTOR + SCONTROL + INTERP + EXTERN + 
               INTERN + GENERAL + MOM_ED,
             data = TRAINING)

varImpPlot(RF.1)

pred = predict(RF.1, TESTING)

#R^2
cor(pred, TESTING$MATH7)^2
