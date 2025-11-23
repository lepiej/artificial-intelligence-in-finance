data.sd <- scale( Credit_DATA_R [ ,1:7 ])

DATA <- as.data.frame(data.sd)

#rodzaje kredytu 

#credit risk
CREDIT.R <- Credit_DATA_R$Credit_risk

#wskazniki czynnikowe nadpisane
CREDIT.R <- as.factor(CREDIT.R)

#dodanie do zbioru
DATA <- cbind(DATA, CREDIT.R)


GENDER <- Credit_DATA_R$Gender
GENDER <- factor(GENDER)

#dodanie
DATA <- cbind(DATA, GENDER)
