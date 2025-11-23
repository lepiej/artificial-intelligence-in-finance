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

#w ten sposob mamy ustandaryzowane skale w zbiorze 
#sprawdzamy czy pozostale zmienne w zbiorze moga nam okreslic ryzyko kredytu 'good'/'bad'

#index podzialu 
library(caret)

index.podz <- createDataPartition (CREDIT.R, times = 1, p=0.8, list=F)
#zbior treningowy 
train.data <- DATA[index.podz, ]
#zbior testowy
test.data <- DATA[- index.podz, ]


#zeby uruchomic model 
library(e1071)

#model klasyfikacja
#suprovides vector model gdzie .. oznaczaja wszystko
#kernel liniowy
model.klas <- svm(CREDIT.R ~ ., data = train.data,  kernel="linear", cost=1, gamma = 0.1)
#model przetrenowany 80%

pre.klas <- predict(model.klas, newdata = test.data)

#teraz porownany z danymi rzeczystwistymi na macierzy pomylek albo macierzy rozczarowan 
cm <- confusionMatrix(pre.klas, test.data$CREDIT.R)
cm
#nie nadaje sie do wprowadzenia w zycie bo chce zaproponowac wiecej dobrych kredytow niz zlych, 
#zupelnie nie rozroznia zlych. kappa to 0 a to rozroznianie nie przez przypadek, 
#P-Value tez nie powinna byc wysoka (brak informacji)
#w tym zbiorze brakuje informacji o dochodach 
#wiec go poprawimy 


model.klas <- svm(CREDIT.R ~ ., data = train.data,  kernel="radial", cost=1, gamma = 0.1)
pre.klas <- predict(model.klas, newdata = test.data)
cm <- confusionMatrix(pre.klas, test.data$CREDIT.R)
cm
#wykrywa juz zle kerdyty
#kappa wciaz jest male


model.klas <- svm(CREDIT.R ~ ., data = train.data,  kernel="polynomial", cost=1, gamma = 0.1)
pre.klas <- predict(model.klas, newdata = test.data)
cm <- confusionMatrix(pre.klas, test.data$CREDIT.R)
cm
#najwieksze kappa

#jeszcze jeden model do sprawdzenia 
model.klas <- svm(CREDIT.R ~ ., data = train.data,  kernel="sigmoid", cost=1, gamma = 0.1)
pre.klas <- predict(model.klas, newdata = test.data)
cm <- confusionMatrix(pre.klas, test.data$CREDIT.R)
cm
#dokladnosc i kappa spadly

#srednio te dane wychodza, zbior pewnie jest przemieszany 
#to podkrecimy paraemty cost i gamma
#wprowadzimy nowy obiekt kalibracyjny 

#dodamy cost i gamme jako lista 3x3 kombinacji do przetestowania 
kalibracja.klas <- tune(svm, train.x = CREDIT.R ~., data = train.data, kernel = "polynomial", ranges = list(cost=c(0.1, 1, 5), gamma=c(0.01, 0.1, 3)) )
pre.klas <- predict(model.klas, newdata = test.data)
cm <- confusionMatrix(pre.klas, test.data$CREDIT.R)
cm

#zmieniamy na kernel = radial
#dodamy cost i gamme jako lista 3x3 kombinacji do przetestowania 
kalibracja.klas <- tune(svm, train.x = CREDIT.R ~., data = train.data, kernel = "radial", ranges = list(cost=c(0.1, 1, 5), gamma=c(0.01, 0.1, 3)) )

#mamy rozne wyniki bo przy losowym indeksie mamy inne zmienne w grupie zajeciowej, wiec wyniki sa inne ale zblizone

#zrobmy wykres ktory przedstawi te dane 
plot(model.klas, data = test.data, Credit_amount ~ Duration)

#mozna uzyc tego samego algorytmu opisujac jedna zmienna 
#przyklad z modelem zaleznosci 
#bierzemy prostrzy model klas i przerabiamy go na podobny do regresji dlugosci splaty kredytu 
model.reg.D <- svm(Duration ~ Credit_amount + Installment_rate + CREDIT.R , data = train.data,  kernel="radial", cost=1, gamma = 0.1)

#przygotowanie predykcji czasu splaty kredytu i klasyfikacji 
pre.DURATION <- predict (model.reg.D, newdata = test.data )

#korelacja miedzy czasem splaty oczekiwanym a rzeczywistym 
postResample(pre.DURATION, test.data$Duration)
#ustawia podstawowe miary ktore sa zroznicowane 
plot(test.data$Credit_amount, test.data$Duration, col = "blue")
points(test.data$Credit_amount, pre.DURATION, col = test.data$CREDIT.R, Twd=3)
#to co czarne to "zly" kredyt -> dluzej splacany 

#wersja z objasnieniem na calym zbiorze 
model.reg.D <- svm(Duration ~., data = train.data,  kernel="radial", cost=1, gamma = 0.1)
pre.DURATION <- predict (model.reg.D, newdata = test.data )
postResample(pre.DURATION, test.data$Duration)

model.reg.D <- svm(Duration ~., data = train.data,  kernel="sigmoid", cost=1, gamma = 0.1)
pre.DURATION <- predict (model.reg.D, newdata = test.data )
postResample(pre.DURATION, test.data$Duration)
#r2 spadl 

plot(test.data$Credit_amount, test.data$Duration, col = "blue")
points(test.data$Credit_amount, pre.DURATION, col = test.data$CREDIT.R, Twd=3)
#ten wykres znacznie rozszerzony pionowo, wskazuje bledy 

#czy z korelacji da sie poprawic predykcje? 
#coreplot 
library(corrplot)
install.packages("corrplot") 

View(Credit_DATA_R)
#przygotowanie macierzy
M <- cor(data.frame(Credit_DATA_R[ , 1:7]))
M
#jedynie kwota kredytu z czasem splaty jest skorelowana znaczaco, cala resta ma korelacje mniejsza niz 0.1 do nawet ujemnych wartosci 
#test na istotnosc zmiennych

cor.test(Credit_DATA_R$Duration, Credit_DATA_R$Installment_rate)
corrplot.mixed(M)

#model regresji opisuje zmienne bez klasyfikacji 
model <- lm(Credit_DATA_R$Duration ~ Credit_DATA_R$Credit_amount + Credit_DATA_R$Installment_rate + as.factor(CREDIT.R) )
summary(model)

# *** oznaczaja, ze zmienna zawiera istotny parametr 