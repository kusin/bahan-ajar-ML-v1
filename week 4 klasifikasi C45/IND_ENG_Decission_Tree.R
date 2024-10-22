library(dplyr)
library(readxl) # read file xlsx
library(party) # lib for ID3 # many bug not recom to used
library(rpart) # lib for C45 # many bug not recom to used
library(C50) # lib for C50
library(RWeka)
library(partykit)

# load dataset
dataset <- read.csv("../dataset/PlayTennis.csv")

# membuat dataset agar bernilai kategori sesuai sarat inputan decision tree
dataset <- data.frame(
  Outlook = as.factor(dataset$Outlook),
  Temperature = as.factor(dataset$Temperature),
  Humidity = as.factor(dataset$Humidity),
  Wind = as.factor(dataset$Wind),
  Play_Tennis = as.factor(dataset$Play_Tennis)
)
dataset

# membuat formula pohon keputusan, dengan menentukan variabel terikat sebagai label dan variabel bebas sebagai node pohonnya
formula_c45 <- Play_Tennis ~ Outlook + Temperature + Humidity + Wind
formula_c50 <- Play_Tennis ~ Outlook + Temperature + Humidity + Wind

# proses perhitungan nilai entropy, informasi gain dan gain ratio.
C45 <- J48(formula_c45, data=dataset) # cara 1 untuk memilih beberapa variabel saja. Umumnya sudah menggunakan seleksi fitur
C50 <- C5.0(formula_c50, data=dataset)

# cara 2
C45 <- J48(Play_Tennis ~ ., data=dataset) # Cara 2 untuk menggunakan semua variabel
C50 <- C5.0(Play_Tennis ~ ., data=dataset)

# Melihat aturan fuzzy dari pohon keputusan yang terbentuk
#See the fuzzy rules of the formed decision tree
print(C45)
print(C50)

# Melihat Akurasi dan galat dari hasil klasifikasi
# Viewing the accuracy and errors of the classification results
summary(C45)
summary(C50)

# membuat plot pohon keputusannya
# create plot decission tree
plot(C45,type="simple")
plot(C50,type="simple")

