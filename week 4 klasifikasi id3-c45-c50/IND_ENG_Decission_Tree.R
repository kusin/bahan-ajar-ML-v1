# link belajar (for study)
# https://rstudio-pubs-static.s3.amazonaws.com/94101_c23179ee360c43e0a63a791e410e1f3a.html
# https://data-flair.training/blogs/r-decision-trees/

## PROGRAM Klasifikasi_Decission_Tree.R
## klasifikasi untuk menentukan apakah dapat bepergian atau tidak
## classification to determine whether or not to travel
## variabel bebas yang digunakan jarak pandang, suhu udara, kelembaban udara, kecepatan angin
## independent variable use outlook, temperature, humadity, wind
## variabel terikat yang digunakan adalah berpergian
## the dependent variable used is travel

# DECLARATION for list used library 
# Deklarasi daftar pustaka yang digunakan
library(dplyr)
library(readxl) # read file xlsx
library(party) # lib for ID3 # many bug not recom to used
library(rpart) # lib for C45 # many bug not recom to used
library(C50) # lib for C50

# library lain yang dapat digunakan untuk metode klasifikasi ID3, c45, c50 adalah library(RWeka)
# library(RWeka) menggunakan gain ratio untuk pembentukan pohon keputisan. Namun jika menggunakan lib(Rweka) anda harus menggunakan lib(partykit)
# karena library(RWeka) dan library(partykit) saling dependen
# another one for lib to use method classification ID3, C45, C50 is library(Rweka)
# library(Rweka) used gain ration for create decission. But if used library(Rweka) you mush use lib(partykit)
# Because library(Rweka) and lib(partykit) Interdependent
library(RWeka)
library(partykit)

# load dataset
dataset <- read_excel("D:/Bahan Ajar Big Data/dataset/dataset_klasifikasi.xlsx")

# membuat dataset agar bernilai kategori sesuai sarat inputan decision tree
dataset <- data.frame(
  Outlook = as.factor(dataset$Outlook),
  Temperature = as.factor(dataset$Temperature),
  Humidity = as.factor(dataset$Humidity),
  Wind = as.factor(dataset$Wind),
  Travel = as.factor(dataset$Travel)
)
dataset

# membuat formula pohon keputusan, dengan menentukan variabel terikat sebagai label dan variabel bebas sebagai node pohonnya
# create formula decision, determine dependent variable as label and determine independent variable as node decision tree
# formula_id3 <- dataset$Travel ~ dataset$Outlook + dataset$Temperature + dataset$Humadity + dataset$Wind
# formula_c45 <- dataset$Travel ~ dataset$Outlook + dataset$Temperature + dataset$Humadity + dataset$Windy
# formula_c50 <- dataset$Travel ~ dataset$Outlook + dataset$Temperature + dataset$Humadity + dataset$Windy
formula_c45 <- Travel ~ Outlook + Temperature + Humidity + Wind
formula_c50 <- Travel ~ Outlook + Temperature + Humidity + Wind

# proses perhitungan nilai entropy, informasi gain dan gain ratio.
# processing for calculate value entropy, information gain dan gain ratio.
#ID3 <- ctree(Travel ~ ., data = dataset)
# cara 1
C45 <- J48(formula_c45, data=dataset) # cara 1 untuk memilih beberapa variabel saja. Umumnya sudah menggunakan seleksi fitur
C50 <- C5.0(formula_c50, data=dataset)
# cara 2
C45 <- J48(Travel ~ ., data=dataset) # Cara 2 untuk menggunakan semua variabel
C50 <- C5.0(Travel ~ ., data=dataset)

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

# Melakukan custom plot pohon keputusan menggunakan lib GGParty
# https://cran.r-project.org/web/packages/ggparty/vignettes/ggparty-graphic-partying.html
library(ggparty)

