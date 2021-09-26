# PROGRAM Pemusatan_Data.R
# Program ini digunakan untuk melihat rata-rata, nilai tengah, modus, ragam, stdv, rentang dari sampel dataset
# Kegunaan pemusatan data untuk melihat variasi data yang akan diolah.
# Hasil dari program ini adalah histrogram untuk visual distribusi data

#This program is used to view the mean, mean, mode, range, stdv, range of a sample dataset
# Use of data centering to see variations in data to be processed.
# The result of this program is a histogram for visual data distribution


# DEKLARASI
# DECLARATION
library(ggplot2)
library(dplyr)
library(modeest)


# ALGORITMA
# inputan data berupa data.frame
# input data in the form of data.frame
data_mhs <- data.frame(
  nim = c(seq("1114091101","1114091200")),
  algoritma_pemrograman = sample(50:100, 200, replace = TRUE),
  struktur_data = sample(50:100, 200, replace = TRUE),
  basis_data = sample(50:100, 200, replace = TRUE),
  statistik = sample(50:100, 200, replace = TRUE),
  #statistik = rnorm(mean=75, sd=6, 200),
  data_mining = sample(50:100, 200, replace = TRUE)
)
data_mhs

# konversi tipe data nim(numerik) menjadi nim(nominal)
# convert data type nim (numeric) to nim (nominal)
data_mhs$nim <- as.character(data_mhs$nim)

# proses melihat ringkasan dataset secara umum
# the process of viewing a summary dataset in general
summary(data_mhs)

# proses rata-rata, nilai tengah, modus
# process average, middle value, mode
mean(data_mhs$statistik) # average
median(data_mhs$statistik) # middle
mfv(data_mhs$statistik) # mode

# proses mencari rentang, standar deviasi, ragam
# process of searching for ranges, standard deviation, variants
range(data_mhs$statistik) #ranges
sd(data_mhs$statistik) #standard deviation
var(data_mhs$statistik) #variants

# melihat sebaran data untuk 1 variabel menggunakan histogram 
# see data distribution for one viable use histrogram
hist(data_mhs$statistik)

# output berupa histogram menggunakan package ggplot2 untuk mengetahui persebaran data
# Output is a histogram using the ggplot2 package to find out the distribution of data
ggplot(data=data_mhs, aes(x=statistik)) + 
  geom_histogram(aes(y=..density..),binwidth=5, colour="blue", fill="grey")+
  geom_density(alpha=.1, fill="yellow") +
  scale_x_continuous(name = "Nilai Statistik") +
  scale_y_continuous(name = "Frequensi") +
  ggtitle("Sebaran data nilai statistik")
