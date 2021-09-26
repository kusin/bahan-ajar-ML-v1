## PROGRAM uji-korelasi-sederhana.R
# melakukan uji korelasi antara tingkat kelelahan dengan hasil tes
# sumber data dari contoh soal ppt pengenalan statistik
# masukan berupa data-frame, proses uji-korelasi pearson, keluaran scatter plot

# perform a correlation test between the level of fatigue and test results
# data source of statistical introduction ppt sample questions
# input in the form of data-frame, pearson correlation-test process, scatter plot output

## DEKLARASI
library(ggpubr) # proses uji korelasi #procces correlation 
library(ggplot2) # keluaran visualisasi data scatter plot # for create scatter plot

## ALGORITMA
# masukan data frame
# create input data frame
dataset <- data.frame(
  x = c(10, 8, 2, 1, 5, 6),
  y = c(2, 3, 9, 7, 6, 5)
)
dataset

# proses uji-korelasi pearson cara manual
# manual pearson correlation-test process. The process is from step 1 to 6
# langkah 1 hitung jumlah data x (dari baris 1 sampai baris n)
# step 1
sum_x <- sum(dataset$x)
sum_x

# langkah 2 hitung jumlah data y (dari baris 1 sampai baris n)
# step 2
sum_y <- sum(dataset$y)
sum_y

# langkah 3 jumlahkan hasil kuadrat data x (dari baris 1 sampai baris n)
# step 3
sum_square_x <- sum(dataset$x * dataset$x)
sum_square_x

# langkah 4 jumlahkan hasil kuadrat data y (dari baris 1 sampai baris n)
# steap 4
sum_square_y <- sum(dataset$y * dataset$y)
sum_square_y

# langkah 5 jumlahkan hasil perkalian x dan y
# step 5
sum_xy <- sum(dataset$x * dataset$y)
sum_xy

# langkah 6 rumus proses perhitungan uji-pearson
# step 6
n <- nrow(dataset) # menghitung jumlah data
r1 <- (n*sum_xy - sum_x*sum_y) / sqrt((n*sum_square_x - sum_x^2) * (n*sum_square_y - sum_y^2))
r1

# Proses Uji Korelasi, Keluaran nilai korelasi. menggunakan fungsi otomatis dari bahasa R
# Correlation Test Process, Output correlation value. using the automatic function of the R language
r2 <- cor.test(dataset$x, dataset$y, method = "pearson")

# Keluaran berupa scatter plot
# Output is a scatter plot
# Diagram scatter plot digunakan untuk melihat hubungan antara variabel bebas dengan terikat
# The scatter plot diagram is used to see the relationship between independent and dependent variables
ggplot(dataset, aes(x=x, y)) + geom_point()


