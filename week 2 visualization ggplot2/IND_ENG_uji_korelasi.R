## PROGRAM uji-korelasi.R
## Digunakan untuk melakukan analisa hubungan curah hujan dengan radiasi matahari, suhu udara, kelembaban udara, kecepatan angin
## Masukan berupa dataset format csv dri Data Online BMKG, wilayah Jakarta periode waktu Febuari 2019 - April 2019
## Proses berupa uji korelasi metode spearman rank, kendall, pearson product moment
## Keluaran berupa Nilai korelasi, Nilai determinasi dan Visualisasi data scatter plot

## Used to analyze the relationship between rainfall and solar radiation, air temperature, humidity, wind speed
## Input in the form of csv dataset format from BMKG Online Data, Jakarta area for the period of February 2019 - April 2019
## The process takes the form of a correlation test with the Spearman Rank method, Kendall, Pearson product moment
## Output is in the form of correlation value, determination value and scatter plot data visualization


## DEKLARASI
library(readxl) # membaca file excel # Read excel file
library(ggpubr) # proses uji korelasi #procces correlation test
library(ggplot2) # keluaran visualisasi data scatter plot #output of scatter plot data visualization
library(ggcorrplot) # keluaran jika membuat headmap (matrik korelasi) # output for matrix correlation

## ALGORITMA
# membaca dataset Data Online BMKG
# read dataset from BMKG
dataset <- read_excel("D:/# Mengajar/Bu Nur/2020-2021 Big Data/Materi/Pertemuan 3/Dataset.xlsx", sheet = "Dataset")

# see summary from dataset
summary(dataset)

# Proses menghitung Uji-Pearson dengan fungsi cor(var x, var y, metode uji-korelasi)
# The process of calculating the Pearson-Test with the corr function (var x, var y, test-correlation method)
# if use method pearson
pearson <- cor(dataset$suhu_rataan, dataset$kelembaban_udara, method = "pearson")
pearson
# if use method kendall
kendal <- cor.test(dataset$suhu_rataan, dataset$kelembaban_udara, method = "kendall")
kendall
# if use method spearman
spearman <- cor.test(dataset$suhu_rataan, dataset$kelembaban_udara, method = "spearman")
spearman

# hilangkan tanggal untuk membuat membuat matrik korelasi
# remove date for create matrix correlation
dataset$Tanggal <- NULL

# membuat uji korelasi seluruh variabel yang digunakan menggunakan fungsi cor_pmat
# make a correlation test of all variables used using the cor_pmat function
p.mat <- cor_pmat(dataset)
p.mat

# output hasil korelasi menggunakan teknik headmap diagram
# output of correlation results using the headmap diagram technique

# membuat headmap atau ada yang menyebutnya sebagai korelasi diagram
# create heatma or correlation diagram
ggcorrplot(p.mat) #cara basic #the basic way

# method 2 adds the correlation value into the heatmap
ggcorrplot(p.mat, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE)
ggcorrplot(p.mat, hc.order = TRUE, type = "upper", outline.col = "white", lab = TRUE)

