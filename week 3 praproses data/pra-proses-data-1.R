# PROGRAM Pra-proses Data
# Model Prediksi Deret Waktu Titik Panas Dengan Memperhatikan Faktor ENSO. (Studi Kasus : Sumatera Selatan)
# variabel terikat adalah titik panas
# variabel bebas adalah ENSO yang terdiri dari SST Nina 3.4, SOI Index, ONI Index.
# pemahaman ini akan digunakan kembali untuk membahas metode LSTM-RNN


# pra proses merupakan cara untuk mempersiapkan sebuah data yang menjadi lebih terstuktur sehingga sesuai dengan inputan yang diinginkan.
# beberapa teknik praproses yang umum dilakukan adalah
#   - normalisasi data (merubah rentang nilai menjadi -1 sampai 1)
#   - pemotongan data (memilih data sesuai dengan rentang data tertentu atau sesuai area studi tertentu)
#   - seleksi fitur (memilih fitur yang paling berpengaruh terdahap label / variabel terikat)
#   - aggregasi data (Mengelompokan data berdasarkan kondisi tertentu)
#   - intergasi data (Menggabungkan beberapa data menjadi sebuah data baru)
#   - transformasi data (Merubah baris menjadi kolom)
#   - penghalusan data (mengisi data-data kosong)


# DEKLARASI pustaka
library(readr) # membaca file excel/csv/txt/json
library(readxl) # membaca file excel/csv/txt/json
library(dplyr) # memanipulasi data
library(ggpubr) # menghitung uji korelasi
library(ggcorrplot) # visualisasi uji korelasi
library(ggplot2) # membuat diagram
library(plotly) # memperindah diagram
library(gridExtra) # menyatukan sebanyak n visualisasi kedalam 1 frame


# DEKLARASI fungsi
# membuat fungsi normalisasi # create a function normalize
# menggunakan teknik max-min # menggunakan teknik max-min
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# ALGOITMA
# akusisi data
# link download titik panas https://firms.modaps.eosdis.nasa.gov/download/create.php
# link download SST Nina 3.4 https://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices
# link download SOI Index https://www.cpc.ncep.noaa.gov/data/indices/soi
# link download ONI Index https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt
# ------------------------------------------------------------------------------


# eksplorasi data analisis
# - visualisasi data (Cukup Jelas - seperti pertemuan 2)
# - analisa statistik (Cukup jelas - seperti pertemuan 2)
# ------------------------------------------------------------------------------


## pra-proses data ENSO (secara manual via excel)
## langkah 1 - transformasi data
## langkah 2 - integrasi data untuk menyatukan data SST Nina 3.4, SOI Index, ONI Index.

# baca dataset enso
df_enso <- read_excel("D:/Bahan Ajar Big Data/dataset/praproses_hasil_data_enso.xlsx", sheet = "data enso")

# proses conversi tipe data sesuai value-nya
df_enso <- data.frame(
  tanggal = as.Date(df_enso$tanggal),
  sst = as.numeric(df_enso$sst),
  soi = as.numeric(df_enso$soi),
  oni = as.numeric(df_enso$oni)
)

# cek apakah tipe data sesuai value-nya
str(df_enso)

# lihat dataset enso
print(head(df_enso))
# ------------------------------------------------------------------------------


## pra-proses data titik panas
## langkah 1 - pemotongan data (menggunakan QGiS) sesuai area studi sumatera selatan.
##             hasil terdapat pada file "praproses_hasil_pemotongan_data_titik_panas.csv"

## langkah 2 - aggregasi data untuk merubah data menjadi data harian.
##             aggregasi dilakukan dengan cara group by berdasarkan acq_date. Kemudian mutate untuk membuat variabel baru (hotspot)
##             hasil terdapat pada file "praproses_hasil_data_titik_panas.csv"

# baca dataset titik panas
df_titik_panas <- read.csv("D:/Bahan Ajar Big Data/dataset/praproses_hasil_data_titik_panas.csv")

# proses conversi tipe data sesuai value-nya
df_titik_panas <- data.frame(
  acq_date = as.Date(df_titik_panas$acq_date),
  hotspot = as.numeric(df_titik_panas$hotspot)
)

# cek apakah tipe data sesuai value-nya
str(df_titik_panas)

# lihat dataset titik panas
print(head(df_titik_panas))


# langkah 3 - visualisasi data untuk melihat pola umum dan fluktuasi data dari data enso dan titik panas
sst <- ggplot(data = df_enso, aes(x = tanggal))+ 
  geom_line(aes(y = sst, color="SST Nina 3.4"), size=1)+
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))+
  scale_colour_manual("",values = c("SST Nina 3.4"="Blue"))+
  labs(title="SST Nina 3.4 tahun 2001 - 2020", x="Tanggal", y="frekuensi")+
  theme(legend.position="top")

soi <- ggplot(data = df_enso, aes(x = tanggal))+ 
  geom_line(aes(y = soi, color="SOI Index"), size=1)+
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))+
  scale_colour_manual("",values = c("SOI Index"="Blue"))+
  labs(title="SOI Index tahun 2001 - 2020", x="Tanggal", y="frekuensi")+
  theme(legend.position="top")

oni <- ggplot(data = df_enso, aes(x = tanggal))+ 
  geom_line(aes(y = oni, color="ONI Index"), size=1)+
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))+
  scale_colour_manual("",values = c("ONI Index"="Blue"))+
  labs(title="ONI Index tahun 2001 - 2020", x="Tanggal", y="frekuensi")+
  theme(legend.position="top")

hotspot <- ggplot(data = df_titik_panas, aes(x = acq_date))+ 
  geom_line(aes(y = hotspot, color="Hotspot"), size=1)+
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))+
  scale_colour_manual("",values = c("Hotspot"="Red"))+
  labs(title="Hotspot Sumsel tahun 2001 - 2020", x="Tanggal", y="frekuensi")+
  theme(legend.position="top")

# menyatukan sebanyak n hasil visualisasi ke dalam 1 frame
p1 <- grid.arrange(sst, soi, oni, hotspot, ncol = 2)
plot(p1)

# Langkah 4 - Integrasi Data untuk menyatukan data enso dengan data titik panas.
df <- data.frame(
  tanggal = df_enso$tanggal,
  sst = df_enso$sst,
  soi = df_enso$soi,
  oni = df_enso$oni,
  hotspot = df_titik_panas$hotspot
)
print(head(df))


# Langkah 5 - Normalisasi Data untuk merubah rentang nilai data menjadi skala -1 sampai 1
# salin df menjadi df_temp
df_temp <- df

# hapus tanggal pada df_temp
df_temp$tanggal <- NULL

# proses normalisasi
df_norm <- normalize(df_temp)

# proses tambahkan tanggal pada df_norm
df_norm <- data.frame(
  tanggal = as.Date(df$tanggal),
  sst = as.numeric(df_norm$sst),
  soi = as.numeric(df_norm$soi),
  oni = as.numeric(df_norm$oni),
  hotspot = as.numeric(df_norm$hotspot)
)
str(df_norm)
print(head(df_norm))

# proses pembuktian normalisasi
sst <- ggplot(data = df_norm, aes(x = tanggal))+ 
  geom_line(aes(y = sst, color="SST Nina 3.4"), size=1)+
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))+
  scale_colour_manual("",values = c("SST Nina 3.4"="Blue"))+
  labs(title="SST Nina 3.4 tahun 2001 - 2020", x="Tanggal", y="frekuensi")+
  theme(legend.position="top")

soi <- ggplot(data = df_norm, aes(x = tanggal))+ 
  geom_line(aes(y = soi, color="SOI Index"), size=1)+
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))+
  scale_colour_manual("",values = c("SOI Index"="Blue"))+
  labs(title="SOI Index tahun 2001 - 2020", x="Tanggal", y="frekuensi")+
  theme(legend.position="top")

oni <- ggplot(data = df_norm, aes(x = tanggal))+ 
  geom_line(aes(y = oni, color="ONI Index"), size=1)+
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))+
  scale_colour_manual("",values = c("ONI Index"="Blue"))+
  labs(title="ONI Index tahun 2001 - 2020", x="Tanggal", y="frekuensi")+
  theme(legend.position="top")

hotspot <- ggplot(data = df_norm, aes(x = tanggal))+ 
  geom_line(aes(y = hotspot, color="Hotspot"), size=1)+
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))+
  scale_colour_manual("",values = c("Hotspot"="Red"))+
  labs(title="Hotspot Sumsel tahun 2001 - 2020", x="Tanggal", y="frekuensi")+
  theme(legend.position="top")

# menyatukan sebanyak n hasil visualisasi ke dalam 1 frame
p2 <- grid.arrange(sst, soi, oni, hotspot, ncol = 2)
plot(p2)



# Langkah 4 - Seleksi fitur untuk melihat seberapa kuat variabel SST Nina 3.4, SOI Index, ONI Index untuk memprediksi titik panas.
sl1 <- ggscatter(
  df_norm, x="sst", y="hotspot",
  cor.coef=TRUE, conf.int.level = 0.95, cor.method="pearson",
  title="Uji Korelasi SST Nina 3.4 dengan Hotspot", xlab="SST Nina 3.4", ylab="Hotspot",
  color='blue', size = 3
)

sl2 <- ggscatter(
  df_norm, x="soi", y="hotspot",
  cor.coef=TRUE, conf.int.level = 0.95, cor.method="pearson",
  title="Uji Korelasi SOI Index dengan Hotspot", xlab="SOI Index", ylab="Hotspot",
  color='blue', size = 3
)

sl3 <- ggscatter(
  df_norm, x="oni", y="hotspot",
  cor.coef=TRUE, conf.int.level = 0.95, cor.method="pearson",
  title="Uji Korelasi ONI Index dengan Hotspot", xlab="ONI Index", ylab="Hotspot",
  color='blue', size = 3
)

p3 <- grid.arrange(sl1, sl2, sl3, ncol = 2)
plot(p3)
