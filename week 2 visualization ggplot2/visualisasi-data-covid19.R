# PROGRAM visualisasi-Covid19.R

# DEKLARASI - pustaka
library(readr) # membaca file excel/csv/txt/json
library(readxl) # membaca file excel/csv/txt/json
library(dplyr) # memanipulasi data
library(ggpubr) # menghitung uji korelasi
library(ggcorrplot) # visualisasi uji korelasi
library(ggplot2) # membuat diagram
library(plotly) # memperindah diagram

# ALGORITMA
# -input data

df <- read_excel("D:/Materi Kuliah - Machine Learning v1/dataset/dataset_covid.xlsx", sheet = "data covid indonesia")

# - inputan dataset dilakukan modifikasi tipe data
# karena tanggal dibaca string
df <- data.frame(
  tanggal = as.Date(df$tanggal),
  positif_kumulatif = as.numeric(df$positif_kumulatif),
  sembuh_kumulatif = as.numeric(df$sembuh_kumulatif),
  meninggal_kumulatif = as.numeric(df$meninggal_kumulatif),
  positif_harian = as.numeric(df$positif_harian),
  sembuh_harian = as.numeric(df$sembuh_harian),
  meninggal_harian = as.numeric(df$meninggal_harian),
  perawatan_kumulatif = as.numeric(df$perawatan_kumulatif),
  perawatan_harian = as.numeric(df$perawatan_harian),  
  persentase_sembuh = as.numeric(df$persentase_sembuh),
  persentase_meninggal = as.numeric(df$persentase_meninggal)
)

# Menampilkan isi data
head(df)

# Menampilkan struktur dataset
str(df)

# Menampilkan ringkasan statistik dataset
summary(df)


# ------------------------------------------------------------------------------
# Proses 1 (Statistik Deskriptif Sederhana)
# Cara manual. Analogi menggunakan bahasa C
# int i, j, hasil, jumlah_positif
# for(i=0; i<baris; i++){
#   for(j=0; j<kolom; j++){
#     hasil = hasil + jumlah_positif[i][j]
#   }  
# }
# Menghitung jumlah positif, sembuh dan meninggal di Indonesia
# Cara otomatis meninggunakan fungsi sum()
print(jumlah_positif <- sum(df$positif_harian))
print(jumlah_sembuh <- sum(df$sembuh_harian))
print(jumlah_meninggal <- sum(df$meninggal_harian))

# Menghitung persentase sembuh dan meninggal
print((jumlah_sembuh/jumlah_positif)*100)
print((jumlah_meninggal/jumlah_positif)*100)


# ------------------------------------------------------------------------------
# proses 2 (Uji korelasi)
# Menghitung korelasi dan signifikansi positif dengan sembuh dan positif dengan meninggal jika dilihat dari pergerakan data secara time series.
# Terdapat beberapa metode korelasi seperti Uji-Kontingensi, Uji-Phi, Uji-Spearman Rank, Uji-Kendall, Uji-Pearson.
# 1. Uji-Kontingensi adalah korelasi dengna skala nominal dan bersifat non-parametrik.
# 2. Uji-Phi adalah korelasi dengna skala nominal dan bersifat non-parametrik. Korelasi ini umumnya untuk data-data dikotomik.
# 3. Uji-Spearman Rank adalah korelasi dengan skala ordinal dan bersifat non-parametrik.
# 4. Uji-Kendall adalah korelasi dengan skala ordinal dan bersifat non-parametrik.
# 5. Uji-Pearson adalah korelasi dengan skala interval dan bersifat parametrik. Dimana datanya akan scontinue
# Karena data covid berskala interval dan berisifat parametrik sehingga metode yang cocok adalah uji-pearson.

# - uji korelasi positif harian dengan sembuh harian
cor.test(df$positif_harian, df$sembuh_harian, method="pearson")

# - uji korelasi positif harian dengan meninggal harian
cor.test(df$positif_harian, df$meninggal_harian, method="pearson")

# hasil uji korelasi
# r = 0.88946, p-value = 2.2e-16
# makanya ......?


# ------------------------------------------------------------------------------
# proses 3 (Visualisasi scatter plot)
ggscatter(
  # menentukan df, var bebas dan terikat
  df, x="positif_harian", y="sembuh_harian",
  
  # menghitung korelasi dan p-value dengan tingkat kepercayaan 95%
  cor.coef=TRUE, conf.int.level = 0.95, cor.method="pearson",
  
  # membuat label-label
  title="Visual Scatter Plot Menggunakan Uji-Pearson", xlab="Positif harian", ylab="Sembuh harian",
  
  # membuat warna dan ukuran scatter
  color='green', size = 3
)

ggscatter(
  df, x="positif_harian", y="meninggal_harian",
  cor.coef=TRUE, conf.int.level = 0.95, cor.method="pearson",
  title="Visual Scatter Plot Menggunakan Uji-Pearson", xlab="Positif harian", ylab="Meninggal harian",
  color='red', size = 3
)

# ------------------------------------------------------------------------------
# proses 4 (Visualisasi ggcorrplot)
# ggcorrplot untuk visualisasi uji-korelasi
# siapkan df 
df_corr <- data.frame(
  positif_harian = as.numeric(df$positif_harian),
  sembuh_harian = as.numeric(df$sembuh_harian),
  meninggal_harian = as.numeric(df$meninggal_harian)
)
df_corr

# langkah 1
x <- round(cor(df_corr, method="pearson"),4)

# langkah 2
ggcorrplot(x, hc.order=FALSE, outline.color ="white", lab=TRUE)


# ggcorrplot untuk visualisasi uji-korelasi
# alternatif lain
df_corr <- df
df_corr$tanggal <- NULL

x <- round(cor(df_corr, method="pearson"),4)
ggcorrplot(x, title = "ggcorrplot for covid-19 indonesia",
  hc.order=FALSE, lab=TRUE,
  type="lower", outline.col="white",
  ggtheme = ggplot2::theme_gray, 
  colors = c("#6D9EC1", "white", "#E46726")
)


# ------------------------------------------------------------------------------
# proses 5 (Visualisasi timeseries)
# gruping data kumulatif postif, sembuh, meninggal
data_kumulatif <- data.frame(
  tanggal_kumulatif = as.Date(df$tanggal, "%Y/%m/%d"),
  positif_kumulatif = df$positif_kumulatif,
  sembuh_kumulatif = df$sembuh_kumulatif,
  meninggal_kumulatif = df$meninggal_kumulatif
)
# ggplotly untuk melihat detail data time seriesnya
ggplotly(
  ggplot(data = data_kumulatif, aes(x = tanggal_kumulatif))+ 
    geom_line(aes(y = positif_kumulatif,color = "Postif"), size=1)+
    geom_line(aes(y = sembuh_kumulatif, color = "Sembuh"), size=1)+
    geom_line(aes(y = meninggal_kumulatif, color = "Meninggal"), size=1)+
    scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))+
    scale_colour_manual("",values = c("Postif"="Blue", "Sembuh"="green", "Meninggal"="Red"))+
    labs(title="Jumlah Kumulatif Secara Nasional", subtitle="Data Pertanggal 7 Juni 2020", x="Tanggal", y="Jumlah Kejadian")+
    theme(legend.position="top")
)

# gruping data harian postif, sembuh, meninggal
data_harian <- data.frame(
  tanggal_harian = as.Date(df$tanggal, "%Y/%m/%d"),
  positif_harian = df$positif_harian,
  sembuh_harian = df$sembuh_harian,
  meninggal_harian = df$meninggal_harian
)
ggplotly(
  ggplot(data = data_harian, aes(x = tanggal_harian))+
    geom_line(aes(y = positif_harian), color = "blue", size = 1) +
    geom_line(aes(y = sembuh_harian), color = "green", size = 1) +
    geom_line(aes(y = meninggal_harian), color = "red", size = 1) +
    labs(title="Jumlah Fluktuasi Harian Secara Nasional", subtitle="Data Pertanggal 7 Juni 2020")+
    xlab('Tanggal')+ ylab('Tanggal Kejadian')+
    scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))
)

# gruping data harian postif, sembuh, meninggal + aggregasi
# lakukan secara mandiri


# ------------------------------------------------------------------------------
# proses 6 (Visualisasi barplot)
df <- read_excel("D:/Bahan Ajar Big Data/dataset/dataset_covid.xlsx", sheet = "data covid provinsi")

# Prepare data: group kasus positif by provinsi
positif <- aggregate(df$jumlah_positif, by=list(df$provinsi), FUN=mean) # aggregate
colnames(positif) <- c("Provinsi","Jumlah.Positif") # change column names
positif <- positif[order(positif$Jumlah.Positif),] # sort     
positif$Provinsi <- factor(positif$Provinsi, level=positif$Provinsi) # to retain the order in plot
head(positif,5)

# Draw plot positif perprovisi
ggplot(positif, aes(x=Provinsi, y=Jumlah.Positif))+
  geom_bar(stat="identity", width=0.6, fill="steelblue") +
  geom_text(aes(label=Jumlah.Positif), vjust=-0.5, size=3.5)+
  labs(title="Jumlah Kasus Positif Perprovinsi", subtitle="Data Pertanggal 7 Juni 2020") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# Prepare data: group kasus sembuh by provinsi
sembuh <- aggregate(df$jumlah_sembuh, by=list(df$provinsi), FUN=mean) # aggregate
colnames(sembuh) <- c("Provinsi","Jumlah.Sembuh") # change column names
sembuh <- sembuh[order(sembuh$Jumlah.Sembuh),] # sort     
sembuh$Provinsi <- factor(sembuh$Provinsi, level=sembuh$Provinsi) # to retain the order in plot
head(sembuh,5)

# Draw plot sembuh perprovisi
ggplot(sembuh, aes(x=Provinsi, y=Jumlah.Sembuh))+
  geom_bar(stat="identity", width=0.6, fill="green") + 
  geom_text(aes(label=Jumlah.Sembuh), vjust=-0.5, size=3.5)+
  labs(title="Jumlah Kasus Sembuh Perprovinsi", subtitle="Data Pertanggal 7 Juni 2020") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# Prepare data: group kasus meninggal by provinsi
meninggal <- aggregate(df$jumlah_meninggal, by=list(df$provinsi), FUN=mean) # aggregate
colnames(meninggal) <- c("Provinsi","Jumlah.Meninggal") # change column names
meninggal <- meninggal[order(meninggal$Jumlah.Meninggal),] # sort     
meninggal$Provinsi <- factor(meninggal$Provinsi, level=meninggal$Provinsi) # to retain the order in plot
head(meninggal,5)

# Draw plot meninggal perprovisi
ggplot(meninggal, aes(x=Provinsi, y=Jumlah.Meninggal))+
  geom_bar(stat="identity", width=0.6, fill="red") + 
  geom_text(aes(label=Jumlah.Meninggal), vjust=-0.5, size=3.5)+
  labs(title="Jumlah Kasus Meninggal Perprovinsi", subtitle="Data Pertanggal 7 Juni 2020") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

