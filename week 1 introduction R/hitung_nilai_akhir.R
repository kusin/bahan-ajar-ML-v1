# PROGRAM hitung_nilai_akhir.R
# program hitung_nilai_akhir.R akan menghitung nilai akhir mahasiswa berserta huruf mutu
# inputan data berupa file csv yang memiliki kolom nim, formatif, uts, uas
# proses hitung nilai akhir = 40% UTS + 30% UTS + 30 UAS
# proses hitung huruf mutu, 80 = A, 70 = B, 60 = C, 50 = D, <50 = E
# output berupa file csv baru yang memiliki kolom nim, formatif, uts, uas, nilai akhir, huruf mutu

# DEKLARASI - pustaka
library(readr) # membaca file excel/csv/txt/json
library(dplyr) # memanipulasi data
library(ggplot2) # membuat diagram
library(plotly) # memperindah diagram

# DEKLARASI - fungsi
hitung_nilai <- function(temp_formatif, temp_uts, temp_uas){
  hasil = temp_formatif*0.4 + temp_uts*0.3 + temp_uas*0.3
  return(hasil)
}
hitung_huruf <- function(nilai_akhir) {
  n=length(nilai_akhir)
  huruf=NULL
  for(i in 1:n) {
    if(nilai_akhir[i] >= 80) {
      huruf[i] = "A"
    } else if(nilai_akhir[i] >= 70) {
      huruf[i] = "B"
    } else if(nilai_akhir[i] >= 60) {
      huruf[i] = "C"
    } else if(nilai_akhir[i] >= 50) {
      huruf[i] = "D"
    } else{
      huruf[i] = "E"
    }
  }
  return(huruf)
}

# ALGORITMA
# - input dataset 
df <- read.csv("D:/Bahan Ajar Big Data/dataset/dataset_hitung_nilai.csv")
View(df)

# - inputan dataset dilakukan modifikasi tipe data
# karena kolom nim dibaca numeric. Sementara kolom nim tidak dilakukan operasi penjumlahan
df<- data.frame(
  nim = as.character(df$nim),
  formatif = as.numeric(df$formatif),
  uts = as.numeric(df$uts),
  uas = as.numeric(df$uas)
)

# Menampilkan isi data
head(df)

# Menampilkan struktur dataset
str(df)

# Menampilkan ringkasan statistik dataset
summary(df)

# proses (menghitung nilai akhir)
nilai_akhir <- hitung_nilai(df$formatif, df$uts, df$uas)
nilai_akhir

# proses (menghitung nilai huruf)
nilai_huruf <- hitung_huruf(nilai_akhir)
nilai_huruf

# proses (menggabung kolom nilai akhir, nilai huruf dengan df)
data_mhs <- cbind(df, nilai_akhir, nilai_huruf) 

# output 1 - memastikan data sesuai harapan
head(data_mhs)
str(data_mhs)
summary(data_mhs)

# output 2 - membuat file csv
# - row.names = FALSE untuk menghilangkan indexing
write.csv(data_mhs, "D:/Bahan Ajar Big Data/dataset/dataset_hitung_nilai_output.csv", row.names = FALSE)

# output 3 - membuat histogrm. Histogram adalah grafik untuk melihat sebaran data
# tanpa pustaka tambahan
hist(data_mhs$nilai_akhir)

# menggunakan pustaka ggplot2
ggplot(data=data_mhs, aes(x=nilai_akhir)) + 
  geom_histogram(aes(y=..density..),binwidth=5, colour="blue", fill="grey")+
  geom_density(alpha=.1, fill="yellow") +
  scale_x_continuous(name = "Nilai Akhir") +
  scale_y_continuous(name = "Frequensi") +
  ggtitle("Sebaran data nilai akhir")

