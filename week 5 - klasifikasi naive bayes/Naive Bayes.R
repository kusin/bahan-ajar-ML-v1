# PROGRAM Naive Bayes
# Naive bayes merupakan teknik klasifikasi untuk mengelompokan data berdasarkan konsep peluang.
# Naive bayes semua variabel bebas diasumsikan tidak saling bergantung.

# Library untuk klasifikasi naive bayes.
library(e1071)
library(readxl)

# ALGORITMA Naive bayes
# Load dataset
# Asumsi dataset yang digunakan hasil proses data latih.
# Namun untuk membuat data latih umumnya menggunakan metode k-cross validation, hold out, bootstraping (Namanya memang mirip sama framework css buatan twitter)
data_latih = read_excel("D:\\Bahan Ajar Big Data\\dataset\\dataset_klasifikasi_materi.xlsx")
data_latih <- data.frame(
  Outlook = as.factor(data_latih$Outlook),
  Temperature = as.factor(data_latih$Temperature),
  Humidity = as.factor(data_latih$Humidity),
  Windy = as.factor(data_latih$Wind),
  Travel = as.factor(data_latih$Travel)
)

# melihat karakteristik data latih
summary(data_latih) # melihat frequensi data setiap variabelnya
str(data_latih) # melihat tipe data dari setiap variabelnya
print(data_latih) # menampilkan dataset

# Setelah melatih dataset, umumnya dilakukan evaluasi untuk menguji data latih.
# Proses ini menggunakan data uji. 
# membuat data uji. Alasan menggunakan as.factor agar menyamakan tipe data yang digunakan pada data latih.
data_uji <- data.frame(
  Outlook = as.factor("Sunny"),
  Temperature = as.factor("Cool"),
  Humidity = as.factor("High"),
  Windy = as.factor("True")
)

# melihat karakteristik data uji
summary(data_uji) # melihat frequensi data setiap variabelnya
str(data_uji) # melihat tipe data dari setiap variabelnya
print(data_uji) # menampilkan dataset

# membuat model klasifikasi naive bayes (menggunakan seluruh variabel) tanpa dikombinasikan dengan seleksi fitur
model <- naiveBayes(data_latih$Travel ~ ., data=data_latih)
print(model)

# klasifikasi naive bayes menggunakan data uji.
hasil <- predict(model, data_uji)
print(hasil)

