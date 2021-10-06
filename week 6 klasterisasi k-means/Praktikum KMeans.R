# PROGRAM K-Means

# DEKLARASI
library(dplyr) # manipulasi data
library(cluster) # proses klaster k-means
library(factoextra) # visualisasi hasil klaster
library(ggplot2) # visualisasi data

# ALGORITMA
# Masukan data berupa data frame
x <- data.frame(
  a = c(1,2,4,5),
  b = c(1,1,3,4)
)

# Melihat dataset
print(x)

# Proses klaster kmeans dengan jumlah k=2
klasterisasi <- kmeans(x, centers = 2)

# Keluaran hasil perhitungan K-Means
print (klasterisasi)

# Visual Hasil KMeans
fviz_cluster(klasterisasi, data = x)
