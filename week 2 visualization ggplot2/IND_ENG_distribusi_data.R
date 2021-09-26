## PROGRAM Distribusi_Data.R
## Digunakan untuk melihat dataset (BMKG => Data cuaca Jakarta periode Febuari 2019) dengan distribusi normal
## Used to view the dataset (BMKG => Jakarta weather data for the period of February 2019) with a normal distribution

## DEKLARASI
library(readxl) # membaca file excel
library(ggplot2) # keluaran visualisasi data scatter plot

## ALGORITMA
# membaca dataset Data Online BMKG
# read the BMKG Online Data dataset
dataset <- read_excel("D:/# Mengajar/Bu Nur/2020-2021 Big Data/Materi/Pertemuan 3/Dataset.xlsx", sheet = "Dataset")

# create data frame minimum temperature
df <- data.frame(
  suhu_minimum = dataset$suhu_minimum
)

# menampung nilai suhu_minimum 
# holds the minimum temperature value
suhu_minimum <- dataset$suhu_minimum

# membuat histrogram suhu minimum
# create a minimum temperature histogram
h1 <- hist(suhu_minimum, breaks = "Sturges", freq= TRUE,
           col="sky blue", xlab="Suhu Minimmum", main="Distribusi Data Suhu Minimum")

# melihat kecocokan suhu minumum dengan distribusi mormal
# see the match between the minimum temperature and the normal distribution
xfit1 <-seq(min(suhu_minimum), max(suhu_minimum), length=length(suhu_minimum))
yfit1 <-dnorm(xfit1, mean=mean(suhu_minimum), sd=sd(suhu_minimum))
yfit1 <- yfit1*diff(h1$mids[1:2])*length(suhu_minimum) 
lines(xfit1, yfit1, col="blue", lwd=2) # menambahkan garis kurva norm

# menampung nilai kelembaban udara
# holds the air humidity value
kelembaban_udara <- dataset$kelembaban_udara

# membuat histrogram kelembaban udara
# make a histogram of air humidity
h2 <- hist(kelembaban_udara, breaks = "Sturges", freq= TRUE,
           col="sky blue", xlab="Kelembaban Udara", main="Distribusi Data Kelembaban Udara")

# melihat kecocokan kelembaban udara dengan distribusi normal
# see the compatibility of air humidity with the normal distribution
xfit2 <-seq(min(kelembaban_udara), max(kelembaban_udara), length=length(kelembaban_udara))
yfit2 <-dnorm(xfit2, mean=mean(kelembaban_udara), sd=sd(kelembaban_udara))
yfit2 <- yfit2*diff(h1$mids[1:2])*length(kelembaban_udara)
lines(xfit2, yfit2, col="blue", lwd=2) # menambahkan garis kurva norm
