# Bagian 1. Review DDP menggunakan bahasa R 
# Link belajar https://www.tutorialspoint.com/r/index.htm
# 1. Menampilkan hello world
# 2. Tipe Data
# 3. Input dan Output
# 4. Runtunan, Percabangan, Perulangan
# 5. Array
# 6. Membuat Fungsi
# ------------------------------------------------------------------------------

# 1. Hello World di Bahasa R

# Cara 1
print("Hello World")

# Cara 2
my_str <- "Hello World"
print(my_str)

# bahasa C
# #include<stdio.h>
# main(){
#   printf("Hello World");
# }

# bahasa Java
# class Hello{
#   public static void main(String[]args){
#     System.out.println("Hello World");
#   }
# }
# ------------------------------------------------------------------------------

# 2. Tipe Data
# Secara sederhana, Bahasa R menggunakan 3 tipe data yaitu logical, character, numeric.
# Tipe data di bahasa R masih banyak lagi. Namun 3 tipe data tersebut sudah cukup
#   untuk mewakili proses pembelajaran hingga UTS

# - logical / boolean
my_logical <- TRUE
print(my_logical)
class(my_logical)

# - character / String
my_string <- "Aryajaya Alamsyah"
print(my_string)
class(my_string)

# - numeric
my_numeric <- 11140910000101
print(format(my_numeric, scientific = FALSE))
class(my_numeric)
# ------------------------------------------------------------------------------

# 3. Input dan Output
# syntax input
#   readline(prompt="keterangan input: ")

# - input
{
  nama <- readline(prompt="Masukan nama anda: ")
  nim <- readline(prompt="Masukan nim anda: ")  
}

# - output cara 1
print(paste("Nama anda : ", nama))
print(paste("NIM anda : ", nim))

# - output cara 2
sprintf("Nama anda : %s", nama)
sprintf("Nim anda : %s", nim)

# - output cara 3
cat("Nama anda :",nim, "\nNim anda :",nama)
# ------------------------------------------------------------------------------

# 4. Runtunan, Percabangan, Perulangan

# Runtunan - menghitung valome balok
# input
panjang <- 10
lebar <- 10
tinggi <- 5

# proses
volume <- panjang * lebar * tinggi

# output
sprintf("Volume balok : %i", volume)

# Percabangan - menentukan angka ganjil genap
bilangan <- 5
if(bilangan %% 2 == 1){
  sprintf("%i merupakan bilangan ganjil", bilangan)
}else{
  sprintf("%i merupakan bilangan genap", bilangan)
}

# Perulangan - menampilkan angka 1 sampai 5
for(i in 1:5){
  print(i)
}

i <- 0
while(i < 5){
  print(i)
  i = i + 1
}
# ------------------------------------------------------------------------------

# 5. Array - Penjumlahan matriks 1 Dimensi dan 2 Dimensi
# Array dan Matriks dianggap sama. Hal ini untuk mempermudah proses pembelajaran.
# Akan tetapi, pada bahasa R terdapat perbedaan antara 
#   - vector
#   - list
#   - matriks
#   - array
#   - factor
#   - dataframe
# Rujukan
# - Lutfi M. 2019. Mengusai Bahasa R - Teori dan Prakter. Bandung(ID):Informatika.

# - Array 1 Dimensi
# input
arr_1 <- matrix(c(1,2,3,4), nrow=4, ncol=1, byrow=TRUE)
arr_1
arr_2 <- matrix(c(5,6,7,8), nrow=4, ncol=1, byrow=TRUE)
arr_2

# proses
arr_3 <- arr_1 + arr_2

# output
print(arr_3)


# - Array 2 Dimensi
# input
arr_1 <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
arr_2 <- matrix(c(5,6,7,8), nrow=2, ncol=2, byrow=TRUE)
arr_2

# proses
arr_3 <- arr_1 + arr_2

# output
print(arr_3)
# ------------------------------------------------------------------------------

# 6. Membuat Fungsi
# Contoh 1 - menghitung array 2 D
# fungsi hitung array
hitung_arr <- function(temp_arr_1, temp_arr_2){
  hasil = temp_arr_1 + temp_arr_2
  return(hasil)
}

# input
arr_1 <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
arr_2 <- matrix(c(5,6,7,8), nrow=2, ncol=2, byrow=TRUE)

# proses
arr_3 <- hitung_arr(arr_1, arr_2)

# output
print(arr_3)

# Contoh 2 - hitung luas lingkaran
# membuat fungsi hitung lingkaran
luas_lingkaran <- function(temp_jari_jari){
  # membuat kondisi.
  # Phi 22/7 jika jari-jari kelipatan 7. Phi 3.14 jika jari-jari bukan kelipatan 7
  if(temp_jari_jari %% 7 == 0){
    luas_lingkaran <- (22/7) * temp_jari_jari * temp_jari_jari
  }
  else{
    luas_lingkaran <- 3.14 * temp_jari_jari * temp_jari_jari
  }
  return(luas_lingkaran)
}
# memanggil fungsi luas lingkaran
luas_lingkaran(10)
# ------------------------------------------------------------------------------

# Gimana gampang kan bahasa R ................ ?


