# Gerekli k�t�phaneleri i�e aktar�n
library(stats)

# M��teri veri setini y�kleyelim
sehir_nufusu <- c(500, 1000, 2000, 800, 1500, 1200, 1800, 900, 1300, 1100)
harcama <- c(10000, 15000, 20000, 12000, 18000, 16000, 19000, 11000, 14000, 13000)
reklam_tiklama <- c(50, 100, 80, 70, 90, 120, 110, 60, 95, 85)
ortalama_sure <- c(5, 7, 10, 6, 8, 9, 7, 6, 8, 7)
harcama_miktari <- c(12000, 18000, 22000, 14000, 20000, 17000, 21000, 13000, 16000, 15000)

# Veri setini birle�tirelim
veri <- data.frame(sehir_nufusu, harcama, reklam_tiklama, ortalama_sure, harcama_miktari)

# Scatter plot (nokta grafi�i) ile ba��ms�z de�i�kenler ile ba��ml� de�i�ken aras�ndaki ili�kiyi g�rselle�tirelim
plot(veri$sehir_nufusu, veri$harcama_miktari, xlab = "�ehir N�fusu (binlerce ki�i)", ylab = "Harcama Miktar� (TL)", main = "�ehir N�fusu - Harcama Miktar� �li�kisi")

# Di�er ba��ms�z de�i�kenler i�in de scatter plot'lar olu�tural�m
plot(veri$harcama, veri$harcama_miktari, xlab = "Son Bir Y�lda Yap�lan Harcama (TL)", ylab = "Harcama Miktar� (TL)", main = "Harcama - Harcama Miktar� �li�kisi")
plot(veri$reklam_tiklama, veri$harcama_miktari, xlab = "Reklam T�klama Say�s�", ylab = "Harcama Miktar� (TL)", main = "Reklam T�klama - Harcama Miktar� �li�kisi")
plot(veri$ortalama_sure, veri$harcama_miktari, xlab = "Web Sitesinde Ge�irilen Ortalama S�re (dakika)", ylab = "Harcama Miktar� (TL)", main = "Ortalama S�re - Harcama Miktar� �li�kisi")

# Korelasyon matrisini hesaplayal�m
cor_matrix <- cor(veri[, c("sehir_nufusu", "harcama", "reklam_tiklama", "ortalama_sure")])

# Korelasyon matrisini g�r�nt�leyelim
cor_matrix

# �oklu lineer regresyon modelini olu�tural�m
model <- lm(harcama_miktari ~ sehir_nufusu + reklam_tiklama + ortalama_sure, data = veri)

# Hata terimlerini hesaplayal�m
hata_terimleri <- residuals(model)

# Shapiro-Wilk testini uygulayal�m
shapiro.test(hata_terimleri)

# Modelin �zetini g�r�nt�leyelim
summary(model)

# Yeni bir m��terinin verilerini kullanarak tahmin yapal�m
yeni_veri <- data.frame(sehir_nufusu = 1500, reklam_tiklama = 95, ortalama_sure = 8)

tahmin <- predict(model, newdata = yeni_veri)
tahmin

