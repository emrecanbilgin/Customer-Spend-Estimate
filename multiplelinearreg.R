# Gerekli kütüphaneleri içe aktarýn
library(stats)

# Müþteri veri setini yükleyelim
sehir_nufusu <- c(500, 1000, 2000, 800, 1500, 1200, 1800, 900, 1300, 1100)
harcama <- c(10000, 15000, 20000, 12000, 18000, 16000, 19000, 11000, 14000, 13000)
reklam_tiklama <- c(50, 100, 80, 70, 90, 120, 110, 60, 95, 85)
ortalama_sure <- c(5, 7, 10, 6, 8, 9, 7, 6, 8, 7)
harcama_miktari <- c(12000, 18000, 22000, 14000, 20000, 17000, 21000, 13000, 16000, 15000)

# Veri setini birleþtirelim
veri <- data.frame(sehir_nufusu, harcama, reklam_tiklama, ortalama_sure, harcama_miktari)

# Scatter plot (nokta grafiði) ile baðýmsýz deðiþkenler ile baðýmlý deðiþken arasýndaki iliþkiyi görselleþtirelim
plot(veri$sehir_nufusu, veri$harcama_miktari, xlab = "Þehir Nüfusu (binlerce kiþi)", ylab = "Harcama Miktarý (TL)", main = "Þehir Nüfusu - Harcama Miktarý Ýliþkisi")

# Diðer baðýmsýz deðiþkenler için de scatter plot'lar oluþturalým
plot(veri$harcama, veri$harcama_miktari, xlab = "Son Bir Yýlda Yapýlan Harcama (TL)", ylab = "Harcama Miktarý (TL)", main = "Harcama - Harcama Miktarý Ýliþkisi")
plot(veri$reklam_tiklama, veri$harcama_miktari, xlab = "Reklam Týklama Sayýsý", ylab = "Harcama Miktarý (TL)", main = "Reklam Týklama - Harcama Miktarý Ýliþkisi")
plot(veri$ortalama_sure, veri$harcama_miktari, xlab = "Web Sitesinde Geçirilen Ortalama Süre (dakika)", ylab = "Harcama Miktarý (TL)", main = "Ortalama Süre - Harcama Miktarý Ýliþkisi")

# Korelasyon matrisini hesaplayalým
cor_matrix <- cor(veri[, c("sehir_nufusu", "harcama", "reklam_tiklama", "ortalama_sure")])

# Korelasyon matrisini görüntüleyelim
cor_matrix

# Çoklu lineer regresyon modelini oluþturalým
model <- lm(harcama_miktari ~ sehir_nufusu + reklam_tiklama + ortalama_sure, data = veri)

# Hata terimlerini hesaplayalým
hata_terimleri <- residuals(model)

# Shapiro-Wilk testini uygulayalým
shapiro.test(hata_terimleri)

# Modelin özetini görüntüleyelim
summary(model)

# Yeni bir müþterinin verilerini kullanarak tahmin yapalým
yeni_veri <- data.frame(sehir_nufusu = 1500, reklam_tiklama = 95, ortalama_sure = 8)

tahmin <- predict(model, newdata = yeni_veri)
tahmin

