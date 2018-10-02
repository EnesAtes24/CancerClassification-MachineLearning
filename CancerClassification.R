# Özellik Bilgisi:
# 1) ID numarası
# 2) Diagnosis(Tanı) (M = malign, B = benign)
# 
# Her bir hücre çekirdeği için on gerçek değerli özellik hesaplanmıştır:
#
# a) radius(yarıçapı) (merkezden merkeze olan mesafelerin ortalaması)
# b) texture(doku) (gri ölçekli değerlerin standart sapması)
# c) perimeter(çevre)
# d) area(alan)
# e) smoothness(pürüzsüzlük) (yarıçap uzunluklarında yerel değişim)
# f) compactness(kompaktlık) (perimeter(çevre)^2 / areaalan - 1.0)
# g) concavity(konkavlık) (konturun içbükey kısımlarının şiddeti)
# h) concave points(içbükey noktalar) (konturun içbükey kısımlarının sayısı)
# i) symmetry
# j) fractal dimension(fraktal boyut) ("kıyı şeridi yaklaşımı" - 1)

library(ggplot2)
library(readr)
data = read.csv("data.csv")
library("ggplot2")
library("corrgram")
library("car")
library("lattice")
library("ROCR")
library("plotly")
library("tree")

data$id = NULL

data$X = NULL

data$outcome[data$diagnosis=="M"] = 1

data$outcome[data$diagnosis=="B"] = 0

data$outcome = as.integer(data$outcome)


# Veri kümesini eğitim örneğine ve test örneğine bölmek gerekiyor.
# 50/50 örnek oluşturacak.
sample_size = floor(0.5 * nrow(data))

set.seed(1729)

train_set = sample(seq_len(nrow(data)), size = sample_size)

training = data[train_set, ] 

testing = data[-train_set, ]


# Veri kümesine bir göz atın

entire_dataset <- ggplot(data, aes(x = diagnosis)) +
                  geom_bar(aes(fill = diagnosis)) +
                  ggtitle("Tüm veri kümesi için Diagnosis(teşhis) dağılımı") + 
                  theme(legend.position="none")

print(entire_dataset)


training_diagonastics <- ggplot(training, aes(x = diagnosis)) +
                         geom_bar(aes(fill = diagnosis)) +
                         ggtitle("Training veri seti için Diagnosis(tanı) dağılımı") +
                         theme(legend.position="none")

print(training_diagonastics)
  

testing_diagonastics <- ggplot(testing, aes(x = diagnosis)) + geom_bar(aes(fill = diagnosis)) +
                        ggtitle("Testing veri seti için (Diagnosis)tanı dağılımı") + 
                        theme(legend.position="none")
                        
print(testing_diagonastics)


all_radius <- ggplot(data, aes(diagnosis, radius_mean)) + 
              geom_boxplot() + 
              ylab("Mean Radius") +
              ggtitle("Her Diagnosis için Mean Radius'un Kutu Grafiği (entire dataset)")

print(all_radius)  


training_radius <- ggplot(training, aes(diagnosis, radius_mean)) + 
                   geom_boxplot() +
                   ylab("Mean Radius") +
                   ggtitle("Her Diagnosis için Mean Radius'un Kutu Grafiği (training dataset)")

print(training_radius)



testing_radius <- ggplot(testing, aes(diagnosis, radius_mean)) + 
                  geom_boxplot() + 
                  ylab("Mean Radius") +
                  ggtitle("Her Diagnosis için Mean Radius'un Kutu Çizimi (testing dataset)")  
  
print(testing_radius)


all_texture <- ggplot(data, aes(diagnosis, texture_mean)) + 
               geom_boxplot() + 
               ylab("Mean Texture") +
               ggtitle("Her Diagnosis için Mean Texture Kutu Çizimi (entire dataset)")

print(all_texture)


training_texture <- ggplot(training, aes(diagnosis, texture_mean)) + 
                    geom_boxplot() + 
                    ylab("Mean Texture") +
                    ggtitle("Her Diagnosis için Mean Texture Kutu Çizimi (training dataset)")

print(training_texture)


testing_texture <- ggplot(testing, aes(diagnosis, texture_mean)) + 
                   geom_boxplot() + 
                   ylab("Mean Texture") +
                   ggtitle("Her Diagnosis için Mean Texture Kutu Çizimi (testing dataset)")


print(testing_texture)


all_perimeter <- ggplot(data, aes(diagnosis, perimeter_mean)) + 
                 geom_boxplot() + 
                 ylab("Mean Perimeter") +
                 ggtitle("Her Diagnosis için Mean Perimeter Kutu Çizimi (entire dataset)")

print(all_perimeter)


training_perimeter <- ggplot(training, aes(diagnosis, perimeter_mean)) + 
                      geom_boxplot() + 
                      ylab("Mean Perimeter") +
                      ggtitle("Her Diagnosis için Mean Perimeter Kutu Çizimi (training dataset)")

print(training_perimeter)

testing_perimeter <- ggplot(testing, aes(diagnosis, perimeter_mean)) + 
                     geom_boxplot() + 
                     ylab("Mean Perimeter") +
                     ggtitle("Her Diagnosis için Mean Perimeter Kutu Çizimi (testing dataset)")
  
print(testing_perimeter)

all_area <- ggplot(data, aes(diagnosis, area_mean)) + 
            geom_boxplot() +
            ylab("Mean Area") +
            ggtitle("Her Diagnosis için Mean Area Kutu Çizimi (entire dataset)")

print(all_area)

training_area <- ggplot(training, aes(diagnosis, area_mean)) + 
                 geom_boxplot() +
                 ylab("Mean Area") +
                 ggtitle("Her Diagnosis için Mean Area Kutu Çizimi (training dataset)")

print(training_area)

testing_area <- ggplot(testing, aes(diagnosis, area_mean)) + 
                geom_boxplot() +
                ylab("Mean Area") +
                ggtitle("Her Diagnosis için Mean Area Kutu Çizimi (testing dataset)")

print(testing_area)

all_smoothness <- ggplot(data, aes(diagnosis, smoothness_mean)) + 
                  geom_boxplot() +
                  ylab("Mean Smoothness") +
                  ggtitle("Her Diagnosis için Mean Smoothness Kutu Çizimi (entire dataset)")

print(all_smoothness)

training_smoothness <- ggplot(training, aes(diagnosis, smoothness_mean)) + 
                       geom_boxplot() +
                       ylab("Mean Smoothness") +
                       ggtitle("Her Diagnosis için Mean Smoothness Kutu Çizimi (training dataset)")

print(training_smoothness)

testing_smoothness <- ggplot(testing, aes(diagnosis, smoothness_mean)) + 
                      geom_boxplot() +
                      ylab("Mean Smoothness") +
                      ggtitle("Her Diagnosis için Mean Smoothness Kutu Çizimi (testing dataset)")

print(testing_smoothness)

all_compactness <- ggplot(data, aes(diagnosis, compactness_mean)) + 
                   geom_boxplot() +
                   ylab("Mean Compactness") +
                   ggtitle("Her Diagnosis için Mean Compactness Kutu Çizimi (entire dataset)")

print(all_compactness)

training_compactness <- ggplot(training, aes(diagnosis, compactness_mean)) + 
                        geom_boxplot() +
                        ylab("Mean Compactness") +
                        ggtitle("Her Diagnosis için Mean Compactness Kutu Çizimi (training dataset)")

print(training_compactness)

testing_compactness <- ggplot(testing, aes(diagnosis, compactness_mean)) + 
                       geom_boxplot() +
                       ylab("Mean Compactness") +
                       ggtitle("Her Diagnosis için Mean Compactness Kutu Çizimi (testing dataset)")

print(testing_compactness)

all_concavity <- ggplot(data, aes(diagnosis, concavity_mean)) + 
                 geom_boxplot() +
                 ylab("Mean Concavity") +
                 ggtitle("Her Diagnosis için Mean Concavity Kutu Çizimi (entire dataset)")

print(all_concavity)

training_concavity <- ggplot(training, aes(diagnosis, concavity_mean)) + 
                      geom_boxplot() +
                      ylab("Mean Concavity") +
                      ggtitle("Her Diagnosis için Mean Concavity Kutu Çizimi (training dataset)")

print(training_concavity)

testing_concavity <- ggplot(testing, aes(diagnosis, concavity_mean)) + 
                     geom_boxplot() +
                     ylab("Mean Concavity") +
                     ggtitle("Her Diagnosis için Mean Concavity Kutu Çizimi (testing dataset)")

print(testing_concavity)

all_concave_points <- ggplot(data, aes(diagnosis, concave.points_mean)) + 
                      geom_boxplot() +
                      ylab("Mean Concave Points") +
                      ggtitle("Her Diagnosis için Mean Concave Points Kutu Çizimi (entire dataset)")

print(all_concave_points)

training_concave_points <- ggplot(training, aes(diagnosis, concave.points_mean)) + 
                           geom_boxplot() +
                           ylab("Mean Concave Points") +
                           ggtitle("Her Diagnosis için Mean Concave Points Kutu Çizimi (training dataset)")

print(training_concave_points)

testing_concave_points <- ggplot(testing, aes(diagnosis, concave.points_mean)) + 
                          geom_boxplot() +
                          ylab("Mean Concave Points") +
                          ggtitle("Her Diagnosis için Mean Concave Points Kutu Çizimi (testing dataset)")

print(testing_concave_points)

all_symmetry <- ggplot(data, aes(diagnosis, symmetry_mean)) + 
                geom_boxplot() +
                ylab("Mean Symmetry") +
                ggtitle("Her Diagnosis için Mean Symmetry Kutu Çizimi (entire dataset)")

print(all_symmetry)

training_symmetry <- ggplot(training, aes(diagnosis, symmetry_mean)) + 
                     geom_boxplot() +
                     ylab("Mean Symmetry") +
                     ggtitle("Her Diagnosis için Mean Symmetry Kutu Çizimi (training dataset)")

print(training_symmetry)

testing_symmetry <- ggplot(testing, aes(diagnosis, symmetry_mean)) + 
                    geom_boxplot() +
                    ylab("Mean Symmetry") +
                    ggtitle("Her Diagnosis için Mean Symmetry Kutu Çizimi (testing dataset)")

print(testing_symmetry)

all_fractal_dimension <- ggplot(data, aes(diagnosis, fractal_dimension_mean )) + 
                         geom_boxplot() +
                         ylab("Mean Fractal Dimension") +
                         ggtitle("Her Diagnosis için Mean Fractal Dimension Kutu Çizimi (entire dataset)")

print(all_fractal_dimension)

training_fractal_dimension <- ggplot(training, aes(diagnosis, fractal_dimension_mean )) + 
                              geom_boxplot() +
                              ylab("Mean Fractal Dimension") +
                              ggtitle("Her Diagnosis için Mean Fractal Dimension Kutu Çizimi (training dataset)")

print(training_fractal_dimension)

testing_fractal_dimension <- ggplot(testing, aes(diagnosis, fractal_dimension_mean )) + 
                             geom_boxplot() +
                             ylab("Mean Fractal Dimension") +
                             ggtitle("Her Diagnosis için Mean Fractal Dimension Kutu Çizimi (testing dataset)")

print(testing_fractal_dimension)


# Tüm veri kümesinin Corrgram'ı
corrgram(data, 
         order=NULL, 
         lower.panel=panel.shade, 
         upper.panel=NULL, 
         text.panel=panel.txt,
         main="Verilerin Corrgramı")

# Training veri kümesinin Corrgram'ı
corrgram(training, 
         order=NULL, 
         lower.panel=panel.shade, 
         upper.panel=NULL, 
         text.panel=panel.txt,
         main="Training verilerinin Corrgramı")

# Testing veri kümesinin Corrgram'ı
corrgram(testing, 
         order=NULL, 
         lower.panel=panel.shade, 
         upper.panel=NULL, 
         text.panel=panel.txt,
         main="Testing verilerinin Corrgramı")

# Bir karar ağacını deneyelim
# Lojistik model için kullanılan sonuç değişkenini düşürme

training$outcome = NULL
testing$outcome = NULL

model_tree = tree(diagnosis ~ radius_mean + 
                    texture_mean +
                    perimeter_mean +
                    area_mean +
                    smoothness_mean	+ 
                    compactness_mean	+
                    concavity_mean	+
                    concave.points_mean +
                    symmetry_mean  +
                    fractal_dimension_mean ,
                  data = training)

# Çalışmamızın sonuçlarını görmek istiyoruz :)
summary(model_tree)

# Şimdi sonuçlarımızı çizmek istiyoruz :))
plot(model_tree, type = "uniform")

# Plot'a text ekleyelim mi? :(
text(model_tree, pretty = 0, cex=0.5)


# Training verilerindeki ağacı kontrol edin
# Dağılım tahmini

model_tree_pred_train = predict(model_tree, training) # her sınıf için olasılık verir
head(model_tree_pred_train)

# Nokta tahmini
# Olasılık çıktısını kategorik çıktıya çevirelim.

maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx = apply(model_tree_pred_train, c(1), maxidx)
training_prediction = c('B', 'M')[idx]
table(training_prediction, training$diagnosis)

# Test verisindeki ağacı kontrol edin
# Dağılım tahmini

model_tree_pred_test = predict(model_tree, testing) # her sınıf için olasılık verir
head(model_tree_pred_test)

# Nokta tahmini
# Olasılık çıktısını kategorik çıktıya çevirelim

maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx = apply(model_tree_pred_test, c(1), maxidx)
testing_prediction = c('B', 'M')[idx]
table(testing_prediction, testing$diagnosis)


cv.tree(model_tree)
plot(cv.tree(model_tree)) # 5 boyutunda bir ağaç gibi görünüyor belki de en iyi olabilir

?corrgram()
