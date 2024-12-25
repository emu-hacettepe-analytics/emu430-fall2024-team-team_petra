install.packages("readxl")
library(readxl)
data_collect_kadin <- read_excel("C:/Users/Necati/Desktop/Data_collection_kadin.xlsx")
str((data_collect_kadin))
head(data_collect_kadin)
data_collect_kadin
# NA iceren satirlari silmek
temiz_veri_kadin <- na.omit(data_collect_kadin)          
temiz_veri_kadin

veri_temiz_kadin <- data_collect_kadin[rowSums(is.na(data_collect_kadin)) != ncol(data_collect_kadin), ]
veri_temiz_kadin

data_son_temiz_kadin <- veri_temiz_kadin[-c(1,2,5,6,7,9,10,13,14,15,17,18,19,20,22,23,24,26,27,28,29,30,33,34,36,37,38,40,41,43,44,45,47,48,49,50,51,53,54,55,57,58,59,61,62,63,65,66,67,68,70,71,72,73,74,75,77,78,79,81,82,83,84,86,87,88,89,91,92,93,94,96,97,98,100,101,103,104,105,106), ]
data_son_temiz_kadin

library(ggplot2)
library(ggthemes)
library(ggrepel)

b<- ggplot(data_son_temiz_kadin, aes(x = `Poverty Rate (%)`, y = Homicide, color = Region)) +
  geom_point(size = 3) +  # Noktalar?? boyutland??rma
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  theme_minimal() +
  labs(title = "Poverty Rate vs Homicide -Female20",
       x = "Yoksulluk Oran?? (%)",
       y = "Su?? Oran??") +
  scale_x_continuous(limits = c(6,18))+
  scale_y_continuous(limits = c(0,28))+  geom_label_repel(aes(label = Region_Number), size = 3, max.overlaps = 50)+theme_fivethirtyeight()
b


min_value <- min(data_son_temiz_kadin$Homicide, na.rm = TRUE)
max_value <- max(data_son_temiz_kadin$Homicide, na.rm = TRUE)

# Sonu??lar?? yazd??rmak
min_value
max_value


# Pearson korelasyonu hesaplamak
correlation_result <- cor.test(data_son_temiz_kadin$`Poverty Rate (%)`, data_son_temiz_kadin$Homicide, method = "pearson")

# Sonu??lar?? g??r??nt??lemek
correlation_result
