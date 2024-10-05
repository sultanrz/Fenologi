# Install dan load paket yang dibutuhkan
library(tidyverse)
library(readxl)

# Load data dari setiap sheet
file_path <- "FENOLOGI SM.xlsx"  # Ganti dengan path file Excel kamu
data_2019 <- read_excel(file_path, sheet = "2019")
data_2020 <- read_excel(file_path, sheet = "2020")
data_2021 <- read_excel(file_path, sheet = "2021")
data_2022 <- read_excel(file_path, sheet = "2022")
data_2023 <- read_excel(file_path, sheet = "2023")

# Pilih dan ubah tipe data kolom 'Unripe fruit', 'Ripe fruit', dan 'Flower' menjadi numeric di setiap dataset
data_2019 <- data_2019 %>%
  select(`Family...4`, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  rename(Family = `Family...4`) %>%
  mutate(`Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2020 <- data_2020 %>%
  select(`Family...4`, `Obs Months...5`, `Unripe fruit...6`, `Ripe fruit...7`, `Flower...8`) %>%
  rename(Family = `Family...4`, 
         `Obs Months` = `Obs Months...5`, 
         `Unripe fruit` = `Unripe fruit...6`, 
         `Ripe fruit` = `Ripe fruit...7`, 
         Flower = `Flower...8`) %>%
  mutate(`Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2021 <- data_2021 %>%
  select(Family, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  mutate(`Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2022 <- data_2022 %>%
  select(Family, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  mutate(`Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2023 <- data_2023 %>%
  select(Family, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  mutate(`Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

# Gabungkan semua data dari setiap tahun setelah memastikan tipe data konsisten
all_data <- bind_rows(data_2019, data_2020, data_2021, data_2022, data_2023)

# Lihat struktur data gabungan
str(all_data)

# Filter data hanya untuk yang memiliki informasi tentang flowering
flowering_data <- all_data %>%
  filter(!is.na(Flower)) %>%
  mutate(Month = as.factor(`Obs Months`))  # Mengubah kolom bulan menjadi faktor

# Membuat visualisasi tren berbunga per family
# Install dan load paket yang dibutuhkan
library(ggplot2)

# Install dan load paket yang dibutuhkan
library(tidyverse)
library(readxl)
library(patchwork)

# Load data dari setiap sheet
file_path <- "FENOLOGI SM.xlsx"  # Ganti dengan path file Excel kamu
data_2019 <- read_excel(file_path, sheet = "2019")
data_2020 <- read_excel(file_path, sheet = "2020")
data_2021 <- read_excel(file_path, sheet = "2021")
data_2022 <- read_excel(file_path, sheet = "2022")
data_2023 <- read_excel(file_path, sheet = "2023")

# Pilih dan ubah tipe data kolom 'Unripe fruit', 'Ripe fruit', dan 'Flower' menjadi numeric di setiap dataset
data_2019 <- data_2019 %>%
  select(`Family...4`, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  rename(Family = `Family...4`) %>%
  mutate(`Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2020 <- data_2020 %>%
  select(`Family...4`, `Obs Months...5`, `Unripe fruit...6`, `Ripe fruit...7`, `Flower...8`) %>%
  rename(Family = `Family...4`, 
         `Obs Months` = `Obs Months...5`, 
         `Unripe fruit` = `Unripe fruit...6`, 
         `Ripe fruit` = `Ripe fruit...7`, 
         Flower = `Flower...8`) %>%
  mutate(`Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2021 <- data_2021 %>%
  select(Family, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  mutate(`Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2022 <- data_2022 %>%
  select(Family, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  mutate(`Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2023 <- data_2023 %>%
  select(Family, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  mutate(`Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

# Gabungkan semua data dari setiap tahun setelah memastikan tipe data konsisten
all_data <- bind_rows(data_2019, data_2020, data_2021, data_2022, data_2023)

# Filter data hanya untuk yang memiliki informasi tentang flowering atau fruiting
all_data_clean <- all_data %>%
  filter(!is.na(Flower) | !is.na(`Ripe fruit`))

# Konversi kolom bulan menjadi angka (1 = January, 2 = February, dst.)
all_data_clean <- all_data_clean %>%
  mutate(Month = case_when(
    `Obs Months` == "January" ~ 1,
    `Obs Months` == "February" ~ 2,
    `Obs Months` == "March" ~ 3,
    `Obs Months` == "April" ~ 4,
    `Obs Months` == "May" ~ 5,
    `Obs Months` == "June" ~ 6,
    `Obs Months` == "July" ~ 7,
    `Obs Months` == "August" ~ 8,
    `Obs Months` == "September" ~ 9,
    `Obs Months` == "October" ~ 10,
    `Obs Months` == "November" ~ 11,
    `Obs Months` == "December" ~ 12
  ))

# Pilih hanya 9 family yang spesifik
selected_families <- c("Anacardiaceae", "Dipterocarpaceae", "Leguminosae", "Lauraceae", 
                       "Malvaceae", "Meliaceae", "Moraceae", "Myrtaceae", "Sapotaceae")

# Filter untuk hanya 9 family yang diminta
filtered_data <- all_data_clean %>%
  filter(Family %in% selected_families)

# Visualisasi tren berbunga (flowering) dan berbuah (fruiting)
flowering_plot <- ggplot(filtered_data, aes(x = Month)) +
  geom_bar(stat = "count", fill = "pink", color = "black") +
  facet_wrap(~ Family, scales = "free_y") +
  labs(title = "Flowering",
       x = "Month",
       y = "Frequency") +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme_minimal()

fruiting_plot <- ggplot(filtered_data, aes(x = Month)) +
  geom_bar(stat = "count", fill = "gray", color = "black") +
  facet_wrap(~ Family, scales = "free_y") +
  labs(title = "Fruiting",
       x = "Month",
       y = "Frequency") +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme_minimal()

# Gabungkan kedua plot
combined_plot <- flowering_plot / fruiting_plot

# Simpan plot sebagai PNG
ggsave("flowering_fruiting_visualization.png", plot = combined_plot, width = 12, height = 8)



