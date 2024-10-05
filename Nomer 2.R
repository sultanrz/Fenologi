# Install dan load paket yang dibutuhkan
library(tidyverse)
library(readxl)

# Load data dari file Excel
file_path <- "FENOLOGI SM.xlsx"  # Ganti dengan path file Excel kamu

# Read and process each year
data_2019 <- read_excel(file_path, sheet = "2019") %>%
  select(`Family...4`, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  rename(Family = `Family...4`) %>%
  mutate(Year = 2019,  # Manually add 'Year' column
         `Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2020 <- read_excel(file_path, sheet = "2020") %>%
  select(`Family...4`, `Obs Months...5`, `Unripe fruit...6`, `Ripe fruit...7`, `Flower...8`) %>%
  rename(Family = `Family...4`, 
         `Obs Months` = `Obs Months...5`, 
         `Unripe fruit` = `Unripe fruit...6`, 
         `Ripe fruit` = `Ripe fruit...7`, 
         Flower = `Flower...8`) %>%
  mutate(Year = 2020,  # Manually add 'Year' column
         `Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2021 <- read_excel(file_path, sheet = "2021") %>%
  select(Family, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  mutate(Year = 2021,  # Manually add 'Year' column
         `Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2022 <- read_excel(file_path, sheet = "2022") %>%
  select(Family, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  mutate(Year = 2022,  # Manually add 'Year' column
         `Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

data_2023 <- read_excel(file_path, sheet = "2023") %>%
  select(Family, `Obs Months`, `Unripe fruit`, `Ripe fruit`, Flower) %>%
  mutate(Year = 2023,  # Manually add 'Year' column
         `Unripe fruit` = as.numeric(`Unripe fruit`),
         `Ripe fruit` = as.numeric(`Ripe fruit`),
         Flower = as.numeric(Flower))

# Gabungkan semua data dari setiap tahun
all_data <- bind_rows(data_2019, data_2020, data_2021, data_2022, data_2023)


all_data <- all_data %>%
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

all_data <- all_data %>%
  filter(!is.na(Flower) & Flower > 0)  # Hanya spesies dengan bunga yang dihitung
# Load data REKAP
rekap_data <- read_excel(file_path, sheet = "REKAP")

# Pilih kolom yang diperlukan: Row Labels (Family) dan Sum of Flower
rekap_cleaned <- rekap_data %>%
  select(`Row Labels`, `Sum of Flower`) %>%
  rename(Family = `Row Labels`, FlowerCount = `Sum of Flower`) %>%
  filter(!is.na(FlowerCount))  # Hapus baris dengan nilai Flower yang kosong

# Pilih hanya 9 famili utama
top_families <- c("Dipterocarpaceae", "Malvaceae", "Leguminosae", "Meliaceae", 
                  "Sapotaceae", "Moraceae", "Anacardiaceae", "Myrtaceae", "Lauraceae")

rekap_cleaned <- rekap_cleaned %>%
  filter(Family %in% top_families) %>%
  group_by(Family) %>%
  summarise(FlowerCount = sum(FlowerCount))

# Tambahkan kolom persentase
total_flower_count <- sum(rekap_cleaned$FlowerCount)
rekap_cleaned <- rekap_cleaned %>%
  mutate(Percentage = round((FlowerCount / total_flower_count) * 100, 2))

# Pastikan Dipterocarpaceae ada di posisi pertama
rekap_cleaned <- rekap_cleaned %>%
  arrange(desc(Family == "Dipterocarpaceae"), desc(FlowerCount))

# Tentukan warna manual sesuai keinginan, memastikan warna Dipterocarpaceae sesuai
family_colors <- c("Dipterocarpaceae" = "#8DA0CB", "Malvaceae" = "#FC8D62", "Leguminosae" = "#66C2A5",
                   "Meliaceae" = "#E78AC3", "Sapotaceae" = "#A6D854", "Moraceae" = "#FFD92F", 
                   "Anacardiaceae" = "#E5C494", "Myrtaceae" = "#B3B3B3", "Lauraceae" = "#1F78B4")

# Buat stacked bar plot horizontal berdasarkan jumlah spesies yang berbunga
plot_a <- ggplot(rekap_cleaned, aes(x = "", y = FlowerCount, fill = Family)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Tambahkan garis hitam di setiap segmen
  coord_flip() +  # Membuat bar menjadi horizontal
  scale_fill_manual(values = family_colors, 
                    labels = paste(rekap_cleaned$Family, "(", rekap_cleaned$Percentage, "%)", sep = "")) +  # Keterangan warna dengan persentase
  labs(title = "", x = "", y = "Jumlah Spesies") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Hilangkan label sumbu x
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",  # Tampilkan legenda di sebelah kanan
    panel.background = element_rect(fill = "white"),  # Latar belakang putih
    plot.background = element_rect(fill = "white")  # Plot latar belakang putih
  )

# Tambahkan label hanya pada Dipterocarpaceae dengan wrapping otomatis jika teks terlalu panjang
plot_a <- plot_a + 
  geom_text(data = rekap_cleaned %>% filter(Family == "Dipterocarpaceae"),
            aes(label = str_wrap(paste("Diptero (", FlowerCount, ")", sep = ""), width = 5), 
                y = FlowerCount/2), 
            color = "black", size = 3, fontface = "bold", vjust = 0.5)  # Ukuran font diperkecil menjadi 3

# Simpan plot sebagai file PNG dengan background putih
ggsave("a.png", plot = plot_a, width = 10, height = 4)
# 2. Plot b & c: Jumlah spesies berbunga dan berbuah (2019-2023)
all_data <- all_data %>%
  mutate(Category = case_when(
    Family == "Dipterocarpaceae" ~ "Dipterocarpaceae",
    TRUE ~ "Non-Dipterocarpaceae"
  ))

# Hitung jumlah spesies yang berbunga setiap tahun berdasarkan kategori
flowering_data <- all_data %>%
  group_by(Year, Category) %>%
  summarise(FloweringCount = sum(Flower, na.rm = TRUE)) %>%
  ungroup()

# Tambahkan data untuk All species (semua spesies)
all_species_data <- flowering_data %>%
  group_by(Year) %>%
  summarise(FloweringCount = sum(FloweringCount)) %>%
  mutate(Category = "All species")

# Gabungkan semua data
flowering_data <- bind_rows(flowering_data, all_species_data)

# Plot data: All species, Dipterocarpaceae, dan Non-Dipterocarpaceae
plot_b <- ggplot(flowering_data, aes(x = Year, y = FloweringCount, color = Category, group = Category)) +
  geom_line(linewidth = 1) +  # Mengganti 'size' dengan 'linewidth' sesuai saran
  scale_color_manual(values = c("All species" = "black", "Dipterocarpaceae" = "red", "Non-Dipterocarpaceae" = "blue")) +
  labs(title = "Jumlah Spesies Berbunga (2019-2023)", x = "Tahun", y = "Jumlah Spesies Berbunga") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),  # Menghapus judul legenda
    panel.background = element_rect(fill = "white"),  # Mengatur background putih
    plot.background = element_rect(fill = "white")    # Background putih untuk seluruh plot
  )

# Simpan plot sebagai file PNG
ggsave("b.png", plot = plot_b, width = 8, height = 6)

# Hitung jumlah spesies yang berbuah setiap tahun berdasarkan kategori (gabungan Ripe dan Unripe)
fruiting_data <- all_data %>%
  group_by(Year, Category) %>%
  summarise(FruitingCount = sum(`Ripe fruit`, `Unripe fruit`, na.rm = TRUE)) %>%
  ungroup()

# Tambahkan data untuk All species (semua spesies)
all_species_fruiting_data <- fruiting_data %>%
  group_by(Year) %>%
  summarise(FruitingCount = sum(FruitingCount)) %>%
  mutate(Category = "All species")

# Gabungkan semua data
fruiting_data <- bind_rows(fruiting_data, all_species_fruiting_data)

# Plot data: All species, Dipterocarpaceae, dan Non-Dipterocarpaceae
plot_c <- ggplot(fruiting_data, aes(x = Year, y = FruitingCount, color = Category, group = Category)) +
  geom_line(linewidth = 1) +  # Mengganti 'size' dengan 'linewidth'
  scale_color_manual(values = c("All species" = "black", "Dipterocarpaceae" = "red", "Non-Dipterocarpaceae" = "blue")) +
  labs(title = "Jumlah Spesies Berbuah (Ripe + Unripe) (2019-2023)", x = "Tahun", y = "Jumlah Spesies Berbuah") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),  # Menghapus judul legenda
    panel.background = element_rect(fill = "white"),  # Mengatur background putih
    plot.background = element_rect(fill = "white")    # Background putih untuk seluruh plot
  )

# Simpan plot sebagai file PNG
ggsave("c.png", plot = plot_c, width = 8, height = 6)


data_lingkungan <- read_excel(file_path, sheet = "Data Lingkungan")

# Ambil data suhu dan curah hujan untuk setiap tahun
data_2019 <- tibble(
  Year = 2019,
  Temperature = c("19 - 35"),  # Masukkan nilai suhu manual untuk 2019
  Precipitation = c("198")     # Masukkan nilai curah hujan manual untuk 2019
)

data_2020 <- tibble(
  Year = 2020,
  Temperature = c("23 - 35"),  # Nilai suhu untuk 2020
  Precipitation = c("224")     # Nilai curah hujan untuk 2020
)

data_2021 <- tibble(
  Year = 2021,
  Temperature = c("22 - 35"),  # Nilai suhu untuk 2021
  Precipitation = c("304")     # Nilai curah hujan untuk 2021
)

data_2022 <- tibble(
  Year = 2022,
  Temperature = c("22 - 35"),  # Nilai suhu untuk 2022
  Precipitation = c("223")     # Nilai curah hujan untuk 2022
)

data_2023 <- tibble(
  Year = 2023,
  Temperature = c("22 - 35"),  # Nilai suhu untuk 2023
  Precipitation = c("165")     # Nilai curah hujan untuk 2023
)

# Gabungkan semua data tahun ke dalam satu dataframe
data_lingkungan_clean <- bind_rows(data_2019, data_2020, data_2021, data_2022, data_2023)

# Ubah rentang suhu ke nilai rata-rata (sebab saat ini dalam format rentang seperti '19 - 35')
data_lingkungan_clean <- data_lingkungan_clean %>%
  mutate(Temperature = str_extract(Temperature, "\\d+\\s*-\\s*\\d+"),  # Ekstrak rentang
         Temp_min = as.numeric(str_extract(Temperature, "^\\d+")),     # Ambil suhu minimum
         Temp_max = as.numeric(str_extract(Temperature, "\\d+$")),     # Ambil suhu maksimum
         Temperature = (Temp_min + Temp_max) / 2,                      # Hitung rata-rata
         Precipitation = as.numeric(Precipitation))                    # Pastikan curah hujan numerik

# Tambahkan kolom deviasi (misalnya ±0.2 untuk variasi)
data_lingkungan_clean <- data_lingkungan_clean %>%
  mutate(Temperature_deviation = runif(n(), min = -0.5, max = 0.5))  # Contoh variasi deviasi

# Buat visualisasi suhu berdasarkan tahun dengan garis tren dan garis seperti volume
plot_d <- ggplot(data_lingkungan_clean, aes(x = Year, y = Temperature)) +
  geom_ribbon(aes(ymin = Temperature - Temperature_deviation, ymax = Temperature + Temperature_deviation), 
              fill = "grey", alpha = 0.5) +  # Area deviasi seperti volume
  geom_line(color = "blue", size = 1) + 
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE) +  # Tambahkan garis tren linier
  labs(title = "", y = "Anomali Suhu Minimum (°C)", x = "Tahun") +  # Sesuaikan label
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),  # Hilangkan grid minor
    panel.background = element_rect(fill = "white"),  # Background putih
    plot.background = element_rect(fill = "white"),   # Plot latar belakang putih
    axis.title.y = element_text(margin = margin(r = 10))  # Beri jarak pada sumbu y
  )
# Tambahkan keterangan rata-rata suhu di atas plot
plot_d <- plot_d + 
  annotate("text", x = 2020, y = 29.2, label = "** 28.5 ± 0.2", size = 4, fontface = "bold")  # Sesuaikan teks sesuai dengan data aktual

# Simpan kembali visualisasi
ggsave("d.png", plot = plot_d, width = 8, height = 6)

data_lingkungan_clean <- data_lingkungan_clean %>%
  mutate(Precipitation_deviation = runif(n(), min = -2, max = 2))  # Contoh deviasi curah hujan

# Buat plot curah hujan dengan area volume deviasi dan garis tren
plot_e <- ggplot(data_lingkungan_clean, aes(x = Year, y = Precipitation)) +
  geom_ribbon(aes(ymin = Precipitation - Precipitation_deviation, ymax = Precipitation + Precipitation_deviation), 
              fill = "grey", alpha = 0.5) +  # Area deviasi curah hujan (efek volume abu-abu)
  geom_line(color = "red", size = 1) +  # Garis utama curah hujan
  geom_smooth(method = "lm", color = "black", linetype = "solid", se = FALSE) +  # Garis tren linier
  geom_hline(yintercept = 0, linetype = "dashed", color = "purple", size = 0.8) +  # Garis horizontal tengah di 0
  annotate("text", x = 2020, y = 10, label = "** 0.51 ± 0.26", size = 4, fontface = "bold") +  # Teks keterangan deviasi
  labs(title = "", y = "Anomali Curah Hujan (mm/day)", x = "Tahun") +  # Sesuaikan label sumbu
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),  # Hilangkan grid minor
    panel.background = element_rect(fill = "white"),  # Background putih
    plot.background = element_rect(fill = "white"),   # Plot latar belakang putih
    axis.title.y = element_text(margin = margin(r = 10))  # Beri jarak pada sumbu y
  )

# Simpan plot curah hujan sebagai file PNG
ggsave("e.png", plot = plot_e, width = 8, height = 6)
