# Script R dari DQLab weekly data challenge : COVID-19 Analysis
---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
# Load packages -----------------------------------------------------------
library(httr)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(ggplot2)
library(hrbrthemes)
```
## 1. Melihat data persebaran COVID-19 di level nasional
```{r}
# GET request for covid 19 data for Indonesia ----------------------------------------------
resp <- GET("https://data.covid19.go.id/public/api/update.json")
cov_id_raw <- content(resp, as = "parsed", simplifyVector = TRUE) 
cov_id_update <- cov_id_raw$update
cov_id_update$penambahan$tanggal
cov_id_update$penambahan$jumlah_positif
cov_id_update$penambahan$jumlah_sembuh
cov_id_update$penambahan$jumlah_meninggal
cov_id_update$total$jumlah_positif
cov_id_update$total$jumlah_meninggal
```
## 2. Mengambil data COVID-19 dari level regional untuk provinsi DKI Jakarta.

```{r}
# Fetch COVID-19 data in regional level for DKI Jakarta -----------------------------------
resp <- GET("https://data.covid19.go.id/public/api/prov_detail_DKI_JAKARTA.json") 
status_code(resp) 
cov_prov_raw <- content(resp, as = "parsed", simplifyVector = TRUE)
str(cov_prov_raw, max.level = 2, vec.len = 3)
```
## 3. Melihat data kasus harian di DKI Jakarta

```{r}
# Inspect the daily cases, pre-process data if necessary ------------------
cov_prov_daily <-
  cov_prov_raw$list_perkembangan %>% 
  transmute(
    date = as.POSIXct(tanggal / 1000, origin = "1970-01-01") %>% 
      as.Date(),
    newcase = KASUS,
    recovered = SEMBUH,
    death = MENINGGAL
  )
head(cov_prov_daily,20)
```
Terdapat rentetan tanggal tanpa kasus positif terutama di minggu ke-2 sampai ke-3 bulan april. Hal ini cukup aneh melihat bahwa di tanggal-tanggal tersebut jumlah kasus sembuh maupun kasus meninggal tetap bertambah. 

Kita coba gali lebih dalam lagi untuk melihat apakah datanya masuk akal. Caranya, kita lihat akumulasi dari total kasus aktif per harinya dengan mengurangi jumlah kasus baru dengan kasus sembuh dan meninggal.

```{r}
# play around
cov_prov_daily_obs <- cov_prov_daily %>%
  transmute(date,
            total_kasus_aktif = cumsum(newcase) - cumsum(recovered) - cumsum(death),
            total_sembuh = cumsum(recovered),
            total_meninggal = cumsum(death))
cov_prov_daily_obs
```
Dapat kita lihat bahwa total kasus aktif di minggu ke-2 hingga ke-4 di bulan maret menunjukkan angka negatif, yang artinya tidak ada kasus baru namun jumlah pasien sembuh dan meninggal bertambah per harinya, yang cukup aneh.

Di sini kita bisa membuat hipotesa bahwa sepertinya ada kesalahan dalam input data sehingga di tanggal-tanggal tersebut banyak data kasus aktif yang tidak tercatat. Hal ini dapat menjelaskan adanya data kasus tanpa tanggal sebanyak 487 kasus di data json RAW dari API.

Hipotesa lainnya adalah kasus-kasus harian di tanggal tersebut tercatat di tanggal yang lebih belakang sehingga terdapat mismatch tersebut.

Untuk saat ini, kejanggalan data tersebut dapat kita kesampingkan terlebih dahulu dengan asumsi jumlah data yang mismatch tidak terlalu besar rasionya dibanding jumlah kasus tercatat (487/21399 = 2,2% ).

## 4. Visuaslisasi data harian

Berikutnya, kita akan visualisasi data harian ini untuk mendapatkan insight yang lebih baik.Kita bisa mulai dengan barchart kasus baru tiap harinya.

```{r, fig.width = 12}
# Turn table into graph! --------------------------------------------------
cov_prov_daily %>% 
  ggplot(aes(date, newcase)) +
  geom_col(fill = "firebrick3") +
  scale_x_date(
    breaks = "2 weeks",
    guide = guide_axis(check.overlap = TRUE, n.dodge = 2),
    labels = scales::label_date(format = "%e %b"),
    expand = c(0.005, 0.005)
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Positif COVID-19 di DKI Jakarta",
    subtitle = "Trend belum menunjukkan adanya perlambatan penambahan jumlah kasus baru tiap harinya",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum_tw(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
```
Hingga sampai awal Agustus ini jumlah kasus makin bertambah setiap harinya, dan mengalami puncaknya di tanggal 29 Juli Lalu.

## 5. Total kasus terkonfirmasi positif, sembuh, dan kasus meninggal di DKI Jakarta

```{r, fig.width = 12, fig.height = 7}
# total kasus di DKI jakarta
cov_prov_total <- cov_prov_daily %>% 
  group_by(
    year = year(date),
  ) %>% 
  summarise(
    across(c(newcase:death), ~ sum(.x, na.rm = TRUE))
  ) %>% 
  transmute (
    year,
    Terkonfirmasi = newcase,
    Sembuh = recovered,
    Meninggal = death
  )%>%
  ungroup() %>%
  pivot_longer(
    cols = Terkonfirmasi:Meninggal,
    names_to = "Status",
    values_to = "Jumlah"
  ) %>%
  ggplot(aes(Status,Jumlah,fill= Status, label = Jumlah ))+
   geom_col(show.legend = FALSE)+
  scale_fill_manual(
    values = c("black", "green", "firebrick2")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Total jumlah kasus per kategori di DKI Jakarta",
    subtitle = "Hingga tanggal 31 Juli 2020, Persentase sembuh berada di angka 61,7% dari total 21399 kasus, dan kasus meninggal sebesar 3,9%",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum_tw(
    base_size = 12, 
    plot_title_size = 21,
    strip_text_face = "italic", 
    grid = FALSE,
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
cov_prov_total
```
## 6. Visualisasi kasus harian dengan menggunakan 3-days moving average

Melakukan moving average untuk 3 data points (hari ini, kemarin dan besoknya) untuk tiap kategori kasus untuk mengurangi fluktuasi pada data

```{r, fig.width = 11, fig.height = 8}
# Daily cases, rolling 3-day average --------------------------------------
cov_prov_daily %>% 
  mutate(
    across(newcase:death, ~ rollmean(.x, k = 3, fill = NA))
  ) %>% 
  pivot_longer(
    cols = newcase:death,
    names_to = "status",
    values_to = "rollmean3day"
  ) %>% 
  mutate(
    status = factor(status, levels = c("newcase", "recovered", "death"), labels = c("Positif", "Sembuh", "Meninggal"))
  ) %>% 
  ggplot(aes(date, rollmean3day, colour = status)) +
  geom_smooth(method = "lm", colour = "black")+
  facet_wrap(~status, ncol = 2, scales = "free_y") +
  geom_line(size = 1.1, show.legend = FALSE) +
  scale_x_date(
    breaks = "15 days",
    guide = guide_axis(check.overlap = FALSE, n.dodge = 2),
    labels = scales::label_date(format = "%d %b"),
    expand = c(0.005, 0.005)
  ) +
  scale_y_continuous(position = "right") +
  scale_colour_manual(
    values = c("firebrick3", "forestgreen", "grey28")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian COVID-19 di DKI Jakarta Dengan 3-days Rolling Average",
    subtitle = "Garis berwarna hitam merupakan garis bantu yang menunjukkan kecenderungan trend untuk setiap kategori kasus",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum_tw(
    base_size = 10, 
    plot_title_size = 21,
    strip_text_face = "italic", 
    grid = FALSE,
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
```
Trend kasus positif dan kasus sembuh cenderung meningkat di DKI Jakarta, sementara jika melihat grafik dari kasus meninggal harian, tidak terlihat pola yang jelas dan dapat dikatakan cenderung random


## 7. Visualisasi kasus COVID-19 secara mingguan

```{r}
# Transform daily cases into weekly cases ---------------------------------
cov_prov_weekly <-
  cov_prov_daily %>% 
  group_by(
    year = year(date),
    week = week(date)
  ) %>% 
  summarise(
    across(c(newcase:death), ~ sum(.x, na.rm = TRUE))
  ) %>% 
  ungroup()
glimpse(cov_prov_weekly)
```

Apakah minggu ini lebih baik dari minggu sebelumnya?

```{r}
# Is this week is better than last week? ----------------------------------
cov_prov_weekly_comparison <- 
  cov_prov_weekly %>% 
  transmute(
    year,
    week,
    newcase,
    newcase_lastweek = dplyr::lag(newcase, 1), # can you explain why we use of `::` operator here?
    newcase_lastweek = replace_na(newcase_lastweek, 0),
    is_better = newcase < newcase_lastweek
  )
glimpse(cov_prov_weekly_comparison)
cov_prov_weekly_comparison %>% 
  count(is_better)
```


```{r ,fig.width = 10}
# Only 8 instances where the current week number of positive case are lower than the previous week
cov_prov_weekly_comparison %>% 
  ggplot(aes(week, newcase, fill = is_better)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di DKI Jakarta",
    subtitle = "Kolom hijau menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum_tw(
    base_size = 8, 
    plot_title_size = 18,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
```
Bagaimana dengan kasus pasien yang sembuh?
```{r,,fig.width = 10}
# KASUS PEKANAN SEMBUH DI DKI JAKARTA
cov_prov_weekly_comparison_rec <- 
  cov_prov_weekly %>% 
  transmute(
    year,
    week,
    recovered,
    recovered_lastweek = dplyr::lag(recovered, 1), # can you explain why we use of `::` operator here?
    recovered_lastweek = replace_na(recovered_lastweek, 0),
    is_better = recovered > recovered_lastweek # lebih baik jika pekan ini lebih banyak yang sembuh
  )
glimpse(cov_prov_weekly_comparison)
cov_prov_weekly_comparison_rec %>% 
  count(is_better)
cov_prov_weekly_comparison_rec %>% 
  ggplot(aes(week, recovered, fill = is_better)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Sembuh COVID-19 di DKI Jakarta",
    subtitle = "Kolom hijau menunjukan penambahan kasus baru lebih banyak dibandingkan satu pekan sebelumnya",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum_tw(
    base_size = 8, 
    plot_title_size = 18,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
```
Tampak tiap minggunya, jumlah pasien yang sembuh lebih banyak dari minggu sebelumnya, menunjukkan tredn yang positif

## 8. Melihat apakah kasus positif lebih sedikit per pekan dibanding pekan lalu dan 2 pekan sebelumnya secara berurutan

```{r}
# check for consecutive weeks
cov_prov_weekly_comparison_2 <- 
  cov_prov_weekly %>% 
  transmute(
    year,
    week,
    newcase,
    newcase_lastweek = dplyr::lag(newcase, 1), 
    newcase_2week = dplyr::lag(newcase,2),
    newcase_2week = replace_na(newcase_2week,0),
    newcase_lastweek = replace_na(newcase_lastweek, 0),
    is_better = newcase < newcase_lastweek & newcase < newcase_2week
  )
glimpse(cov_prov_weekly_comparison_2)
cov_prov_weekly_comparison_2
cov_prov_weekly_comparison_2 %>% 
  count(is_better)
```


```{r,,fig.width = 10}
# plot for 2 consecutive weeks
cov_prov_weekly_comparison_2 %>% 
  ggplot(aes(week, newcase, fill = is_better)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Trend per Pekan Kasus Positif COVID-19 di DKI Jakarta",
    subtitle = "Hanya ada 4 minggu dimana jumlah kasus sepekan lebih rendah dari kasus 2 pekan sebelumnya secara berturut-turut",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum_tw(
    base_size = 8, 
    plot_title_size = 18,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
```
Hanya ada 4 pekan dimana kasus mingguannya lebih rendah dari pekan sebelum dan 2 pekan sebelumnya

## 8. Seventh day amplification factor untuk kota Jakarta
```{r}
# seventh day amplification factor (A7)
cov_prov_daily_A7 <- cov_prov_daily %>%
  filter(date >= as.Date("2020-03-25")) %>% # kasus positif sebelum 25 maret kebanyakan tidak tercatat
  transmute(date,
            total_kasus_positif = cumsum(newcase),
            total_sembuh = cumsum(recovered),
            total_meninggal = cumsum(death)) %>%
  mutate(
    across(total_kasus_positif:total_meninggal, ~ rollmean(.x, k = 3, fill = NA)),
    aktif_h_7= dplyr::lag(total_kasus_positif, 7),
    aktif_h_7 = replace_na(aktif_h_7, 0),
    rasio_A7 = ifelse(aktif_h_7 != 0, total_kasus_positif/aktif_h_7, 0))
```



```{r, fig.width = 11, fig.height = 8}
# plot the A7 metrics
cov_prov_daily_A7 %>% 
  filter(date > as.Date("2020-04-01")) %>%
  ggplot() +
  geom_col(aes(date, rasio_A7, fill = date == "2020-06-18"),alpha=0.6, adjust=0.75,show.legend = FALSE) +
  geom_hline(aes(yintercept = 1),size= 1.5 ) +
  annotate("text",max(cov_prov_daily_A7$date), 1, vjust = -0.8,hjust = +1, label = "A7 = 1",size = 7)+
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "firebrick"))+
  scale_x_date(
    breaks = "10 days",
    guide = guide_axis(check.overlap = FALSE, n.dodge = 1),
    labels = scales::label_date(format = "%e %b"),
    expand = c(0.005, 0.005)
  )+
  labs(
    x = NULL,
    y = "A7",
    title = "The Seventh-day Amplification Factor (A7) di Provinsi DKI Jakarta",
    subtitle = "Kolom hijau menunjukan nilai A7 yang paling mendekati batas A7 = 1",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum_tw(
    base_size = 13, 
    plot_title_size = 24,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
```
angka A7 hampir menyentuh nilai 1 yang merupakan batas bahwa tidak ada 'silent carrier' atau OTG (Orang tanpa gejala) yang mentransmisikan virus kepada pasien baru dalam durasi 1 minggu di tanggal 18 Juni 2020. Namun perlahan-lahan angka ini justru naik di hari-hari berikutnya, dan sekarang berada di sekitar angka 1,2
