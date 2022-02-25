library(tidyverse)

## 1. Read in the four files
df_18 <- readxl::read_xlsx('data/IowaDNR_2018_Data_Merged.xlsx', sheet = 'Sheet2')
#View(df_18)

df_19 <- readxl::read_xlsx('data/IowaDNR_2019_Data_Merged.xlsx', sheet = 'combined')
#View(df_19)

df_20 <- readxl::read_xlsx('data/IowaDNR_2020_Data_Merged.xlsx', sheet = 'Sheet1')
#View(df_20)


df_21 <- readxl::read_xlsx('data/IowaDNR_2021_Data_Merged.xlsx', sheet = 'Sheet1')
#View(df_21)

## 2. Find the columns that exist in all the 4 files

# a) Find the column (names) that exist in all the four files
common_colnames <-  Reduce(intersect, list(colnames(df_18), colnames(df_19), colnames(df_20), colnames(df_21)))
## Only two columns "Collected Date", "pH" were found in all the 4 dataframes; Maybe the same value is named differently in the 4 dataframes?
## I am gonna do a survey the next


# b) df_21 has the least number of columns, I am gonna check the df_21 columns existence situations in other df below:

# `Label`: change the "Sample_ID" in df_18 to "Label".
# `Client Reference`: not in df_18.
# `Environmental Location`: not in df_18.

# `Microcystin`: in df_18, there are "Microcystin Value [ug/L]" and "Microcystin \r\nRAW Value \r\n[ug/L]" ???

# `TKP`: in df_21, change "TKP" to "TKP (mg P/L)".
# `ortho-P`: not in df_18.
# `TKN`: change it in df_21 to "TKN (mg N/L)".
# `Dissolved Oxygen (mg/L)`: only observed in df_21.
# `16S`: change the "16S rRNA gene\r\n(copies/mL)" in df_18 to `16S`.
# `AM`: change "MicrocystismcyA gene\r\n(copies/mL)" in df_18 to "mcyA.M"; change "AM" in df_21 to "mcyA.M".
# `AP`: change "PlanktothrixmcyA gene\r\n(copies/mL)" in df_18 to "mcyA.P"; change "AP" in df_21 to "mcyA.P".
# `AA`: change "AanabaenamcyA gene\r\n(copies/mL)" in df_18 to "mcyA.A"; change "AA" in df_21 to "mcyA.A".

# c) Next, modify each dataframe as advised above
new_df_18 <- df_18 %>% 
  rename(Label = "Sample_ID",
         `16S` = `16S rRNA gene\r\n(copies/mL)`,
         mcyA.M = `MicrocystismcyA gene\r\n(copies/mL)`,
         mcyA.P = `PlanktothrixmcyA gene\r\n(copies/mL)`,
         mcyA.A = `AanabaenamcyA gene\r\n(copies/mL)`) %>% 
  mutate(year = "2018")

new_df_19 <- df_19 %>% 
  mutate(year = '2019')

new_df_20 <- df_20 %>% 
  mutate(year = '2020')

new_df_21 <- df_21 %>% 
  rename(`TKP (mg P/L)` = TKP,
         `TKN (mg N/L)` = TKN,
         mcyA.M = AM,
         mcyA.P = AP,
         mcyA.A = AA) %>% 
  mutate(year = '2021')


# d) So now including the year variable, there should be 10 common columns in the four modified dataframes
new_common_cols <- Reduce(intersect, list(colnames(new_df_18), colnames(new_df_19), colnames(new_df_20), colnames(new_df_21)))

## 3. extract the new common columns in each dataframe and concatenate them into one dataframe
DNR_all_years <-  do.call("rbind", list(new_df_18 %>% select(new_common_cols),
                      new_df_19 %>% select(new_common_cols),
                      new_df_20 %>% select(new_common_cols),
                      new_df_21 %>% select(new_common_cols)))

write.table(DNR_all_years, file = "data/DNR_all_years.csv", quote = FALSE, row.names = FALSE, sep = ',')
