library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)


product_codes_HS22_V202501 <- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Team Workstreams_2024/Suppy Chain/Data collection/CBAM good/BACI_HS22_V202501/product_codes_HS22_V202501.csv")
BACI_HS2023 <- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Team Workstreams_2024/Suppy Chain/Data collection/CBAM good/BACI_HS22_V202501/BACI_HS22_Y2023_V202501.csv")
BACI_HS2023$t<-NULL

country_codes_V202501 <- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Team Workstreams_2024/Suppy Chain/Data collection/CBAM good/BACI_HS22_V202501/country_codes_V202501.csv")
African_Countries_ISO_code <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/R/BACI_HS92_V202401b/African Countries ISO code.xlsx")
country_codes_V202501$Is_African<-0
country_codes_V202501$country_iso2<-NULL
EU_ETS <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Team Workstreams_2024/Suppy Chain/Data collection/CBAM good/EU_ETS.xlsx")

country_codes_V202501$Is_African[country_codes_V202501$country_iso3%in%African_Countries_ISO_code$ISO3]<-1
CBAM_list <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Team Workstreams_2024/Suppy Chain/Data collection/CBAM good/CBAM_list.xlsx")
CBAM_hs2022<-CBAM_list$code6digit

#Trade of CBAM good
BACI_HS2023<-BACI_HS2023[BACI_HS2023$k%in%CBAM_hs2022,]


African_countries<-country_codes_V202501$country_code[country_codes_V202501$Is_African==1]

African_BACI_HS2023<-BACI_HS2023[BACI_HS2023$i%in%African_countries,]

ABHS2023<-merge(African_BACI_HS2023,country_codes_V202501, by.x = "i" ,by.y = "country_code",all.x = T  )
ABHS2023$Is_African<-NULL
colnames(ABHS2023)<-c("i","j","k","v","q","name_i","iso3_i")

ABHS2023<-merge(ABHS2023,country_codes_V202501, by.x = "j" ,by.y = "country_code",all.x = T  )
ABHS2023$country_iso2<-NULL

colnames(ABHS2023)<-c("j","i","k","v","q","name_i","iso3_i","name_j","iso3_j","j_African")

ABHS2023$EU_ETS<-ifelse(ABHS2023$iso3_j%in%EU_ETS$`ISO Code`,1,0)

ABHS_EU2023<-ABHS2023[ABHS2023$EU_ETS==1,]

ABHS_EU2023<-merge(ABHS_EU2023,CBAM_list, by.x = "k" ,by.y = "code6digit",all.x = T  )




ABHS_EU2023_Aggreg<- aggregate(v ~ iso3_i+name_i+Type, data = ABHS_EU2023, sum)
ABHS_EU2023_Aggreg$million_v<-ABHS_EU2023_Aggreg$v/1000



tab <- with(ABHS_EU2023_Aggreg,
            tapply(million_v, list(name_i, Type), sum, default = 0))

CBAM_default <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Team Workstreams_2024/Suppy Chain/Data collection/CBAM good/CBAM_default.xlsx")


testoi<-CBAM_default

testoi$HScode<-substr(testoi$`CN code`,1,6)
testoi$CN_BIS<-testoi$`CN code`
testoi$dupid<-paste0(testoi$HScode,"-",testoi$`Total emissions(tonne CO₂e/tonne goods)`)

CBAM_simp<-CBAM_default

CBAM_simp$`Direct emissions (tonne CO₂e/tonne goods)`<-NULL
CBAM_simp$`Indirect emissions (tonne CO₂e/tonne goods)`<-NULL

CBAM_simp$HScode<-substr(testoi$`CN code`,1,6)

CBAM_simp$dupid<-paste0(CBAM_simp$HScode,"-",CBAM_simp$`Total emissions(tonne CO₂e/tonne goods)`)

CBAM_simp$`CN code`<-NULL
Little_simp<-unique(CBAM_simp)


Little_simp <- CBAM_simp %>%
  group_by(dupid) %>%
  summarise(
    avg_emission = mean(`Total emissions(tonne CO₂e/tonne goods)`, na.rm = TRUE),
    .groups = "drop"
  )



Little_simp$HScode  <- sapply(strsplit(Little_simp$dupid, "-"), `[`, 1)
Little_simp$Emission <- sapply(strsplit(Little_simp$dupid, "-"), `[`, 2)


product_codes_HS22_V202501$fourdigit<-substr(product_codes_HS22_V202501$code,1,4)

VLsimp_simp<-Little_simp[nchar(Little_simp$HScode)==4,]

PC_2022<-product_codes_HS22_V202501[product_codes_HS22_V202501$fourdigit%in%VLsimp_simp$HScode,]

Bigsimp<-merge(PC_2022,VLsimp_simp, by.x = "fourdigit",  by.y= "HScode")
CBAM_simp1<-Little_simp[!nchar(Little_simp$HScode)==4,]

harmonized_bigsimp<-cbind.data.frame(HSCODE=Bigsimp$code,avg_emission=Bigsimp$avg_emission,dupid=Bigsimp$dupid)
harmonized_CBAM_simp<-cbind.data.frame(HSCODE=CBAM_simp1$HScode,avg_emission=CBAM_simp1$avg_emission,dupid=CBAM_simp1$dupid)
Full_data_set<-rbind.data.frame(harmonized_bigsimp,harmonized_CBAM_simp)


product_codes_HS22_V202501$fivedigit<-substr(product_codes_HS22_V202501$code,1,5)

L_simp<-Little_simp

L_simp2<-L_simp[nchar(L_simp$HScode)==5,]
PC_2022<-product_codes_HS22_V202501[product_codes_HS22_V202501$fivedigit%in%L_simp2$HScode,]
Bigsimp<-merge(PC_2022,L_simp2, by.x = "fivedigit",  by.y= "HScode")
Full_data_set1<-Full_data_set[!nchar(Full_data_set$HSCODE)==5,]


Bigsimp$description<-NULL
Bigsimp$fourdigit<-NULL

harmonized_bigsimp<-cbind.data.frame(HSCODE=Bigsimp$code,avg_emission=Bigsimp$avg_emission,dupid=Bigsimp$dupid)
Full_data_set2<-rbind.data.frame(Full_data_set1,harmonized_bigsimp)
#Full_data_set2

ETS_to_HS <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Team Workstreams_2024/Suppy Chain/Data collection/CBAM good/ETS to HS.xlsx")


#####################################################################################
#####################################################################################
#################################Showing the overlapping values
library(dplyr)

# Identify codes that appear more than once
repeated_codes <- ETS_to_HS %>%
  group_by(code6digit) %>%
  filter(n() > 1) %>%
  ungroup()

# Summarize: keep only unique benchmark values
benchmark_summary <- repeated_codes %>%
  group_by(code6digit) %>%
  summarise(Benchmarks = list(unique(`Benchmark21-25`))) %>%
  # Remove those with only one unique value
  filter(lengths(Benchmarks) > 1)

# View remaining codes with multiple unique benchmark values
print(benchmark_summary)

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Unnest to expand list column into individual rows
plot_data <- benchmark_summary %>%
  unnest_longer(Benchmarks) %>%
  mutate(Benchmarks = as.numeric(Benchmarks))  # ensure numeric for plotting


# --- 1. Prepare data ---

# Ensure both identifiers have the same type
plot_data <- plot_data %>%
  mutate(
    code6digit = as.character(code6digit),
    Benchmarks = as.numeric(Benchmarks)
  )

Full_data_set <- Full_data_set %>%
  mutate(
    HSCODE = as.character(HSCODE),
    avg_emission = as.numeric(avg_emission)
  )

# --- 2. Keep only products present in both datasets ---
common_codes <- intersect(plot_data$code6digit, Full_data_set$HSCODE)

plot_common <- plot_data %>%
  filter(code6digit %in% common_codes)

full_common <- Full_data_set %>%
  filter(HSCODE %in% common_codes)

# --- 3. Merge the two datasets ---
merged_common <- left_join(
  plot_common,
  full_common,
  by = c("code6digit" = "HSCODE")
)

# --- 4. Plot both Benchmark and avg_emission ---

ggplot(merged_common) +
  geom_point(aes(x = code6digit, y = Benchmarks, color = "ETS Benchmark"),
             size = 3, alpha = 0.8) +
  geom_line(aes(x = code6digit, y = Benchmarks, group = code6digit, color = "ETS Benchmark"),
            alpha = 0.5) +
  geom_point(aes(x = code6digit, y = avg_emission, color = "CBAM default value"),
             size = 3, shape = 17, alpha = 0.8) +
  labs(
    title = "ETS Benchmark vs CBAM default value",
    x = "HS Code (6-digit)",
    y = "Value",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )


#####################################################################################
#####################################################################################
#################################Average values
HS22to92<- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Team Workstreams_2024/Suppy Chain/Data collection/CBAM good/HS2022toHS1992ConversionAndCorrelationTables(2).xlsx")

library(dplyr)

result <- ETS_to_HS %>%
  left_join(HS22to92, by = c("code6digit" = "From HS 1992")) %>%
  select(everything(), `From HS 2022`)

ETS_to_HS<-result



library(dplyr)

aggregated_ETS <- ETS_to_HS %>%
  group_by(`From HS 2022`) %>%
  summarise(`Benchmark21-25` = mean(`Benchmark21-25`, na.rm = TRUE))

aggregated_CBAM <- Full_data_set2 %>%
  group_by(`HSCODE`) %>%
  summarise(`avg_emission` = mean(`avg_emission`, na.rm = TRUE))

        
merged_data <- merge(
  aggregated_ETS,
  aggregated_CBAM,
  by.x = "From HS 2022",
  by.y = "HSCODE")


merged_data$Differences<- merged_data$avg_emission-merged_data$`Benchmark21-25`
  
merged_data %>%
  arrange(Differences) %>%
  mutate(`From HS 2022` = factor(`From HS 2022`, levels = `From HS 2022`)) %>%
  ggplot(aes(x = `From HS 2022`, y = Differences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Difference between CBAM and ETS Benchmark per Product",
    x = NULL,
    y = "CBAM DV - ETS Bench (tonne CO2/tonne goods) "
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
merged_data$Twodigit<-substr(merged_data$`From HS 2022`,1,2)


#####################################################################################
#####################################################################################
#################################Aggregate everything

merged_data <- left_join(merged_data, HS22to92, by = "From HS 2022")

merged_data$`From HS 1992`%in%CBAM_list$code6digit

CBAM_listbis<-CBAM_list
CBAM_listbis$code6digit2012<-NULL
CBAM_listbis$`Code 4 digit`<-NULL
CBAM_listbis$`Code 2 digit`<-NULL
CBAM_listbis$description<-NULL
CBAM_listbis<-unique(CBAM_listbis)

merged_data$`From HS 2022`<-NULL
merged_data<-unique(merged_data)

merged_data$`From HS 1992`%in%CBAM_listbis$code6digit

merged_data2 <- merge(merged_data, CBAM_listbis,
                      by.x = "From HS 1992",
                      by.y = "code6digit",
                      all.x = TRUE)



library(dplyr)

merged_data2 <- merged_data2 %>%
  group_by(`From HS 1992`, Twodigit, Type) %>%
  summarise(
    `Benchmark21-25` = mean(`Benchmark21-25`, na.rm = TRUE),
    avg_emission = mean(avg_emission, na.rm = TRUE),
    Differences = mean(Differences, na.rm = TRUE),
    .groups = "drop"
  )


merged_data2$Differences<-merged_data2$avg_emission-merged_data2$`Benchmark21-25`


merged_data2 %>%
  arrange(Differences) %>%
  mutate(`From HS 1992` = factor(`From HS 1992`, levels = `From HS 1992`)) %>%
  ggplot(aes(x = `From HS 1992`, y = Differences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Difference between CBAM DV and ETS Benchmark per Product",
    x = NULL,
    y = "CBAM DV - ETS Bench (tonne CO2/tonne goods) "
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


summary_table <- merged_data2 %>%
  group_by(Type) %>%
  summarise(
    n_products = n(),
    avg_Differences = mean(Differences, na.rm = TRUE),
    max_Differences = max(Differences, na.rm = TRUE),
    min_Differences = min(Differences, na.rm = TRUE)
  ) %>%
  arrange(Type)


ABHS_EU2023_AP<- aggregate(v ~ iso3_i+name_i+k, data = ABHS_EU2023, sum)

library(tidyr)

table_ABHS <- ABHS_EU2023_AP %>%
  pivot_wider(
    id_cols = k,
    names_from = iso3_i,
    values_from = v,
    values_fill = 0
  )

table_ABHS<-table_ABHS[table_ABHS$k%in%merged_data2$`From HS 1992`,]


ABHS <- left_join(table_ABHS, merged_data2, 
                         by = c("k" = "From HS 1992"))


ABHS_Data<-ABHS
Data_help<-cbind.data.frame(Code=ABHS_Data$k,Type=ABHS_Data$Type,Differences=ABHS_Data$Differences)


ABHS_Data$Twodigit<-NULL
ABHS_Data$`Benchmark21-25`<-NULL
ABHS_Data$avg_emission<-NULL
ABHS_Data$Type<-NULL
ABHS_Data$Differences<-NULL
ABHS_Data$k<-NULL

row.names(ABHS_Data)<-Data_help$Code

Full_data_country<-ABHS_Data*Data_help$Differences

#Delatenegative
Full_data_country1<-Full_data_country[Data_help$Differences>0,]
Data_help2<-Data_help[Data_help$Differences>0,]

#Aluminium
Aluminium_FDC<-Full_data_country1[Data_help2$Type=="Aluminium",]

#Cement
Cement_FDC<-Full_data_country1[Data_help2$Type=="Cement",]

#Fertiliser
Fertiliser_FDC<-Full_data_country1[Data_help2$Type=="Fertiliser",]

#Iron and steel
IoS_FDC<-Full_data_country1[Data_help2$Type=="iron and steel",]

FDC<-cbind.data.frame(Aluminium=colSums(Aluminium_FDC),Cement= colSums(Cement_FDC),Fertiliser = colSums(Fertiliser_FDC),Iron_steel = colSums(IoS_FDC))

#FDC*Price*turned into million
Certificate<-FDC*70.11/1000 

Certificate<-round(Certificate,digits =3 )




#CBAM Default value are in Tonnes CO2e emissions per tonne of good


#CBAM Default value are in Tonne CO2e/tonne goods
#CBAM Default value are in t CO2 e/t
#Price EUR/Tonne CO2e


