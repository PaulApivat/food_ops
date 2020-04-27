# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

library(tidyverse)

# load data
df <- read_csv("food_data.csv")

# exploratory
str(df)

# dealing with Timestamp data
# change chr --> POSIXlt
strptime(df$Timestamp, "%m/%d/%Y %H:%M:%OS")
df$Timestamp <- strptime(df$Timestamp, "%m/%d/%Y %H:%M:%OS")

# must change to POSIXct format before plotting
df$Timestamp <- as.POSIXct(df$Timestamp)

# delete empty rows because 50,000+ empty rows is a pain
# delete 233 -> 50,669
df <- df[-c(216:50669),]

# first plot success
ggplot(data = df, mapping = aes(x=Timestamp, y=df$`ยอดผลิต (kg)`))+geom_point()

####### Data Dictionary

## change column names
1	Timestamp                                # Timestamp
2	วันที่                                      # date
3	Line ผลิต                                 # line
4	คนดูแล                                    # manager
5	จำนวนคนเต็ม (คน) ตามสังกัด HR               # empl_total
6	คนมาทำงานต่อวัน (คน) ตามสังกัด HR            # empl_daily
7	ส่งออกต่างแผนก (คน) (แผนกที่1)               # dept_exch
8	ค่าแรงปกติ (บาท) ตามสังกัด HR                # normal_wage
9	รับค่าแรงต่างแผนก (บาท)                     # dept_wage
10	จำนวนชั่วโมง OT ที่ขออนุมัติ (ชั่วโมง)            # OT_total
11	ค่าแรง OT (บาท) ของแผนกตัวเอง              # OT_wage
12	ค่าแรงรวมผลิต Actual (บาท)                 # actual_wage
13	ค่าแรงรวมผลิต STD (บาท)                    # std_wage
14	ยอดผลิต (kg)                              # prod_total_kg
15	ยอดผลิต (ถาด)                             # prod_total_tray
16	โรงงาน                                   # factory
17	หมายเหตุ: กดเลือกในเวรหยุดของไลน์ผลิต           # NA
18	ส่งไปแผนก (แผนกที่1)                          # NA 
19	ส่งออกต่างแผนก (คน) (แผนกที่2)                 # NA
20	จำนวนชั่วโมงที่ไป (แผนกที่1) [หน่วย :ชั่วโมง]       # NA
21	ส่งไปแผนก (แผนกที่2)                          # NA
22	จำนวนชั่วโมงที่ไป (แผนกที่2) [หน่วย :ชั่วโมง]       # NA
23	ส่งออกต่างแผนก (คน) (แผนกที่3)                 # NA
24	ส่งไปแผนก (แผนกที่3)                          # NA
25	จำนวนชั่วโมงที่ไป (แผนกที่3) [หน่วย :ชั่วโมง]       # NA
26	ค่าแรง OT (บาท) ของต่างแผนกที่รับมา             # NA
27	%OT                                      # OT_percent
28	ค่าแรง STD/ยอดผลิต ( บาทต่อKG)              # std_wage_per_prod
29	ค่าแรงรวมผลิต Actual/ยอดผลิต ( บาทต่อKG)     # actual_wage_per_prod
30	ใช้ค่าแรงน้อยกว่า STD                        # wage_below_std
31	ใช้ค่าแรงมากกว่า STD                        # wage_above_std
32	Cal %OT                                  # cal_OT_percent
33	Cal ค่าแรง vs.STD (น้อยกว่า+/มากกว่า -)      # cal_wage_vs_std
34	Cal ค่าแรง STD/ยอดผลิต (บาท/KG)              # NA
35	Cal ค่าแรง Actual/ยอดผลิต (บาท/KG)           # NA

## column name change
colnames(df)[2] <- 'date'
colnames(df)[3] <- 'line'
colnames(df)[4] <- 'manager'
colnames(df)[5] <- 'empl_total'
colnames(df)[6] <- 'empl_daily'
colnames(df)[7] <- 'dept_exch'
colnames(df)[8] <- 'normal_wage'
colnames(df)[9] <- 'dept_wage'
colnames(df)[10] <- 'OT_total'
colnames(df)[11] <- 'OT_wage'
colnames(df)[12] <- 'actual_wage'
colnames(df)[13] <- 'std_wage'
colnames(df)[14] <- 'prod_total_kg'
colnames(df)[15] <- 'prod_total_tray'
colnames(df)[16] <- 'factory'
# columns 17-26
colnames(df)[27] <- 'OT_percent'
colnames(df)[28] <- 'std_wage_per_prod'
colnames(df)[29] <- 'actual_wage_per_prod'
colnames(df)[30] <- 'wage_below_std'
colnames(df)[31] <- 'wage_above_std'
colnames(df)[32] <- 'cal_OT_percent'
colnames(df)[33] <- 'cal_wage_vs_std'

### basic plots with column name change
# manager and dept_wage
ggplot(df, aes(x=manager, y=dept_wage)) + geom_bar(stat = 'identity')

# filter  Kimchi Production line, visualizat total production by date
df %>%
    filter(line=='8/10 กิมจิ')%>%
    ggplot(aes(x=date, y=prod_total_kg))  + geom_bar(stat = 'identity')

# re-name line, manager, 

# create two new columns after line and manager
df2 <- df

# insert column using tibble package (add_column()) 
df2 <- add_column(df2, line2 = NA, .after = "manager")
df2 <- add_column(df2, manager2 = NA, .after = "line2")

# change thai to english conditionally
# note: both columns have to be the same data type
# note: had to convert NA as logical to character first
df2$line2 <- as.character(df2$line2)

df2$line2 = ifelse(df2$line=='8/10 กิมจิ', '8/10 kimchi', df2$line2)
df2$line2 = ifelse(df2$line=='9/10 ผลไม้', '9/10 fruit', df2$line2)
df2$line2 = ifelse(df2$line=='2/10 ข้าวปั้น', '2/10 onikiri', df2$line2)
df2$line2 = ifelse(df2$line=='3/10 Appetizer & ข้าวโพด & ไข่ตุ๋น', '3/10 appetizer', df2$line2)
df2$line2 = ifelse(df2$line=='KW3 7/8กุ้ง ปลาแล่ ปลาหมึก  เทอร์โมฟอร์ม', 'KW3 7/8 shrimp squid', df2$line2)
df2$line2 = ifelse(df2$line=='KW3 2/8ข้าวกล่อง ห้องปรุง', 'KW3 2/8 rice seasoning', df2$line2)
df2$line2 = ifelse(df2$line=='KW3 3/8ข้าวกล่อง ห้องตักบรรจุ', 'KW3 3/8 rice utensils', df2$line2)
df2$line2 = ifelse(df2$line=='7/10 ฟรีสและบรรจุ KW1', '7/10 freeze contain KW1', df2$line2)
df2$line2 = ifelse(df2$line=='5/10 ขนมหวาน', '5/10 dessert', df2$line2)
df2$line2 = ifelse(df2$line=='KW3 6/8ปูอัด', 'KW3 6/8 crab stick', df2$line2)
df2$line2 = ifelse(df2$line=='KW3 4/8ฟรีสและบรรจุ กะกลางคืน KW3', 'KW3 4/8 freeze contain KW3', df2$line2)
df2$line2 = ifelse(df2$line=='KW3 5/8เต้าหู้ ลูกชิ้น Kamaboko', 'KW3 5/8 tofu kamaboko', df2$line2)
df2$line2 = ifelse(df2$line=='KW3 8/8เบ็ดเตล็ด repack RTC  +  RTE  +กิมจิ', 'KW3 8/8 repack RTC_RTE_kimchi', df2$line2)
df2$line2 = ifelse(df2$line=='6/10 ข้าวกล่อง Chill + Frozen KW1', '6/10 Rice Chill + Frozen KW1', df2$line2)
df2$line2 = ifelse(df2$line=='1/10 เตรียมวัตถุดิบ ผัก  เนื้อ  ส่วนผสม  KW1', '1/10 raw material KW1', df2$line2)
df2$line2 = ifelse(df2$line=='4/10 TV Dinner+ BBQ', '4/10 TV Dinner BBQ', df2$line2)
df2$line2 = ifelse(df2$line=='KW3 1/8ข้าวกล่อง ห้องเตรียมวัตถุดิบ ส่วนผสม', 'KW3 1/8 rice raw material ingredients', df2$line2)
df2$line2 = ifelse(df2$line=='10/10 เบ็ดเตล็ด K1', '10/10 misc K1', df2$line2)







