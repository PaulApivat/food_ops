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
3	Line ผลิต                                 # production_line
4	คนดูแล                                    # manager
5	จำนวนคนเต็ม (คน) ตามสังกัด HR               # total_employee
6	คนมาทำงานต่อวัน (คน) ตามสังกัด HR            # daily_employee
7	ส่งออกต่างแผนก (คน) (แผนกที่1)               # dept_1
8	ค่าแรงปกติ (บาท) ตามสังกัด HR                # normal_wage
9	รับค่าแรงต่างแผนก (บาท)                     # dept_wage
10	จำนวนชั่วโมง OT ที่ขออนุมัติ (ชั่วโมง)            # OT_total
11	ค่าแรง OT (บาท) ของแผนกตัวเอง              # OT_wage
12	ค่าแรงรวมผลิต Actual (บาท)                 # actual_wage
13	ค่าแรงรวมผลิต STD (บาท)                    # standard_wage
14	ยอดผลิต (kg)                              # production_total_kg
15	ยอดผลิต (ถาด)                             # production_total_tray
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
28	ค่าแรง STD/ยอดผลิต ( บาทต่อKG)              # standard_wage_production_total
29	ค่าแรงรวมผลิต Actual/ยอดผลิต ( บาทต่อKG)     # actual_wage_production_total
30	ใช้ค่าแรงน้อยกว่า STD                        # wage_below_standard
31	ใช้ค่าแรงมากกว่า STD                        # wage_above_standard
32	Cal %OT                                  # cal_OT_percent
33	Cal ค่าแรง vs.STD (น้อยกว่า+/มากกว่า -)      # cal_wage_vs_std
34	Cal ค่าแรง STD/ยอดผลิต (บาท/KG)              # NA
35	Cal ค่าแรง Actual/ยอดผลิต (บาท/KG)           # NA




