# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

library(tidyverse)
library(patchwork)

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
5	จำนวนคนเต็ม (คน) ตามสังกัด HR               # empl_total            (sum all + sum by line over time)
6	คนมาทำงานต่อวัน (คน) ตามสังกัด HR            # empl_daily            (sum all + sum by line over time)
7	ส่งออกต่างแผนก (คน) (แผนกที่1)               # dept_exch
8	ค่าแรงปกติ (บาท) ตามสังกัด HR                # normal_wage           (sum all + sum by line)
9	รับค่าแรงต่างแผนก (บาท)                     # dept_wage
10	จำนวนชั่วโมง OT ที่ขออนุมัติ (ชั่วโมง)            # OT_total            
11	ค่าแรง OT (บาท) ของแผนกตัวเอง              # OT_wage            (sum by line over time)
12	ค่าแรงรวมผลิต Actual (บาท)                 # actual_wage        (sum by line over time)
13	ค่าแรงรวมผลิต STD (บาท)                    # std_wage           (sum by line over time)
14	ยอดผลิต (kg)                              # prod_total_kg      (sum by line over time)
15	ยอดผลิต (ถาด)                             # prod_total_tray    (sum by line over time)
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
30	ใช้ค่าแรงน้อยกว่า STD                        # wage_below_std           (erase)
31	ใช้ค่าแรงมากกว่า STD                        # wage_above_std           (erase)
32	Cal %OT                                  # cal_OT_percent
33	Cal ค่าแรง vs.STD (น้อยกว่า+/มากกว่า -)      # cal_wage_vs_std          (*key sum by line over time)
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
df2$manager2 <- as.character(df2$manager2)

# change thai to eng line2
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

# change thai to eng manager2
df2$manager2 = ifelse(df2$manager=='สมพร  (แหว๋ว)  /เด่นดาว (ดาว)', 'somporn', df2$manager2)
df2$manager2 = ifelse(df2$manager=='ปภาวี  (ปาล์ม)', 'baphawee', df2$manager2)
df2$manager2 = ifelse(df2$manager=='ลาวัลย์', 'lawun', df2$manager2)
df2$manager2 = ifelse(df2$manager=='วรรณา(นา)', 'wuna', df2$manager2)
df2$manager2 = ifelse(df2$manager=='จำปี (หมู)/จารุวรรณ  (จอย)', 'jumbhee / jaloowun', df2$manager2)
df2$manager2 = ifelse(df2$manager=='อุบลรัตน์ (รัตน์)/จารุวรรณ  (จอย)', 'ubonrhat / jaloowun', df2$manager2)
df2$manager2 = ifelse(df2$manager=='ปรางทอง  (แต)', 'pla-thong', df2$manager2)
df2$manager2 = ifelse(df2$manager=='สมจิตต์ (ติ๊ดตี่ --กะกลางวัน)  +  ชบาไพร (แจ๋ว-- กะกลางคืน)', 'somjit / shabapai', df2$manager2)
df2$manager2 = ifelse(df2$manager=='ประทีป  (โต้ง)/ สุพิศ', 'prateep / supit', df2$manager2)
df2$manager2 = ifelse(df2$manager=='กนกรัตน์ (ตุ๊ก)/จารุวรรณ  (จอย)', 'kranokrhat / jaloowun', df2$manager2)
df2$manager2 = ifelse(df2$manager=='สุรีรัตน์ (เปี๊ยก)', 'sulirhat', df2$manager2)
df2$manager2 = ifelse(df2$manager=='ทองสุข', 'thongsuk', df2$manager2)
df2$manager2 = ifelse(df2$manager=='ศรีรุธ ( จิ๋ม)', 'srilut', df2$manager2)
df2$manager2 = ifelse(df2$manager=='สุธีรา (แจง)/จารุวรรณ  (จอย)', 'sutheera / jaloowun', df2$manager2)
df2$manager2 = ifelse(df2$manager=='ศรีวรรณ (แขก)', 'sriwan', df2$manager2)
df2$manager2 = ifelse(df2$manager=='กัญญารัตน์ (แมว)', 'kanyarat', df2$manager2)




#### Filter using dplyr::filter and grepl

## Filter for "KW3" in column line2
View(dplyr::filter(df2, grepl("KW3", line2)))

## Filter out(!) "KW3" in column line2
View(dplyr::filter(df2, !grepl("KW3", line2)))

## Filter for "jaloowun" in column manager2
# note: 'jaloowun' is paired with several managers
View(dplyr::filter(df2, grepl("jaloowun", manager2)))


###### EDA: Basic Plots #######

# basic bar plot of production lines (line2) by normal_wages
# note: issues employing reorder() in aes()

# not quite reorder
ggplot(data = df2, mapping = aes(x=reorder(line2, normal_wage), y=normal_wage)) 
+ geom_bar(stat = 'identity') 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

# add na.rm = TRUE improves somewhat
ggplot(data = df2, mapping = aes(x=reorder(line2, normal_wage, na.rm = TRUE), y=normal_wage)) 
+ geom_bar(stat = 'identity') 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

# note: better practice to reorder data, not aesthetics?
# source: https://stackoverflow.com/questions/25230300/ggplot-and-reorder-not-working-even-with-stats

# alternative, create sum_normal_wage first, then use that in ggplot()
df2 %>% 
    group_by(line2) %>% 
    summarize(sum_normal_wage = sum(normal_wage, na.rm = TRUE)) %>% 
    ggplot(aes(x=reorder(line2, sum_normal_wage), y=sum_normal_wage)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


# same for sum_dept_wage
df2 %>% 
    group_by(line2) %>% 
    summarize(sum_dept_wage = sum(dept_wage, na.rm = TRUE)) %>% 
    ggplot(aes(x=reorder(line2, sum_dept_wage), y=sum_dept_wage)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

# same for sum_OT_wage
df2 %>% 
    group_by(line2) %>% 
    summarize(sum_OT_wage = sum(OT_wage, na.rm = TRUE)) %>% 
    ggplot(aes(x=reorder(line2, sum_OT_wage), y=sum_OT_wage)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

# same for sum_actual_wage
df2 %>% 
    group_by(line2) %>% 
    summarize(sum_actual_wage = sum(actual_wage, na.rm = TRUE)) %>% 
    ggplot(aes(x=reorder(line2, sum_actual_wage), y=sum_actual_wage)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


# reorder doesn't quite work 
ggplot(data = df2, mapping = aes(x=reorder(line2, std_wage, na.rm = TRUE), y=std_wage)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


# now reorder works; sum_std_wage
df2 %>% 
    group_by(line2) %>% 
    summarize(sum_std_wage = sum(std_wage, na.rm = TRUE)) %>% 
    ggplot(aes(x=reorder(line2, sum_std_wage), y=sum_std_wage)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


# reorder work for sum_prod_total_kg
df2 %>% 
    group_by(line2) %>% 
    summarize(sum_prod_total_kg = sum(prod_total_kg, na.rm = TRUE)) %>% 
    ggplot(aes(x=reorder(line2, sum_prod_total_kg), y=sum_prod_total_kg)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

##### change factory from character to factor
df2$factory <- as.factor(df2$factory)

## reorder sum_prod_total_kg AND fill = factory
df2 %>% 
    # important: to group_by factory as well otherwise, fill doesn't work
    group_by(line2, factory) %>% 
    summarize(sum_prod_total_kg = sum(prod_total_kg, na.rm = TRUE)) %>% 
    ggplot(aes(x=reorder(line2, sum_prod_total_kg), y=sum_prod_total_kg, fill = factory)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

##### Summary Bar Chart in Patchwork ######
### cover: OT_percent, std_wage_per_prod, actual_wage_per_prod, wage_below_std, 
### wage_above_std, cal_OT_percent, cal_wage_vs_std

library(patchwork)

OT_percent_by_line2 <- df2 %>% 
                        group_by(line2, factory) %>% 
                        summarize(sum_OT_percent = sum(OT_percent, na.rm = TRUE)) %>% 
                        ggplot(aes(x=reorder(line2, sum_OT_percent), y=sum_OT_percent, fill = factory)) 
                        + geom_bar(stat = 'identity') 
                        + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


std_wage_per_prod_by_line2 <- df2 %>% 
                            group_by(line2, factory) %>% 
                            summarize(sum_std_wage_per_prod = sum(std_wage_per_prod, na.rm = TRUE)) %>% 
                            ggplot(aes(x=reorder(line2, sum_std_wage_per_prod), y=sum_std_wage_per_prod, fill = factory)) 
                            + geom_bar(stat = 'identity') 
                            + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


actual_wage_per_prod_by_line2 <- df2 %>% 
                                group_by(line2, factory) %>% 
                                summarize(sum_actual_wage_per_prod = sum(actual_wage_per_prod, na.rm = TRUE)) %>% 
                                ggplot(aes(x=reorder(line2, sum_actual_wage_per_prod), y=sum_actual_wage_per_prod, fill = factory)) 
                                + geom_bar(stat = 'identity') 
                                + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


wage_below_std_by_line2 <- df2 %>% 
                        group_by(line2, factory) %>% 
                        summarize(sum_wage_below_std = sum(wage_below_std, na.rm = TRUE)) %>% 
                        ggplot(aes(x=reorder(line2, sum_wage_below_std), y=sum_wage_below_std, fill = factory)) 
                        + geom_bar(stat = 'identity') 
                        + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

wage_above_std_by_line2 <- df2 %>% 
                    group_by(line2, factory) %>% 
                    summarize(sum_wage_above_std = sum(wage_above_std, na.rm = TRUE)) %>% 
                    ggplot(aes(x=reorder(line2, sum_wage_above_std), y=sum_wage_above_std, fill = factory)) 
                    + geom_bar(stat = 'identity') 
                    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

# cal_OT_percent
# percentages (i.e., "98.94%") presented as character, need to change type
# use parse_number() from readr package

cal_OT_percent_by_line2 <- df2 %>% 
                    group_by(line2, factory) %>% 
                    # important: use parse_number() here
                    summarize(sum_cal_OT_percent = sum(parse_number(cal_OT_percent), na.rm = TRUE)) %>% 
                    ggplot(aes(x=reorder(line2, sum_cal_OT_percent), y=sum_cal_OT_percent, fill = factory)) 
                    + geom_bar(stat = 'identity') 
                    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


cal_wage_vs_std_by_line2 <- df2 %>% 
                    group_by(line2, factory) %>% 
                    summarize(sum_cal_wage_vs_std = sum(cal_wage_vs_std, na.rm = TRUE)) %>% 
                    ggplot(aes(x=reorder(line2, sum_cal_wage_vs_std), y=sum_cal_wage_vs_std, fill = factory)) 
                    + geom_bar(stat = 'identity') 
                    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


### Patchwork line up
## seven plot, 3 column

OT_percent_by_line2 
+ std_wage_per_prod_by_line2 
+ actual_wage_per_prod_by_line2 
+ wage_below_std_by_line2 
+ wage_above_std_by_line2 
+ cal_OT_percent_by_line2 
+ cal_wage_vs_std_by_line2 
+ plot_layout(ncol = 3)

##################### Post Feedback from Pim #######################

##### empl_total            (sum all + sum by line over time)
##### จำนวนคนเต็ม (คน) ตามสังกัด HR 

# change empl_total (chr) to numeric
df2$empl_total <- as.numeric(df2$empl_total)

# sum_empl_total (248)
df2 %>% filter(line2=='8/10 kimchi') %>% summarize(sum_empl_total = sum(empl_total))

# instead of doing line-by-line, need to do group_by(line2), then sum_all_empl_total
# deal with NA
df2 %>% group_by(line2) %>% summarize(sum_empl_total = sum(empl_total, na.rm = TRUE))
df2 %>% group_by(line2) %>% summarize(sum_empl_total = sum(empl_total, na.rm = TRUE)) %>% arrange(desc(sum_empl_total))

# new data frame of just empl_total summed by line (across time)
sum_empl_total_df <- df2 %>% 
                    group_by(line2) %>% 
                    summarize(sum_empl_total = sum(empl_total, na.rm = TRUE)) %>% 
                    arrange(desc(sum_empl_total))

# basic bar plot
ggplot(data = sum_empl_total_df, mapping = aes(x=reorder(line2, sum_empl_total), y=sum_empl_total)) 
+ geom_bar(stat = 'identity') 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

# Some basic bar plot, fill=factory (using original df2 data frame)
empl_total_line2 <- df2 %>% 
    # group_by need to include factory to also use factory in 'fill'
    group_by(line2, factory) %>% 
    summarize(sum_empl_total = sum(empl_total, na.rm = TRUE)) %>% 
    arrange(desc(sum_empl_total)) %>% 
    ggplot(aes(x=reorder(line2, sum_empl_total), y=sum_empl_total, fill=factory)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


####### empl_daily            (sum all + sum by line over time)
####### คนมาทำงานต่อวัน (คน) ตามสังกัด HR
empl_daily_line2 <- df2 %>% 
    # group_by need to include factory to also use factory in 'fill'
    group_by(line2, factory) %>% 
    summarize(sum_empl_daily = sum(empl_daily, na.rm = TRUE)) %>% 
    arrange(desc(sum_empl_daily)) %>% 
    ggplot(aes(x=reorder(line2, sum_empl_daily), y=sum_empl_daily, fill=factory)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


######## normal_wage           (sum all + sum by line)
######## ค่าแรงปกติ (บาท) ตามสังกัด HR 
normal_wage_line2 <- df2 %>% 
    group_by(line2, factory) %>% 
    summarize(sum_normal_wage = sum(normal_wage, na.rm = TRUE)) %>% 
    arrange(desc(sum_normal_wage)) %>% 
    ggplot(aes(x=reorder(line2, sum_normal_wage), y=sum_normal_wage, fill=factory)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

######## OT_wage            (sum by line over time)
######## ค่าแรง OT (บาท) ของแผนกตัวเอง              
OT_wage_line2 <- df2 %>% 
    group_by(line2, factory) %>% 
    summarize(sum_OT_wage = sum(OT_wage, na.rm = TRUE)) %>% 
    arrange(desc(sum_OT_wage)) %>% 
    ggplot(aes(x=reorder(line2, sum_OT_wage), y=sum_OT_wage, fill=factory)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


######### actual_wage        (sum by line over time)
######### ค่าแรงรวมผลิต Actual (บาท)                 
actual_wage_line2 <- df2 %>% 
    group_by(line2, factory) %>% 
    summarize(sum_actual_wage = sum(actual_wage, na.rm = TRUE)) %>% 
    arrange(desc(sum_actual_wage)) %>% 
    ggplot(aes(x=reorder(line2, sum_actual_wage), y=sum_actual_wage, fill=factory)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


########## std_wage           (sum by line over time)
########## ค่าแรงรวมผลิต STD (บาท)                    
std_wage_line2 <- df2 %>% 
    group_by(line2, factory) %>% 
    summarize(sum_std_wage = sum(std_wage, na.rm = TRUE)) %>% 
    arrange(desc(sum_std_wage)) %>% 
    ggplot(aes(x=reorder(line2, sum_std_wage), y=sum_std_wage, fill=factory)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

########## prod_total_kg      (sum by line over time)
########## ยอดผลิต (kg)                              
prod_total_kg_line2 <- df2 %>% 
    group_by(line2, factory) %>% 
    summarize(sum_prod_total_kg = sum(prod_total_kg, na.rm = TRUE)) %>% 
    arrange(desc(sum_prod_total_kg)) %>% 
    ggplot(aes(x=reorder(line2, sum_prod_total_kg), y=sum_prod_total_kg, fill=factory)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

########## prod_total_tray    (sum by line over time)
########## ยอดผลิต (ถาด)                             
prod_total_tray_line2 <- df2 %>% 
    group_by(line2, factory) %>% 
    summarize(sum_prod_total_tray = sum(prod_total_tray, na.rm = TRUE)) %>% 
    arrange(desc(sum_prod_total_tray)) %>% 
    ggplot(aes(x=reorder(line2, sum_prod_total_tray), y=sum_prod_total_tray, fill=factory)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


########### cal_wage_vs_std          (*key sum by line over time)
########## Cal ค่าแรง vs.STD (น้อยกว่า+/มากกว่า -)      
cal_wage_vs_std_line2 <- df2 %>% 
    group_by(line2, factory) %>% 
    summarize(sum_cal_wage_vs_std = sum(cal_wage_vs_std, na.rm = TRUE)) %>% 
    arrange(desc(sum_cal_wage_vs_std)) %>% 
    ggplot(aes(x=reorder(line2, sum_cal_wage_vs_std), y=sum_cal_wage_vs_std, fill=factory)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


# patchwork layout
library(patchwork)

empl_total_line2 
+ empl_daily_line2 
+ normal_wage_line2 
+ OT_wage_line2 
+ actual_wage_line2 
+ std_wage_line2 
+ prod_total_kg_line2 
+ prod_total_tray_line2 
+ cal_wage_vs_std_line2 
+ plot_layout(ncol = 3)

###### Pim Feedback round 2

# new_colum
df2 <- df2 %>% mutate(new_colum = actual_wage / prod_total_kg)
colnames(df2)[38] <- "new_column"

# another chart
df2 %>% group_by(line2, factory) %>% 
    summarize(sum_new_colum = sum(new_colum, na.rm = TRUE)) %>% 
    arrange(desc(sum_new_colum)) %>% 
    ggplot(aes(x=reorder(line2, sum_new_colum), y=sum_new_colum, fill=factory)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


# new_column2
df2 <- df2 %>% mutate(new_column2 = std_wage / prod_total_kg)

# objective compare new_colum vs new_column2
# change line2 into factor
# ERROR: geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?
df2 %>% 
    filter(line2=='9/10 fruit') %>% 
    # add group = 1 gets rid of error geom_path
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=new_column, colour = 'new_column')) 
    + geom_line(aes(y=new_column2, colour = 'new_column2'))

## new_column (red) --> actual_wage / prod_total_kg
## new_column2 (green) --> std_wage / prod_total_kg

# 8/10 kimchi
df2 %>% filter(line2=='8/10 kimchi') %>% ggplot(aes(date, group = 1)) + geom_line(aes(y=new_column, colour = 'new_column')) + geom_line(aes(y=new_column2, colour = 'new_column2'))

# 5/10 dessert
df2 %>% filter(line2=='5/10 dessert') %>% ggplot(aes(date, group = 1)) + geom_line(aes(y=new_column, colour = 'new_column')) + geom_line(aes(y=new_column2, colour = 'new_column2'))

# 1/10 raw material KW1
df2 %>% filter(line2=='1/10 raw material KW1') %>% ggplot(aes(date, group = 1)) + geom_line(aes(y=new_column, colour = 'new_column')) + geom_line(aes(y=new_column2, colour = 'new_column2'))

# 2/10 onikiri
df2 %>% filter(line2=='2/10 onikiri') %>% ggplot(aes(date, group = 1)) + geom_line(aes(y=new_column, colour = 'new_column')) + geom_line(aes(y=new_column2, colour = 'new_column2'))

# TV Dinner BBQ
df2 %>% 
    filter(line2=='4/10 TV Dinner BBQ') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=new_column, colour = 'new_column', size=20)) 
    + geom_line(aes(y=new_column2, colour = 'new_column2', size=20))


###### Next step: Comparison of new_column vs new_column2 by production line, April 13 - 30th
# Step 1: Ref Data Dictionary - create: line2, actual_wage, std_wage, prod_total_kg, factory
# Step 2: Change all production line(s) from Thai to English
# Step 3: create actual_wage_per_kg, std_wage_per_kg
# Step 4: create comparison line charts for each production line

# Step 1: 
df3 <- read.csv("food_ops.csv")

# survey column names
names(df3)
# change column names date, line, manager, actual_wage, std_wage, factory, 
colnames(df3)[2] <- 'date'
colnames(df3)[3] <- 'line'
colnames(df3)[4] <- 'manager'
colnames(df3)[12] <- 'actual_wage'
colnames(df3)[13] <- 'std_wage'
colnames(df3)[14] <- 'prod_total_kg'
colnames(df3)[15] <- 'prod_total_tray'
colnames(df3)[16] <- 'factory'

# create four new columns: line2, manager2, 

# insert column using tibble package (add_column()) 
df3 <- add_column(df3, line2 = NA, .after = "manager")
df3 <- add_column(df3, manager2 = NA, .after = "line2")


### STep 2 
# convert thai characters in line (& manager) into english for line2, manager2
# most manual step, need to write a function to do this
df3$line2 = ifelse(df3$line=='8/10 กิมจิ', '8/10 kimchi', df3$line2)

df3$line2 = ifelse(df3$line=='9/10 ผลไม้', '9/10 fruit', df3$line2)
df3$line2 = ifelse(df3$line=='2/10 ข้าวปั้น', '2/10 onikiri', df3$line2)
df3$line2 = ifelse(df3$line=='3/10 Appetizer & ข้าวโพด & ไข่ตุ๋น', '3/10 appetizer', df3$line2)
df3$line2 = ifelse(df3$line=='KW3 7/8กุ้ง ปลาแล่ ปลาหมึก  เทอร์โมฟอร์ม', 'KW3 7/8 shrimp squid', df3$line2)
df3$line2 = ifelse(df3$line=='KW3 2/8ข้าวกล่อง ห้องปรุง', 'KW3 2/8 rice seasoning', df3$line2)
df3$line2 = ifelse(df3$line=='KW3 3/8ข้าวกล่อง ห้องตักบรรจุ', 'KW3 3/8 rice utensils', df3$line2)
df3$line2 = ifelse(df3$line=='7/10 ฟรีสและบรรจุ KW1', '7/10 freeze contain KW1', df3$line2)
df3$line2 = ifelse(df3$line=='5/10 ขนมหวาน', '5/10 dessert', df3$line2)
df3$line2 = ifelse(df3$line=='KW3 6/8ปูอัด', 'KW3 6/8 crab stick', df3$line2)
df3$line2 = ifelse(df3$line=='KW3 4/8ฟรีสและบรรจุ กะกลางคืน KW3', 'KW3 4/8 freeze contain KW3', df3$line2)
df3$line2 = ifelse(df3$line=='KW3 5/8เต้าหู้ ลูกชิ้น Kamaboko', 'KW3 5/8 tofu kamaboko', df3$line2)
df3$line2 = ifelse(df3$line=='KW3 8/8เบ็ดเตล็ด repack RTC  +  RTE  +กิมจิ', 'KW3 8/8 repack RTC_RTE_kimchi', df3$line2)
df3$line2 = ifelse(df3$line=='6/10 ข้าวกล่อง Chill + Frozen KW1', '6/10 Rice Chill + Frozen KW1', df3$line2)
df3$line2 = ifelse(df3$line=='1/10 เตรียมวัตถุดิบ ผัก  เนื้อ  ส่วนผสม  KW1', '1/10 raw material KW1', df3$line2)
df3$line2 = ifelse(df3$line=='4/10 TV Dinner+ BBQ', '4/10 TV Dinner BBQ', df3$line2)
df3$line2 = ifelse(df3$line=='KW3 1/8ข้าวกล่อง ห้องเตรียมวัตถุดิบ ส่วนผสม', 'KW3 1/8 rice raw material ingredients', df3$line2)
df3$line2 = ifelse(df3$line=='10/10 เบ็ดเตล็ด K1', '10/10 misc K1', df3$line2)

# delete after rows After April 30th: 346 - 50,800
# non-use empty rows slow down calculations
# delete 346 -> 50,800
df3 <- df3[-c(346:50800),]



### STep 3
# create actual_wage_per_kg (formerly new_column), std_wage_per_kg (formerly new_column2)

# actual_wage_per_kg
# error, wrong data type, need to change factor to numeric
# WARNING: results in implicit coercion (cannot simply use as.numeric())
# Work-around: re-save file to df4 (prepare data shape to match df3)
df3$actual_wage <- as.numeric(levels(df4$actual_wage))[df4$actual_wage]
df3$std_wage <- as.numeric(levels(df4$std_wage))[df4$std_wage]


df3 <- df3 %>% mutate(actual_wage_per_kg = actual_wage / prod_total_kg)
df3 <- df3 %>% mutate(std_wage_per_kg = std_wage / prod_total_kg)

#### Step 4

kimchi <- df3 %>% 
    filter(line2=='8/10 kimchi') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: 8/10 Kimchi')


fruit <- df3 %>% 
    filter(line2=='9/10 fruit') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: 9/10 Fruit')

dessert <- df3 %>% 
    filter(line2=='5/10 dessert') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg'), size=5) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg'), size=5) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: 5/10 Dessert')

Raw_Mat <- df3 %>% 
    filter(line2=='1/10 raw material KW1') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: 1/10 Raw Material KW1')

crab_sticks <- df3 %>% 
    filter(line2=='KW3 6/8 crab stick') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: KW3 6/8 Crab Stick')

onikiri <- df3 %>% 
    filter(line2=='2/10 onikiri') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: 2/10 onikiri')


appetizer <- df3 %>% 
    filter(line2=='3/10 appetizer') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: 3/10 appetizer')


shrimp_squid <- df3 %>% 
    filter(line2=='KW3 7/8 shrimp squid') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: KW3 7/8 shrimp squid')


rice_seasoning <- df3 %>% 
    filter(line2=='KW3 2/8 rice seasoning') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: KW3 2/8 rice seasoning')

# patchwork
kimchi + fruit + dessert + Raw_Mat + crab_sticks + onikiri + appetizer + shrimp_squid + rice_seasoning + plot_layout(ncol = 3)



rice_utensils <- df3 %>% 
    filter(line2=='KW3 3/8 rice utensils') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: KW3 3/8 rice utensils')

freeze_contain <- df3 %>% 
    filter(line2=='7/10 freeze contain KW1') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: 7/10 freeze contain KW1')

tofu_kamaboko <- df3 %>% 
    filter(line2=='KW3 5/8 tofu kamaboko') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: KW3 5/8 tofu kamaboko')

repack_rtc <- df3 %>% 
    filter(line2=='KW3 8/8 repack RTC_RTE_kimchi') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: KW3 8/8 repack RTC_RTE_kimchi')

rice_chill <- df3 %>% 
    filter(line2=='6/10 Rice Chill + Frozen KW1') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: 6/10 Rice Chill + Frozen KW1')

tv_dinner <- df3 %>% 
    filter(line2=='4/10 TV Dinner BBQ') %>% 
    ggplot(aes(date, group = 1)) 
    + geom_line(aes(y=actual_wage_per_kg, colour = 'actual_wage_per_kg')) 
    + geom_line(aes(y=std_wage_per_kg, colour = 'std_wage_per_kg')) 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
    + labs(y = 'Actual Wages per KG Produced', title = 'Production Line: 4/10 TV Dinner BBQ')

# second patchwork
rice_utensils + freeze_contain + tofu_kamaboko + repack_rtc + rice_chill + tv_dinner + plot_layout(ncol = 3)

