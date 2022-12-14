library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(caTools)
library(xgboost)
library(SHAPforxgboost)
library(randomForest)
library(party)
library(mltools)
library(missForest)

#####
##########raw data: both
both = fread("both_33.csv", stringsAsFactors = F)
both$RE_RECOMMEND = NULL

#fill nas with 0
both[,c(5, 8:9, 24:28)][is.na(both[,c(5, 8:9, 24:28)])] = 0;summary(both[,c(5, 8:9, 24:28)])

#fill treebel col with NA
both$LACK_A[both$LACK_A == ""] = NA; both$LACK_B[both$LACK_B == ""] = NA; both$LACK_C[both$LACK_C == ""] = NA
both$LACK_D[both$LACK_D == ""] = NA; both$LACK_E[both$LACK_E == ""] = NA; 
both$LACK_F[both$LACK_F == ""] = NA; both$LACK_G[both$LACK_G == ""] = NA
both$ECONOMY_STATUS[both$ECONOMY_STATUS == ""] = NA; both$KIDS_A[both$KIDS_A == ""] = NA
both$KIDS_B[both$KIDS_B == ""] = NA;both$KIDS_C[both$KIDS_C == ""] = NA; 
both$MARRIAGE[both$MARRIAGE == ""] = NA; both$HOUSE_ECONOMY_STATUS[both$HOUSE_ECONOMY_STATUS == ""] = NA;

#fix interval days: remove <0 & fix >90 with new df
both$Y[both$INTERVAL_DAY <0] = 0; both$Y[both$NEW_INTERVAL_DAY<0] = 0

i = 1
for(i in 1:nrow(both)){
  if(both$INTERVAL_DAY[i]<=90 & !is.na(both$INTERVAL_DAY[i])) {both$NEWNEW_INTERVAL_DAY[i] = both$INTERVAL_DAY[i]}
  if(both$INTERVAL_DAY[i]>90& !is.na(both$INTERVAL_DAY[i])) {both$NEWNEW_INTERVAL_DAY[i] = both$NEW_INTERVAL_DAY[i]}
  if(is.na(both$INTERVAL_DAY[i])) {both$NEWNEW_INTERVAL_DAY[i] = NA}
  print(i)
}
summary(both$NEWNEW_INTERVAL_DAY) 

both$NEWNEW_INTERVAL_DAY[both$Y == 0] = NA
both$INTERVAL_DAY = NULL; both$NEW_INTERVAL_DAY = NULL; 
both = both[,c(1:10,49,11:48)]; names(both)[11] = "INTERVAL_DAY" #save point


#both$NEWNEW_INTERVAL_DAY1<-ifelse(both$NEWNEW_INTERVAL_DAY>90,na.rm = T,1,both$NEWNEW_INTERVAL_DAY)


#turn time col from sec into min
both$time_min_max = round(both$TIMEDIF_MAX/60, digits = 0)
both$time_min_med = round(both$TIMEDIF_MED/60, digits = 0)

#rm outlier: VISIT, TIMEDIF
quantile(both$VISIT, prob = 0.999) #43
both = subset(both, both$VISIT <=43)

quantile(both$TIMEDIF, prob = 0.99)

#fill nas with mice


#EDA of visit
visit_per = both[,c(5, 7:9,11,31)]
visit_per$VISIT_BOX_per = visit_per$VISIT_BOX/visit_per$VISIT
visit_per$VISIT_BOX_OLD_per = visit_per$VISIT_BOX_OLD/visit_per$VISIT
visit_per$VISIT_PRINT_per = visit_per$VISIT_PRINT/visit_per$VISIT
subset(visit_per, visit_per$VISIT_BOX_per<=1& visit_per$VISIT_BOX_OLD_per <=1& visit_per$VISIT_PRINT_per <=1) %>%
  filter(VISIT>1) %>%
  ggplot() + 
  geom_histogram(aes(x = VISIT_PRINT_per), fill = "lightslateblue") + 
  labs(x = "BBOX visit proportion", caption = "total VISIT >1")

visit_per$unit[visit_per$UNIT_NO %in% c("HE4","S91","HI4")] = "good";visit_per$unit[visit_per$UNIT_NO %in% c("HA4","AL4","FK4")] = "hmmm"
visit_per %>% filter(!is.na(unit) & VISIT_BOX_per<=1) %>%
  ggplot() + geom_boxplot(aes(x = unit, y = VISIT_BOX_per))


##########raw data: box
box = fread("box_24.csv", stringsAsFactors = F)
box$RE_RECOMMEND = NULL
box[,c(5, 19:23)][is.na(box[,c(5, 19:23)])] = 0;summary(box[,c(5, 19:23)])

#if Y = 0, then INTERVAL_DAY = NA
box$INTERVAL_DAY[both$Y == 0] = NA

#fix interval days: remove <0 & fix >90 with new df
box = subset(box, box$INTERVAL_DAY >=0|is.na(box$INTERVAL_DAY))

#rm outlier: VISIT, TIMEDIF
quantile(box$VISIT, prob = 0.999) #does not remove

###############################################################################
##########clustering
library(flexclust)
both2 = both[both$UNIT_NO%in%c("HE4","S91","HI4","HA4","AL4","FK4"),]
both2$min = round(both2$TIMEDIF/60, digits = 0)
spl = sample.split(both2$Y, SplitRatio = 0.25); both2 = subset(both2, spl)
#spl = sample.split(both$Y, SplitRatio = 0.1); both2 = subset(both, spl)
hc = both2[,c(5,7,11,49)] %>% scale %>% dist %>% hclust
both2$group = cutree(hc, k=5) %>% factor

both2$unit[both2$UNIT_NO %in% c("HE4","S91","HI4")] = "good";both2$unit[both2$UNIT_NO %in% c("HA4","AL4","FK4")] = "hmmm"
both2 %>% filter(min<50 & UNIT_NO %in% c("TD4", "AX4", "FE4", "NB1", "RI4", "GF4", "A11")) %>%
  ggplot(aes(x = min, y = VISIT, col = group)) + geom_point(size = 2, alpha = 0.5)


##########xgb
both2 = both[,c(2,5:8,15:29,31:32,34:49,51)]
names(ag)[1] = "AGIDNO"
both2 = left_join(both2, ag, by = "AGIDNO")
both2 = left_join(both2, ques, by = "UNIT_NO")

#both2 = both[,c(5:8,15:29,31:32,34:49,51)]
both2 = both2[both2$VISIT_BOX_OLD <= 24,]
both2[,c(1,4,22,39)] = NULL

both2 = both2 %>% mutate_if(is.character, as.factor) %>% as.data.table()
#both2$UNIT_NO = NULL
both2$EDUCATION_CD = as.factor(both2$EDUCATION_CD)
aa = missForest(both2[,c(18:20)], maxiter = 10, ntree = 100)
aa = aa$ximp; both2$EDUCATION_CD = aa$EDUCATION_CD; rm(aa)

set.seed(5271)
spl = sample.split(both2$Y, SplitRatio = 0.7)
TR = subset(both2, spl); TS = subset(both2,!spl)
TR = TR %>% mutate_if(is.character, as.factor) %>% as.data.table()
TS = TS %>% mutate_if(is.character, as.factor) %>% as.data.table()
TR = one_hot(TR, naCols = T); TS = one_hot(TS, naCols = T) #save point

TR.1 = xgb.DMatrix(data = as.matrix(TR[,-1]), label = TR$Y)
TS.1 = xgb.DMatrix(data = as.matrix(TS[,-1]), label = TS$Y)

cv = xgb.cv(data = TR.1, nfold = 10, nrounds = 100, early_stopping_rounds = 5)
mod = xgb.train(TR.1, label = TR$Y, nround = 47, 
                params=list("objective" = "binary:logistic",  "eval_metric" = "mlogloss"))
pred = predict(mod, TS.1)

mx = table(TS$Y, pred > 0.5); mx;sum(diag(mx)) / sum(mx) #acc
colAUC(pred, TS$Y)
rm(pred, mx)

TR2 = as.matrix(TR[,-1]); TS2 = as.matrix(TS[,-1])
shap_values = shap.values(xgb_model = mod, X_train = TS2)

png(file = "shap_ques2.png")
print(shap.plot.summary.wrap2(shap_values$shap_score, TS2, top_n = 15))
dev.off()

shap_long = shap.prep(xgb_model = mod, X_train = TR2)
shap.plot.dependence(data_long = shap_long, x="ques_sum",
                     color_feature = "time_dif")
rm(TR2,TS2,c3.TR,c3.TS,shap_values,shap_long)
###############################################################################
#clustering by 2 dimensions: customers percentage
unit_per = fread("D:/box/淡水經理會議/df7.csv", stringsAsFactors = F)
tt = unit_per %>% group_by(UNIT_NO) %>%
  summarise(total = sum(SLID)) %>% as.data.frame()
tt2 = unit_per %>% filter(SOURCE == "BOX") %>% select(UNIT_NO, SLID)
unit_per = left_join(tt, tt2, by = "UNIT_NO"); rm(tt,tt2)
unit_per$per = round(unit_per$SLID/ unit_per$total, digits = 2)
unit_per$rank[unit_per$per > 0.06] = "H";unit_per$rank[unit_per$per <= 0.06] = "L"
unit_per$rank[unit_per$total < 8000] = "L"

# add unit rank into both2
both3 = left_join(both2, unit_per[,c(1,5)], by = "UNIT_NO")
both31 = both3[both3$rank == "H",];both32 = both3[both3$rank == "L",]



###############################################################################
#calculate usage time
#single click time dif
time = fread("TIME_CAL.csv", stringsAsFactors = F)
Sys.setlocale("LC_TIME", "C")

time$tt = substr(time$LOG_TIME, 11,18); time$date = substr(time$LOG_TIME, 1,9)
time$date = as.Date(time$date, format = "%d%b%Y")
time$datetime = as.POSIXct(paste(time$date, time$tt), format = "%Y-%m-%d %H:%M:%S")
time$tt = NULL;time$date = NULL;time$LOG_TIME = NULL
time$t2 = lag(time$datetime)
time$dif = time$datetime - time$t2
time$dif = as.numeric(time$dif)

time$new_key = paste(time$AGIDNO, time$SELECT_ID, sep = "");time$x1 = lag(time$new_key)
time$x2 = lag(time$DATE);time$x3 = time$new_key == time$x1
time$x4 = time$DATE == time$x2;time$x5[time$x3 == F| time$x4 == F] = 1
time$dif[time$x5 == 1] = NA;time$x6 = NA


time$x6 = as.numeric(time$x6)

id = time$AGIDNO %>% unique()
spl = id[sample.split(id, SplitRatio = 0.05)]

time2 = time[time$AGIDNO %in% spl,]
t0 = Sys.time()
i = 1
k = 0
for(i in 1:nrow(time2)){
  #k = 0
  if(!is.na(time2[i,7])){time2[i,14] = k}
  if(is.na(time2[i,7])){time2[i,14] = k+1}
  if(is.na(time2[i,7])){k = k+1}
  
}
Sys.time() - t0

#whole usage time dif
tt = time2 %>% group_by(x6) %>% summarise(total = sum(dif, na.rm = T))
tt$total = as.numeric(tt$total)
quantile(tt$total, probs = c(0.8,0.9,0.95,0.96,0.97,0.98,0.99))

time2 %>% filter(FUNCTION_NAME == "進入互動首頁" & dif<10000) %>% 
  ggplot() + geom_histogram(aes(x = dif))  
  geom_vline(xintercept = c(2227.4,3616,7834.6), col = "red")

#new usage def
time$dif[time$FUNCTION_NAME %in% c("進入互動首頁", "互動首頁") & time$dif > 8282] = NA
time$dif[time$dif > 30000] = NA

rm(time, time2)
# time of usage by ag
ag_time = fread("AG_TIME.csv", stringsAsFactors = F)
names(ag_time)[1] = "EMPLOYEE_ID_SAS"
ag_time$time_dif = ag_time$DIF/ag_time$TIMES
ag = left_join(sale, ag_time, by = "EMPLOYEE_ID_SAS")
rm(ag_time)

ag$TIMES[is.na(ag$TIMES)] = 0; ag$DATE[is.na(ag$DATE)] = 0


ag_cor = ag[,7:16]
ag_cor$box_per[is.na(ag_cor$box_per)] = 0; ag_cor$time_dif[is.na(ag_cor$time_dif)] = 0; ag_cor$SLID[is.na(ag_cor$SLID)] = 0
z1 = cor(ag_cor,use = 'complete.obs')
z1[lower.tri(z1)] = NA
z1 = melt(z1)
z1 = z1[!is.na(z1$value),]
z1$value = round(z1$value, digits = 2)

ggplot(data = z1, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation\nbetween options") +
  theme_minimal()+ 
  #labs(title = '各選擇間相關性分析', caption = '日期: 2019/12/19 ~ 今天',
  #     x = '', y = '') +
  theme(axis.text.x = element_text(angle = 80)) +
  coord_fixed()
rm(ag_cor, z1)

ag_unit = ag %>% group_by(UNIT_NO) %>%
  summarise(ag = n(),
            POLICY = mean(POLICY, na.rm = T),
            POLICY1 = mean(POLICY1, na.rm = T),
            TIMES = mean(TIMES, na.rm = T),
            SLID = mean(SLID, na.rm = T),
            DATE = mean(DATE, na.rm = T),
            time_dif = mean(time_dif, na.rm = T))
ag_unit = left_join(ag_unit, unit[,c(1,19)], by = "UNIT_NO")
###########
#ag questionarre
ques = fread("D:/box/排排看分析/stage3/202010月問卷/ques.csv", stringsAsFactors = F)
ques = ques[!ques$UNIT_NO == ""]
#ques = ques[!ques$UNIT_NO %in% c("未作答", "都說了匿名還要單位代碼", "無", "乍31", "", "0"),]
ques$UNIT_NO = toupper(ques$UNIT_NO); ques$UNIT_NO = substr(ques$UNIT_NO, 1, 3)
unit_no = unit$UNIT_NO %>% unique
ques = ques[ques$UNIT_NO %in% unit_no]
rm(unit_no)

q2 = ques[,7:11] %>% mutate_if(is.character, as.numeric) %>% as.data.table()
ques = cbind(ques[,1:6], q2, ques[,12]); rm(q2)

ques$edu_freq1[ques$edu_freq == "單位很少甚至沒有教/不固定教學"] = 0
ques$edu_freq1[ques$edu_freq == "超過一個月"] = 1
ques$edu_freq1[ques$edu_freq == "每月一次"] = 2
ques$edu_freq1[ques$edu_freq == "二週一次"] = 3
ques$edu_freq1[ques$edu_freq == "每週一次"] = 4

ques$edu_mood1[ques$edu_mood == "本單位未教學"] = 0
ques$edu_mood1[ques$edu_mood == "非常不滿意"] = 1
ques$edu_mood1[ques$edu_mood == "有些不滿意"] = 2
ques$edu_mood1[ques$edu_mood == "沒意見"] = 3
ques$edu_mood1[ques$edu_mood == "還算滿意"] = 4
ques$edu_mood1[ques$edu_mood == "非常滿意"] = 5

ques$edu_course1[ques$edu_course == "本單位未教學"] = 0
ques$edu_course1[ques$edu_course == "非常不滿意"] = 1
ques$edu_course1[ques$edu_course == "有些不滿意"] = 2
ques$edu_course1[ques$edu_course == "沒意見"] = 3
ques$edu_course1[ques$edu_course == "還算滿意"] = 4
ques$edu_course1[ques$edu_course == "非常滿意"] = 5

#calculating modes
modes <- function(v) {
       uniqv <- unique(v)
       uniqv[which.max(tabulate(match(v, uniqv)))]
}

unit_ques = ques %>% group_by(UNIT_NO) %>%
  summarise(edu_freq = mean(edu_freq1),
            edu_mood = mean(edu_mood1),
            edu_course = mean(edu_course1),
            fam_1 = mean(fam_1),
            fam_2 = mean(fam_2),
            fam_3 = mean(fam_3),
            fam_4 = mean(fam_4),
            fam_all = mean(fam_all))
#unit performance
unit_sale = fread("D:/box/淡水經理會議/df2.csv", stringsAsFactors = F)
unit_sv = fread("D:/box/淡水經理會議/df6.csv", stringsAsFactors = F)
unit_contact = fread("D:/box/淡水經理會議/df7.csv", stringsAsFactors = F)

unit_contact = unit_contact[unit_contact$SOURCE == "BOX"]
unit_contact = left_join(unit_contact, unit_sv, by = "UNIT_NO");rm(unit_sv)
unit_contact$SOURCE = NULL
unit_contact$contact_per = unit_contact$SLID/ unit_contact$TOTAL_SV_SLID

unit_sale = left_join(unit_sale, unit_contact[,c(1,4)], by = "UNIT_NO")

unit = left_join(unit_sale, unit_ques, by = "UNIT_NO")
unit$ques_sum = rowSums(unit[,11:18])
unit$ques_nor = normalize(unit$ques_sum)

rm(unit_contact, unit_ques, unit_sale)
#ag -> unit
ag_unit = ag %>% group_by(UNIT_NO) %>%
  summarise(time = median(time_dif, na.rm = T), sale = sum(POLICY, na.rm = T), ag = n())
ag_unit$avg_sale = ag_unit$sale/ag_unit$ag

both2 = fread("both2.csv", stringsAsFactors = F)
both2$niche = rowSums(both2[,4:11])

ag2 = ag[,c(3, 5, 7:8, 14:18)]
ag2 = left_join(ag2, unit[,c(1,19)], by = "UNIT_NO"); ag2$UNIT_NO = NULL
ag2 = ag2[!ag$TIMES == 0,]
mod = lm(POLICY1~., data = ag2); summary(mod)

#whole 2020 of each unit
unit_2020 = left_join(unit_2020, ag_unit[,c(1,9)], by = "UNIT_NO")
# plot
ggplot(unit_2020) + geom_point(aes(x = SLID, y = POLICY1, color = ques_sum)) + 
  #scale_size_continuous(range = c(1, 3)) +
  #scale_color_gradient(low="blue", mid="white", high="red") +
  scale_color_gradientn(name = "單位推動", colours = rainbow(5)) + 
  labs(x = "總互動人數", y = "促約件數", caption = "2020") 
  
