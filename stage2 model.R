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
library(parallel)

##### raw data
a1 = fread("visit13.csv", stringsAsFactors = F)
a2 = fread("visit23.csv", stringsAsFactors = F)

save(a1, a2, a5, file = "stage2.Rdata")

names(a1)[c(5:7)] = c("MEAN_DATEDIF", "MIN_DATE", "MAX_TIMEDIF")
a3 = rbind(a1,a2)

##### 
###clean
# date
Sys.setlocale("LC_TIME", "C")
a1$MIN_DATE = as.Date(a1$MIN_DATE, format = "%d%b%Y")
a2$MIN_DATE = as.Date(a2$MIN_DATE, format = "%d%b%Y")

#NAs
a3$GENDER[a3$GENDER == ""] = NA
a3$CONTACT_CITY_CD[a3$CONTACT_CITY_CD == ""] = NA
a3$LACK_A[a3$LACK_A == ""] = NA
a3$KIDS_C[a3$KIDS_C == ""] = NA; a3$KIDS_B[a3$KIDS_B == ""] = NA; a3$KIDS_A[a3$KIDS_A == ""] = NA
a3$ECONOMY_STATUS[a3$ECONOMY_STATUS == ""] = NA
a3$HOUSE_ECONOMY_STATUS[a3$HOUSE_ECONOMY_STATUS == ""] = NA
a3$MARRIAGE[a3$MARRIAGE == ""] = NA

#wk month for 2020
a3$WK2020 = NA

a3$WK2020[a3$MIN_DATE <= as.Date("2019-12-25")] = 13
a3$WK2020[a3$MIN_DATE >= as.Date("2019-12-26") & a3$MIN_DATE <= as.Date("2020-01-22")] = 1
a3$WK2020[a3$MIN_DATE >= as.Date("2020-01-23") & a3$MIN_DATE <= as.Date("2020-02-26")] = 2
a3$WK2020[a3$MIN_DATE >= as.Date("2020-02-27") & a3$MIN_DATE <= as.Date("2020-03-25")] = 3
a3$WK2020[a3$MIN_DATE >= as.Date("2020-03-26") & a3$MIN_DATE <= as.Date("2020-04-22")] = 4
a3$WK2020[a3$MIN_DATE >= as.Date("2020-04-23") & a3$MIN_DATE <= as.Date("2020-05-20")] = 5
a3$WK2020[a3$MIN_DATE >= as.Date("2020-05-21") & a3$MIN_DATE <= as.Date("2020-06-17")] = 6
a3$WK2020[a3$MIN_DATE >= as.Date("2020-06-18") & a3$MIN_DATE <= as.Date("2020-07-15")] = 7
a3$WK2020[a3$MIN_DATE >= as.Date("2020-07-16") & a3$MIN_DATE <= as.Date("2020-08-12")] = 8

#position_level
a3$position_level = NA

a3$position_level[a3$WK_MONTH <= 2] = "N"
a3$position_level[a3$POSITION_NAME %in% c("行銷總監（Ａ）", "行銷總監（Ｂ）",
                                          "業務經理（一）", "業務經理（二）", "業務經理(二)", 
                                          "業務副理", "業務襄理（一）", "業務襄理(一)") &
                    a3$WK_MONTH > 2] = "H"
a3$position_level[a3$POSITION_NAME %in% c("業務襄理（二）", "業務襄理(二)", "業務主任（一）",
                                          "業務主任(一)", "業務主任（二）", "業務主任(二)",
                                          "業務主任 (三)", "儲備壽險主任", "壽險主任",
                                          "行銷主任（Ｔ）", "行銷主任（Ｕ）", "行銷主任（Ｐ）",
                                          "行銷主任（Ｒ）") &
                    a3$WK_MONTH > 2] = "L"
a3$position_level[a3$POSITION_NAME %in% c("處長（二）", "展業區經理（二）", "通訊處經理",
                                          "推展處襄理（二）", "推展處經理（一）", "展業區襄理（二）",
                                          "區主任（一）", "推展處副理", "區主任（二）", 
                                          "推展處襄理（一）", "展業區副理", "展業區襄理（一）",
                                          "推展處長（一）", "展業區區主任（一）", "處長（一）",         
                                          "課長", "推展處經理（二）", "推展處總監", "展業區經理（一）",
                                          "協理", "展業區總監") &
                    a3$WK_MONTH > 2] = "S"
a3$position_level[a3$POSITION_NAME %in% c("區部專員", "組訓專員（Ａ）",
                                          "組訓專員（Ｂ）", "組訓專員（Ｃ）") &
                    a3$WK_MONTH > 2] = "XX"
#edu & wk
a3$EDUCATION_CD = as.character(a3$EDUCATION_CD)
a3$WK2020 = as.character(a3$WK2020)

#Y
a3$Y[is.na(a3$MIN_INTERVALDAY)] = 0
a3$Y[!is.na(a3$MIN_INTERVALDAY)] = 1

######
##### ag
ag = a5 %>% 
  group_by(AGIDNO_SAS) %>%
  summarise(cs = n(),total_visit = sum(VISIT), yy = sum(Y),
            step3211 = sum(STEP_3211_1), steppipi = sum(STEP_PIPI_1),
            steprecommend = sum(STEP_RECOMMEND_1), steprerecommend = sum(RE_RECOMMEND_1), 
            stepsharegragh = sum(STEP_3211_SHAREGRAGH_1), stepjianjian = sum(STEP_3211_JIANJIAN_1),
            stepdetaila = sum(STEP_DETAIL_A_1), stepdetailb = sum(STEP_DETAIL_B_1), 
            stepdetailc = sum(STEP_DETAIL_C_1), stepdetaild = sum(STEP_DETAIL_D_1),
            stepdetaile = sum(STEP_DETAIL_E_1), stepdetailf = sum(STEP_DETAIL_F_1),
            stepdetailg = sum(STEP_DETAIL_G_1), stepnrepot = sum(STEP_N_REPORT_1))
ag = left_join(ag, a5[,c(2,32,34:35,59)] %>% unique, by = "AGIDNO_SAS")
ag$niche = ag$step3211 + ag$steppipi + ag$steprecommend + ag$steprerecommend + 
  ag$stepsharegragh+ ag$stepjianjian +ag$stepdetaila + ag$stepdetailb + 
  ag$stepdetailc + ag$stepdetaild + ag$stepdetaile + ag$stepdetailf + 
  ag$stepdetailg + ag$stepnrepot
ag$nichewidth = 1 - ((ag$step3211/ag$niche)^2 + (ag$steppipi/ag$niche)^2 +
            (ag$steprecommend/ag$niche)^2 + (ag$steprerecommend/ag$niche)^2 + 
            (ag$stepsharegragh/ag$niche)^2+ (ag$stepjianjian/ag$niche)^2 + 
            (ag$stepdetaila/ag$niche)^2 + (ag$stepdetailb/ag$niche)^2 +
            (ag$stepdetailc/ag$niche)^2 + (ag$stepdetaild/ag$niche)^2 +
            (ag$stepdetaile/ag$niche)^2 + (ag$stepdetailf/ag$niche)^2 +
            (ag$stepdetailg/ag$niche)^2 + (ag$stepnrepot/ag$niche)^2)
ag$nichewidth4 = ag$nichewidth^4
ag$total_visit = log(ag$total_visit)

ag_elite = ag[ag$total_visit > 18,]
##### 
###bind
ag1 = ag %>% mutate_if(is.character, as.factor) %>% as.data.table()
ag1 = one_hot(ag1[,c(2:23,25)])

z1 = cor(ag1,use = 'complete.obs');
z1 = melt(z1)
ggplot(data = z1, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation\nbetween options") +
  labs(title = '各選擇間相關性分析', caption = '日期: 2019/12/19 ~ 今天',
       x = '', y = '') +
  theme(axis.text.x = element_text(angle = 80)) #heatmap

a1 %>% filter(TIMEDIF <= 270) %>%
  ggplot() + geom_point(aes(x = STEP_3211_T, y = STEP_PIPI_T))

a1 %>% filter(TIMEDIF<5000) %>%
  ggplot() + geom_histogram(aes(x = TIMEDIF))

quantile(a1$STEP_3211_T, probs = c(0.1, 0.9), na.rm = T)

rm(ag1, z1)


#####
##### fill nas & eda
####a4: old customers
a4 = subset(a3, is.na(a3$GENDER) & is.na(a3$SLAGE) & is.na(a3$CONTACT_CITY_CD) & is.na(a3$EDUCATION_CD))
a4 = a4[,c(1:27,33:35,49:60)]


####a5: new customers
a5 = a3[!(is.na(a3$GENDER)& is.na(a3$SLAGE) & is.na(a3$CONTACT_CITY_CD) & is.na(a3$EDUCATION_CD)),]

###a5 feature engineering
# visit > 1 or not
a5$visit_1[a5$VISIT == 1] = 0; a5$visit_1[a5$VISIT > 1] = 1
# max t to log
a5$log_max_t = log(a5$MAX_TIMEDIF)
# step to one hot 
a5$STEP_3211_1[a5$STEP_3211 == 0] = 0;a5$STEP_3211_1[a5$STEP_3211 > 0] = 1
a5$STEP_PIPI_1[a5$STEP_PIPI == 0] = 0;a5$STEP_PIPI_1[a5$STEP_PIPI > 0] = 1
a5$STEP_RECOMMEND_1[a5$STEP_RECOMMEND == 0] = 0;a5$STEP_RECOMMEND_1[a5$STEP_RECOMMEND > 0] = 1
a5$RE_RECOMMEND_1[a5$RE_RECOMMEND == 0] = 0;a5$RE_RECOMMEND_1[a5$RE_RECOMMEND > 0] = 1
a5$STEP_3211_SHAREGRAGH_1[a5$STEP_3211_SHAREGRAGH == 0] = 0;a5$STEP_3211_SHAREGRAGH_1[a5$STEP_3211_SHAREGRAGH > 0] = 1
a5$STEP_3211_JIANJIAN_1[a5$STEP_3211_JIANJIAN == 0] = 0;a5$STEP_3211_JIANJIAN_1[a5$STEP_3211_JIANJIAN >0] =1
a5$STEP_LLB_1[a5$STEP_LLB == 0] = 0;a5$STEP_LLB_1[a5$STEP_LLB > 0] = 1
a5$STEP_DETAIL_A_1[a5$STEP_DETAIL_A == 0] = 0;a5$STEP_DETAIL_A_1[a5$STEP_DETAIL_A > 0] = 1
a5$STEP_N_REPORT_1[a5$STEP_N_REPORT == 0] = 0;a5$STEP_N_REPORT_1[a5$STEP_N_REPORT > 0] = 1

a5$KIDS_A[a5$KIDS_A == "新手爸媽"] = 1; a5$KIDS_B[a5$KIDS_B == "子女未成年"] = 1; a5$KIDS_C[a5$KIDS_C == "子女成年"] = 1
a5$KIDS_A[is.na(a5$KIDS_A)] = 0; a5$KIDS_B[is.na(a5$KIDS_B)] = 0; a5$KIDS_C[is.na(a5$KIDS_C)] = 0
a5$KIDS_A = as.numeric(a5$KIDS_A); a5$KIDS_B = as.numeric(a5$KIDS_B); a5$KIDS_C = as.numeric(a5$KIDS_C)
a5$KIDS_D[a5$KIDS_A == 0 & a5$KIDS_B == 0 & a5$KIDS_C == 0] = 1 #no kids

a5$niche = a5$STEP_3211_1 + a5$STEP_PIPI_1 + a5$STEP_RECOMMEND_1 + a5$RE_RECOMMEND_1 +
  a5$STEP_3211_SHAREGRAGH_1 + a5$STEP_3211_JIANJIAN_1 + a5$STEP_DETAIL_A_1 +
  a5$STEP_DETAIL_B_1 + a5$STEP_DETAIL_C_1 + a5$STEP_DETAIL_D_1 + a5$STEP_DETAIL_E_1 +
  a5$STEP_DETAIL_F_1 + a5$STEP_DETAIL_G_1 + a5$STEP_N_REPORT_1

a5$nichewidth = 1 - ( (a5$STEP_3211_1/a5$niche)^2 + (a5$STEP_PIPI_1/a5$niche)^2 +
  (a5$STEP_RECOMMEND_1/a5$niche)^2 + (a5$RE_RECOMMEND_1/a5$niche)^2 +
  (a5$STEP_3211_SHAREGRAGH_1/a5$niche)^2 + (a5$STEP_3211_JIANJIAN_1/a5$niche)^2 +
  (a5$STEP_DETAIL_A_1/a5$niche)^2 + (a5$STEP_DETAIL_B_1/a5$niche)^2 + 
  (a5$STEP_DETAIL_C_1/a5$niche)^2 + (a5$STEP_DETAIL_D_1/a5$niche)^2 +
  (a5$STEP_DETAIL_E_1/a5$niche)^2 + (a5$STEP_DETAIL_F_1/a5$niche)^2 +
  (a5$STEP_DETAIL_G_1/a5$niche)^2 + (a5$STEP_N_REPORT_1/a5$niche)^2)

a5$nichewidth = a5$nichewidth ^ 3

#####remove niche = 0
a5 = subset(a5, a5$niche != 0)

#fill nas
a5_forest = a5[,c(7,28:48,58:59)]

a5_forest = a5_forest %>% mutate_if(is.character, as.factor)%>% as.data.table()
t0 = Sys.time()
mod_forest = missForest(a5_forest ,maxiter = 1, ntree = 3)
Sys.time() - t0
a5_forest = mod_forest$ximp

a6 = a5
a6[,c(7,28:48,58:59)] = a5_forest
# new df
a51 = a6[,c(26:29,31:32,34,36:48,61:79,59,60)] #,26:29,31:32,34,36:47,78,48,58:60

a51 = a51 %>% mutate_if(is.character, as.factor) %>% as.data.table()

a52 = one_hot(a51, naCols = T)
#c2<-sapply(a52[,21:101],function(x){replace(x, is.na(x), 0)}) %>% as.data.frame()
#a52[,21:101] = c2[,1:81]; rm(c2)

#c2<-sapply(a52[,c(4:5,8:11,33:83,88:92)],function(x){replace(x, is.na(x), 0)}) %>% as.data.frame()
#a52[,c(4:5,8:11,33:83,88:92)] = c2[,1:62]; rm(c2)

#####
set.seed(8787)
spl = sample.split(a52$Y, SplitRatio = 0.97)
TR = subset(a52, spl); TS = subset(a52, !spl)

a52.TR = xgb.DMatrix(data = as.matrix(TR[,-80]), label = TR$Y)
a52.TS = xgb.DMatrix(data = as.matrix(TS[,-80]), label = TS$Y)

cv = xgb.cv(data = a52.TR, nfold = 10, nrounds = 100, early_stopping_rounds = 5)
mod1 = xgb.train(a52.TR, label = TR$Y, nround = 45, 
                 params=list("objective" = "binary:logistic",  "eval_metric" = "mlogloss"))
pred = predict(mod1, a52.TS)
mx = table(TS$Y, pred > 0.5); mx;sum(diag(mx)) / sum(mx) #acc
colAUC(pred, TS$Y)
rm(pred, mx)

TR2 = as.matrix(TR[,-80]); TS2 = as.matrix(TS[,-80])
shap_values = shap.values(xgb_model = mod1, X_train = TS2)
shap.plot.summary.wrap2(shap_values$shap_score, TS2, top_n = 15)

shap_long = shap.prep(xgb_model = mod1, X_train = TR2)
shap.plot.dependence(data_long = shap_long, x="KIDS_C",
                     color_feature = "ECONOMY_STATUS_經濟能力高")
rm(TR2,TS2,c3.TR,c3.TS,shap_values,shap_long)

imp = xgb.importance(model = mod1)
xgb.plot.importance(importance_matrix = imp[1:20]);rm(imp)



#####
##### avg_intervalday






