z1 = cor(eda,use = 'complete.obs')# %>% as.data.frame();
z1 = melt(z1)
ggplot(data = z1, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation\nbetween options") +
  labs(title = '各選擇間相關性分析', caption = '日期: 2020/03/01 ~ 2020/07/31',
       x = '', y = '') +
  theme(axis.text.x = element_text(angle = 80)) #heatmap
rm(z1)
#############

eda = a5[,c(7,60)]
eda$min = round(eda$MAX_TIMEDIF/60,digits = 0)
eda = eda %>% filter(min < 33) %>%
    group_by(min) %>%
    summarise(y = sum(Y), total = n(), per = y/total)

ag_elite %>% #filter(total_visit >= 100) %>%
  ggplot() + geom_point(aes(x = nichewidth, y = yy/cs)) +
  #geom_smooth(aes(x = nichewidth, y = yy/cs)) +
  geom_hline(yintercept = 0.102783, color = "red") +
  geom_histogram(aes(x = position_level, y = total_visit/300000), stat = "Identity", fill = "lightslateblue", alpha = 0.4) +
  #scale_fill_discrete(name = "是否使用\n\"3211分享圖片\"",
  #                      labels = c("N", "Y")) +
  #theme(legend.position = c(0.7,0.7),
  #      legend.title = element_text(size = 15), legend.text = element_text(size = 15)) +
  labs(title = "使用廣度與促約率相關性", 
       caption = "日期: 2020/03/01 ~ 2020/07/31\n取拜訪次數前20%", x = "使用廣度\nniche wdth", y= "促約率")

eda %>%
  ggplot() + geom_point(aes(x = min, y = per)) +
  geom_hline(yintercept = 0.102783, color = "red") +
  labs(title = "使用分鐘數與促約率相關性", x = "互動分鐘數", y= "促約率",
       caption = "日期:2020/03/01~2020/07/31\n篩選32分鐘以下(佔全體95%)")



eda = ag %>% filter(position_level != "XX") %>%
  group_by(position_level) %>%
  summarise(y = sum(yy), total = sum(cs), per = y/total)

eda2 = subset(a5[,c(32,66)], a5$STEP_3211_SHAREGRAGH == 1) %>% table %>% as.data.frame()
eda3 = subset(a5[,c(32,66)], a5$STEP_3211_SHAREGRAGH == 0) %>% table %>% as.data.frame()
eda2$STEP_3211_SHAREGRAGH_1 = NULL; eda3$STEP_3211_SHAREGRAGH_1 = NULL
names(eda2)[2] = "gr1";names(eda3)[2] = "gr0"
eda3 = merge(eda2, eda3, by = "MKT_DEPT_NAME")
eda3$per = round(eda3$gr1/(eda3$gr1 + eda3$gr0),2)

eda4 = a5[,c(7,32,60:76)]
eda4$total_step = eda4$STEP_3211_1 + eda4$STEP_PIPI_1 + eda4$STEP_RECOMMEND_1 + eda4$RE_RECOMMEND_1 +
  eda4$STEP_3211_SHAREGRAGH_1 + eda4$STEP_3211_JIANJIAN_1 + eda4$STEP_LLB_1 + eda4$STEP_DETAIL_A_1 +
  eda4$STEP_DETAIL_B_1 + eda4$STEP_DETAIL_C_1 + eda4$STEP_DETAIL_D_1 + eda4$STEP_DETAIL_E_1 + 
  eda4$STEP_DETAIL_F_1 + eda4$STEP_DETAIL_G_1 + eda4$STEP_N_REPORT_1


eda = eda4 %>%
  group_by(MKT_DEPT_NAME) %>%
  summarise(med = median(total_step), avg = mean(total_step))
eda3 = a5 %>% 
  group_by(MKT_DEPT_NAME) %>%
  summarise(yy = sum(Y), n = n(), per = (yy/n))
eda2 = merge(eda, eda3[,c(1,4)], by = "MKT_DEPT_NAME")




rm(eda, eda2, eda3, eda4)





