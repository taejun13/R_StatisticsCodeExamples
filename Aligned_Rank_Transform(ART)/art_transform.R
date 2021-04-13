# 1. Load raw data

library(dplyr)
names = c("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12")
group = c("alphabet", "digit")
method = c("baseline","hetero")
pose = c("armFront","armBody")
mode = c("training","main")
block = c("1","2")
# 1. 1 Letter Accuracy [%]  

base_df = data.frame()
for (q in 2:2){
  for(p in 1:2){
    for(k in 1:2){
      for(j in 2:2){
        for (i in 1:12){
          file_name = paste("data/",names[i],"_",group[j],"_",method[k],"_",pose[p],"_", mode[q], ".csv",sep="")
          file_data = read.csv(file_name, header=T, stringsAsFactors = F)
          base_df = rbind(base_df,file_data)
        }  
      }  
    }  
  }  
}

base_df
base_df$id = factor(base_df$id, levels=names)
base_df$rt = base_df$enterstamp - base_df$playendstamp

for(i in 1:nrow(base_df)){
  if(base_df$blocknum[i] == "1")
    base_df$blocknum[i] = "3"
  else if (base_df$blocknum[i] == "2")
    base_df$blocknum[i] = "4"
}


base_df

result = group_by(base_df, id, group, strategy, armpose) %>%
  summarise(
    count = n(),
    correct = mean(correct)*100,
    rt = mean(rt)
  )
print(result,n=100)

result
result_df = as.data.frame(result)
result_df

result_df$count <- NULL
result_df$strategy = factor(result_df$strategy, levels=method)
result_df$armpose = factor(result_df$armpose, levels=pose)
result_df

str(result_df)

result_df

# ART Examples
install.packages("ARTool")
library(ARTool)

data(Higgins1990Table5)
str(Higgins1990Table5)

m <- art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data = Higgins1990Table5)
summary(m)

anova(m)

# My Codes
m <- art(correct ~ armpose*strategy + Error(id) , data = result_df)
anova(m)
summary(m)

# Post hoc pairwise comparison
library(emmeans)
emmeans(artlm(m, "cond"), pairwise ~ cond)

# Estimated Marginal Means(EMM) pairwise comparison with Tukey-adjustment

