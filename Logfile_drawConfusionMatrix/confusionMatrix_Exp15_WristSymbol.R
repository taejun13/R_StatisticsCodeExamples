library(ggcorrplot)
library(caret)
library(e1071)
library(readxl)
library(ggplot2)
library(dplyr)


# Names
names = c("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12")
armpose = c("armbody","armfront")
method = c("baseline","2color","4color")
mode = c("training","main")
block=c("1","2")

# 1. 1 Letter Accuracy [%]  
base_df = data.frame()
for(q in 1:2){
  for(p in 2:2){
    for (k in 1:1){
      for (j in 1:1){
        for (i in 1:12){
          file_name = paste("Exp15_data/",names[i],"_", armpose[j],"_", method[k],"_",mode[p],"_", block[q],".csv",sep="")
          file_data = read.csv(file_name, header=T, stringsAsFactors = F)
          base_df = rbind(base_df,file_data)
        }  
      }  
    }
  }    
}

base_df$id = factor(base_df$id, levels=names)
base_df

# Confusion matrix
actual_first <- as.factor(base_df$realPattern)
predicted_first <- as.factor(base_df$userAnswer)

actual_first_1 = strtoi(substr(actual_first,1,1))
actual_first_2 = strtoi(substr(actual_first,2,2))
actual_first_3 = strtoi(substr(actual_first,3,3))

predicted_first_1 = strtoi(substr(predicted_first,1,1))
predicted_first_2 = strtoi(substr(predicted_first,2,2))
predicted_first_3 = strtoi(substr(predicted_first,3,3))

#Only in ArmBody(Right) Condition
target = predicted_first_3
for (i in 1:length(target)){
  
  if(target[i] == 1)
    target[i] = 3
  else if (target[i] == 2)
    target[i] = 1
  else if (target[i] == 3)
    target[i] = 4
  else if (target[i] == 4)
    target[i] = 2
  
}
predicted_first_3 = target

str(target)

3


actual_first_real = as.factor(c(actual_first_1, actual_first_2, actual_first_3))
predicted_first_real = as.factor(c(predicted_first_1, predicted_first_2, predicted_first_3))



str(actual_first_real)
str(predicted_first_real)

length(actual_first_real)
length(predicted_first_real)


cm_first <- confusionMatrix(predicted_first_real, actual_first_real)
cm_temp = cm_first$table 

cm_df <- as.data.frame(as.matrix(cm_temp))
cm_df

ggplot(cm_df) +
  geom_tile(aes(x=Prediction, y=Reference, fill=Freq)) +
  coord_equal() +
  geom_text(aes(x=Prediction, y=Reference, label = sprintf("%1.0f", Freq)), vjust = 0.3, size= 7)+
  labs(title="", x="User Response", y="Actual Pattern", size =5) +
  scale_y_discrete(limits = rev(levels(cm_df$Reference))) +
  scale_fill_gradient(low="white", high="slategrey", limits=c(0,621)) +
  theme_bw() +
  theme(text=element_text(size=14, family="sans"),
        axis.text = element_text(size=20))

