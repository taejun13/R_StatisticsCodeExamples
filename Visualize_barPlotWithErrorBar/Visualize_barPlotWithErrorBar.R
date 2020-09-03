library(ggplot2)

base_df = data.frame("posture"= c("Forward","Right","Down"), "rf"= c("Reference Frame 1","Reference Frame 2"), "accuracy" = c(54.3, 45.1, 60.9, 58, 44.2, 57.8), "stderr"=c(6.899335717, 7.967433715, 7.188010851, 7.563288526, 7.188010851, 6.552925555))
base_df$posture = factor(base_df$posture, levels= c("Forward","Right","Down"))


# +  geom_text(aes(label = accuracy), 
#             size =6, 
#             color="white",
#             position = position_dodge(0.9), 
#             vjust=1.6)
p <- ggplot(base_df, aes(x=rf, y=accuracy, fill=posture)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), width=0.88) +
  geom_errorbar(aes(ymin=accuracy-stderr, ymax=accuracy+stderr), 
                color="black", 
                width=.2,
                size=0.5,
                position=position_dodge(.9)) 

p+labs(x="",y="Accuracy(%)", fill="Arm Posture") +
  theme(axis.text=element_text(size=11,family="sans"))


names(wf[wf=="TT Arial"])

windowsFonts()
font_import()

?labs

?element_text
