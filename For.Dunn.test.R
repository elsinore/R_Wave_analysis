G0.1<-x[, 2][x$Label == 0][x$Group[x$Label == 0]==1]
G0.2<-x[, 2][x$Label == 0][x$Group[x$Label == 0]==2]
G1.1<-x[, 2][x$Label == 1][x$Group[x$Label == 1]==1]
G1.2<-x[, 2][x$Label == 1][x$Group[x$Label == 1]==2]
DG0.1<-data.frame(value = G0.1, Group = "G0.1")
DG0.2<-data.frame(value = G0.2, Group = "G0.2")
DG1.1<-data.frame(value = G1.1, Group = "G1.1")
DG1.2<-data.frame(value = G1.2, Group = "G1.2")
data<-rbind(DG0.1, DG0.2, DG1.1, DG1.2)
p.value <- kruskal.test(data$value, data$Group)$p.value
ggplot(x, aes(x=x$Tag, y=x[, 2], fill = x$Group == 1)) +
  geom_boxplot(position="dodge") +
  geom_signif(annotation=formatC(anno1, digits=2),
              y_position=max(x[, 2])*1.16, xmin=1, xmax=2, textsize = 7, 
              tip_length = c(1-(max(x[, 2][x$Label == 0])/(max(x[, 2])*1.12)), 
                             1-(max(x[, 2][x$Label == 1])/(max(x[, 2])*1.12))))+
  labs(y = "Arbitrary Unit", title = "Integrated" ) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
        axis.title.x=element_blank(), axis.text=element_text(size=14), 
        axis.title=element_text(size=14,face="bold")) + ylim(NA, max(x[, 2])*1.23)