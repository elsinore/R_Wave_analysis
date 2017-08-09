groupChisqB04<-data.frame(Group1 = c(0,0), Group2 = c(0,0))
rownames(groupChisqB04) <- c(x$Tag[x$Label == 0][1],
                             x$Tag[x$Label == 1][1])
groupChisqB04[1,1] <- length(x$Group[x$Label == 0][x$Group[x$Label == 0]==1])
groupChisqB04[1,2] <- length(x$Group[x$Label == 0][x$Group[x$Label == 0]==2])
groupChisqB04[2,1] <- length(x$Group[x$Label == 1][x$Group[x$Label == 1]==1])
groupChisqB04[2,2] <- length(x$Group[x$Label == 1][x$Group[x$Label == 1]==2])