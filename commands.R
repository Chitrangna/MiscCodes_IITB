
#RK Narayan Grandmothers tale

GT <- read.csv("C:/Users/HP/Desktop/5.1/en207/RKNGT.csv",header=TRUE)
str(GT)

plot(GT$Sentence,GT$Words)

#plot both= b, line and points
plot(GT$Sentence,GT$Words, type="b",
     xlab="Sentence",ylab="Words",main="Words in the first
100 sentences of R K Narayan's Grandmother's tale")

#histogram like high density vertical lines
plot(GT$Sentence,GT$Words, type="h",
     xlab="Sentence",ylab="Words",main="Words in the first
100 sentences of R K Narayan's Grandmother's tale")

dotchart(GT$Words)

hist(GT$Words,xlab="Number of Words")

#median as mid line
boxplot(GT$Words,xlab="Number of Words")

par(mfrow=c(2,1))
boxplot(GT$Words,xlab="Number of Words",
        horizontal=TRUE)
stripchart(GT$Words,xlab="Number of Words")

mean(GT$Words)
median(GT$Words)
sd(GT$Words)
Range(GT$Words)
quantile(GT$Words,0.9)
summary(GT$Words)

#marital status edition
AllLines = readLines("MaritalStatusAgeWiseIndia.csv")
View(AllLines)
dotchart(X$X2)
RemoveSixth = AllLines[-6]
X <- read.csv(textConnection(RemoveSixth),skip=4,
              header=TRUE,blank.lines.skip = TRUE)
str(X)
View(X)

plot(X$X2[2:18], xlab="age",ylab="num")
label_list <- c("0-9","10-14","15-19","20-24","25-29","30-34",
                "35-39","40-44","45-49","50-54","55-59","60-64",
                "65-69","70-74","75-79","80+","Not stated")
plot(X$X2[2:18],xaxt="n",xlab="Age range",ylab="Numbers")
axis(1,at=X$X2[2:18],labels=FALSE)
text(seq(1, 17, by=1), par("usr")[3] - 0.2, labels = label_list,
     srt = 45, offset = 1.5, pos = 1, xpd = TRUE)

y <- X$X2[2:18]*10^{-8}
plot(y,xaxt="n",xlab="Age range",
     ylab="Numbers (in tens of crores)")
axis(1,at=seq(1,17, by= 1),labels=FALSE)
text(seq(1,17, by= 1), par("usr")[3]-0.2,
     labels = label_list, srt = 90, offset = 0.5,
     pos = 1,xpd=TRUE)

plot(y,type="h",xaxt="n",xlab="Age range",ylab="Numbers (in tens of crores)")
axis(1,at=seq(1,17, by= 1),labels=FALSE)
text(seq(1,17, by= 1), par("usr")[3]-0.2, labels = label_list,
     srt = 90, offset = 0.5, pos = 1,xpd=TRUE)

pie(y)
pie(y,labels=X$X1)

stem(y,scale=1)

R <- subset(X,X1=="10-14" & X.4=="Rural" & X.3=="INDIA")
U <- subset(X,X1=="10-14" & X.4=="Urban" & X.3=="INDIA")
View(R)
View(U)
r <- c("Never married"=R$X5,"Currently married"=R$X8,
       "Widowed"=R$X11,"Separated"=R$X14,
       "Divorced"=R$X17,"Unspecified"=R$X20)
u <- c("Never married"=U$X5,"Currently married"=U$X8,
       "Widowed"=U$X11,"Separated"=U$X14,
       "Divorced"=U$X17,"Unspecified"=U$X20)
View(r)
par(mfrow=c(1,2))
pie(r,main="Rural,10-14 years")
pie(u,main="Urban, 10-14 years")

par(mfrow=c(2,1))
dotchart(r[2:6],main="Rural, 10-14 years",xlab="Numbers")
dotchart(u[2:6],main="Urban, 10-14 years",xlab="Numbers")

#rainfall
X <- read.csv("RainfallData.csv", header=TRUE,na.strings = TRUE)

str(X)
Columns <- c("JAN","FEB","MAR","APR","MAY","JUN",
             "JUL","AUG","SEP","OCT","NOV","DEC",
             "ANNUAL","JF","MAM","JJAS","OND")
colnames(X)<-Columns
X[Columns] <- sapply(X[Columns],as.numeric)

NIK <- subset(X,SUBDIVISION=="North Interior Karnataka")
hist(NIK$ANNUAL,main="North Interior Karnataka",xlab="Annual Rainfall")

Z1 <- as.numeric(X[3777,Columns[1:12]])
plot(Z1)

Z1 <- as.numeric(X[3777,Columns[1:12]])
Z2 <- as.numeric(X[3796,Columns[1:12]])
Z3 <- as.numeric(X[3818,Columns[1:12]])
Z <- data.frame("1956"=Z1,"1975"=Z2,"1999"=Z3)
plot(Z)

plot(Z1,ylab="Z1/Z2/Z3",xlab="Month")
par(new=TRUE)
plot(Z2,col="red",yaxt="n",ylab="",xlab="")
par(new=TRUE)
plot(Z3,col="green",yaxt="n",ylab="",xlab="")
