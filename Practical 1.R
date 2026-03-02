load("U:/MPS235_practicals/MPS235.RData")
load("MPS235.RData")
str(florida)
library(tidyverse) #if dont have install.packages("tidyverse")
glimpse(florida)
attach(florida)

#Task 1
plot(x=BUSH, y=BUCHANAN, pch=16, col="grey12", xlab="Votes for Bush", ylab="Votes for Buchanan", cex=1.25)

scat1 <- florida |> ggplot(aes(x=BUSH, y=BUCHANAN)) + geom_point(size=3, colour="grey15") + labs(x="Votes for Bush", y="Votes for Buchanan")
print(scat1)

#Task 2
lr.BuBu <- lm (BUCHANAN ~ BUSH, data=florida)
anova(lr.BuBu)
deviance(lr.BuBu)
summary(lr.BuBu)

#Task 3
plot(x=BUSH, y=BUCHANAN, pch=16, col="grey12", xlab="Votes for Bush", ylab="Votes for Buchanan", cex=1.25)
abline(lr.BuBu, col="orangered", lty=1, lwd=2)

plot(x=BUSH, y=BUCHANAN, pch=16, col="grey12", xlab="Votes for Bush", ylab="Votes for Buchanan", cex=1.25)
abline(a=lr.BuBu$coefficients[1], b=lr.BuBu$coefficients[2], col="orangered", lty=1, lwd=2)

scat1 + geom_smooth(method='lm', formula= y ~ x, se=FALSE, colour="orangered")

#Task 4 
out.buch <- which.max(abs(lr.BuBu$residuals))

plot(x=BUSH, y=BUCHANAN, pch=16, col="grey12", xlab="Votes for Bush", ylab="Votes for Buchanan", cex=1.25)
abline(lr.BuBu, col="orangered", lty=1, lwd=2)
text(x=BUSH[out.buch], y=BUCHANAN[out.buch]-130, labels = County[out.buch], col="red", cex=0.75)
points(x=BUSH[out.buch], y=BUCHANAN[out.buch], pch=16, col="red", cex=1.25)

scat1 + geom_smooth(method='lm', formula = y~ x, se=FALSE, colour="orangered") + annotate("point", x=BUSH[out.buch], y=BUCHANAN[out.buch], colour="red", size=3) + annotate("text", x=BUSH[out.buch], y=BUCHANAN[out.buch]-100, colour="red", size=3, label=County[out.buch] )

#Task 5
fit.BuBu <- predict(lr.BuBu, newdata = data.frame(BUSH=BUSH[out.buch]),
                    interval = "prediction", level = 0.99)
fit.BuBu
BUCHANAN[out.buch]

plot(x=BUSH, y=BUCHANAN, pch=16, col="grey12", xlab="Votes for Bush", 
     ylab="Votes for Buchanan", cex=1.25)
text(x=BUSH[out.buch], y=BUCHANAN[out.buch]-130, 
     labels = County[out.buch], col="red")
abline(lr.BuBu, col="orangered", lty=1, lwd=2)
points(x=BUSH[out.buch], y=fit.BuBu[1], col="blue", pch=16, cex=1.3)
text(x=BUSH[out.buch], y=fit.BuBu[1]+175, col="blue", label="Predicted", srt=10)

scat1 +
    geom_smooth(method='lm', formula=y~x, se=FALSE, colour="orangered") + 
    annotate("point", x=BUSH[out.buch], y=BUCHANAN[out.buch],
             colour="red", size=3) +
    annotate("text", x=BUSH[out.buch], y=BUCHANAN[out.buch]-100,
             colour="red", size=3, label=County[out.buch] ) +
    annotate("point", x=BUSH[out.buch], y=fit.BuBu[1], 
             colour="blue", size=3) +
    annotate("text", x=BUSH[out.buch], y=fit.BuBu[1]+150,
             colour="blue", size=4, label="Predicted", angle=10)

#Task 6 
rs.BuBu <- lm (BUCHANAN ~ 1, data=florida)
summary(rs.BuBu)

anova(rs.BuBu, lr.BuBu)

#Task 7
