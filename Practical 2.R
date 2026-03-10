#bpvt : the score for each child on the British Picture Vocabulary Test (BPVT), taken when the children were 4 years old. This measures knowledge of vocabulary.
#rad : the Reading Age Deficiency (RAD) for each child, measured when the children were 7 years old. This is defined as the Reading Age, as measured by a reading test, minus the chronological age, in months. A negative Reading Age Deficiency is taken as an indication of potential dyslexia.
install.packages("patchwork")#for combining ggplots into layouts 
install.packages("gtsummary")#for statistical summary tables
install.packages("gt")#for formatting tables
install.packages("stargazer")#regression table 
library(tidyverse)
library(patchwork)
library(gtsummary)
library(gt)
library(stargazer)

glimpse(dyslexia)
attach(dyslexia)

#Task 1
#Investigate whether there is evidence of a relationship between BPVT and RAD by considering appropriate statistical models.
#If you determine that a relationship exists, investigate whether it is best described by a linear or a quadratic relationship
xy.lin <- dyslexia |> ggplot(aes(y=rad, x=bpvt)) +
+    geom_point(size=4, colour="dodgerblue", shape=16) +
+    labs(y="Reading Age Deficiency", x="British Picture Vocabulary Test")

xy.log <- dyslexia |> ggplot(aes(y=rad, x=log(bpvt))) +
+    geom_point(size=2, colour="dodgerblue", shape=16) +
+    labs(y="Reading Age Deficiency", x="log British Picture Vocabulary Test")

xy.qua <- dyslexia |> ggplot(aes(y=rad, x=bpvt^2)) +
+    geom_point(size=2, colour="dodgerblue", shape=16) +
+    labs(y="Reading Age Deficiency", x="British Picture Vocabulary Test^2")

xy.lin / (xy.log | xy.qua)

tbl_summary(
+    dyslexia,
+    missing_text = "(Missing)",
+    type = all_continuous() ~ "continuous2",
+    statistic = all_continuous() ~ 
+        c("{median} ({p25}, {p75})", "({min}, {max})", "{mean}, ({sd})"),
+    label = list(
+        bpvt = "British Picture Vocabulary Test",
+        rad = "Reading Age Deficiency"
+    )
+ ) |>
+    modify_header(label ~ "**Variable**") |>
+    modify_caption("**Summary statistics from dyslexia: mean, IQR and standard deviation.**") |>
+    bold_labels() |>
+    as_gt(include = -cols_align)

round(cor(rad, bpvt), 3)
round(cor(rad, log(bpvt)), 3)
round(cor(rad, bpvt^2), 3)
dys.enc <- lm(rad ~ bpvt + I(bpvt^2), data=dyslexia)
stargazer::stargazer(dys.enc, type="text", digits=3, ci = T, ci.level = 0.99)
dys.lin <- lm(rad ~ bpvt, data=dyslexia)

anova(dys.lin,dys.enc)
summary(dys.lin)
dys.null <- lm(rad ~ 1, data=dyslexia)
anova(dys.null, dys.lin)

#Task 2
summary(dys.lin)
round(confint(dys.lin)[1,], 3)
round(confint(dys.lin)[2,], 3)
round(summary(dys.lin)$sigma^2, 3)
round(summary(dys.lin)$r.squared, 3)

#Task 3
dys.pred <- data.frame(predict(dys.lin, interval = "prediction", level=0.99))
dys.pred$bpvt <- dyslexia$bpvt
dys.pred$rad <- dyslexia$rad
dys.cint <- data.frame(predict(dys.lin, interval = "confidence", level=0.99))
dys.cint$bpvt <- dyslexia$bpvt
dys.cint$rad <- dyslexia$rad

dyslexia |> ggplot(aes(y=rad, x=bpvt)) +
+    geom_point(size=3, colour="dodgerblue", shape=16) +
+    labs(y="Reading Age Deficiency", x="British Picture Vocabulary Test") +
+    geom_smooth(method="lm", formula= y~x, colour="orangered", linewidth=2, 
+                linetype="solid", se=F)  +
+    geom_ribbon(data= dys.cint, aes(ymin = lwr, ymax = upr), fill = "red3", 
+                alpha = 0.2) +
+    geom_ribbon(data= dys.pred, aes(ymin = lwr, ymax = upr), fill = "blue3", 
+                alpha = 0.2)

#Task 4
#Interpret these results in the context of the effectiveness of BPVT at detecting dyslexia. What can you conclude from your analysis and what can you not conclude?

#Task 5
#Write up your results in a brief report. Make sure you relate your conclusions to the original problem.
