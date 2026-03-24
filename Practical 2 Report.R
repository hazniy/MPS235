---
output: 
   bookdown::pdf_document2:
       keep_tex: true # a tex file will be saved
       fig_caption: true
       fontsize: 11pt
       df_print: kable
       latex_engine: pdflatex
       template: svm-latex-ms.tex #formats the docuemnt
       citation_package: natbib
       bibliography: references.bib #save your references here
       includes:
         in_header: preamble.tex #adds packages
       biblio-style: apalike #controls the format of bibiliography
       link-citations: true
       extra_dependencies: subfig
       toc: false
title: "Linear Regression Analysis of BPVT Scores and Reading Age Deficiency (RAD)"
author:
- name: Reg Num **240145846**
  
abstract: "Simple linear regression suggests there exists a relationship between BPVT scores and reading age deficiency (RAD), with the natural scale instead using log BPVT and quadratic BPVT models."
geometry: pdftex, scale=0.85
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, tidy=TRUE, message=FALSE, warning=FALSE)
options(knitr.kable.NA = '')
library(kableExtra)
library(stargazer)
library(tidyverse)
library(cowplot)
library(patchwork)
library(gridExtra)
library(gtsummary)
library(showtext)
library(gglm)
library(scales)

knitr::opts_chunk$set(echo = FALSE, tidy=TRUE,fig.align='center',
                 external=TRUE,
                 echo=FALSE,
                 warning=FALSE,
                 fig.pos='H',
                 tab.pos='H')
```

# Background {#background}

The dyslexia dataset gives the results from a dyslexia study on 34 children. The variables are bpvt, which is the score for each child on the *British Picture Vocabulary Test (BPVT)*, taken when the children were 4 years old. This measures knowledge of vocabulary. *Reading age deficiency (RAD)* for each child is measured when the children were 7 years old. This is defined as the Reading Age, as measured by a reading test, minus the chronological age, in months. A negative Reading Age Deficiency is taken as an indication of potential dyslexia.

We want to find evidence of a relationship between BPVT and RAD.

# EDA {#EDA}

We first carry out EDA with scatter plots of RAD against functions of BPVT in Figure \@ref(fig:scat), showing linear, log, and quadratic forms. Summary statistics and the corresponding correlations show that log BPVT does not improve the model (Cor = $0.54$ for natural scale, Cor = $0.537$ for log BPVT), and ANOVA comparison between the linear and quadratic models also indicates that the quadratic term is not significant ($p = 0.6135$) , hence we stick with the simple linear model.

```{r scat, fig.cap="Scatter plots of RAD against BPVT and transformations of BPVT, showing the possible linear, logarithmic, and quadratic relationships between the variables.", fig.pos="!ht", out.width="85%"}
load("MPS235.RData")
attach(dyslexia)
xy.lin <- dyslexia |> ggplot(aes(y=rad, x=bpvt)) +
  geom_point(size=4, colour="dodgerblue", shape=16) +
  labs(y="Reading Age Deficiency", x="British Picture Vocabulary Test")

xy.log <- dyslexia |> ggplot(aes(y=rad, x=log(bpvt))) +
 geom_point(size=2, colour="dodgerblue", shape=16) +
 labs(y="Reading Age Deficiency", x="log British Picture Vocabulary Test")

xy.qua <- dyslexia |> ggplot(aes(y=rad, x=bpvt^2)) +
 geom_point(size=2, colour="dodgerblue", shape=16) +
 labs(y="Reading Age Deficiency", x="British Picture Vocabulary Test^2")

xy.lin / (xy.log | xy.qua)
```

# Modelling {#models}

To investigate the relationship between BPVT and RAD, a linear model is fitted to the data.  Let $y_i$=the RAD for child $i$ and $x_i$=BPVT score, for $i=1, \dots, 34$. We assume
\begin{equation}
y_i = \beta_0 + \beta_1 x_i + \varepsilon_i (\#eq:lr)
\end{equation}
with $\varepsilon_i \thicksim \mathrm N(\varepsilon_i ~|~ 0, \sigma^2)$, iid.   The fitted regression is displayed in Figure \@ref(fig:lm1).

```{r lm1, fig.cap="Scatter plot of RAD against BPVT scores with the fitted linear regression line. The shaded regions represent the 99% confidence interval for the mean response and the 99% prediction interval for individual observations.", out.width="85%"}
dys.lin <- lm(rad ~ bpvt, data=dyslexia)
dys.pred <- data.frame(predict(dys.lin, interval = "prediction", level=0.99))
dys.pred$bpvt <- dyslexia$bpvt
dys.pred$rad <- dyslexia$rad

dys.cint <- data.frame(predict(dys.lin, interval = "confidence", level=0.99))
dys.cint$bpvt <- dyslexia$bpvt
dys.cint$rad <- dyslexia$rad

dyslexia |> ggplot(aes(y=rad, x=bpvt)) +
  geom_point(size=3, colour="dodgerblue", shape=16) +
  labs(y="Reading Age Deficiency", x="British Picture Vocabulary Test") +
  geom_smooth(method="lm", formula= y~x, colour="orangered", linewidth=2, 
              linetype="solid", se=F)  +
  geom_ribbon(data= dys.cint, aes(ymin = lwr, ymax = upr), fill = "red3", 
              alpha = 0.2) +
  geom_ribbon(data= dys.pred, aes(ymin = lwr, ymax = upr), fill = "blue3", 
              alpha = 0.2)
```

# Results {#results}

```{r presd}
dys.lin <- lm(rad ~ bpvt, data=dyslexia)
```

Under the fitted model,
```{r fiteq, results='asis'}
equatiomatic::extract_eq(dys.lin, use_coefs = T, coef_digits = 3, label="fitted")
```

This suggests that for each one-unit increase in BPVT score, the expected RAD increases by approximately 0.63 months. 

## Support for the regression {#test}

To measure the statistical support of the regression \@ref(eq:lr), we tested it against the assumption of no linear relationship, $\beta_1=0$. The results of the test in Table \@ref(tab:test) provide strong statistical evidence against the constant model.

```{r test, results='asis'}
dys.null <- lm(rad ~ 1, data=dyslexia)
stargazer(anova(dys.null, dys.lin), summary = F,title="Results from testing the constant model against the linear regression. The test suggests strong statistical evidence against the constant model.", header = F, label="tab:test")
```

# Conclusions {#conclusions}

There's a relationship between the BPVT scores and Reading Age Deficiency (RAD) with the fitted linear model suggest that higher BPVT scores are associated with higher RAD values on average. The linear model seems to fit the rest of the data well, but we have not verified if the assumptions of the model hold. A large proportion of the variation in RAD remains unexplained, suggesting that other factors may also influence RAD.

The model assumes that RAD, given BPVT and the model parameters, follows a Gaussian distribution, which may be open to criticism. One possible extension would be to include additional explanatory variables that may help better explain variation in reading ability.


