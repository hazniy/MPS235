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
title: "Linear models suggest outlying results in the 2000 Florida elections"
author:
- name: Reg Num **123456789**
  
abstract: "Simple linear regression suggests the results in Palm Beach and Dade are different from the rest of the State.  Some concerns about the quality of the model fit are discussed."
geometry: pdftex, scale=0.85
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, tidy=TRUE, message=FALSE, warning=FALSE)
options(knitr.kable.NA = '')
#install.packages("bookdown")
#install.packages("tinytex")
#tinytex::install_tinytex()
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

The 2000 US presidential election was narrowly won by Republican candidate George W. Bush. Bush won the state of Florida with 567 more votes than the Democratic candidate Al Gore. Had Gore won Florida, he would have won the presidency.

There was criticism of the Florida ballot paper design, in particular, the *butterfly ballot* used in Palm Beach county, where it was claimed that some voters, intending to vote for Gore, mistakenly voted for the Reform candidate Pat Buchanan. 

The graphical summary of the data in Figure \@ref(fig:scat) suggest that the Palm Beach observation may indeed be out of line with the rest.

```{r scat, fig.cap="Scatter plot of the number of votes for Bush and Buchanan in Florida.  Note that the outlying point on the top corresponds to Palm Beach.", fig.pos="!ht", out.width="85%"} 
load("MPS235.RData")
attach(florida)
scat1 <- florida |> ggplot(aes(x=BUSH, y=BUCHANAN)) +
  geom_point(size=3, colour="dodgerblue4") +
  scale_x_continuous(breaks =c(0, 50000, 150000, 250000), labels = comma) +
  labs(x="Votes for Bush", y="Votes for Buchanan")  +
  theme_minimal(base_size = 12)  +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype="solid"))

bp.sh <- florida |> ggplot(aes(x = BUSH)) +
  geom_boxplot(linewidth=1.5, fill="dodgerblue2", alpha=0.3,
               outlier.colour = "red", outlier.shape = 18,
               outlier.size = 3) +
  scale_x_continuous(breaks =c(0, 50000,100000, 150000, 200000, 250000, 300000), labels = comma) +
  scale_y_discrete(labels="") +
  labs(x="Votes for Bush")  +
  theme_minimal(base_size = 12)  +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype="solid")) + 
  coord_flip()

bp.an <- florida |> ggplot(aes(x = BUCHANAN)) +
  geom_boxplot(linewidth=1.5, fill="orangered3", alpha=0.3,
               outlier.colour = "red", outlier.shape = 18,
               outlier.size = 3) +
  scale_x_continuous(breaks =seq(0, 4000, by=750), labels = comma) +
  scale_y_discrete(labels="") +
  labs(x="Votes for Buchanan")  +
  theme_minimal(base_size = 12)  +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype="solid")) + 
  coord_flip()

scat1 | (bp.sh / bp.an)
```



# Modelling {#models}

To investigate the claim, a linear model is fitted to the data.  Let $y_i$=Number of votes for Buchanan and $x_i$=Number of votes for Bush, for $i=1, \dots, 67$. We assume
\begin{equation}
y_i = \beta_0 + \beta_1 x_i + \varepsilon_i (\#eq:lr)
\end{equation}
with $\varepsilon_i \thicksim \mathrm N(\varepsilon_i ~|~ 0, \sigma^2)$, iid.   The fitted regression is displayed in Figure \@ref(fig:lm1).

```{r lm1, fig.cap="Scatter plot of the number of votes for Bush and Buchanan in Florida with the fitted regression line.  The model seems to capture the general trend of the data, the point correspondig to Palm Beach (identified in red) might appear like an outlier.", out.width="85%"}
lr.BuBu <- lm (BUCHANAN ~ BUSH, data=florida)
out.buch <- which.max(abs(lr.BuBu$residuals))

scat1 +
  geom_smooth(method='lm', se=FALSE, colour="purple", 
              linewidth=2, linetype="solid", formula= y~x) + 
  annotate("point", x=BUSH[out.buch], y=BUCHANAN[out.buch], colour="red", size=4, shape=18) +
  annotate("text", x=BUSH[out.buch], y=BUCHANAN[out.buch]-100, colour="red", size=4, label=County[out.buch] ) 
```

# Results {#results}

```{r presd}
fit.BuBu <- predict(lr.BuBu, newdata = data.frame(BUSH=BUSH[out.buch]), 
                    interval = "prediction", level = 0.99)
```

Under the fitted model,
```{r fiteq, results='asis'}
equatiomatic::extract_eq(lr.BuBu, use_coefs = T, coef_digits = 3, label="fitted")
```

the expected number of votes for Buchanan would be `r round(fit.BuBu[1], 0)`, while the observed were `r BUCHANAN[out.buch]`.  This difference is really large as illustrated in Figure \@ref(fig:predict).

```{r predict, out.width="85%", fig.cap="Comparison between the observed number of votes and the predicted by the fitted regression for Buchanan in Palm Beach. The second outlying point corresponds to Dade county."}
scat1 +
  geom_smooth(method='lm', formula= y ~ x, se=FALSE, colour="purple", linewidth=2) + 
  annotate("point", x=BUSH[out.buch], y=BUCHANAN[out.buch], colour="red", size=4) +
  annotate("text", x=BUSH[out.buch], y=BUCHANAN[out.buch]-180, colour="red", size=4, label=County[out.buch] ) +
  annotate("point", x=BUSH[out.buch], y=fit.BuBu[1], colour="blue", size=5) +
  annotate("text", x=BUSH[out.buch], y=fit.BuBu[1]+200, colour="blue", size=5, label="Expected", angle=10)  +
  geom_segment(aes(x=BUSH[out.buch], y =fit.BuBu[1],
                   xend=BUSH[out.buch], yend=BUCHANAN[out.buch]),
               linetype="dashed", linewidth=1.3, colour="forestgreen")
```

## Support for the regression {#test}

To measure the statistical support of the regression \@ref(eq:lr), we tested it against the assumption of no linear relationship, $\beta_1=0$. The results of the test in Table \@ref(tab:test) provide strong statistical evidence against the constant model.

```{r test, results='asis'}
rs.BuBu <- lm (BUCHANAN ~ 1, data=florida)
stargazer(anova(rs.BuBu, lr.BuBu), summary = F,title="Results from testing the constant model against the linear regression.  The test suggests strong statistical evidence against the constant mdoel.", header = F, label="tab:test")
```

# Conclusions {#conclusions}

The Palm Beach result is not consistent with the trend observed in the rest of the State, but neither does Dade.  The linear model seems to fit the rest of the data well, but we have not verified if the assumptions of the model hold.  We do not have a statistical measure of how good the model actually fits the data, but the average number of votes for Buchanan was `r round(mean(florida$BUCHANAN), 0)`; the standard error of the regression, `r round(summary(lr.BuBu)$sigma, 2)`, is much larger than this average, suggesting a rather poor model fit.

The model assumes the number of votes for Buchanan, given the number of votes for Bush and the model parameters, follow a Gaussian distribution, which may be open to criticism.  One possible alternative would be to transform to (a function of) vote share.


