---
title: "Research Concentrates on Female Respondents"
author: "Shuoyu Chen, Yiling Song"
date: "18/10/2020"
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## Research Concentrates on Female Respondents

# Shuoyu Chen & Yiling Song 
# October 18, 2020

## Abstract

Nowadays, life satisfaction is a topic of great concern, and people pay more and more attention on women at the same time. Therefore, we would like to use statistic analysis like logistic regression to find the factors that would affect the life satisfaction of women.\
We produced some plots and performed a logistic regression analysis to research the relationships among age, total number of children, whether this woman worked last week and satisfaction of life. The results show that age, total number of children have influence on the probability of working or not for a female respondent, and for age and satisfication with life, it seems like most female respondents have rating above 5.0 no matter the age.

## Introduction

Histograms can show the distribution of a numerical variable obviously, and provide us a rough overview for our selected datas.\
Scatter plots usually demonstrate the linear relationship between two numerical variables. However, we can only find the general display of data points between life satisfaction and age with different number of children they have in this situation. On account of the large value of respondents, we have to break the datas into smaller age groups for clearly observation.\
Logistic regression is to use a logistic function to model a binary response variable. In this event, the variable 'whether worked last week' gave us a binary possibility, 'worked' or 'did not work'. Therefore, we decided to research the relationship between 'age' and 'whether worked' along with 'total number of children' since these two were related factor that would make a difference about working.

## Data

```{r, include=FALSE}
library(tidyverse)
library(cowplot)
```

```{r, echo=FALSE}
gss_2017<-read.csv("gss.csv")
gss_data<-gss_2017%>%
  filter(sex=="Female")%>%
  select(AGE=age,
         TOTAL=total_children,
         F_L=feelings_life,
         W_L=worked_last_week) %>%
  mutate(if_worked=ifelse(W_L=="Yes", yes=1, no=0))
```
The data is from the the 2017 General Social Survey(GSS) on the Family, makes it easier to read and analyse by using r code(provided by Rohan Alexander and Sam Caetano). We selected a few of contents as the main research objects, they are respectively 'age', 'total number of children', 'feeling of life' and 'whether worked last week or not' for all the female respondents in the 2017 GSS. We selected these parts because we want to investigate whether women of different ages with different number of children had a relationship with work and life satisfication.\

The 2017 GSS provides very specific information of the respondents which gives us the opportunity to do various research topics. However, since the target population for the 2017 GSS included all persons age of 15 or older in Canada(excluding residents of the Yukon, Northwest Territories, and Nunavut; and full-time residents of institutions), the survey through the 2017 GSS can only represent most of Canadian citizens but not all. What's more, not every respondents filled the survey completely or filled the survey with enough patience. 

## Model

In this research, we are going to use a logit model to help us determine how age and total number of children affect whether female respondents worked or not in last week.\

A logit model, also called a logistic model, is used to model the probability of a certain class or event existing, such as the event of worked or not in last week in this research.\

The general model of logit regression is:
$$\log(\frac{p}{1-p}) = \beta_0 + \beta_1x_1 + \beta_2x_2 + ... + \beta_kx_k $$
While $p$ is the probability of the event of interest occurring, coefficients like $\beta_1, ... , \beta_k$ represent change in log odds. \

In this research, our predictor variables are age and number of children, which are separately corresponding to $x_{age}, x_{tot1}, ..., x_{tot7}$ in our model, and the probability of worked or not in last week is corresponding to $p$. So for example, if we want to estimate the probability of work or not in last week of a female respondent with 2 children of age 30, firstly, we used r code to run a logit regression analysis and we get the coefficients which are $\beta_0, \beta_{age}, ..., \beta_{tot7}$ for our $x_{age}, ..., x_{tot7}$; secondly, fill $x_{age}=30, x_{tot2}=1$ and all other $x$ with value 0; finally, just calculate the value $p$ to get the probability.

## Results

```{r, echo=FALSE, message=FALSE, warning=FALSE}
plot_age<-ggplot(gss_data, aes(AGE)) + geom_histogram() + labs(title="Age")
plot_total<-ggplot(gss_data, aes(TOTAL)) + geom_histogram() + labs(title="Total number of Children")
plot_fl<-ggplot(gss_data, aes(F_L)) + geom_histogram() + labs(title="Feeling of Life")
cowplot::plot_grid(plot_age, plot_total, plot_fl, labels = "AUTO")
```

For plotA, B, C, they are the histograms corresponding to female respondents' age, total number of children and feeling of life.\
According to plotA, we can see female respondents of age 80 has the largest population; for plotB, female respondents with 0 or 2 children are in the majority; and for plotC, the distribution is left-skewed, and most female respondents have around 8 point on feeling of life.\

```{r, echo=FALSE, message=FALSE, warning=FALSE}
group1<-gss_data%>%
  filter(AGE<=18)
plot_1<-ggplot(group1, aes(x=AGE, y=F_L, color=TOTAL)) + 
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("Age") +
  ylab("Feeling of Life")+
  scale_color_gradient(low="light blue", high="dark red")

group2<-gss_data%>%
  filter(18<AGE&AGE<=35)
plot_2<-ggplot(group2, aes(x=AGE, y=F_L, color=TOTAL)) + 
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("Age") +
  ylab("Feeling of Life")+
  scale_color_gradient(low="light blue", high="dark red")

group3<-gss_data%>%
  filter(35<AGE&AGE<=50)
plot_3<-ggplot(group3, aes(x=AGE, y=F_L, color=TOTAL)) + 
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("Age") +
  ylab("Feeling of Life")+
  scale_color_gradient(low="light blue", high="dark red")

group4<-gss_data%>%
  filter(50<AGE&AGE<=65)
plot_4<-ggplot(group4, aes(x=AGE, y=F_L, color=TOTAL)) + 
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("Age") +
  ylab("Feeling of Life")+
  scale_color_gradient(low="light blue", high="dark red")

group5<-gss_data%>%
  filter(65<AGE)
plot_5<-ggplot(group5, aes(x=AGE, y=F_L, color=TOTAL)) + 
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("Age") +
  ylab("Feeling of Life")+
  scale_color_gradient(low="light blue", high="dark red")

PLOT_1<-cowplot::plot_grid(plot_1, plot_2, plot_3, plot_4, plot_5, labels = c("D", "E", "F", "G", "H"))
PLOT_2 <- ggdraw() +
  draw_label(
    "Different Age Groups of Feeling of Life with Different Number of Children",
    fontface = 'bold',
    x = 0,
    hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))
plot_grid(PLOT_2, PLOT_1, ncol = 1, rel_heights = c(0.1, 1))
```

For plotD to plotH, as the title, these are scatter plots for female respondents of age and feeling of life, color-coded by the number of children and divided into age groups. \
We can see, in plotD, the group of age under 18, the number of children is 0 for all female respondents in this group. In the subsequent plots, which are groups of age above 18, the red points, in other word, the total number of children tends to increase until age 50(from plotE & plotF), then stays relatively stable between age of 50-80(from plotG & plotH).\
No matter which age groups, most female respondents have point above 5.0 on feeling of life.\

```{r, echo=FALSE}
mylogit<-glm(if_worked~AGE + as.factor(TOTAL),
               data=gss_data, family="binomial")
summary(mylogit)
```
This is our logit regression analysis, this model is used to help us determine how age and total number of children affect whether female respondents worked or not in last week. \

According to this analysis, we can find out $\beta_0, \beta_{age}, ..., \beta_{tot7}$ corresponding to 2.871, -0.055, -0.065, -0.037, -0.068, -0.507, -0.805, -0.728, -1.227(to 3 decimal places). So the equation will be:
$$\log(\frac{\hat{p}}{1-\hat{p}}) = 2.871-0.055x_{age}-0.065x_{tot1}-0.037x_{tot2}-0.068x_{tot3}-0.507x_{tot4}-0.805x_{tot5}-0.728x_{tot6}-1.227x_{tot7} $$
For every additional unit increase in age we expect the log odds of working in last week to decrease by 0.055.

## Discussion

In conclusion, most women satisfied their life and had two or less children. Generally, as the age or number of children larger, women were less likely to work, and since total number of children increases from 18 years old to around 50 years old, which means probability of women worked(last week) actually decreases with growing of age. 

# Weaknesses & Next Steps

When the logit regression also includes another variable, like 'total number of children' in this situation, we ignore the possibility of being zero. What's more, some of blanks from the respondents we get in this data are corresponding to NA or missing, this may affect our analysis results, another point is, we do not consider complicated situation in this research, for example, whether the child is adopted or not, we believe this is also worth to discuss. For further research, we can narrow the scope of the research and be concentrated on more specific topic. Take an example, female respondents with rating of satisfication with life under 5.0, will the total number of children has relationship on working or not? What's about their marrital status? Through narrowing the conditions, we can investigate more.

## References

1. Data from: \
https://sda-artsci-utoronto-ca.myaccess.library.utoronto.ca/sdaweb/html/gss.htm, "Canadian general social surveys (GSS)".\
2. A tidy format of raw dataframe from: \
https://q.utoronto.ca/courses/184060/files/9422740/download?download_frd=1, "gss_cleaning.R", Rohan Alexander and Sam Caetano.\
3. Materials for making plots nicer: \
https://wilkelab.org/cowplot/articles/plot_grid.html, "Arranging plots in a grid", Claus O. Wilke.\
http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually, "ggplot2 colors : How to change colors automatically and manually?".\
4. Material helps analysis:\
https://sda-artsci-utoronto-ca.myaccess.library.utoronto.ca/sdaweb/dli2/gss/gss31/gss31/more_doc/GSS31_User_Guide.pdf, "User's Guide (Format pdf)".\
Slides, "Logistic Regression Intro", Samantha-Jo Caetano.\
5. R packages:\
tidyverse, cowplot.\

