Introduction to Financial Econometrics บทที่ 4
================
เฉลิมพงษ์ คงเจริญ

# ตัวอย่างที่ 4.1 การประมาณค่าและทดสอบแบบจำลองกำหนดราคาสินทรัพย์ CAPM

``` r
data<-read.csv("https://raw.githubusercontent.com/chaleampong/finecono/main/capm.csv")
plot.ts(data$set, ylab="%")
lines(data$ptt, col=2, lty=2)
```

![](chapter4_example_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## ประมาณค่าสมการ dynamic

``` r
capm_ptt<-lm(ptt~set, data=data)
summary(capm_ptt)
```

    ## 
    ## Call:
    ## lm(formula = ptt ~ set, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.7631 -1.4051 -0.2855  1.1198 14.4052 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.83193    0.17774  -4.681 5.09e-06 ***
    ## set          0.54715    0.02922  18.725  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.536 on 213 degrees of freedom
    ## Multiple R-squared:  0.6221, Adjusted R-squared:  0.6203 
    ## F-statistic: 350.6 on 1 and 213 DF,  p-value: < 2.2e-16

## การทดสอบ autocorrelation และ heteroskedasticity

bptest สำหรับการทดสอบ heteroskedasticity

bgtest สำหรับการทดสอบ autocorrelation

``` r
library(sandwich)
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
bptest(capm_ptt)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  capm_ptt
    ## BP = 3.9157, df = 1, p-value = 0.04784

``` r
bgtest(capm_ptt)
```

    ## 
    ##  Breusch-Godfrey test for serial correlation of order up to 1
    ## 
    ## data:  capm_ptt
    ## LM test = 2.5031, df = 1, p-value = 0.1136

## การประมาณค่าด้วย dynlm

``` r
library(dynlm)
eps<-capm_ptt$residuals
eps<-ts(eps,start=c(2002,3), frequency=12)
dynlm(eps~L(eps)-1)
```

    ## 
    ## Time series regression with "ts" data:
    ## Start = 2002(4), End = 2020(1)
    ## 
    ## Call:
    ## dynlm(formula = eps ~ L(eps) - 1)
    ## 
    ## Coefficients:
    ## L(eps)  
    ## 0.1076

# ตัวอย่างที่ 4.2 การประยุกต์สมการถดถอยแบบพลวัตการสนิม

``` r
library(dynlm)
head(data)
```

    ##       date     ptt    set
    ## 1 2/1/2002  -1.301  6.773
    ## 2 3/1/2002  -0.788 -1.446
    ## 3 4/1/2002 -11.028 -2.619
    ## 4 5/1/2002   4.405  7.524
    ## 5 6/1/2002   1.246 -6.613
    ## 6 7/1/2002  -3.984 -5.239

``` r
ptt<-ts(data$ptt, start=c(2002,2), frequency = 12)
set<-ts(data$set, start=c(2002,2), frequency = 12)
m2<-dynlm(ptt~L(ptt)+set+L(set))
summary(m2)
```

    ## 
    ## Time series regression with "ts" data:
    ## Start = 2002(3), End = 2019(12)
    ## 
    ## Call:
    ## dynlm(formula = ptt ~ L(ptt) + set + L(set))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.8726 -1.4387 -0.2438  1.1406 13.8291 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.66317    0.18914  -3.506 0.000556 ***
    ## L(ptt)       0.10739    0.06807   1.578 0.116137    
    ## set          0.54573    0.02986  18.278  < 2e-16 ***
    ## L(set)      -0.01496    0.04800  -0.312 0.755690    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.509 on 210 degrees of freedom
    ## Multiple R-squared:  0.6352, Adjusted R-squared:   0.63 
    ## F-statistic: 121.9 on 3 and 210 DF,  p-value: < 2.2e-16

# ตัวอย่างที่ 4.3 การปรับ standard errors

เราสามารถใช้ชุดคำสั่ง *SANDWICH* และ *coeftest* ใน Package *LMTEST*

การเลือก type ระหว่าง HC0 ถึง HC4 เป็นการปรับ heteroskedasticity

การเลือก vcov = NeweyWest(capm_ptt, lag=4)

``` r
library(sandwich)
library(lmtest)
# Heteroskedasticity robust
coeftest(capm_ptt, df=Inf, vcov=vcovHC(capm_ptt, type="HC0"))
```

    ## 
    ## z test of coefficients:
    ## 
    ##              Estimate Std. Error z value  Pr(>|z|)    
    ## (Intercept) -0.831927   0.196975 -4.2235 2.405e-05 ***
    ## set          0.547154   0.045523 12.0194 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
coeftest(capm_ptt, df=Inf, vcov=vcovHC(capm_ptt, type="HC3"))
```

    ## 
    ## z test of coefficients:
    ## 
    ##              Estimate Std. Error z value  Pr(>|z|)    
    ## (Intercept) -0.831927   0.201694 -4.1247 3.712e-05 ***
    ## set          0.547154   0.048652 11.2463 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Autocorrelation robust
coeftest(capm_ptt, df=Inf, vcov=NeweyWest(capm_ptt, lag=4))
```

    ## 
    ## z test of coefficients:
    ## 
    ##              Estimate Std. Error z value  Pr(>|z|)    
    ## (Intercept) -0.831927   0.219509 -3.7899 0.0001507 ***
    ## set          0.547154   0.050161 10.9079 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
