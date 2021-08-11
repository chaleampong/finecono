Introduction to Financial Econometrics บทที่ 7
================
เฉลิมพงษ์ คงเจริญ

# ตัวอย่างที 7.1

``` r
set.seed(123456)
e1<-rnorm(1000)
e2<-rnorm(1000)
y1<-cumsum(e1)
y2<-cumsum(e2)
ts.plot(cbind(y1,y2))
```

![](chapter7_example_files/figure-gfm/sim_spurious-1.png)<!-- -->

## Spurious Regression

``` r
# level
summary(lm(y1~y2))
```

    ## 
    ## Call:
    ## lm(formula = y1 ~ y2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -20.6445  -7.3933  -0.4525   6.6639  23.0842 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11.43603    0.31329  36.503  < 2e-16 ***
    ## y2           0.16237    0.02885   5.628 2.36e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.905 on 998 degrees of freedom
    ## Multiple R-squared:  0.03077,    Adjusted R-squared:  0.02979 
    ## F-statistic: 31.68 on 1 and 998 DF,  p-value: 2.363e-08

``` r
# difference
summary(lm(diff(y1)~diff(y2)))
```

    ## 
    ## Call:
    ## lm(formula = diff(y1) ~ diff(y2))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7704 -0.6595  0.0417  0.6420  2.9843 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 0.010060   0.031404   0.320    0.749
    ## diff(y2)    0.003539   0.030842   0.115    0.909
    ## 
    ## Residual standard error: 0.9926 on 997 degrees of freedom
    ## Multiple R-squared:  1.32e-05,   Adjusted R-squared:  -0.0009898 
    ## F-statistic: 0.01316 on 1 and 997 DF,  p-value: 0.9087

# ตัวอย่าง 7.5 การพิจารณาความสัมพันธ์ระหว่าง future กับ sprot prices ของ SET50

## นำเข้าข้อมูล

นำเข้าข้อมูลรายเดือนของราคา future และ spot ของ SET แล้ว
แปลงเป็นราคาในรูปของ log

``` r
set50 <- read.csv("https://raw.githubusercontent.com/chaleampong/finecono/main/set50_m0619.csv")
head(set50)
```

    ##        date   spot futures
    ## 1 4/30/2006 533.86   537.5
    ## 2 5/31/2006 492.52   490.4
    ## 3 6/30/2006 471.54   468.8
    ## 4 7/31/2006 482.63   475.4
    ## 5 8/31/2006 482.43   476.7
    ## 6 9/30/2006 480.30   490.0

``` r
lfutures <-(log(set50$futures))
lspot <-(log(set50$spot))
```

## ทดสอบ unit root

เราทดสอบ Unit root กับ lspot และ lfuture ด้วย Dickey Fuller test
โดยใช้คำสั่ง `ur.df` ใน package`urca`

กรณีของ lspot พบว่าค่าสถิติเท่ากับ -1.3894 มากกว่าค่า critical value
(-2.88) แสดงว่า lspot เป็น unit root แล้วเมื่อพิจารณา diff(lspot)
ค่าสถิติเท่ากับ -8.95 น้อยกว่า critical value แสดงว่า diff(lspot) เป็น
stationary ดังนั้น lspot เป็น I(1)

เมื่อเราพิจารณา lfuture เราก็ได้ข้อสรุปเช่นเดียวกัน คือ lfuture เป็น
I(1) ดังนั้น เราไม่สามารถประมาณค่า OLS กับตัวแปรทั้งสองได้ ยกเว้นในกรณี
Cointegration

``` r
library(urca)
summary(ur.df(lspot, type="drift", selectlags = "BIC"))
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression drift 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.35828 -0.02705  0.00859  0.03554  0.12318 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  0.12349    0.08605   1.435   0.1532  
    ## z.lag.1     -0.01800    0.01295  -1.389   0.1666  
    ## z.diff.lag   0.16317    0.07741   2.108   0.0366 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05847 on 160 degrees of freedom
    ## Multiple R-squared:  0.03574,    Adjusted R-squared:  0.02368 
    ## F-statistic: 2.965 on 2 and 160 DF,  p-value: 0.0544
    ## 
    ## 
    ## Value of test-statistic is: -1.3894 1.3627 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau2 -3.46 -2.88 -2.57
    ## phi1  6.52  4.63  3.81

``` r
summary(ur.df(diff(lspot), type="drift", selectlags = "BIC"))
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression drift 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.34650 -0.02793  0.00554  0.03458  0.13836 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.00461    0.00464   0.994    0.322    
    ## z.lag.1     -0.91601    0.10234  -8.951  8.8e-16 ***
    ## z.diff.lag   0.07702    0.07848   0.981    0.328    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05876 on 159 degrees of freedom
    ## Multiple R-squared:   0.43,  Adjusted R-squared:  0.4228 
    ## F-statistic: 59.97 on 2 and 159 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Value of test-statistic is: -8.9508 40.0633 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau2 -3.46 -2.88 -2.57
    ## phi1  6.52  4.63  3.81

``` r
summary(ur.df(lfutures, type="drift", selectlags = "BIC"))
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression drift 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.39211 -0.02713  0.01023  0.03316  0.13382 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  0.13219    0.09101   1.452   0.1483  
    ## z.lag.1     -0.01930    0.01371  -1.408   0.1611  
    ## z.diff.lag   0.13740    0.07767   1.769   0.0788 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06243 on 160 degrees of freedom
    ## Multiple R-squared:  0.02857,    Adjusted R-squared:  0.01643 
    ## F-statistic: 2.353 on 2 and 160 DF,  p-value: 0.09837
    ## 
    ## 
    ## Value of test-statistic is: -1.4079 1.3667 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau2 -3.46 -2.88 -2.57
    ## phi1  6.52  4.63  3.81

``` r
summary(ur.df(diff(lfutures), type="drift", selectlags = "BIC"))
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression drift 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.37875 -0.03199  0.00796  0.03711  0.15094 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.004792   0.004952   0.968    0.335    
    ## z.lag.1     -0.946150   0.103883  -9.108 3.41e-16 ***
    ## z.diff.lag   0.078324   0.078407   0.999    0.319    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06274 on 159 degrees of freedom
    ## Multiple R-squared:  0.4434, Adjusted R-squared:  0.4364 
    ## F-statistic: 63.34 on 2 and 159 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Value of test-statistic is: -9.1078 41.4813 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau2 -3.46 -2.88 -2.57
    ## phi1  6.52  4.63  3.81

## ทดสอบ cointegration กรณีทราบค่าสัมประสิทธิ์

จากทฤษฎีความสัมพันธ์ระหว่างราคา future และ spot
เราทราบว่าตัวแปรทั้งสองมีความสัมพันธ์ดังนี้
*l**o**g*(*f**u**t**u**r**e*) = *l**o**g*(*s**p**o**t*) + *c**o**s**t*
เราสามารถทดสอบ cointegration โดยแบ่งเป็นสองขั้นตอนดังนี้

1.  สร้างตัวแปร
    *u* = *l**o**g*(*f**u**t**u**r**e*) − *l**o**g*(*s**p**o**t*)

2.  ทดสอบ unit root กับตัวแปร u พบว่าค่าสถิติเท่ากับ -8.13 ซึ่งน้อยกว่า
    ค่า critical value (-2.88) เราสามารถปปฏิเสธสมมุติฐานว่า u เป็น unit
    root และสรุปว่าตัวแปรทั้งสอง cointegrated กัน

``` r
u<-lfutures-lspot
summary(ur.df(u, type="drift", selectlags = "BIC"))
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression drift 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0276335 -0.0038363  0.0008359  0.0034954  0.0282398 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.0019137  0.0006433  -2.975  0.00339 ** 
    ## z.lag.1     -0.4835587  0.0798764  -6.054 9.71e-09 ***
    ## z.diff.lag  -0.0511800  0.0785379  -0.652  0.51556    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.00712 on 160 degrees of freedom
    ## Multiple R-squared:  0.2561, Adjusted R-squared:  0.2468 
    ## F-statistic: 27.54 on 2 and 160 DF,  p-value: 5.258e-11
    ## 
    ## 
    ## Value of test-statistic is: -6.0538 18.3258 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau2 -3.46 -2.88 -2.57
    ## phi1  6.52  4.63  3.81

## ตัวอย่างที่ 7.6 การทดสอบ cointegration กรณีไม่ทราบค่าสัมประสิทธิ์

กรณีที่เราไม่ทราบค่าสัมประสิทธิ์ เราสามารถทดสอบ cointegration ได้ดังนี้

1.  ประมาณค่าสมการความสัมพันธ์ระหว่างตัวแปรทั้งสองด้วยสมการดังนี้
    *l**o**g*(*f**u**t**u**r**e**s*) = *β*<sub>0</sub> + *β*<sub>1</sub>*l**o**g*(*s**p**o**t*) + *u*
    ซึ่งในที่นี้ใช้คำสั่ง `lm(lfutures~lspot)` จะได้ผลดังนี้
    $\\widehat{lfutures} = -0.064 + 1.009 lspot$ แล้วสร้าง residuals

2.  ทดสอบ unit root กับ residuals ด้วย Dickey Fuller
    พบว่าค่าสถิติเท่ากับ -6.71 ซึ่งนำไปเปรียบเทียบกับค่า critical value
    จากตาราง Phillips and Orliaris ซึ่ง จำนวนตัวแปร - 1(n-1)=1
    และรูปแบบของสมการ cointegration มีค่าคงที่ หากเลือก significance
    level ที่ 0.05 จะได้ค่า critical value เท่ากับ -3.3654
    ซึ่งค่าสถิติน้อยกว่า c.v. ดังนั้น เราสามารถปฏิเสธสมมุติฐานหลักที่ว่า
    residuals เป็น unit root และสรุปว่าตัวแปรทั้งสองเป็น cointegration

``` r
library(dynlm)
library(zoo)
lspot<-ts(lspot, frequency = 12, start=c(2006,4))
lfutures<-ts(lfutures, frequency = 12, start=c(2006,4))
m1<-dynlm(lfutures~lspot)
summary(m1)
```

    ## 
    ## Time series regression with "ts" data:
    ## Start = 2006(4), End = 2019(12)
    ## 
    ## Call:
    ## dynlm(formula = lfutures ~ lspot)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0198129 -0.0038723 -0.0000842  0.0025807  0.0281109 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.06423    0.01090  -5.895  2.1e-08 ***
    ## lspot        1.00909    0.00164 615.341  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.00747 on 163 degrees of freedom
    ## Multiple R-squared:  0.9996, Adjusted R-squared:  0.9996 
    ## F-statistic: 3.786e+05 on 1 and 163 DF,  p-value: < 2.2e-16

``` r
uhat<-ts(m1$residuals, frequency = 12, start=c(2006,4))
library(urca)
summary(ur.df(uhat, type="drift", selectlags = "BIC"))
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression drift 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0211000 -0.0036460 -0.0000285  0.0026266  0.0299191 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6.542e-05  5.305e-04  -0.123    0.902    
    ## z.lag.1     -5.695e-01  8.488e-02  -6.710  3.2e-10 ***
    ## z.diff.lag  -2.409e-02  7.819e-02  -0.308    0.758    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.006773 on 160 degrees of freedom
    ## Multiple R-squared:  0.2923, Adjusted R-squared:  0.2835 
    ## F-statistic: 33.05 on 2 and 160 DF,  p-value: 9.7e-13
    ## 
    ## 
    ## Value of test-statistic is: -6.71 22.5131 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau2 -3.46 -2.88 -2.57
    ## phi1  6.52  4.63  3.81

## ตัวอย่างที่ 7.7 การประมาณค่า Error Correction Model (ECM)

``` r
ecm1<-dynlm(diff(lfutures)~L(uhat)+L(diff(lspot))+L(diff(lfutures)))
summary(ecm1)
```

    ## 
    ## Time series regression with "ts" data:
    ## Start = 2006(6), End = 2019(12)
    ## 
    ## Call:
    ## dynlm(formula = diff(lfutures) ~ L(uhat) + L(diff(lspot)) + L(diff(lfutures)))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.36226 -0.03129  0.00691  0.03930  0.12627 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)        0.003840   0.004829   0.795  0.42767   
    ## L(uhat)           -2.153152   0.774549  -2.780  0.00609 **
    ## L(diff(lspot))    -0.508303   0.767942  -0.662  0.50899   
    ## L(diff(lfutures))  0.684209   0.734172   0.932  0.35278   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06145 on 159 degrees of freedom
    ## Multiple R-squared:  0.0647, Adjusted R-squared:  0.04705 
    ## F-statistic: 3.666 on 3 and 159 DF,  p-value: 0.01367

``` r
ecm2<-dynlm(diff(lspot)~L(uhat)+L(diff(lspot))+L(diff(lfutures)))
summary(ecm2)
```

    ## 
    ## Time series regression with "ts" data:
    ## Start = 2006(6), End = 2019(12)
    ## 
    ## Call:
    ## dynlm(formula = diff(lspot) ~ L(uhat) + L(diff(lspot)) + L(diff(lfutures)))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.33937 -0.03121  0.00459  0.03474  0.11988 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)        0.003904   0.004573   0.854   0.3945  
    ## L(uhat)           -1.562351   0.733366  -2.130   0.0347 *
    ## L(diff(lspot))    -0.551246   0.727111  -0.758   0.4495  
    ## L(diff(lfutures))  0.717850   0.695136   1.033   0.3033  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05818 on 159 degrees of freedom
    ## Multiple R-squared:  0.05119,    Adjusted R-squared:  0.03328 
    ## F-statistic: 2.859 on 3 and 159 DF,  p-value: 0.03877

# ตัวอย่างที่ 7.8 การทดสอบ Johansen’s cointegration

ต่อเนื่องจากตัวอย่างที่ 7.6

## หาอันดับ VECM ที่เหมาะสม

ขั้นตอนแรกในการทดสอบ คือการหาอันดับที่เหมาะสมสำหรับ VECM
โดยจะเป็นอันดับของ VAR-1

เราประมาณค่า VAR ด้วย package `vars` จะพบว่าอันดับที่เหมาะสมคือ VAR(4)
และเราสามารถประมาณค่าแบบจำลอง VECM(3)

    ##      lfutures    lspot
    ## [1,] 6.286929 6.280134
    ## [2,] 6.195221 6.199535
    ## [3,] 6.150176 6.156004
    ## [4,] 6.164157 6.179250
    ## [5,] 6.166887 6.178836
    ## [6,] 6.194405 6.174411

    ## 
    ## VAR Estimation Results:
    ## ======================= 
    ## 
    ## Estimated coefficients for equation lfutures: 
    ## ============================================= 
    ## Call:
    ## lfutures = lfutures.l1 + lspot.l1 + lfutures.l2 + lspot.l2 + lfutures.l3 + lspot.l3 + lfutures.l4 + lspot.l4 + const 
    ## 
    ## lfutures.l1    lspot.l1 lfutures.l2    lspot.l2 lfutures.l3    lspot.l3 
    ## -0.79539697  2.01095453 -0.63800486  0.36008883 -0.17298465  0.51668910 
    ## lfutures.l4    lspot.l4       const 
    ## -0.11430046 -0.16637568 -0.01241799 
    ## 
    ## 
    ## Estimated coefficients for equation lspot: 
    ## ========================================== 
    ## Call:
    ## lspot = lfutures.l1 + lspot.l1 + lfutures.l2 + lspot.l2 + lfutures.l3 + lspot.l3 + lfutures.l4 + lspot.l4 + const 
    ## 
    ## lfutures.l1    lspot.l1 lfutures.l2    lspot.l2 lfutures.l3    lspot.l3 
    ## -1.14892572  2.35160293 -0.63378659  0.35614876 -0.07680762  0.39957833 
    ## lfutures.l4    lspot.l4       const 
    ## -0.27837224  0.02759510  0.01419804

## ทดสอบ Johansen’s test

เราทดสอบ Johansen’s test โดยใช้คำสั่ง `ca.jo` ใน package `urca`
โดยระบุตัวสถิติที่ใช้คือ trace statistic ด้วย `type=c("trace")`
และรูปแบบของ cointegration มีค่าคงที่ `ecdet=c("const")`
และจำนวนอันดับของ VAR `k=4`โดยเก็บผลไว้ในชื่อ `fsprice.rc`
และเรียกดูผลด้วย `summary(fsprice.rc)`

จะได้ค่าสถิติซึ่งมีการทดสอบแบบเป็นลำดับ

-   *H*<sub>0</sub> : *r* = 0 vs *H*<sub>1</sub> : *r* \> 0 ค่าสถิติ
    44.18 มากกว่าค่า c.v.19.96 กรณี significance level = 0.05
    เราสามารถปฏิเสธสมมุติฐานหลัก และสรุปว่า r>0

-   *H*<sub>0</sub> : *r* = 1 vs *H*<sub>1</sub> : *r* \> 1 ค่าสถิติ
    2.83 น้อยกว่าค่า c.v. 9.24 กรณี significance level = 0.05
    เราไม่สามารถปฏิเสธสมมุติฐานหลัก และยอมรับว่า r=1 หรือ lfutures และ
    lspot มี cointegration 1 ความสัมพันธ์

<!-- -->

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: trace statistic , without linear trend and constant in cointegration 
    ## 
    ## Eigenvalues (lambda):
    ## [1] 2.240470e-01 1.723171e-02 2.255141e-17
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##           test 10pct  5pct  1pct
    ## r <= 1 |  2.83  7.52  9.24 12.97
    ## r = 0  | 44.18 17.85 19.96 24.60
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##             lfutures.l2  lspot.l2  constant
    ## lfutures.l2  1.00000000  1.000000  1.000000
    ## lspot.l2    -1.00776458 -1.354511 -1.241907
    ## constant     0.05564508  2.444837  1.494701
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##            lfutures.l2   lspot.l2      constant
    ## lfutures.d   -2.218536 0.04773547 -5.221707e-13
    ## lspot.d      -1.626270 0.04716210 -3.783474e-13

เราสามารถประมาณค่าแบบจำลอง VECM ด้วยคำสั่ง `cajorls`
และระบุความสัมพันธ์ตามผลการทดสอบ `r=1`

    ## $rlm
    ## 
    ## Call:
    ## lm(formula = substitute(form1), data = data.mat)
    ## 
    ## Coefficients:
    ##               lfutures.d  lspot.d
    ## ect1          -2.2185     -1.6263
    ## lfutures.dl1  -1.5250     -0.8995
    ## lspot.dl1      1.7264      1.0865
    ## 
    ## 
    ## $beta
    ##                    ect1
    ## lfutures.l2  1.00000000
    ## lspot.l2    -1.00776458
    ## constant     0.05564508

# ตัวอย่างที่ 7.9 ความสัมพันธ์ระหว่างผลตอบแทนพันธบัตรรัฐบาล

## VECM กรณีมีตัวแปรมากกว่า 2 ตัวแปร

ตัวอย่างนี้เราพิจารณาความสัมพันธ์ระหว่างอัตราดอกเบี้ยพันธบัตรรัฐบาลอายุ
1 3 เดือน และ 5 ปีตามลำดับ

``` r
tbond <-read.csv("https://raw.githubusercontent.com/chaleampong/finecono/main/tbond.csv", header = TRUE)
head(tbond)
```

    ##      Month   m1   m3   m6   y1   y2   y3   y4   y5
    ## 1 01/01/05 1.78 1.95 2.22 2.50 2.88 3.28 3.66 4.11
    ## 2 02/01/05 1.83 1.98 2.20 2.46 2.94 3.33 3.68 4.08
    ## 3 03/01/05 1.91 2.07 2.26 2.54 2.96 3.29 3.56 3.90
    ## 4 04/01/05 2.10 2.19 2.33 2.61 2.97 3.26 3.48 3.74
    ## 5 05/01/05 2.28 2.41 2.59 2.86 3.13 3.30 3.46 3.65
    ## 6 06/01/05 2.23 2.40 2.61 2.86 3.14 3.32 3.41 3.56

``` r
m1<-tbond$m1
m3<-tbond$m3
y5<-tbond$y5
ts.plot(cbind(m1,m3,y5), lty=c(1:3), ylab="%",col=c("black", "red", "blue"), main="treasury bond at different maturities")
legend("topright", legend = c("1 month", "3 months", "5 years"), col=c("black", "red", "blue"), lty = 1:3, xjust = 1, yjust = 1)
```

![](chapter7_example_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## พิจารณาอันดับ VAR

ขั้นตอนแรกของการทดสอบคือการหาอันดับที่เหมาะสมของ VECM
โดยการจัดตัวแปรทั้งสามให้อยู่ในรูปเมทริกซ์ `rterm` ด้วยคำสั่ง `cbind`
หลังจากนั้น ประมาณค่า VAR ด้วย package `vars` และคำสั่ง `VAR`
โดยระบุข้อมูลที่ประมาณค่าคือ `rterm` จำนวนอันดับที่สูงที่สุด
`lag.max=6`และเลือก model selection คือ AIC ด้วย `ic=c("AIC")`

หากพิจารณาแบบจำลอง VAR ที่เหมาะสมคือ VAR(3) ดังนั้น
เราจะประมาณค่าแบบจำลอง VECM(2)

    ## 
    ## VAR Estimation Results:
    ## ======================= 
    ## 
    ## Estimated coefficients for equation m1: 
    ## ======================================= 
    ## Call:
    ## m1 = m1.l1 + m3.l1 + y5.l1 + m1.l2 + m3.l2 + y5.l2 + m1.l3 + m3.l3 + y5.l3 + const 
    ## 
    ##        m1.l1        m3.l1        y5.l1        m1.l2        m3.l2        y5.l2 
    ## -0.148170687  1.468544693  0.017887883  0.319187528 -0.565154976  0.035796258 
    ##        m1.l3        m3.l3        y5.l3        const 
    ## -0.100551087  0.015396105 -0.054976949 -0.009814944 
    ## 
    ## 
    ## Estimated coefficients for equation m3: 
    ## ======================================= 
    ## Call:
    ## m3 = m1.l1 + m3.l1 + y5.l1 + m1.l2 + m3.l2 + y5.l2 + m1.l3 + m3.l3 + y5.l3 + const 
    ## 
    ##       m1.l1       m3.l1       y5.l1       m1.l2       m3.l2       y5.l2 
    ## -0.77262617  2.06919377  0.07347698  0.40365078 -0.63296798 -0.00663435 
    ##       m1.l3       m3.l3       y5.l3       const 
    ## -0.35137281  0.25663998 -0.05718647  0.00346167 
    ## 
    ## 
    ## Estimated coefficients for equation y5: 
    ## ======================================= 
    ## Call:
    ## y5 = m1.l1 + m3.l1 + y5.l1 + m1.l2 + m3.l2 + y5.l2 + m1.l3 + m3.l3 + y5.l3 + const 
    ## 
    ##       m1.l1       m3.l1       y5.l1       m1.l2       m3.l2       y5.l2 
    ## -0.94700137  1.02677142  1.27187191 -0.41862278  0.01796799 -0.41027244 
    ##       m1.l3       m3.l3       y5.l3       const 
    ##  0.34032262  0.01702885  0.06041757  0.12019345

## ทดสอบ Johansen’s cointegration

เราทดสอบ Johansen’s test โดยใช้คำสั่ง `ca.jo` ใน package `urca`
โดยระบุตัวสถิติที่ใช้คือ trace statistic ด้วย `type=c("trace")`
และรูปแบบของ cointegration มีค่าคงที่ `ecdet=c("const")`
และจำนวนอันดับของ VAR `k=3`โดยเก็บผลไว้ในชื่อ `rterm.rc`
และเรียกดูผลด้วย `summary(rterm.rc)`

จะได้ค่าสถิติซึ่งมีการทดสอบแบบเป็นลำดับ (ในกรณีนี้ใช้ significance level
เท่ากับ 0.1)

-   *H*<sub>0</sub> : *r* = 0 vs *H*<sub>1</sub> : *r* \> 0
    ค่าสถิติเท่ากับ 44.49 > Critical value (=32)
    เราสามารถปฏิเสธสมมุติฐานหลักที่ว่า r=0 และยอมรับว่า r>0

-   *H*<sub>0</sub> : *r* = 1 vs *H*<sub>1</sub> : *r* \> 1
    ค่าสถิติเท่ากับ 19.69 > Critical value (=17.85)
    เราสามารถปฏิเสธสมมุติฐานหลักที่ว่า r=1 และยอมรับว่า r>1

-   *H*<sub>0</sub> : *r* = 2 vs *H*<sub>1</sub> : *r* \> 2
    ค่าสถิติเท่ากับ 4.31 \< Critical value (=7.52)
    เราไม่สามารถปฏิเสธสมมุติฐานหลักที่ว่า r=2

สรุปว่าตัวแปรทั้ง 3 cointegrated กัน และมีความสัมพันธ์ 2 สมการ

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: trace statistic , without linear trend and constant in cointegration 
    ## 
    ## Eigenvalues (lambda):
    ## [1] 0.15818825 0.10127853 0.02951317 0.00000000
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##           test 10pct  5pct  1pct
    ## r <= 2 |  4.31  7.52  9.24 12.97
    ## r <= 1 | 19.69 17.85 19.96 24.60
    ## r = 0  | 44.49 32.00 34.91 41.07
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##                m1.l3      m3.l3      y5.l3  constant
    ## m1.l3     1.00000000  1.0000000  1.0000000  1.000000
    ## m3.l3    -1.01768859 -1.8270107 -1.1379178 -1.443085
    ## y5.l3     0.02286757  0.6652551  0.4414349  1.046099
    ## constant  0.01082500 -0.1360052 -1.0450896 -4.791725
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##           m1.l3        m3.l3        y5.l3     constant
    ## m1.d -0.9588657  0.034340910 -0.005009448 2.724167e-16
    ## m3.d -0.7547702  0.052367170 -0.017945128 2.186883e-16
    ## y5.d -0.8972888 -0.004266062 -0.123746712 3.509375e-16

## ประมาณค่าแบบจำลอง VECM

จากผลการทดสอบข้างต้นที่พบกว่าตัวแปรทั้งสาม cointegrated
และมีความสัมพันธ์ 2 สมการ เราสามารถประมาณ VECM ด้วยคำสั่ง `cajorls`
โดยระบุรูปแบบสมการเช่นเดียวกับ `rterm.rc` และจำนวนความสัมพันธ์ `r=2`
โดยเก็บผลการประมาณค่าไว้ในชื่อ `rterm.vecm` ซึ่งเมื่อเรียกผลออกมา
จะสามารถแบ่งออกเป็นสองส่วน

ส่วนแรกในบริเวณ `$beta` จะระบุความสัมพันธ์ระยะยาว หรือสมการ
cointegration ตามคอลัมน์ โดยที่ที่และคอมันน์จะระบุด้วยชื่อ `ect1` และ
`ect2` ซึ่งสามารถเขียนเป็นสมการได้ดังนี้

*e**c**t*1<sub>*t* − 1</sub> = 1*m*1<sub>*t*</sub> − (−4.14*x*10<sup>−16</sup>)*m*3<sub>*t*</sub> − 0.78*y*5<sub>*t*</sub> + 0.195
ซึ่งเราค่าสัมประสิทธิ์หน้า m3 มีค่าน้อยมาก เราสามารถตัดออก
และเขียนสมการใหม่ได้เป็น $ m1_t = 0.78 y5_t - 0.195+ect1\_{t-1}$

*e**c**t*2<sub>*t* − 1</sub> = (2.22*x*10<sup>−16</sup>)*m*1<sub>*t*</sub> + 1*m*3<sub>*t*</sub> − 0.794*y*5 − *t* + 0.181
ซึ่งเราค่าสัมประสิทธิ์หน้า m1 มีค่าน้อยมาก เราสามารถตัดออก
และเขียนสมการใหม่ได้เป็น
1*m*3<sub>*t*</sub> = 0.794*y*5 − *t* − 0.181 + *e**c**t*2<sub>*t* − 1</sub>

ส่วนสองในบริเวณ `$rlm` จะระบุการปรับตัวในระยะสั้น หรือ VECM
โดยแต่ละคอลัมน์จะแทนแต่ละสมการ ได้แก่ m1.d (*Δ**m*1<sub>*t*</sub>) m3.d
(*Δ**m*3<sub>*t*</sub>) และ y5.d (*Δ**y*5<sub>*t*</sub>) ยกตัวอย่างเช่น
สมการ m1.d สามารถเขียนได้ดังนี้

*Δ**m*1<sub>*t*</sub> =  − 0.92*e**c**t*1<sub>*t* − 1</sub> + 0.91*e**c**t*2<sub>*t* − 1</sub> − 1.14*Δ**m*1<sub>*t* − 1</sub> + 1.46*Δ**m*3<sub>*t* − 1</sub> + 0.02*Δ**y*5<sub>*t* − 1</sub> − 0.82*Δ**m*1<sub>*t* − 2</sub> + 0.90*Δ**m*3<sub>*t* − 2</sub> + 0.05*Δ**y*5<sub>*t* − 2</sub>

    ## $rlm
    ## 
    ## Call:
    ## lm(formula = substitute(form1), data = data.mat)
    ## 
    ## Coefficients:
    ##         m1.d      m3.d      y5.d    
    ## ect1    -0.92452  -0.70240  -0.90155
    ## ect2     0.91309   0.67245   0.92095
    ## m1.dl1  -1.14885  -0.77507  -0.96384
    ## m3.dl1   1.46761   1.06584   1.00361
    ## y5.dl1   0.01948   0.07918   0.31122
    ## m1.dl2  -0.82876  -0.36819  -1.36022
    ## m3.dl2   0.90168   0.43010   1.00247
    ## y5.dl2   0.05479   0.07082  -0.11098
    ## 
    ## 
    ## $beta
    ##                   ect1          ect2
    ## m1.l3     1.000000e+00  2.220446e-16
    ## m3.l3    -4.440892e-16  1.000000e+00
    ## y5.l3    -7.849079e-01 -7.937354e-01
    ## constant  1.954578e-01  1.814237e-01
