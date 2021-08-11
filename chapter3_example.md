Introduction to Financial Econometrics ตัวอย่างบทที่ 3
================
เฉลิมพงษ์ คงเจริญ

# รูปที่ 3.1 ตัวอย่างข้อมูลที่สร้างจาก process AR(1)

ตัวอย่างข้างล่างสร้าง series ที่ถูกอธิบายด้วย AR(1) process โดยใช้คำสั่ง
`arima.sim` เราระบุคำสั่งว่าเป็น AR(1) ที่มีค่าสัมประสิทธิ์เท่ากับ 0.9
ด้วย `list(order=c(1,0,0), ar=0.9)` และจำนวนตัวอย่างเท่ากับ `n=200`

``` r
y1<-arima.sim(list(order=c(1,0,0), ar=0.9), n=200)
plot.ts(y1, main="AR(1) with coefficient = 0.9")
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
y2<-arima.sim(list(order=c(1,0,0), ar=-0.9), n=200)
plot.ts(y2, main="AR(1) with coefficient = -0.9")
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

# การพิจาณา Autocorrelation Function (ACF)

ในการสร้างแบบจำลองอนุกรมเวลา
ขั้นตอนแรกของการพิจารณาคือการพิจารณาว่าตัวแปรที่เราสนใจเป็น white noise
หรือไม่ ซึ่งหากเป็น white noise
แสดงว่าข้อมูลในแต่ละตัวอย่างไม่มีความสัมพันธ์ระหว่างกัน
เราไม่สามารถสร้างแบบจำลองอนุกรมเวลาโดยใช้ข้อมูลในอดีตได้
เครื่องมือแรกคือการพิจารณา ACF และเรายังสามารถใช้ ACF และ PACF
ในการเลือกรูปแบบของแบบจำลองที่เหมาะสมได้

## รูปที่ 3.2

``` r
#Theoretical ACF 
par(mfrow=c(1,2))
plot(ARMAacf(ar=0.9, ma=0, 20), type="h", ylab="ACF", xlab="Lag")
plot(ARMAacf(ar=-0.9, ma=0, 20), type="h", ylab="ACF", xlab="Lag")
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-2-1.png)<!-- --> ##
รูปที่ 3.3

``` r
#Theoretical PACF
par(mfrow=c(2,2))
plot(ARMAacf(ar=0.9, ma=0, 20, pacf=TRUE), type="h", ylab="ACF", xlab="Lag")
plot(ARMAacf(ar=-0.9, ma=0, 20, pacf=TRUE), type="h", ylab="ACF", xlab="Lag")
plot(ARMAacf(ar=c(-0.2,0.35), ma=0, 20, pacf=TRUE), type="h", ylab="ACF", xlab="Lag")
plot(ARMAacf(ar=c(0.2,-0.35), ma=0, 20, pacf=TRUE), type="h", ylab="ACF", xlab="Lag")
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# ตัวอย่างที่ 3.3 การสร้างแบบจำลองสำหรับความแตกต่างระหว่างอัตราดอกเบี้ยด้วยแบบจำลอง AR

ในตัวอย่างนี้ (ตัวอย่าง 2.3)
เราพิจารณาข้อมูลความแตกต่างระหว่างอัตราดอกเบี้ยลูกค้าชั้นดีของไทยและสหรัฐอเมริกา

``` r
int<-read.csv("https://raw.githubusercontent.com/chaleampong/EC435/master/mlr.csv", header = TRUE)
head(int)
```

    ##    month   MLR Usprime diff_th_us
    ## 1 Jan-78 10.75    7.93       2.82
    ## 2 Feb-78 10.75    8.00       2.75
    ## 3 Mar-78 10.75    8.00       2.75
    ## 4 Apr-78 10.75    8.00       2.75
    ## 5 May-78 10.75    8.27       2.48
    ## 6 Jun-78 11.00    8.63       2.37

``` r
ts.plot(int$diff_th_us, ylab ="%", xlab="time")
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### พิจารณา ACF และ PACF

จาก ACF จะเห็นได้ว่ามีค่าค่อยๆลดลงและแตกต่างจากศูนย์ไปถึง lag ที่ 25
ในขณะที่ PACF เริ่มมีค่าไม่แตกต่างศูนย์ที่ lag ที่ 4 ดังนั้น
เราน่าจะใช้แบบจำลอง AR(3) ในการอธิบายตัวแปรดังกล่าว

``` r
par(mfrow=c(1,2))
acf(int$diff_th_us)
pacf(int$diff_th_us)
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# ตัวอย่างที่ 3.4 การประมาณค่าและตรวจสอบความเหมาะสมของแบบจำลอง AR(3)

เราสามารถประมาณค่าแบบจำลอง AR(3) ได้ด้วยคำสั่ง `arima` โดยระบุข้อมูล
`int$diff_th_us` และ ลำดับใน `c(p,d,q)` ซึ่งในกรณีของแบบจำลอง AR(p)
เราจะระบุค่า p และกำหนดให้ d=0, q=0

``` r
m1<-arima(int$diff_th_us, order=c(3,0,0))
m1
```

    ## 
    ## Call:
    ## arima(x = int$diff_th_us, order = c(3, 0, 0))
    ## 
    ## Coefficients:
    ##          ar1      ar2     ar3  intercept
    ##       1.3294  -0.4986  0.1334     3.0553
    ## s.e.  0.0549   0.0878  0.0549     0.8102
    ## 
    ## sigma^2 estimated as 0.3054:  log likelihood = -269.11,  aic = 548.21

เราสามารถเลือกวิธีการในการประมาณค่า MLE ด้วย `method=c("ML")`

``` r
m2<-arima(int$diff_th_us, order=c(3,0,0), method=c("ML"))
m2
```

    ## 
    ## Call:
    ## arima(x = int$diff_th_us, order = c(3, 0, 0), method = c("ML"))
    ## 
    ## Coefficients:
    ##          ar1      ar2     ar3  intercept
    ##       1.3295  -0.4987  0.1334      3.056
    ## s.e.  0.0549   0.0878  0.0549      0.810
    ## 
    ## sigma^2 estimated as 0.3054:  log likelihood = -269.11,  aic = 548.21

เราสามารถเช็ครากของ polynomial จะเห็นได้ว่าคำตอบมีทั้งที่เป็น real และ
complex number ดังนั้นเราพิจารณามอดูลัสจะพบว่ามีค่าน้อยกว่า 1
แสดงว่าอนุกรมเวลาไม่นิ่ง (nonstationary)

``` r
p1<-m1$coef
roots<-polyroot(p1)
roots
```

    ## [1]  0.4008889+0.5949388i -0.8454280-0.0000000i  0.4008889-0.5949388i

``` r
Mod(roots)
```

    ## [1] 0.7174009 0.8454280 0.7174009

## การทดสอบความเพียงพอของแบบจำลอง

เราทดสอบว่า residuals ของแบบจำลอง m1 เป็น white noise หรือไม่
โดยใช้คำสั่ง `Box.test` กับ `m1$residuals` โดยเก็บผลไว้ที่ `adqtest`

จากค่า LB statistics =14.009 เราสามารถนำไปเปรียบเทียบกับ critical
chi-square (df=12-3, *α* = 0.05)=16.92 เนื่องจาก LB statistics \<
critical chi-sq เราไม่สามารถปฏิเสธสมมุติฐานหลักว่า residuals เป็น white
noise แสดงว่าแบบจำลอง AR(3) เพียงพอในการอธิบาย
*d**i**f**f*<sub>*t*</sub>*h*<sub>*u*</sub>*s*

นอกจากนี้เราสามารถคำนวณค่า p-value โดยใช้คำสั่ง `pchisq(value, df)`
สำหรับคำนวณหา CDF ไปยังจุด value และสามารถคำนวณพื้นที่ปลายหางด้วย 1-CDF
ก็จะเป็นค่า p-value

ในกรณีนี้ p-value = 0.12 \> *α* (=0.05)
เราไม่สามารถปฏิเสธสมมุติฐานหลักที่ว่า residuals เป็น white noise

``` r
adqtest<-Box.test(m1$residuals, lag=12, type="Ljung")
adqtest
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  m1$residuals
    ## X-squared = 14.009, df = 12, p-value = 0.3002

``` r
#calculate p-value with df=12-3
pv<-1-pchisq(adqtest$statistic, 9)
pv
```

    ## X-squared 
    ## 0.1220215

# ตัวอย่างที่ 3.5 การพยากรณ์

เราสามารถพยากรณ์ได้ด้วยคำสั่ง `predict` และเลือกแบบจำลอง `m1`
และเวลาที่จะพยากรณ์ไปข้างหน้า `n.ahead=12`

``` r
predict(m1, n.ahead=12)
```

    ## $pred
    ## Time Series:
    ## Start = 325 
    ## End = 336 
    ## Frequency = 1 
    ##  [1] 0.6315599 0.7591960 0.8838012 0.9900253 1.0861380 1.1775686 1.2653645
    ##  [8] 1.3493145 1.4293392 1.5055784 1.5782294 1.6474738
    ## 
    ## $se
    ## Time Series:
    ## Start = 325 
    ## End = 336 
    ## Frequency = 1 
    ##  [1] 0.5526068 0.9192767 1.1561316 1.3212131 1.4504817 1.5586128 1.6512076
    ##  [8] 1.7312090 1.8008091 1.8617626 1.9154472 1.9629439

เราสามารถเปรียบเทียบความสามารถในการพยากรณ์โดยใช้การพยากรณ์ออกไปนอกช่วย
(out-of-sample) ซึ่งเรามีข้อมูลทั้งหมด 324 ตัวอย่าง
หากเราแบ่งข้อมูลเป็นช่วงประมาณค่า (1-312) และใช้ช่วง (313-324)
สำหรับเปรียบเทียบ

``` r
length(int$diff_th_us)
```

    ## [1] 324

``` r
m3<-arima(int$diff_th_us[1:312], order=c(3,0,0))
m3.prediction<-predict(m3, n.ahead=12)
#comparing real and prediction series
ts.plot(int$diff_th_us)
points(m3.prediction$pred, col="blue", type="l")
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

# ตัวอย่างที่ 3.6 การสร้างแบบจำลองสำหรับผลได้ตอบแทนรายเดือนจาก SET โดยใช้แบบจำลอง MA(q)

เราพิจารณาข้อมูลผลได้ตอบแทนรายเดือนของการลงทุนในตลาดหลักทรัพย์แห่งประเทศไทย
ราคาปิดรายเดือนระหว่างเดือนเมษายน 2518 ถึงพฤศจิกายน 2555

``` r
mset <- read.csv("https://raw.githubusercontent.com/chaleampong/finecono/master/mset.csv", header = FALSE)
head(mset)
```

    ##       V1
    ## 1 100.00
    ## 2  89.98
    ## 3  91.64
    ## 4  98.02
    ## 5  98.39
    ## 6  92.10

``` r
ret<-diff(log(mset$V1))
ts.plot(ret)
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## พิจารณา ACF และ PACF

จาก ACF จะเห็นได้ว่า ACF แตกต่างจากศูนย์ที่ lag = 1,2
และไม่แตกต่างจากศูนย์ที่ lag ตั้งแต่ 3 เป็นต้นไป
ดังนั้นแบบจำลองที่เหมาะสมน่าจะเป็น MA(2)

``` r
library(forecast)
par(mfrow=c(1,2))
Acf(ret)
pacf(ret)
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## ประมาณค่าแบบจำลอง MA(2)

``` r
m1<- arima(ret, order = c(0,0,2))
m1
```

    ## 
    ## Call:
    ## arima(x = ret, order = c(0, 0, 2))
    ## 
    ## Coefficients:
    ##          ma1     ma2  intercept
    ##       0.0886  0.1001     0.0057
    ## s.e.  0.0469  0.0487     0.0046
    ## 
    ## sigma^2 estimated as 0.006795:  log likelihood = 485.64,  aic = -963.28

## พิจารณาว่าแบบจำลองเพียงพอที่จะอธิบายตัวแปรหรือไม่

เนื่องจากค่า chi-square = 13.608 ซึ่งน้อยกว่า critical chi-square
(df=12-2=10)=18.31 เราไม่สามารถปฎิเสธสมมุติฐานหลักที่ว่า residuals เป็น
white noise แสดงว่าแบบจำลองเพียงพอในการอธิบาย ret

``` r
Box.test(m1$residuals, lag=12, type="Ljung")
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  m1$residuals
    ## X-squared = 13.608, df = 12, p-value = 0.3265

``` r
#calculate p-value with df=12-2
pv<-1-pchisq(13.608, 12-2)
pv
```

    ## [1] 0.1916343

``` r
# Box test with m-q degree of freedom
Box.test(m1$residuals, lag=12, type="Ljung", fitdf=2)
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  m1$residuals
    ## X-squared = 13.608, df = 10, p-value = 0.1917

# รูปที่ 3.10 ACF and PACF

``` r
par(mfrow=c(1,2))
plot(ARMAacf(ar=0.9, ma=-0.5, 20), type="h", ylab="ACF", xlab="Lag")
plot(ARMAacf(ar=0.9, ma=-0.5, 20, pacf=TRUE), type="h", ylab="PACF", xlab="Lag")
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

# ตัวอย่างที่ 3.8

เราพิจารณาผลได้ตอบแทนของการซื้อขายเงินตราต่างประเทศโดยคำนวณจากอัตราแลกเปลี่ยนบาทต่อดอลลาร์สหรัฐ
(thbusd) เราสามารถนำเข้าข้อมูลและคำนวณผลได้ตอบแทนดังนี้

``` r
exc<- read.csv("https://raw.githubusercontent.com/chaleampong/finecono/main/thbusd.csv", header = TRUE)
head(exc)
```

    ##   Jul.Day YYYY.MM.DD Wdy THB.USD
    ## 1 2450816   1/2/1998 Fri  48.023
    ## 2 2450819   1/5/1998 Mon  50.366
    ## 3 2450820   1/6/1998 Tue  52.665
    ## 4 2450821   1/7/1998 Wed  52.203
    ## 5 2450822   1/8/1998 Thu  53.957
    ## 6 2450823   1/9/1998 Fri  53.204

``` r
excret <- diff(log(exc$THB.USD))
ts.plot(excret, main = "Exchange rate return")
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## พิจารณา ACF และ PACF

จากการพิจารณาจะเห็นได้ว่า ACF และ PACF มีค่าที่แตกต่างจากศูนย์ที่ lag
ค่อนข้างยาว แบบจำลองที่เหมาะสมควรจะเป็น ARMA(p,q)

``` r
par(mfrow=c(1,2))
Acf(excret)
pacf(excret)
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## การใช้ model selection ในการเลือกแบบจำลอง ARMA(p,q) ที่เหมาะสม

เราสามารถใช้ function `auto.arima` ใน package `forecast` เพื่อใช้ model
selection criteria เช่น AIC หรือ BIC ในการเลือกแบบจำลองที่เหมาะสม

เมื่อใช้เกณฑ์ AIC จะพบว่าแบบจำลองที่เหมาะสมคือ ARMA(4,1)

``` r
library(forecast)
excret_mod<-auto.arima(excret, d=0, D=0, max.p=6, max.q= 6, ic = c("aic"), stepwise= FALSE, trace = TRUE)
```

    ## 
    ##  Fitting models using approximations to speed things up...
    ## 
    ##  ARIMA(0,0,0) with zero mean     : -27687.05
    ##  ARIMA(0,0,0) with non-zero mean : -27686.59
    ##  ARIMA(0,0,1) with zero mean     : -27685.47
    ##  ARIMA(0,0,1) with non-zero mean : -27684.98
    ##  ARIMA(0,0,2) with zero mean     : -27686.42
    ##  ARIMA(0,0,2) with non-zero mean : -27686.04
    ##  ARIMA(0,0,3) with zero mean     : -27685.03
    ##  ARIMA(0,0,3) with non-zero mean : -27684.61
    ##  ARIMA(0,0,4) with zero mean     : -27684.86
    ##  ARIMA(0,0,4) with non-zero mean : -27684.53
    ##  ARIMA(0,0,5) with zero mean     : -27686.83
    ##  ARIMA(0,0,5) with non-zero mean : -27686.36
    ##  ARIMA(1,0,0) with zero mean     : -27750.99
    ##  ARIMA(1,0,0) with non-zero mean : -27750.89
    ##  ARIMA(1,0,1) with zero mean     : -27796.91
    ##  ARIMA(1,0,1) with non-zero mean : -27797.65
    ##  ARIMA(1,0,2) with zero mean     : -27795.67
    ##  ARIMA(1,0,2) with non-zero mean : -27796.47
    ##  ARIMA(1,0,3) with zero mean     : -27795.46
    ##  ARIMA(1,0,3) with non-zero mean : -27796.17
    ##  ARIMA(1,0,4) with zero mean     : -27793.6
    ##  ARIMA(1,0,4) with non-zero mean : -27794.28
    ##  ARIMA(2,0,0) with zero mean     : -27809.99
    ##  ARIMA(2,0,0) with non-zero mean : -27810.46
    ##  ARIMA(2,0,1) with zero mean     : -27852.21
    ##  ARIMA(2,0,1) with non-zero mean : -27852.38
    ##  ARIMA(2,0,2) with zero mean     : -27853.44
    ##  ARIMA(2,0,2) with non-zero mean : -27853.85
    ##  ARIMA(2,0,3) with zero mean     : -27852.53
    ##  ARIMA(2,0,3) with non-zero mean : -27853.02
    ##  ARIMA(3,0,0) with zero mean     : -27809.06
    ##  ARIMA(3,0,0) with non-zero mean : -27809.4
    ##  ARIMA(3,0,1) with zero mean     : -27831.91
    ##  ARIMA(3,0,1) with non-zero mean : -27831.87
    ##  ARIMA(3,0,2) with zero mean     : -27855.9
    ##  ARIMA(3,0,2) with non-zero mean : -27856.58
    ##  ARIMA(4,0,0) with zero mean     : -27843.36
    ##  ARIMA(4,0,0) with non-zero mean : -27844.24
    ##  ARIMA(4,0,1) with zero mean     : -27876.06
    ##  ARIMA(4,0,1) with non-zero mean : -27876.82
    ##  ARIMA(5,0,0) with zero mean     : -27850.55
    ##  ARIMA(5,0,0) with non-zero mean : -27851.11
    ## 
    ##  Now re-fitting the best model(s) without approximations...
    ## 
    ## 
    ## 
    ## 
    ##  Best model: ARIMA(4,0,1) with non-zero mean

``` r
excret_mod
```

    ## Series: excret 
    ## ARIMA(4,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##           ar1      ar2      ar3      ar4     ma1    mean
    ##       -0.5876  -0.0238  -0.0066  -0.0227  0.5981  -1e-04
    ## s.e.   0.2641   0.0196   0.0208   0.0199  0.2636   1e-04
    ## 
    ## sigma^2 estimated as 3.437e-05:  log likelihood=13849.44
    ## AIC=-27684.88   AICc=-27684.85   BIC=-27641.33

# ตัวอย่างที่ 3.9 การทดสอบ Unit root ด้วย ADF

ทดสอบยูนิทรูทของ SET index รายวัน เราสามารถทดสอบได้ด้วย package *urca* ,
*stats*, *tseries* ในที่นี้เราจะใช้ *urca*

``` r
setd <- read.csv("https://raw.githubusercontent.com/chaleampong/finecono/main/setdaily.csv", header = TRUE)
head(setd)
```

    ##   X      date  index
    ## 1 1  1/5/1998 366.18
    ## 2 2  1/6/1998 370.27
    ## 3 3  1/7/1998 370.31
    ## 4 4  1/8/1998 360.17
    ## 5 5  1/9/1998 349.67
    ## 6 6 1/12/1998 339.17

``` r
ts.plot(setd$index)
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
Acf(setd$index)
```

![](chapter3_example_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

เราสามารถทดสอบ unit root ด้วยคำสั่ง `ur.df` จาก package `urca`
ซึ่งเป็นการทดสอบ Dickey Fuller Unit root
โดยเรากำหนดตัวแปรที่ต้องการทดสอบ `setd$index`
แล้วเลือกรูปแบบของสมการที่เราใช้ทดสอบซึ่งสามารถเลือกได้
เป็นสมการที่ไม่มีค่าคงที่ (none) มีค่าคงที่ (drift) และมีเส้นแนวโน้ม
(trend) ด้วยการระบุใน type เช่น `type=c("trend"`) แล้วเลือกว่าจะมี lag
ของตัวแปรตามเท่ากับเท่าใด ซึ่งมีทางเลือกในการระบุจำนวนเลย เช่น `lags=1`
หรือใช้ model selection criteria เช่น `selectlags = c("AIC")`

ในกรณีแรกที่ระบุ lag=1 เราเก็บผลไว้ในชื่อ `setd.df` ค่าสถิติที่ได้คือ
-1.863 ซึ่งมากกว่า critical value ที่ *α* = 0.05 เท่ากับ -3.41 ดังนั้น
เราไม่สามารถปฏิเสธสมมุติฐานที่ว่า setd เป็น unit root

``` r
library(urca)
setd.df<-ur.df(setd$index, type=c("trend"), lags=1 )
summary(setd.df)
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression trend 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -108.380   -4.745   -0.105    4.837   74.125 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  0.4325591  0.4122648   1.049  0.29414   
    ## z.lag.1     -0.0021279  0.0011419  -1.864  0.06247 . 
    ## tt           0.0006223  0.0002840   2.191  0.02851 * 
    ## z.diff.lag   0.0478816  0.0165720   2.889  0.00388 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.355 on 3636 degrees of freedom
    ## Multiple R-squared:  0.003526,   Adjusted R-squared:  0.002704 
    ## F-statistic: 4.289 on 3 and 3636 DF,  p-value: 0.004982
    ## 
    ## 
    ## Value of test-statistic is: -1.8635 2.3752 2.4005 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau3 -3.96 -3.41 -3.12
    ## phi2  6.09  4.68  4.03
    ## phi3  8.27  6.25  5.34

``` r
setd.df2<-ur.df(setd$index, type = c("trend"), selectlags = c("AIC"))
summary(setd.df2)
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression trend 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -108.380   -4.745   -0.105    4.837   74.125 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  0.4325591  0.4122648   1.049  0.29414   
    ## z.lag.1     -0.0021279  0.0011419  -1.864  0.06247 . 
    ## tt           0.0006223  0.0002840   2.191  0.02851 * 
    ## z.diff.lag   0.0478816  0.0165720   2.889  0.00388 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.355 on 3636 degrees of freedom
    ## Multiple R-squared:  0.003526,   Adjusted R-squared:  0.002704 
    ## F-statistic: 4.289 on 3 and 3636 DF,  p-value: 0.004982
    ## 
    ## 
    ## Value of test-statistic is: -1.8635 2.3752 2.4005 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau3 -3.96 -3.41 -3.12
    ## phi2  6.09  4.68  4.03
    ## phi3  8.27  6.25  5.34

## การทดสอบด้วย Phillips-Perron

``` r
setd.pp<-ur.pp(setd$index, type="Z-tau", model="trend")
summary(setd.pp)
```

    ## 
    ## ################################## 
    ## # Phillips-Perron Unit Root Test # 
    ## ################################## 
    ## 
    ## Test regression with intercept and trend 
    ## 
    ## 
    ## Call:
    ## lm(formula = y ~ y.l1 + trend)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -108.674   -4.712   -0.062    4.765   68.930 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 1.4901935  0.7300869   2.041   0.0413 *  
    ## y.l1        0.9980139  0.0011418 874.061   <2e-16 ***
    ## trend       0.0005988  0.0002841   2.108   0.0351 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.363 on 3638 degrees of freedom
    ## Multiple R-squared:  0.9987, Adjusted R-squared:  0.9987 
    ## F-statistic: 1.42e+06 on 2 and 3638 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Value of test-statistic, type: Z-tau  is: -1.8587 
    ## 
    ##            aux. Z statistics
    ## Z-tau-mu              1.7473
    ## Z-tau-beta            2.1804
    ## 
    ## Critical values for Z statistics: 
    ##                      1pct      5pct     10pct
    ## critical values -3.966098 -3.413711 -3.128565

# ตัวอย่างที่ 3.11 การสร้างแบบจำลอง ARIMA(p,1,q)

จากตัวอย่างที่ผ่านมาเราทราบว่า setd$index เป็น unit root
เราแปลงข้อมูลด้วยการหาผลต่าง (first difference) แล้วทดสอบ unit root

``` r
setd.df.diff<-ur.df(diff(setd$index), type=c("trend"), selectlags=c("AIC"))
summary(setd.df.diff)
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression trend 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -108.598   -4.722   -0.124    4.877   74.206 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.0698727  0.3102897  -0.225   0.8218    
    ## z.lag.1     -0.9149379  0.0228892 -39.972   <2e-16 ***
    ## tt           0.0001630  0.0001477   1.104   0.2697    
    ## z.diff.lag  -0.0402991  0.0165759  -2.431   0.0151 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.353 on 3635 degrees of freedom
    ## Multiple R-squared:  0.4775, Adjusted R-squared:  0.4771 
    ## F-statistic:  1107 on 3 and 3635 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Value of test-statistic is: -39.9724 532.5983 798.8972 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau3 -3.96 -3.41 -3.12
    ## phi2  6.09  4.68  4.03
    ## phi3  8.27  6.25  5.34

จากค่าสถิติเท่ากับ -39.97 ซึ่งน้อยกว่าค่า CV (-3.41)
เราสามารถปฏิเสธสมมุติฐาน และสรุปว่า setd$index เป็น I(1)

แบบจำลองที่เหมาะสมกับ setd คือแบบจำลองที่ d=1
ดังนั้นเรากำหนดค่าดังกล่าวในคำสั่ง `auto.arima` และให้คำสั่งทดลองหา p, q
ที่เหมาะสม ซึ่งในกรณีนี้ p=0 และ q=0

``` r
library(forecast)
setd_mod<-auto.arima(setd$index, d=1, max.p=6, max.q=6 , ic= c("bic"), stepwise = FALSE, trace = TRUE)
```

    ## 
    ##  Fitting models using approximations to speed things up...
    ## 
    ##  ARIMA(0,1,0)                    : 26628.85
    ##  ARIMA(0,1,0) with drift         : 26634.47
    ##  ARIMA(0,1,1)                    : 26629.41
    ##  ARIMA(0,1,1) with drift         : 26635.24
    ##  ARIMA(0,1,2)                    : 26630.26
    ##  ARIMA(0,1,2) with drift         : 26636.28
    ##  ARIMA(0,1,3)                    : 26638.17
    ##  ARIMA(0,1,3) with drift         : 26644.16
    ##  ARIMA(0,1,4)                    : 26646.04
    ##  ARIMA(0,1,4) with drift         : 26651.97
    ##  ARIMA(0,1,5)                    : 26654.13
    ##  ARIMA(0,1,5) with drift         : 26660.03
    ##  ARIMA(1,1,0)                    : 26629.57
    ##  ARIMA(1,1,0) with drift         : 26635.44
    ##  ARIMA(1,1,1)                    : 26634.88
    ##  ARIMA(1,1,1) with drift         : 26640.9
    ##  ARIMA(1,1,2)                    : 26639.11
    ##  ARIMA(1,1,2) with drift         : 26645.13
    ##  ARIMA(1,1,3)                    : 26647.14
    ##  ARIMA(1,1,3) with drift         : 26653.14
    ##  ARIMA(1,1,4)                    : Inf
    ##  ARIMA(1,1,4) with drift         : Inf
    ##  ARIMA(2,1,0)                    : 26632.56
    ##  ARIMA(2,1,0) with drift         : 26638.62
    ##  ARIMA(2,1,1)                    : 26640.4
    ##  ARIMA(2,1,1) with drift         : 26646.42
    ##  ARIMA(2,1,2)                    : 26648.05
    ##  ARIMA(2,1,2) with drift         : 26654.05
    ##  ARIMA(2,1,3)                    : Inf
    ##  ARIMA(2,1,3) with drift         : Inf
    ##  ARIMA(3,1,0)                    : 26639.82
    ##  ARIMA(3,1,0) with drift         : 26645.77
    ##  ARIMA(3,1,1)                    : 26646.41
    ##  ARIMA(3,1,1) with drift         : 26652.19
    ##  ARIMA(3,1,2)                    : Inf
    ##  ARIMA(3,1,2) with drift         : 26640.92
    ##  ARIMA(4,1,0)                    : 26647.17
    ##  ARIMA(4,1,0) with drift         : 26652.99
    ##  ARIMA(4,1,1)                    : 26654.8
    ##  ARIMA(4,1,1) with drift         : 26660.58
    ##  ARIMA(5,1,0)                    : 26655.22
    ##  ARIMA(5,1,0) with drift         : 26660.96
    ## 
    ##  Now re-fitting the best model(s) without approximations...
    ## 
    ## 
    ## 
    ## 
    ##  Best model: ARIMA(0,1,0)

``` r
setd_mod
```

    ## Series: setd$index 
    ## ARIMA(0,1,0) 
    ## 
    ## sigma^2 estimated as 87.77:  log likelihood=-13312.56
    ## AIC=26627.12   AICc=26627.12   BIC=26633.32
