Introduction to Financial Econometrics – ตัวอย่างบทที่ 2
================
เฉลิมพงษ์ คงเจริญ

# ตัวอย่างที่ 2.3 การคำนวณคุณลักษณะทางสถิต และการทดสอบสำหรับผลได้ตอบแทนของหลักทรัพย์ ปตท.

ตัวอย่างนี้ใช้ข้อมูลผลได้ตอบแทนในรูปลอการิทึมจากตัวอย่างในบทที่ 1
ในหัวข้อนี้เราจะคำนวณค่าสถิติเบื้องต้นของ ptt$lret โดยใช้ชุดคำสั่ง
*fBasics* ซึ่งฟังก์ชัน *basicStats*

``` r
# Import Data
ptt <- read.csv("https://raw.githubusercontent.com/chaleampong/EC435/master/ptt_d_02_19.csv")
# Calculate log return
ptt.lret<-diff(log(ptt$Price))*100
library(fBasics)
basicStats(ptt.lret)
```

    ##                ptt.lret
    ## nobs        4389.000000
    ## NAs            0.000000
    ## Minimum      -18.589938
    ## Maximum       14.953173
    ## 1. Quartile   -0.947874
    ## 3. Quartile    1.001681
    ## Mean           0.058170
    ## Median         0.000000
    ## Sum          255.308816
    ## SE Mean        0.029703
    ## LCL Mean      -0.000063
    ## UCL Mean       0.116404
    ## Variance       3.872376
    ## Stdev          1.967835
    ## Skewness      -0.035383
    ## Kurtosis       5.951371

## การคำนวณสถิติเบื้องต้น

เราสามารถใช้ฟังก์ชัน ``` mean``stdev ``` `skewness` และ `kurtosis`
ในการคำนวณหาค่าสถิติเบื้องต้น จาก package `fBasics` และฟังก์ชัน
`basicStats` ในการคำนวณตัวสถิติหลายๆตัวพร้อมกัน

``` r
mean(ptt.lret)
```

    ## [1] 0.05817016

``` r
stdev(ptt.lret)
```

    ## [1] 1.967835

``` r
skewness(ptt.lret)
```

    ## [1] -0.03538271
    ## attr(,"method")
    ## [1] "moment"

``` r
kurtosis(ptt.lret)
```

    ## [1] 5.951371
    ## attr(,"method")
    ## [1] "excess"

``` r
basicStats(ptt.lret)
```

    ##                ptt.lret
    ## nobs        4389.000000
    ## NAs            0.000000
    ## Minimum      -18.589938
    ## Maximum       14.953173
    ## 1. Quartile   -0.947874
    ## 3. Quartile    1.001681
    ## Mean           0.058170
    ## Median         0.000000
    ## Sum          255.308816
    ## SE Mean        0.029703
    ## LCL Mean      -0.000063
    ## UCL Mean       0.116404
    ## Variance       3.872376
    ## Stdev          1.967835
    ## Skewness      -0.035383
    ## Kurtosis       5.951371

## การทดสอบค่าเฉลี่ย

จากค่าสถิติข้างต้น เราสามารถคำนวณค่า
$t = \\frac{0.058}{1.968/\\sqrt{4389}}= 1.95$ หรือใช้คำสั่ง `t.test`

``` r
t.test(ptt.lret)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  ptt.lret
    ## t = 1.9584, df = 4388, p-value = 0.05025
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.0000634752  0.1164037879
    ## sample estimates:
    ##  mean of x 
    ## 0.05817016

## การทดสอบการแจกแจงปกติ

เราใช้ `normalTest` จาก package `fBasics`ในการทดสอบการแจกแจงปกติ
จากค่าสถิติ = 6477.3 \> critical chi-sq ที่ df=2 (5.99) และ p-value \<
0.05 เราสามารถปฏิเสธสมมุติฐานหลักที่ว่า ptt.lret แจกแจงเป็น Normal

``` r
normalTest(ptt.lret, method="jb")
```

    ## 
    ## Title:
    ##  Jarque - Bera Normalality Test
    ## 
    ## Test Results:
    ##   STATISTIC:
    ##     X-squared: 6487.0171
    ##   P VALUE:
    ##     Asymptotic p Value: < 2.2e-16 
    ## 
    ## Description:
    ##  Wed Aug 11 20:51:31 2021 by user: chale

# ความสัมพันธ์ระหว่างตัวแปรในช่วงเวลาที่ต่างกัน

## ตัวอย่างที่ 2.4 การคำนวณสหสัมพันธ์ในตัวเอง (Autocorrelation Function)

จาก sample autocorrelation ที่แสดงใน ACF จะเห็นได้ว่าที่ lag ที่
2,3,6,12,22 มีค่าแตกต่างจากศูนย์อย่างมีนัยสำคัญ

![](chapter2_example_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](chapter2_example_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

## ตัวอย่างที่ 2.5 การทดสอบ Portmanteau หรือการทดสอบ White noise

เมื่อทดสอบด้ว Portmanteau test (Ljung-Box) โดยเลือกพิจารณา lag ย้อนไป 5
periods จะพบว่าค่า p-value \< 0.05 แสดงว่า มี autocorrelation ที่ lag ใด
lag หนึ่งมีค่าไม่เท่ากับศูนย์ แสดงว่าตัวแปรมีความสัมพันธ์กับตัวเองในอดีต

``` r
Box.test(ptt.lret, lag = 5, type = "Ljung-Box")
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  ptt.lret
    ## X-squared = 25.018, df = 5, p-value = 0.0001382
