Introduction to Financial Econometrics บทที่ 1
================
เฉลิมพงษ์ คงเจริญ

# ตัวอย่างอนุกรมเวลาทางการเงิน

เราสามารถดาวน์โหลดข้อมูลอนุกรมเวลาทางการเงินจาก Yahoo Finance โดยใช้
package quantmod หรือ tidyquant

## รูปที่ 1.1 ราคาหุ้นธนาคารกรุงเทพ

``` r
library(tidyverse) #for import with read_csv
```

``` r
library(quantmod)
getSymbols("BBL.BK", src="yahoo", from="2009-01-02", to="2020-12-31")
```

    ## [1] "BBL.BK"

``` r
plot(BBL.BK$BBL.BK.Close, main="ราคาปิดหุ้นธนาคารกรุงเทพ")
```

![](chapter1_example_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
p<-BBL.BK$BBL.BK.Close
ret<-diff(log(p))*100
plot(ret, main="ผลตอบแทนรายวันของหุ้นธนาคารกรุงเทพ")
```

![](chapter1_example_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## รูปที่ 1.3 อัตราแลกเปลี่ยนเงินบาทและดอลลาร์สหรัฐรายวัน

ข้อมูลจาก
<https://www.bot.or.th/Thai/Statistics/FinancialMarkets/Pages/StatExchangeRate.aspx>
จัดรูปใหม่เก็บไว้ใน www.github.com/chaleampong/finecono

อ่านข้อมูลใช้ function read_csv แทนที่จะเป็น read.csv

``` r
# Import data from Github
exc <- read_csv("https://raw.githubusercontent.com/chaleampong/finecono/main/exc_d_97_2021.csv")
# change date character to date variable
exc$date<-as.Date(exc$date, format="%d/%m/%Y")
ggplot(data=exc, aes(x=date, y=usd))+geom_line()
```

![](chapter1_example_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# calculate return and add to dataframe
exc$ret_usd<-c(NA, diff(log(exc$usd)))
ggplot(data=exc, aes(x=date, y=ret_usd))+geom_line()
```

![](chapter1_example_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

## รูปที่ 1.5 อัตราดอกเบี้ยพันธบัตรรัฐบาล

ข้อมูลจาก
<https://www.bot.or.th/App/BTWS_STAT/statistics/BOTWEBSTAT.aspx?reportID=223&language=TH>

จัดรูปใหม่เก็บไว้ใน www.github.com/chaleampong/finecono

plot อัตราดอกเบี้ยที่ระยะเวลาไถ่ถอน (maturity) ต่างกัน

``` r
intrate<-read_csv("https://raw.githubusercontent.com/chaleampong/finecono/main/intrate_m_05_21.csv")
intrate$month<-as.Date(intrate$month, format="%m/%d/%Y")
ggplot(data=intrate, aes(x=month, y=value, color=Maturity))+geom_line(aes(x=month,y=tbond_1y, col='1-year '))+geom_line(aes(x=month,y=tbond_3y, col='3-year'))+ geom_line(aes(x=month,y=tbond_5y, col='5-year'))
```

![](chapter1_example_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# ตัวอย่างคำนวณผลได้ตอบแทนด้วยโปรแกรม R

## ตัวอย่าง 1.6

การคำนวณผลได้ตอบแทนด้วยโปรแกรม R ใช้ข้อมูลรายวันของหลักทรัพย์ ปตท. (PTT)
ระหว่างเดือน มกราคม 2002 ถึง ธันวาคม 2019

``` r
ptt<-read.csv("https://raw.githubusercontent.com/chaleampong/finecono/main/ptt_d_02_19.csv")
head(ptt)
```

    ##        Date Price
    ## 1  1/2/2002 3.425
    ## 2  1/4/2002 3.500
    ## 3  1/7/2002 3.475
    ## 4  1/8/2002 3.500
    ## 5  1/9/2002 3.475
    ## 6 1/10/2002 3.500

``` r
plot(ptt$Price, type="l")
```

![](chapter1_example_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
n<-nrow(ptt)   #จำนวนตัวอย่าง
# คำนวณ simple net return
ptt.ret<-((ptt$Price[2:n])-ptt$Price[1:n-1])*100/ptt$Price[1:n-1]
head(ptt.ret)
```

    ## [1]  2.1897810 -0.7142857  0.7194245 -0.7142857  0.7194245  0.0000000

``` r
# คำนวณ log return
ptt.lret<-diff(log(ptt$Price))*100
head(ptt.lret)
```

    ## [1]  2.1661497 -0.7168489  0.7168489 -0.7168489  0.7168489  0.0000000

## Download CSV from Yahoo Finance

``` r
PTT.BK <- read_csv("C:/Users/chale/Downloads/PTT.BK.csv")
head(PTT.BK)
```

    ## # A tibble: 6 x 7
    ##   Date        Open  High   Low Close `Adj Close`     Volume
    ##   <date>     <dbl> <dbl> <dbl> <dbl>       <dbl>      <dbl>
    ## 1 2001-12-06  3.8   3.82  3.55  3.58        1.60 1736808000
    ## 2 2001-12-07  3.62  3.62  3.55  3.55        1.59  412533000
    ## 3 2001-12-10  3.55  3.55  3.55  3.55        1.59          0
    ## 4 2001-12-11  3.55  3.55  3.4   3.4         1.52  409158000
    ## 5 2001-12-12  3.42  3.52  3.4   3.5         1.57  307291000
    ## 6 2001-12-13  3.5   3.52  3.5   3.5         1.57  372961000

``` r
ggplot(PTT.BK, aes(x=Date, y=Close_Price))+geom_line(aes(x=Date, y=Close))
```

![](chapter1_example_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
PTT.BK.ret<-diff(log(PTT.BK$Close))
PTT.BK$lret<-c(NA,PTT.BK.ret)
ggplot(PTT.BK, aes(x=Date, y=LogReturn))+geom_line(aes(x=Date, y=lret))
```

![](chapter1_example_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->
