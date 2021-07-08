#必要であれば、tidyverseのライブラリをインストールする
# install.packages("tidyverse")
library(tidyverse)

### 1.仮想データの作成 ###

# 私的価値xは一様分布
# 入札者は全て2人ずつ

# オークション回数を設定する（適宜、変更する）
count_auction = 1000000

# オークション回数、入札者数、入札者iの私的価値
df1 <- data.frame(auction = c(1:count_auction),
                  num = as.numeric(2),
                  xi = runif(count_auction))

# 自分以外のjの私的価値
df2 <- data.frame(xj = runif(100000))

# 各行での最大値（最大の順序統計量、今回はxjと同じ?）
df2$y1 <- apply(df2, 1, max)
summary(df2)
hist(df2$y1)

# くっつける
df <- cbind(df1,df2)
head(df)

# i,jそれぞれの入札額b、落札額p
bx <- Vectorize(b)
library(tidyverse)
df <- df %>% 
  mutate(bi = bx(.$xi)) %>% 
  mutate(bj = bx(.$xj)) %>% 
  mutate(p = ifelse(bi>bj, bi, bj))

# 確認
head(df)

###########

### 2.入札関数を求める ###

# 一様分布の累積分布関数:F(x)
Large_f <- function(x){
  punif(x)
}

# 第1順序統計量の累積分布関数:G(x)=F(x)^(N-1)
G <- function(x){
  Large_f(x) ^ (ncol(df2)-1)
}

# xをtに置き換える:G(t)
t <- function(t){
  G(t)
}

# 関数tを変数tについて0からxまで積分
z <- function(x){
  integrate(t, 0, x)
}

# G(x)とG(t)を入札関数の式に代入
b <- function(x){
  Z <- z(x)
  b <- x - Z$value/G(x)
  return(b)
}

# xに値を入れて確認
# z(0.2)
# b(0.2)


###########

### 3.シミュレーション ###

# 2000個の私的価値のデータを一つに
B <- data.frame(bij = c(df$bi,df$bj))

# 入札額の累積分布関数をノンパラメトリックに推定
H <- function(b, M=100000, N=2, bij=B$bij){
  sum(ifelse(bij<=b, 1, 0))/(M*N)
}
#  b=0.1のとき
# print(H(0.1))

# 確率密度関数を数値微分によって求める
h<- function(b, e=1e-4){
  ((H(b + e)-H(b - e))/(2*e))
}

# bに値を入れて確認
h(0.1)

# 私的価値の推定
x <- function(b, N=2){
  b + H(b)/((N-1)*h(b))
}

# bに値を入れて確認
x(0.1)

###########
# Macの場合は、フォントファミリーを指定
# par(family= "HiraKakuProN-W3")

# 入札額の確率密度関数のグラフを作成
h1 <- Vectorize(h)
plot(h1, xlim=c(0,1), main="確率密度関数", xlab="b", ylab="h(b)")

# 私的価値の確率密度関数のグラフを作成
x1 <- Vectorize(x)
plot(x1, xlim=c(0,1), ylim=c(0,1), main="私的価値", xlab="b", ylab="x")


###########
