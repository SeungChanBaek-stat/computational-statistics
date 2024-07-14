# R에서 나타낼 수 있는 가장 큰 양의 정수
.Machine$integer.max

# 양정밀도에서 양의 정수부 최대값, 음의 정수부 최소값
.Machine$double.max.exp
.Machine$double.min.exp

# 양의 무한, 음의 무한
.Machine$double.xmax
2^{1023}
2^{1024}
-2^{1024}

# 0에 가장 가까운 수
.Machine$double.xmin
2^{-1022}
2^{-(1022 + 1)}
2^{-(1022 + 51)}
2^{-(1022 + 52)}
2^{-(1022 + 53)}

# 오버플로우 문제
2^1024 * 2^(-1024)
2^1078 * 2^(-1078)
2^(1024) / 2^1024
2^(0124) - 2^(1024)

# 유효숫자
.Machine$double.eps
2^(-52)

# 두 수의 차이
0.6 - 0.5 == 0.1
0.6 - 0.5
print(0.6-0.5, 16)
print(0.1, 16)

# 탄젠트 예시
print(tan(pi/4), 16)
print(tan(pi/4 + pi), 16)
print(tan(pi/4 + pi*2), 16)
print(tan(pi/4 + pi*3), 16)

# Exercise 1
# 십진수를 이진 소수로 변환하는 함수
decimal_to_binary_fraction <- function(decimal, digits) {
  binary_fraction <- ""
  value <- decimal
  for (i in 1:digits) {
    value <- value * 2
    if (value >= 1) {
      binary_fraction <- paste0(binary_fraction, "1")
      value <- value - 1
    } else {
      binary_fraction <- paste0(binary_fraction, "0")
    }
  }
  return(binary_fraction)
}

# 1/3을 이진 소수로 변환
decimal <- 1 / 3

# 소수점 아래 5자리
binary_fraction_5 <- decimal_to_binary_fraction(decimal, 5)
print(paste("5자리 이진 소수: 0.", binary_fraction_5, sep=""))

# 소수점 아래 52자리
binary_fraction_52 <- decimal_to_binary_fraction(decimal, 52)
print(paste("52자리 이진 소수: 0.", binary_fraction_52, sep=""))



# 수식과 함수의 계산
factorial(100)
factorial(200)

C <- 1
for (k in 1:100){
  C <- C * (100+k) / k
}
C


# exponential 함수
exponential <- function(x){
  sum <-1; temp <- 1; k <- 1
  while (abs(temp) > .Machine$double.eps){
    temp <- temp * x/k
    sum <- sum + temp
    k <- k+1
  }
  return (sum)
}
print(exponential(1), 16)



# 수정된 exponential
exp.1 <- function(x){
  sum <- 1
  temp <- 1
  for (k in 1:100){
    if (abs(temp) < .Machine$double.eps) return (sum)
    temp <- temp * x/k
    sum <- sum + temp
  }
  return ("Computation is incomplete within the limit")
}
print(exp.1(1), 16)
print(exp.1(100), 16)
print(exp.1(1)^100, 16)
print(exp(1), 16)
print(exp(100), 16)




# Exercise 2
sine <- function(x){
  sum <- x; temp <- x; k <- 1
  while (abs(temp) > .Machine$double.eps){
    temp <- temp * (-1) * x/(2*k) * x/(2*k + 1)
    sum <- sum + temp
    k <- k + 1
  }
  return (sum)
}
print(sine(pi/4), 16)
print(1/sqrt(2), 16)



# 1000 X 100 행렬 처리시간
A <- matrix(runif(1000*100), nrow = 1000, ncol = 100)
p.time <- proc.time()
for (k in 1:100){
  r <- rep(0,1000)
  for (i in 1:1000) {
    for (j in 1:100) {
      r[i] <- r[i] + A[i,j]
    }
  }
}
r
proc.time() - p.time


# 1000 X 100 행렬 처리시간 - 벡터연산
p.time <- proc.time()
for (k in 1:100) r <- apply(A,1,sum)
r
proc.time() - p.time


# Exercise 3.
# 1000 X 100 행렬 처리시간
A <- matrix(runif(1000*100), nrow = 1000, ncol = 100)
p.time <- proc.time()
c <- rep(Inf, 100)
for (j in 1:100) {
  for (i in 1:1000) {
    if (A[i,j] < c[j]) {
      c[j] <- A[i,j]
    }
  }
}
c
proc.time() - p.time


# 1000 X 100 행렬 처리시간 - 벡터연산
p.time <- proc.time()
c <- rep(Inf, 100)
c <- apply(A,2,min)
c
proc.time() - p.time


# 정밀도를 늘리는 방법
sqrt(2)
print(sqrt(2), 15)

install.packages("Rmpfr")
library(Rmpfr)
num <- mpfr(2, 105)
sqrt_2 <- sqrt(num)
sqrt_2

print(sqrt_2, 31)
Const("pi", 105)
