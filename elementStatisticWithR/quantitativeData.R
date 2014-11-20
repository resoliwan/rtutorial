#http://www.r-tutor.com/elementary-statistics/quantitative-data
#quantitiveData also knows as continuos data, consist of numeric data that suppot arithmetic operation
# data - faithful is a collection of observations of the Old Faithful geyser in the USA YellowStone National Park
#[,1]	 eruptions	 numeric	 Eruption time in mins
#[,2]	 waiting	 numeric	 Waiting time to next eruption (in mins)
#   eruptions waiting
# 1     3.600      79
# 2     1.800      54
# 3     3.333      74
# 4     2.283      62
# 5     4.533      85
# 6     2.883      55

head(faithful)
nrow(faithful)
?faithful

#Frequency distribution of Quantitative Data
#Problem find the frequency distribution of the eruption duration of faithful
#1.find the range of eruption duration
duration = faithful$eruptions
range(duration)

#2.해당 컬랙션의 빈도수를 나타내기위해서 동일한 범위를 같는 break point 를 만든다. range 가 1.6~ 5.1 임으로 0.5 만큼 증가하는 breaks 를 만들자
breaks = seq(1.5, 5.5, by=0.5)
breaks
?seq

#3.classify the eruption duration accoring to the half-unit-length sub-intervals with cut
#logical, indicating if the intervals should be closed on the right
# 닫힘은 <= 을 의미 열림은 < 의미
#[1.5,2) [2,2.5) [2.5,3) [3,3.5) [3.5,4) [4,4.5) [4.5,5) [5,5.5) 위와 같은 구간 right = FALSE
#cut(c(3), breaks, right=FALSE)  ==> [3,3.5) 3은 [3,3.5) 좌측이 닫힌 구간이기 때문에 3 <= x < 3.5 구간에 포함
#(1.5,2] (2,2.5] (2.5,3] (3,3.5] (3.5,4] (4,4.5] (4.5,5] (5,5.5] right=TRUE
#cut(c(3), breaks, right=TRUE) ==> (2.5,3] 3은 (2.5,3] 오른쪽이 닫혀있기 때문에 2.5 < x <= 3 구간에 포함

duration.cut = cut(duration, breaks, right=FALSE)
duration.cut
?cut

#4.compute the frequency of eruption in each sub-interval
duration.freq = table(duration.cut)
duration.freq
cbind(duration.freq)

#exercise
#1.find the frequency distribution of the erqution waiting periods in faithful
waiting = faithful$waiting
head(waiting)
range(waiting)
table(waiting) # 값의 범위가 정수인지 소수인지 확인
break2 = seq(40, 100, by = 5) # 정수이기때문에 5의 단위로 구분
waiting.cut = cut(waiting, break2, right = FALSE)
waiting.freq = table(waiting.cut)
waiting.freq

#histogram a histogram consists of parallel vertical bars that graphically show the frequency distribution of a quantitive variable. the area of each bar equal to the frequency
#of itmes found in each class

#find the histogram of the eruption duration in faithful
duration = faithful$eruptions
hist(duration, right = FALSE)

colors = c("red","yellow","green","violet","orange","blue","pink","cyan")
hist(duration, right = FALSE, col = colors, main = "Old Faithful Eruption", xlab = "Duration minutes")

#exercise
#find the histogram of the eruption waiting period in faithful
period = faithful$waiting
hist(period, right = FALSE, col = colors, main = "Old Faithful Waiting", xlab = "Duration minutes")

#relative Frequency Distribution of Quantitative data
# relative frequency distribution of a data variable is summary of the frequency propotion in a collection of none-overlapping categories
# relative frequecnty = frequency/ totCount

#Problem
# find the relative frequency distribution of the eruption duration in faithful
duration = faithful$eruptions
range(duration)
breaks = seq(1.5, 5.5, by = 0.5)
breaks
?cut
duration.cut = cut(duration, breaks, right = FALSE)
duration.cut
head(duration)
cut(head(duration), breaks, right = FALSE)

duration.freq = table(duration.cut)
duration.freq
duration.relfreq = duration.freq / nrow(faithful)
cbind(duration.relfreq)
# display option 을 변경해서 소수 2쨰짜리까지 보기
old = options(digits = 1)
duration.relfreq
# 원상태로 복구하기
options(old)

old = options(digits = 1)
#결합해서 두개를 볼수 있다.
cbind(duration.freq, duration.relfreq)
options(old)

#exercise
# find the relative frequency distribution of the eruption waiting period in faithful
head(faithful)
head(faithful$waiting)
waiting = faithful$waiting
range(waiting)
table(waiting)
breaks = seq(40, 100, by = 10)
breaks
waiting.cut = cut(waiting, breaks, right = FALSE)
waiting.cut
waiting.freq = table(waiting.cut)
waiting.freq
waiting.relfreq = waiting.freq / nrow(faithful)
waiting.relfreq
cbind(waiting.relfreq)
cbind(waiting.freq, waiting.relfreq)

#Cumulative Frequency Distribution
# the cumulative frequency distribution of a quantitative variable is a summary of data frequency below a given level
# shows the total number of eruptions whose duration are less than or equal to a set of chosen level

#Problem
#find the cumulative distribution of the eruption duration in faithful
duration = faithful$eruption
range(duration)
breaks = seq(1.5, 5.5, by = 0.5)
duration.cut = cut(duration, breaks, right = FALSE)
duration.freq = table(duration.cut)
duration.freq
duration.cumfreq = cumsum(duration.freq)
duration.cumfreq

#find the cumulative distribution of the eruption waiting period in faithful
waiting = faithful$waiting
breaks = seq(40,100, by = 10)
waiting.cut = cut(waiting, breaks, right = FALSE)
waiting.freq = table(waiting.cut)
waiting.freq
waiting.cumfreq = cumsum(waiting.freq)
waiting.cumfreq

#Cumlative frequency graph
duration = faithful$eruption
breaks = seq(1.5, 5.5, by = 0.5)
duration.cut = cut(duration, breaks, right = FALSE)
duration.freq = table(duration.cut)
# graph가 0에서 부터 시작하게 0을 앞에 더한다.
duration.cumfreq0 = c(0, cumsum(duration.freq))
duration.cumfreq0
#graph 를 뛰운다.
plot(breaks, duration.cumfreq0, main = "Old faithful Eruption", xlab = 'Duration minutes', ylab = "Cumlative eruptions")
# plot 을 먼저 뛰우지 않으면 에러난다.
lines(breaks, duration.cumfreq0)

#exercise
#find the cumulative frequency graph of the eruption waiting periods in faithful
waiting = faithful$waiting
breaks = seq(40, 100, by = 10)
waiting.cut = cut(waiting, breaks, right = FALSE)
waiting.freq = table(waiting.cut)
waiting.cumsum = cumsum(waiting.freq)
waiting.cumsum0 = c(0, waiting.cumsum)
plot(breaks, waiting.cumsum0, main = "Old faithful eruption waiting", xlab = "Waiting Duration minutes", ylab = "Cumulative eruption waiting")
lines(breaks, waiting.cumsum0)
?plot
?lines

#built in function 을 사용해서 그릴수도 있다.
Fn = ecdf(waiting)
plot(Fn, main = "Old faithful eruption waiting", xlab = "Waiting Duration minutes", ylab = "Cumulative eruption waiting")
?ecdf

#Stem-and-LeafFlot
duration = faithful$eruptions
head(duration)
stem(head(duration))
#  The decimal point is at the |
#
#  1 | 8
#  2 | 39
#  3 | 36
#  4 | 5

#exercise
# find the stem-and-leaf plot for eruption waiting periods in faithful
waiting = faithful$waiting
head(waiting)
stem(head(waiting))

#  The decimal point is 1 digit(s) to the right of the |
# | 오른쪽에 위치한 숫자1자리 이후부터 소수점 시작
#
#  5 | 45
#  6 | 2
#  7 | 49
#  8 | 5

#Scatter plot
#A scatter plot pairs up values of two quantitative variables in a data set and display them as geometric points inside a Catesian diagram

duration = faithful$eruptions
waiting = faithful$waiting
head(cbind(duration, waiting))
plot(duration, waiting, xlab = "Eruption duration", ylab = "Time waitied")

#draw a trend line
#get linear regression
abline(lm(waiting ~ duration))