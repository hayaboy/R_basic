?c()

stu_height <- c(166, 168, 170, 172, 174)

m <- mean(stu_height)

var(stu_height)
sd(stu_height)

#상관과 회귀

#단순 선형 회귀

df <- read.csv(choose.files(), header = TRUE)

cor(df$adv, df$sales)

?lm

linear_model1 <- lm(sales ~ adv , df)

linear_model1


summary(linear_model1)

0.8879209 ^ 2



#다중 선형 회귀

x1<-c(2, 4, 6, 8)

x2<-c(0, 4, 2, 3)

y<-c(81, 93, 91, 97)

# +는 x1과 x2 독립적으로 고려해서 y에 미치는 영향 분석 

# *는 x1과 x2 이 둘의 교호작용 항인 x1:x2를 모두 포함함

linear_model2 <- lm(y ~ x1+x2 )

linear_model2

#교호작용 포함한 모델

linear_model3_1 <- lm(y ~ x1*x2 )

linear_model3_1


linear_model3_2 <- lm(y ~ x1+x2+x1:x2)

linear_model3_2



ls()

rm(list = ls())

#회귀


perch_length<-c(
  8.4, 13.7, 15.0, 16.2, 17.4, 18.0, 18.7, 19.0, 19.6, 20.0,
  21.0, 21.0, 21.0, 21.3, 22.0, 22.0, 22.0, 22.0, 22.0, 22.5,
  22.5, 22.7, 23.0, 23.5, 24.0, 24.0, 24.6, 25.0, 25.6, 26.5,
  27.3, 27.5, 27.5, 27.5, 28.0, 28.7, 30.0, 32.8, 34.5, 35.0,
  36.5, 36.0, 37.0, 37.0, 39.0, 39.0, 39.0, 40.0, 40.0, 40.0,
  40.0, 42.0, 43.0, 43.0, 43.5, 44.0
)


perch_length


perch_weight <- c(5.9, 32.0, 40.0, 51.5, 70.0, 100.0, 78.0, 80.0, 85.0, 85.0,
                  110.0, 115.0, 125.0, 130.0, 120.0, 120.0, 130.0, 135.0, 110.0,
                  130.0, 150.0, 145.0, 150.0, 170.0, 225.0, 145.0, 188.0, 180.0,
                  197.0, 218.0, 300.0, 260.0, 265.0, 250.0, 250.0, 300.0, 320.0,
                  514.0, 556.0, 840.0, 685.0, 700.0, 700.0, 690.0, 900.0, 650.0,
                  820.0, 850.0, 900.0, 1015.0, 820.0, 1100.0, 1000.0, 1100.0,
                  1000.0, 1000.0)



cor(perch_length,perch_weight)




lm_model<-lm(perch_weight ~ perch_length)



summary(lm_model)

test_data<-c( 8.4, 18. , 27.5, 21.3, 22.5, 40. , 30. , 24.6, 39. , 21. , 43.5,
              16.2, 28. , 27.3)

test_data_df<-as.data.frame(test_data)

predict(lm_model, newdata = test_data_df)

