### SYMULACJE -----------------------------------------------------------------

library(MASS)
library(ggplot2)
library(patchwork)

beta = c(3,3)

# ZADANIE 1---------------------------------------------------------------------

fun2 <- function(p = 10000){
  chisq = c()
  alpha = c()
  for (i in 1:p){
    X = matrix(rnorm(1000*2,mean = 0, sd = 1/sqrt(1000)), ncol = 2)
    mu_i = exp(X%*%beta)
    Y = rpois(1000, mu_i) 
    model1 = glm.nb(Y~X-1) 
    model2 = glm(Y~X-1, family = 'poisson')
    chisq[i] = -2 * (logLik(model2) - logLik(model1))
    alpha[i] = 1/summary(model1)$theta
  }
  df = data.frame(chisq = chisq, alpha = alpha) 
  return(df)
}
x = fun2()
n = 10000
bins = round(sqrt(n))

wykres_chisq = ggplot(data = x, aes(x = chisq)) +
  geom_histogram(aes(y = ..density..), bins = 50, colour = 'gray30', fill = 'steelblue') +
  geom_density(colour = 'red', bw = 0.3, linewidth = 1.5) +
  coord_cartesian(ylim = c(0, 0.5))+
  theme_gray() +
  labs(title = expression(paste('Rozkład statystyki ', chi^2)),
       x = expression(paste('Wartości ', chi^2)),
       y = "Gęstość")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12)
  )


sd_est <- quantile(x$alpha, 0.75) / qnorm(0.75)

wykres_alpha = ggplot(data = x, aes(x = alpha)) +
  geom_histogram(aes(y = ..density..), bins = 50, colour = 'gray30', fill = 'skyblue3') +
  stat_function(fun = dnorm, args = list(mean = 0, sd = sd_est), 
                colour = 'red', linewidth = 1.5) +
  coord_cartesian(ylim = c(0, 10.5)) +
  theme_gray() +
  labs(title = expression(paste('Rozkład ', hat(alpha))),
       x = expression(paste('Wartości ', hat(alpha))),
       y = "Gęstość") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12)
  )

 wykres_alpha + wykres_chisq


n <- nrow(x)
test_ALPHA <- rnorm(n, mean = 0, sd = sd_est)

qq_data <- data.frame(
  theoretical = sort(test_ALPHA),
  sample = sort(x$alpha)
)

qqplot_alpha<- ggplot(qq_data, aes(x = theoretical, y = sample)) +
  geom_point(size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red",lwd=1) +
  theme_gray() +
  labs(title = expression(paste("Wykres QQ dla ", hat(alpha))),
       x = "Kwantyle teoretyczne",
       y = "Kwantyle empiryczne") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12))

emp_chisq <- sort(x$chisq)

n <- length(emp_chisq)
mix_theory <- c(rep(0, n/2), qchisq(ppoints(n/2), df = 1))
mix_theory <- sort(mix_theory)

df_qq <- data.frame(
  Theoretical = mix_theory,
  Empirical = emp_chisq
)

qqplot_chisq<-ggplot(df_qq, aes(x = Theoretical, y = Empirical)) +
  geom_point(size= 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "red",lwd=1) +
  theme_gray()+
  labs(
    title = expression(paste("Wykres QQ dla ",chi^2)),
    x = "Kwantyle teoretyczne",
    y = "Kwantyle empiryczne")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12)
  )

qqplot_alpha + qqplot_chisq


### ANALIZA DANYCH -------------------------------------------------------------

# ZADANIE 2 --------------------------------------------------------------------
dane1 = read.csv2("D:/Users/Eweli/OneDrive/Pulpit/Studia/Zaawansowane modele liniowe/Raport3/DebTrivedi.csv", sep = ',', header = TRUE) # pobiernie danych

dane2 = subset(dane1, select = c(ofp, hosp, health, numchron, gender, school, privins))

# ZADANIE 3---------------------------------------------------------------------

ofp = dane2$ofp


bins = round(sqrt(length(ofp)))
wykres_ofp = ggplot(data = dane2, aes(x = ofp))+
  geom_histogram(bins = bins, colour = 'black', fill = 'skyblue') +
  theme_gray() +
  labs(title = 'Wartość zmiennej ofp',
       x = 'Liczba wizyt w gabinecie lekarskim',
       y = "Liczba obserwacji")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12)
  )

wykres_ofp

f_ofp = log(ofp + 0.5) # zmienna pomocnicza
dane2[,8] = f_ofp

# pozostałe zmienne

hosp = dane2$hosp 
health = dane2$health
numchron = dane2$numchron
gender = dane2$gender
school = dane2$school
privins = dane2$privins

library(RColorBrewer)


theme_custom <- theme_gray() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

fill_scale <- scale_fill_brewer(palette = "Pastel2")

# hosp

dane2$hosp2 = ifelse(dane2$hosp >= 3, "3+", as.character(dane2$hosp))

dane2$hosp2 = factor(dane2$hosp2, levels = c("0", "1", "2", "3+"))

boxplot_hosp = ggplot(dane2, aes(x = hosp2, y = f_ofp, fill = hosp2)) +
  geom_boxplot() +
  labs(title = "Liczba hospitalizacji", x = "Liczba pobytów w szpitalu", y = "log(ofp + 0.5)") +
  theme_custom + guides(fill = "none")+
  fill_scale


# health

dane2$health = factor(dane2$health, levels = c("poor", "average", "excellent"), labels = c("Słabe", "Przeciętne", "Doskonałe"))


boxplot_health = ggplot(dane2, aes(x = health, y = f_ofp, fill = health)) +
  geom_boxplot() +
  labs(title = "Subiektywne odczucie pacjentów", x = "Stan zdrowia", y = "log(ofp + 0.5)") +
  theme_custom + guides(fill = "none") +
  fill_scale

boxplot_hosp + boxplot_health 

## numchron

dane2$numchron2 = ifelse(dane2$numchron >= 4, "4+", as.character(dane2$numchron))

dane2$numchron2 = factor(dane2$numchron2, levels = c("0", "1", "2", "3", "4+"))

boxplot_numchron = ggplot(dane2, aes(x = numchron2, y = f_ofp, fill = numchron2)) +
  geom_boxplot() +
  labs(title = "Liczba przewlekłych chorób", x = "Liczba chorób", y = "log(ofp + 0.5)") +
  theme_custom + guides(fill = "none") +
  fill_scale

# gender

dane2$gender = factor(dane2$gender, levels = c("male", "female"), labels = c("Mężczyzna", "Kobieta"))

boxplot_gender = ggplot(dane2, aes(x = gender, y = f_ofp, fill = gender)) +
  geom_boxplot() +
  labs(title = "Płeć pacjenta", x = "Płeć", y = "log(ofp + 0.5)") +
  theme_custom + guides(fill = "none") +
  fill_scale
boxplot_numchron+boxplot_gender

## school


dane2$school <- as.character(dane2$school)

for(i in 1:length(dane2$school)){
  val <- as.numeric(dane2$school[i])
  if (!is.na(val)) {
    if (val %in% 0:7) {
      dane2$school[i] <- "0-7"
    } else if (val == 8) {
      dane2$school[i] <- "8"
    } else if (val %in% 9:11) {
      dane2$school[i] <- "9-11"
    } else if (val == 12) {
      dane2$school[i] <- "12"
    } else if (val %in% 13:18) {
      dane2$school[i] <- "13-18"
    }
  }
}

dane2$school <- factor(dane2$school, levels = c("0-7", "8", "9-11", "12", "13-18"))


boxplot_school = ggplot(dane2, aes(x = school, y = f_ofp, fill = school)) +
  geom_boxplot() +
  labs(x = 'Liczba lat edukacji', y = 'Wartość log(ofp + 0.5)') +
  ggtitle('Wykres pudełkowy liczby lat edukacji') +
  theme_custom + guides(fill = "none") +
  fill_scale

# privins

dane2$privins = factor(dane2$privins, levels = c("no", "yes"), labels = c("Nie", "Tak"))

boxplot_privins = ggplot(dane2, aes(x = privins, y = f_ofp, fill = privins)) +
  geom_boxplot() +
  labs(title = "Dodatkowe ubezpieczenie zdrowotne", x = "Ubezpieczenie", y = "log(ofp + 0.5)") +
  theme_custom + guides(fill = "none") +
  fill_scale
boxplot_school + boxplot_privins

# ZADANIE 4 --------------------------------------------------------------------

dane1 = read.csv2(".../DebTrivedi.csv", sep = ',', header = TRUE) # pobiernie danych

dane3 = subset(dane1, select = c(ofp, hosp, health, numchron, gender, school, privins))

library(pscl)

# 1.POISSON

model_poiss = glm(ofp ~., data = dane3, family = 'poisson')
AIC(model_poiss)
BIC(model_poiss)
# 2.NB

model_nb = glm.nb(ofp ~., data = dane3)
AIC(model_nb) # 24 359.11
BIC(model_nb) # 24 416.62

# 3.ZIPR

model_zipr = zeroinfl(ofp ~., data = dane3)

# 4.ZINBR
model_zinbr = zeroinfl(ofp~., data = dane3, dist = 'negbin')


# 5. Poissona z bariera
model_poiss_hurdle = hurdle(ofp ~., data = dane3)

# 6. ujemny dwumianowy z bariera

model_nb_hurdle = hurdle(ofp~., data = dane3, dist = 'negbin')


# Redukcja i testy

summary(model_poiss) # wszystkie istotne 

summary(model_nb) # wszystkie istotne

summary(model_zipr) # usuwamy health i intercpet

model_zipr_zredukowany = zeroinfl(ofp ~. |hosp + numchron + gender + school + privins-1, data = dane3)

AIC(model_zipr) #  32300.06
AIC(model_zipr_zredukowany) # 32298.49 

BIC(model_zipr) #  32402.31
BIC(model_zipr_zredukowany) # 32387.96 

summary(model_zinbr) # usuwamy health i intercept

model_zinbr_zredukowany = zeroinfl(ofp~.|numchron +hosp+ gender + school + privins - 1, data = dane3, dist = 'negbin')
summary(model_zinbr_zredukowany)

# sprawdzmy 

AIC(model_zinbr) # 24215.29
AIC(model_zinbr_zredukowany) # 24211.44

BIC(model_zinbr) # 24323.93
BIC(model_zinbr_zredukowany) # 24305.98

summary(model_poiss_hurdle) # usuwamy intercept

model_poiss_hurdle_zredukowany = hurdle(ofp ~.|hosp + health + numchron + gender + school + privins - 1, data = dane3)
summary(model_poiss_hurdle_zredukowany) # usuwamy health

model_poiss_hurdle_zredukowany_2 = hurdle(ofp ~.|hosp + numchron + gender + school + privins - 1, data = dane3)
summary(model_poiss_hurdle_zredukowany_2)


AIC(model_poiss_hurdle) # 32300.9
AIC(model_poiss_hurdle_zredukowany_2) # 32300.88 

BIC(model_poiss_hurdle) # 32403.15
BIC(model_poiss_hurdle_zredukowany_2) # 32390.35 

summary(model_nb_hurdle) # intercept

model_nb_hurdle_zredukowany = hurdle(ofp~.|hosp + numchron + gender + school + privins - 1, data = dane3, dist = 'negbin')
summary(model_nb_hurdle_zredukowany)

BIC(model_nb_hurdle)


llr_test <- function(model_full, model_reduced) {
  ll_full <- logLik(model_full)
  ll_red <- logLik(model_reduced)
  
  test_stat <- 2 * (as.numeric(ll_full) - as.numeric(ll_red))
  df <- attr(ll_full, "df") - attr(ll_red, "df")
  p_value <- pchisq(test_stat, df = df, lower.tail = FALSE)
  
  return(c(round(test_stat, 2), df, round(p_value, 4)))
}


tabela_llr <- data.frame(
  Model = c("ZIPR", "ZINBR", "Poisson Hurdle", "NB Hurdle"),
  `Chi²` = numeric(4),
  `df` = integer(4),
  `p-value` = numeric(4)
)

tabela_llr[1, 2:4] <- llr_test(model_zipr, model_zipr_zredukowany)
tabela_llr[2, 2:4] <- llr_test(model_zinbr, model_zinbr_zredukowany)
tabela_llr[3, 2:4] <- llr_test(model_poiss_hurdle, model_poiss_hurdle_zredukowany_2)
tabela_llr[4, 2:4] <- llr_test(model_nb_hurdle, model_nb_hurdle_zredukowany)


colnames(tabela_llr) = c("Model", "Statystyka testowa","df","p-wartość")
saveRDS(tabela_llr, file = ".../tabela_llr")


# ESTYMATORY BETA --------------------------------------------------------------

beta_pois = round(coefficients(model_poiss),2)
beta_nb = round(coefficients(model_nb),2)

coef_all <- coefficients(model_zipr_zredukowany)

coef_count <- coef_all[grep("^count_", names(coef_all))]
names(coef_count) <- sub("^count_", "", names(coef_count))

beta_zipr = round(coef_count,2)

coef_all <- coefficients(model_zinbr_zredukowany)

coef_count <- coef_all[grep("^count_", names(coef_all))]
names(coef_count) <- sub("^count_", "", names(coef_count))

beta_zinbr <- round(coef_count,2)

coef_all<-coefficients(model_poiss_hurdle_zredukowany)
coef_count <- coef_all[grep("^count_", names(coef_all))]
names(coef_count) <- sub("^count_", "", names(coef_count))

beta_poiss_hurdle = round(coef_count,2)

coef_all<-coefficients(model_nb_hurdle_zredukowany)
coef_count <- coef_all[grep("^count_", names(coef_all))]
names(coef_count) <- sub("^count_", "", names(coef_count))

beta_nb_hurdle = round(coef_count,2)


tabela_beta <- data.frame(
  Poisson = beta_pois,
  NB = beta_nb,
  ZIPR = beta_zipr,
  ZINBR = beta_zinbr,
  Poisson_z_bariera = beta_poiss_hurdle,
  NB_z_bariera = beta_nb_hurdle
)

colnames(tabela_beta)<-c("Poisson","NB","ZIPR","ZINBR","Poisson z barierą","NB z barierą")
saveRDS(tabela_beta, file = ".../tabela_beta.rds")

# ESTYMATOR GAMMA --------------------------------------------------------------


coef_all <- coefficients(model_zipr_zredukowany)

coef_zero <- coef_all[grep("^zero_", names(coef_all))]
names(coef_zero) <- sub("^zero_", "", names(coef_zero))

gamma_zipr = round(coef_zero,2)

coef_all <- coefficients(model_zinbr_zredukowany)

coef_zero <- coef_all[grep("^zero_", names(coef_all))]
names(coef_zero) <- sub("^zero_", "", names(coef_zero))

gamma_zinbr <-round(coef_zero,2)

coef_all<-coefficients(model_poiss_hurdle_zredukowany_2)
coef_zero <- coef_all[grep("^zero_", names(coef_all))]
names(coef_zero) <- sub("^zero_", "", names(coef_zero))

gamma_poiss_hurdle = round(coef_zero,2)

coef_all<-coefficients(model_nb_hurdle_zredukowany)
coef_zero <- coef_all[grep("^zero_", names(coef_all))]
names(coef_zero) <- sub("^zero_", "", names(coef_zero))

gamma_nb_hurdle = round(coef_zero,2)


tabela_gamma <- data.frame(
  zmienne = names(coef_count),
  Poisson = c("","","","","","","",""),
  NB = c("","","","","","","",""),
  ZIPR = c("",gamma_zipr[1],"","",gamma_zipr[2],gamma_zipr[4],gamma_zipr[5],gamma_zipr[6]),
  ZINBR = c("","","","",gamma_zinbr[1],"",gamma_zinbr[2],gamma_zinbr[4]),
  Poissom_b = c("",gamma_poiss_hurdle[1],"","",gamma_poiss_hurdle[2],gamma_poiss_hurdle[4],gamma_poiss_hurdle[5],gamma_poiss_hurdle[6]),
  NB_b = c("",gamma_nb_hurdle[1],"","",gamma_nb_hurdle[2],gamma_nb_hurdle[4],gamma_nb_hurdle[5],gamma_nb_hurdle[6])
)

colnames(tabela_gamma)<-c("","Poisson","NB","ZIPR","ZINBR","Poisson z barierą","NB z barierą")
saveRDS(tabela_gamma, file = "..../tabela_gamma.rds")



nparam <- function(model) length(coef(model))

llik <- function(model) logLik(model)[1]

zero_poiss <- sum(dpois(0, predict(model_poiss, type = "response")))
zero_nb <- sum(dnbinom(0, size = model_nb$theta, mu = predict(model_nb, type = "response")))
zero_zipr <- sum(predict(model_zipr_zredukowany, type = "zero") +
                   (1 - predict(model_zipr_zredukowany, type = "zero")) * 
                   exp(-predict(model_zipr_zredukowany, type = "response")))

zero_zinbr <- sum(predict(model_zinbr_zredukowany, type = "zero") +
                    (1 - predict(model_zinbr_zredukowany, type = "zero")) * 
                    dnbinom(0, size = model_zinbr_zredukowany$theta, 
                            mu = predict(model_zinbr_zredukowany, type = "response")))

zero_poiss_hurdle <- colSums(predict(model_poiss_hurdle_zredukowany_2, type = "prob"))[1]
zero_nb_hurdle <- colSums(predict(model_nb_hurdle_zredukowany, type = "prob"))[1]


tabela_statystyki <- data.frame(
  Statystyka = c("Liczba parametrów", "Log-likelihood", "AIC", "BIC", "E[liczba zer]"),
  Poisson = c(nparam(model_poiss), 
              round(llik(model_poiss), 2), 
              round(AIC(model_poiss), 2), 
              round(BIC(model_poiss), 2), 
              round(zero_poiss, 2)),
  
  NB = c(nparam(model_nb), 
         round(llik(model_nb), 2), 
         round(AIC(model_nb), 2), 
         round(BIC(model_nb), 2), 
         round(zero_nb, 2)),
  
  ZIPR = c(nparam(model_zipr_zredukowany), 
           round(llik(model_zipr_zredukowany), 2), 
           round(AIC(model_zipr_zredukowany), 2), 
           round(BIC(model_zipr_zredukowany), 2), 
           round(zero_zipr, 2)),
  
  ZINBR = c(nparam(model_zinbr_zredukowany), 
            round(llik(model_zinbr_zredukowany), 2), 
            round(AIC(model_zinbr_zredukowany), 2), 
            round(BIC(model_zinbr_zredukowany), 2), 
            round(zero_zinbr, 2)),
  
  Poisson_b = c(nparam(model_poiss_hurdle_zredukowany_2), 
                round(llik(model_poiss_hurdle_zredukowany_2), 2), 
                round(AIC(model_poiss_hurdle_zredukowany_2), 2), 
                round(BIC(model_poiss_hurdle_zredukowany_2), 2), 
                round(zero_poiss_hurdle, 2)),
  
  NB_b = c(nparam(model_nb_hurdle_zredukowany), 
           round(llik(model_nb_hurdle_zredukowany), 2), 
           round(AIC(model_nb_hurdle_zredukowany), 2), 
           round(BIC(model_nb_hurdle_zredukowany), 2), 
           round(zero_nb_hurdle, 2))
)


colnames(tabela_statystyki)<-c("","Poisson","NB","ZIPR","ZINBR","Poisson z barierą","NB z barierą")
saveRDS(tabela_statystyki, file = ".../tabela_statystyki.rds")



