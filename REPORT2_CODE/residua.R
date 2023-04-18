library('ggplot2')
library('latex2exp')
library('emojifont')


data <- read.csv('residua.csv')

plot(data)

xt <- ggplot(data, aes(X, x))+
  geom_point(col='palevioletred4')+
  xlab(TeX('$t$'))+
  ylab(TeX('$\\epsilon_t$'))
  theme(axis.title=element_text(size=18,face="bold"))
xt
ggsave('X_t.pdf', xt, 'pdf', width=10, dpi=700)

mean(data$x)
sd(data$x)


dyst_szum <- ggplot(data, aes(x))+
  stat_ecdf(geom='point', aes(col='dystrybuanta empiryczna'), cex=1)+ 
  stat_function(fun=pnorm, args=list(mean=0, sd=3.446763), 
               aes(col='dystrybuanta teoretyczna'), linewidth=1.5)+
  labs(x = TeX("$\\epsilon_t"), y = TeX("$F(\\epsilon_t)"), color="Legenda")+
  scale_color_manual(values=c('palevioletred4', 'palevioletred1'))
dyst_szum
ggsave('dyst_szum.pdf', dyst_szum, 'pdf', width=10, dpi=700)



dens_szum <- ggplot(data, mapping=aes(x))+
  geom_histogram(aes(y=after_stat(count / sum(count))), binwidth=1, fill='lightpink', col='black') +
  geom_density(aes(color='gęstość empiryczna'), linewidth=1.5, show_guide=FALSE) + 
  stat_density(aes(color='gęstość empiryczna'), linewidth=1.5, geom="line", position = "identity")+
  stat_function(fun=dnorm, args=list(mean=0, sd=3.446763), aes(col='gęstość teoretyczna'), linewidth=2, show_guide = FALSE)+
  xlab(TeX('$\\epsilon_t$'))+ylab('Częstość')+
  #stat_density(aes(color='gęstość teretyczna'), linewidth=1.5, geom="line", position = "identity")+
  scale_colour_manual("Legenda", values=c("palevioletred4", "palevioletred1", "palevioletred1"))
dens_szum
ggsave('dens_szum.pdf', dens_szum, 'pdf', width=10, dpi=700)



qqplot <- ggplot(data, aes(sample=x)) + stat_qq(aes(col='rozkład szumu'), 
                                                  distribution = stats::qnorm,
                                                  dparams = list(mean=0, sd=3.446763)) +
  stat_qq_line(aes(col='rozkład normalny'), linewidth=1.5, 
               distribution = stats::qnorm,
               dparams = list(mean=0, sd=3.446763))+
  xlab(TeX('$N(0,3.44)$'))+ylab('Rozkład szumu')+
  scale_colour_manual("Legenda", values=c("palevioletred1", "palevioletred4", "palevioletred4"))
qqplot
ggsave('qqplot_et.pdf', qqplot, 'pdf', width=10, dpi=700)


library(tseries)
library(nortest)
jarque.bera.test(data$x)
lillie.test(data$x)
shapiro.test(data$x)
ljunga.box
pacf(data$x)
acf(data$x)






emp_acorr <- function(X,h){
  Z <- emp_ACVF(X,h)
  Y <- emp_ACVF(X,0)
  return(Z/Y)
}


emp_ACVF <- function(X,h){
  n <- length(X)
  xm <- mean(X)
  X1 <- X[1:(n-abs(h))]
  X2 <- X[(1+abs(h)):n]
  return(1/n*sum((X1-xm)*(X2-xm)))
}



h <- 0:50
corr <- c()
for(i in h) corr[i+1] <- emp_acorr(data$x,i)
df <- data.frame(h,corr)


ac <- ggplot(df, aes(h,corr))+geom_point(col='palevioletred4', size=2.5)+ylab('ACF')
ac
ggsave('szum_acf.pdf', ac, 'pdf', width=10, dpi=700)


pacff <- ggplot(df, aes(h,c(1,pacf(data$x,50,plot=F)$acf)))+geom_point(col='palevioletred4', size=2.5)+ylab('PACF')
pacff
ggsave('szum_pacf.pdf', pacff, 'pdf', width=10, dpi=700)