library(readxl)
library(ggplot2)

my_df <- read.csv(file="/home/xenakas/Desktop/hw2/Inflation.csv", header=TRUE, sep=",")
my_df <- cbind(my_df, diff = my_df$Средняя.инфляция.после.перехода - my_df$Средняя.инфляция.перехода)

df_targ <- subset(my_df, my_df$Таргетирует.инфляцию==1)
df_razviv <- subset(my_df, my_df$Развивающая.страна==1)
df_razvit <- subset(my_df, my_df$Развивающая.страна==0)

ggplot(df_targ, aes(x=df_targ$Средняя.инфляция.перехода, y=df_targ$Средняя.инфляция.после.перехода)) + geom_point(aes(color=as.factor(df_targ$Развивающая.страна))) + geom_smooth(method='lm',formula=y~x) + xlab("Средняя инфляция до перехода") +
    ylab("Средняя инфляция после перехода") + scale_colour_discrete(name="Группа стран", labels=c("Развивающиеся", "Развитые"))

t.test(df_targ$Средняя.инфляция.перехода, df_targ$Средняя.инфляция.после.перехода, paired = TRUE)

# ggplot(my_df, aes(x=my_df$Средняя.инфляция.перехода, y=my_df$Средняя.инфляция.после.перехода)) + geom_point(aes(color=as.factor(my_df$Развивающая.страна))) + geom_smooth(method='lm',formula=y~x) + xlab("Средняя инфляция до перехода") +
#   ylab("Средняя инфляция после перехода") + scale_colour_discrete(name="Группа стран", labels=c("Развивающиеся", "Развитые"))

#################################

model_a1 <- lm(data=my_df, diff ~ my_df$Таргетирует.инфляцию)  
summary(model_a1)

model_a2 <- lm(data=df_razviv, diff ~ df_razviv$Таргетирует.инфляцию)  
summary(model_a2)

model_a3 <- lm(data=df_razvit, diff ~ df_razvit$Таргетирует.инфляцию)  
summary(model_a3)

#################################

model_b1 <- lm(data=my_df, diff ~ my_df$Таргетирует.инфляцию + my_df$Средняя.инфляция.перехода)  
summary(model_b1)

model_b2 <- lm(data=df_razviv, diff ~ df_razviv$Таргетирует.инфляцию  + df_razviv$Средняя.инфляция.перехода)  
summary(model_b2)

model_b3 <- lm(data=df_razvit, diff ~ df_razvit$Таргетирует.инфляцию  + df_razvit$Средняя.инфляция.перехода)  
summary(model_b3)


