# Set current working directory.
setwd("/Users/User/P2")

# Get and print current working directory.
print(getwd())

df <- read.csv("chl_2017-2021.csv")
head(df)

df_MST_chl_oc4me <- df [, c('Date', 'MST_chl_oc4me')]
head(df_MST_chl_oc4me)

library(plotly)

fig <- plot_ly(df_MST_chl_oc4me, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~Date, y = ~MST_chl_oc4me)%>%
  layout(showlegend = F)
fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)


fig


library(imputeTS)

head(df_MST_chl_oc4me$MST_chl_oc4me)
statsNA(df_MST_chl_oc4me$MST_chl_oc4me)


# Random Imputation
df_MST_chl_oc4me_rand <- na.random(df_MST_chl_oc4me)
head(df_MST_chl_oc4me_rand)

# Last Obs. Carried Forward
df_MST_chl_oc4me_locf <- na.locf(df_MST_chl_oc4me,option = "locf")
head(df_MST_chl_oc4me_locf)
tail(df_MST_chl_oc4me_locf)

# Next Obs. Carried Backward
df_MST_chl_oc4me_nocb <- na.locf(df_MST_chl_oc4me,option = "nocb")
head(df_MST_chl_oc4me_nocb)
tail(df_MST_chl_oc4me_nocb)

# Linear Interpolation
df_MST_chl_oc4me_lin <- na.interpolation(df_MST_chl_oc4me)
head(df_MST_chl_oc4me_lin)
tail(df_MST_chl_oc4me_lin)


#CALCULATION MSE AND RMSE
df_MST_chl_oc4me$MST_chl_oc4me[is.na(df_MST_chl_oc4me$MST_chl_oc4me)]<-0
head(df_MST_chl_nn)
y_true <- df_MST_chl_oc4me$MST_chl_oc4me
head(y_true)

#rand
y_rand <- df_MST_chl_oc4me_rand$MST_chl_oc4me
head(y_rand)

mse_rand <- mean((y_rand - y_true)^2)
rmse_rand <- sqrt(mean((y_true - y_rand)^2))

#locf
y_locf <- df_MST_chl_oc4me_locf$MST_chl_oc4me
head(y_locf)

mse_locf <- mean((y_locf - y_true)^2)
rmse_locf <- sqrt(mean((y_true - y_locf)^2))


#nocb
y_nocb <- df_MST_chl_oc4me_nocb$MST_chl_oc4me
head(y_nocb)

mse_nocb <- mean((y_nocb - y_true)^2)
rmse_nocb <- sqrt(mean((y_true - y_nocb)^2))

#Linear
y_lin <- df_MST_chl_oc4me_lin$MST_chl_oc4me
head(y_lin)

mse_lin <- mean((y_lin - y_true)^2)
rmse_lin <- sqrt(mean((y_true - y_lin)^2))

data.frame(methods=c("Random", "LOCF", "NOCB","Linear"),
           MSE=c(mse_rand, mse_locf, mse_nocb, mse_lin),
           RMSE=c(rmse_rand, rmse_locf, rmse_nocb, rmse_lin))

#Save the best imputation

write.csv(df_MST_chl_oc4me_lin, file = 'MST_chl_oc4me-cleaned.csv',row.names = FALSE)

fig2 <- plot_ly(df_MST_chl_oc4me_lin, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~Date, y = ~MST_chl_oc4me)%>%
  layout(showlegend = F)
fig2 <- fig2 %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)


fig2
