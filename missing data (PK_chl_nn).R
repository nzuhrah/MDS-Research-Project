# Set current working directory.
setwd("/Users/User/P2")

# Get and print current working directory.
print(getwd())

df <- read.csv("chl_2017-2021.csv")
head(df)

df_PK_chl_nn <- df [, c('Date', 'PK_chl_nn')]
head(df_PK_chl_nn)

library(plotly)

fig <- plot_ly(df_PK_chl_nn, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~Date, y = ~PK_chl_nn)%>%
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

head(df_PK_chl_nn$PK_chl_nn)
statsNA(df_PK_chl_nn$PK_chl_nn)


# Random Imputation
df_PK_chl_nn_rand <- na.random(df_PK_chl_nn)
head(df_PK_chl_nn_rand)

# Last Obs. Carried Forward
df_PK_chl_nn_locf <- na.locf(df_PK_chl_nn,option = "locf")
head(df_PK_chl_nn_locf)
tail(df_PK_chl_nn_locf)

# Next Obs. Carried Backward
df_PK_chl_nn_nocb <- na.locf(df_PK_chl_nn,option = "nocb")
head(df_PK_chl_nn_nocb)
tail(df_PK_chl_nn_nocb)

# Linear Interpolation
df_PK_chl_nn_lin <- na.interpolation(df_PK_chl_nn)
head(df_PK_chl_nn_lin)
tail(df_PK_chl_nn_lin)


#CALCULATION MSE AND RMSE
df_PK_chl_nn$PK_chl_nn[is.na(df_PK_chl_nn$PK_chl_nn)]<-0
head(df_PK_chl_nn)
y_true <- df_PK_chl_nn$PK_chl_nn
head(y_true)

#rand
y_rand <- df_PK_chl_nn_rand$PK_chl_nn
head(y_rand)

mse_rand <- mean((y_rand - y_true)^2)
rmse_rand <- sqrt(mean((y_true - y_rand)^2))

#locf
y_locf <- df_PK_chl_nn_locf$PK_chl_nn
head(y_locf)

mse_locf <- mean((y_locf - y_true)^2)
rmse_locf <- sqrt(mean((y_true - y_locf)^2))


#nocb
y_nocb <- df_PK_chl_nn_nocb$PK_chl_nn
head(y_nocb)

mse_nocb <- mean((y_nocb - y_true)^2)
rmse_nocb <- sqrt(mean((y_true - y_nocb)^2))

#Linear
y_lin <- df_PK_chl_nn_lin$PK_chl_nn
head(y_lin)

mse_lin <- mean((y_lin - y_true)^2)
rmse_lin <- sqrt(mean((y_true - y_lin)^2))

data.frame(methods=c("Random", "LOCF", "NOCB","Linear"),
           MSE=c(mse_rand, mse_locf, mse_nocb, mse_lin),
           RMSE=c(rmse_rand, rmse_locf, rmse_nocb, rmse_lin))

#Save the best imputation

write.csv(df_PK_chl_nn_nocb, file = 'PK_chl_nn-cleaned.csv', row.names = FALSE)

fig2 <- plot_ly(df_PK_chl_nn_nocb, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~Date, y = ~PK_chl_nn)%>%
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

