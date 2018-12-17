# Leitura e limpeza dos dados:
  
# Vamos primeiro carregar os pacotes do R 
library(readr)
library(dplyr)
library(ggplot2)
library(dtplyr)
library(xgboost)
train = read_csv("train.csv",col_names=TRUE)

train <- na.omit(train)

n = nrow(train) # número de linhas para a camada dos gráficos

head(train)

#  Vamos explorar um pouco a variável Demand:
train %>% 
    count(Demanda_uni_equil) %>% 
    filter(Demanda_uni_equil < 100) %>% 
    ggplot(aes(x = Demanda_uni_equil, y = n)) +
        geom_bar(stat = "identity") +
        ggtitle("Distribuição da Demanda")

train %>% 
    count(Demanda_uni_equil) %>% 
    filter(Demanda_uni_equil > 25, Demanda_uni_equil < 100) %>% 
    ggplot(aes(x = Demanda_uni_equil, y = n)) +
        geom_bar(stat = "identity") +
        ggtitle("Distribution of Demand - The Tail")

#### Producto_ID:

# Temos um total de 891 Product ID's para visualizar de uma vez.

train %>% count(Producto_ID, sort = TRUE) -> product_count
top_products = product_count$Producto_ID[1:12]


train %>% 
    filter(Producto_ID %in% top_products) %>% 
    filter(Demanda_uni_equil < 20) %>% 
    ggplot(aes(x = Demanda_uni_equil, fill = Producto_ID)) +
    geom_bar() +
    facet_wrap( ~ Producto_ID) +
    ggtitle("Distribuição de Demanda para os Produtos mais requisitados")

train %>% 
    filter(Producto_ID %in% top_products) %>% 
    filter(Demanda_uni_equil < 20) %>% 
    ggplot(aes(x = Demanda_uni_equil, fill = Producto_ID)) +
    geom_bar(position = "dodge") +
    ggtitle("Distribuição de Demanda por Produtos - Empilhados e Normalizados")

# Previsão da Damanda_uni_equil
library(data.table)
train = fread('train.csv', 
              select = c('Semana','Producto_ID', 'Cliente_ID', 'Demanda_uni_equil'))
test = fread('test.csv', select = c("Semana",'id','Cliente_ID', 'Producto_ID'))
train = train[Semana > 3]

train$Demanda_uni_equil = log1p(train$Demanda_uni_equil)

# Neste ponto faz-se uma mescla dos dados de teste + treino:

train$id = 0; test$Demanda_uni_equil = 0; train$tst = 0; test$tst = 1
rec_train = rbind(train[Semana == 9], test)

# Agora processa a média (demanda + contagem) nos dados de treino nas semanas de 3 - 8,
# depois junta com as semanas 9, 10 ,11:
  
train[Semana <= 8][, .(mean_client_prod = mean(Demanda_uni_equil), 
                       count_client_prod = .N),
                   by = .(Producto_ID, Cliente_ID)] %>% 
  merge(rec_train, all.y = TRUE, by = c("Producto_ID", "Cliente_ID")) -> rec_train


train[Semana <= 8][, .(mean_prod = mean(Demanda_uni_equil),
                       count_prod = .N),
                   by = .(Producto_ID)] %>% 
  merge(rec_train, all.y = TRUE, by = c("Producto_ID")) -> rec_train


train[Semana <= 8][, .(mean_cliente = mean(Demanda_uni_equil),
                       count_cliente = .N),
                   by = .(Cliente_ID)] %>% 
  merge(rec_train, all.y = TRUE, by = c("Cliente_ID")) -> rec_train

# Aplica-se o algoritmo XGBoosting nos dados de treino na semana 9,
# fazendo as previsões para as semanas 10 e 11 e gravando-as no arquivo de resultado:

y_train = rec_train$Demanda_uni_equil[rec_train$tst == 0] 

dtrain = xgb.DMatrix(as.matrix(rec_train[tst == 0] %>% select(- Demanda_uni_equil, -id, - tst)),
                     label = y_train)
model = xgb.train(data = dtrain, nrounds = 25, max_depth = 8, eta = 0.5)

preds = predict(model, as.matrix(rec_train[tst == 1] %>% select(- Demanda_uni_equil, -id, - tst)))
preds = expm1(preds)
preds[preds < 0] = 0
id = rec_train[tst == 1]$id

solution = data.frame(id = as.integer(id), Demanda_uni_equil = preds)
write.csv(solution, "xgboost_mean_resultado_previsao.csv", row.names = FALSE)
