#https://stackoverflow.com/questions/26110665/stratified-sampling-or-proportional-sampling-in-r

library(data.table)
library(tidyverse)

stratified <- function(df, group, size, select = NULL, 
                       replace = FALSE, bothSets = FALSE) {
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)
  
  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}

dt <- fread("/garagem/mangos/metricas_priced/extracao_cpfs.psv.gz")

dt[,idade := as.numeric(idade)]
dt[,idade := abs(idade)]

prop.table(table(dt$mangos))

prop.table(table(dt[mangos == 0]$classe_social))
prop.table(table(dt[mangos == 1]$classe_social))


prop.table(table(dt[mangos == 0]$sexo))
prop.table(table(dt[mangos == 1]$sexo))
  

summary(dt[mangos == 0]$idade)
summary(dt[mangos == 1]$idade)

# dt[!is.na(idade)] %>% 
# ggplot() +
#   aes(idade,colour = as.character(mangos)) +
#   geom_histogram(alpha=I(.2))

hist(dt[mangos == 0]$idade)
hist(dt[mangos == 1]$idade)

N = 100
myData <- data.frame(a=1:N,b=round(rnorm(N),2),group=round(rnorm(N,4),0))
(prop.table(table(myData$group)))
recebe_amostra <- stratified(myData,"group",.3)

prop.table(table(recebe_amostra$group))

#5240 mulheres

#retirar amostra de 

dt[,faixa_etaria := cut(idade,c(seq(25,70,by = 10)))]
dt[idade <= 25, faixa_etaria := "(0,25]"]
dt[idade > 65, faixa_etaria := "(65,]"]
table(dt$faixa_etaria)

# dicionario_faixa <- dt[!is.na(faixa_etaria)][,.N,faixa_etaria][order(faixa_etaria)]
# dicionario_faixa[,code := c(2:5,1,6)]
# dicionario_faixa[order(code)]
# dicionario_faixa[,N := NULL]
# 
# dt[dicionario_faixa, code := code, on = "faixa_etaria"]
take_sample <- dt[mangos == 0][!is.na(idade)]

prop.table(table(take_sample$sexo))
prop.table(table(dt[mangos == 0]$sexo))

prop.table(table(take_sample$classe_social))
prop.table(table(dt[mangos == 0]$classe_social))

prop_mulheres <- round(nrow(dt[mangos == 1])*nrow(dt[mangos == 1 & sexo != "M"])/nrow(dt[mangos == 1]))/
                 nrow(dt[sexo == "F" & mangos == 0])
#prop_mulheres <- round(prop_mulheres,4)

mulheres <- stratified(as.data.frame(take_sample[sexo != "F"]),"faixa_etaria",prop_mulheres)

prop_homens <- round(nrow(dt[mangos == 1])*nrow(dt[mangos == 1 & sexo != "F"])/nrow(dt[mangos == 1]))/
  nrow(dt[sexo == "M" & mangos == 0])

homens <- stratified(as.data.frame(take_sample[sexo == "M"]),"faixa_etaria",prop_homens)

prop.table(table(homens$classe_social))
prop.table(table(dt[mangos == 1]$classe_social))

prop.table(table(homens$faixa_etaria))
prop.table(table(dt[mangos == 1]$faixa_etaria))


prop.table(table(mulheres$classe_social))
prop.table(table(dt[mangos == 1]$classe_social))

prop.table(table(mulheres$faixa_etaria))
prop.table(table(dt[mangos == 1]$faixa_etaria))
