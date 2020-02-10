# Robert_Grant
```{r}
#==========
#MOTIVATION
#==========
This readme provides the code and reasoning for my Financial econometrics paper, which studies exchange rate volatility with a Markov-switching GARCH (MS-GARCH) model.
```
Loading packages required throughout this paper
```{r}
#=======
PACKAGES
#=======
if (!require("devtools")) install.packages("devtools")
if (!require("rmsfuns")) devtools::install_github("Nicktz/rmsfuns")
if(!require("pacman")) install.packages("pacman")
load_pkg("MSGARCH")
library(rmsfuns)
load_pkg("tidyverse")
load_pkg("ggplot2")
load_pkg("MTS")
```

```{r}
#===========
#DATA IMPORT
#===========
library(tidyverse)
currency <-list.files("E:/Exchangerates", full.names = T, recursive = T) %>% as.list %>% map_df(~read_rds(.))
```

Creating a table reporting mean and volatility the log-differenced exchange rate series' (Returns)
```{r ShortTable, results = 'asis'} 
library(knitr)
data <- weekly %>% tbl_df() %>%  group_by(Name) %>% summarise("Mean (%)" = mean(Return)*100, "Volatility (%)"  = sqrt(var(Return))) 
table <- kable(data, caption = "Exchang rate mean and volatility", digits = 5)
print(table)
```
This plots the return series, fits all six figures into one output graph
```{r Figure1, warning =  FALSE, fig.align = 'center', fig.cap = "Log differenced exchange rates \\label{Figure1}", fig.ext = 'png', fig.height = 4, fig.width = 6}
Ret <- ggplot(data = weekly) + # Opens plot environment. Now let's add layers:
geom_line(aes(x = date, y = Return, colour=Name), size = 0.3) + theme_bw() +
labs(title = "",
caption = "Data was downloaded from Bloomberg", x = "", y = "Log difference Values") +
facet_wrap(~Name, scales = "free") + guides(color = FALSE)
print(Ret)
```

This specifies the MS-GARCH model for each of the six countries
```{r Figure2, warning =  FALSE, fig.align = 'center', fig.cap = "Filtered probabilities of high volatility regime - emerging market economies \\label{Figure2}", fig.ext = 'png', fig.height = 4, fig.width = 7}
#======================
#MARKOV SWITCHING GARCH 
#======================
#CREATING MODEL SPECS
#----------------------
#EM group
#-----

#Creating model specs
ZAR <- weekly %>% filter(Name %in% "SA")
specZAR <- CreateSpec(variance.spec = list(model = c("gjrGARCH")),
                         distribution.spec = list(distribution = c("std")),
                         switch.spec = list(K = 2),
                         constraint.spec = list(regime.const = "nu"))
fit_spec_ZAR <- FitML(spec = specZAR,data=ZAR$Return)

BRZ <- weekly %>% filter(Name %in% "Brazil")
specBRZ <- CreateSpec(variance.spec = list(model = c("gjrGARCH")),
                         distribution.spec = list(distribution = c("std")),
                         switch.spec = list(K = 2),
                         constraint.spec = list(regime.const = "nu"))
fit_spec_BRZ <- FitML(spec = specBRZ,data=BRZ$Return)

IND <- weekly %>%  filter(Name %in% "India")
spec_1 <- CreateSpec(variance.spec = list(model = c("gjrGARCH")),
                         distribution.spec = list(distribution = c("std")),
                         switch.spec = list(K = 2),
                         constraint.spec = list(regime.const = "nu"))
fit_spec_IND <- FitML(spec = spec_1,data=IND$Return)

#----------------------
#Filtered probabilities
#----------------------
#SA
ZARfilter <- as.data.frame(matrix(data = NA, nrow = nrow(ZAR), ncol = specZAR$K))
  ZARfilter$Date <- ZAR$date 
  
  for (i in 1:specZAR$K) {
    ZARfilter[,i] <- State(fit_spec_ZAR)$FiltProb[, 1, i, drop = TRUE]
    colnames(ZARfilter)[i] <- paste("Regime", i,sep = "")
  }

  filter_probs_long_ZAR <- gather(ZARfilter,Regime,Probability,Regime1,factor_key = T) 
  ZARplot <- ggplot(data = filter_probs_long_ZAR)+geom_line(aes(x=Date,y=Probability))+facet_grid(.~Regime)+theme_bw()
  
#Brazil
BRZfilter <- as.data.frame(matrix(data = NA, nrow = nrow(BRZ), ncol = specBRZ$K)) 
BRZfilter$Date <- BRZ$date # Add the date column to make nicer plots 
  
  for (i in 1:specBRZ$K) {
    BRZfilter[,i] <- State(fit_spec_BRZ)$FiltProb[, 1, i, drop = TRUE]
    colnames(BRZfilter)[i] <- paste("Regime", i,sep = "")
    }
  
  filter_probs_long_BRZ <- gather(BRZfilter,Regime,Probability,Regime1,factor_key = T) 
  BRZplot <- ggplot(data = filter_probs_long_BRZ)+geom_line(aes(x=Date,y=Probability))+facet_grid(.~Regime)+theme_bw()
  
#IND
 INDfilter <- as.data.frame(matrix(data = NA, nrow = nrow(IND), ncol = spec_1$K)) #
  INDfilter$Date <- IND$date # Add the date column to make nicer plots 
  
  for (i in 1:spec_1$K) {
    INDfilter[,i] <- State(fit_spec_IND)$FiltProb[, 1, i, drop = TRUE]
    colnames(INDfilter)[i] <- paste("Regime", i,sep = "")
    }
  
  filter_probs_long_IND <- gather(INDfilter,Regime,Probability,Regime1,factor_key = T) # Convert to long datatype for neat ggplots
  INDplot <- ggplot(data = filter_probs_long_IND)+geom_line(aes(x=Date,y=Probability))+facet_grid(.~Regime)+theme_bw()
  
#AE group
#--------
#Create model spec
AUS <- weekly %>% filter(Name %in% "Australia")
specAUS <- CreateSpec(variance.spec = list(model = c("gjrGARCH")),
                         distribution.spec = list(distribution = c("std")),
                         switch.spec = list(K = 2),
                         constraint.spec = list(regime.const = "nu"))
fit_spec_AUS <- FitML(spec = specAUS,data=AUS$Return)

CAN <- weekly %>% filter(Name %in% "Canada")
specCAN <- CreateSpec(variance.spec = list(model = c("gjrGARCH")),
                         distribution.spec = list(distribution = c("std")),
                         switch.spec = list(K = 2),
                         constraint.spec = list(regime.const = "nu"))
fit_spec_CAN <- FitML(spec = specCAN,data=CAN$Return)

NOR <- weekly %>%  filter(Name %in% "Norway")
specNOR <- CreateSpec(variance.spec = list(model = c("gjrGARCH")),
                         distribution.spec = list(distribution = c("std")),
                         switch.spec = list(K = 2),
                         constraint.spec = list(regime.const = "nu"))
fit_spec_NOR <- FitML(spec = specNOR,data=NOR$Return)

#----------------------
#filtered probabilities
#----------------------
AUSfilter <- as.data.frame(matrix(data = NA, nrow = nrow(AUS), ncol = specAUS$K))
  AUSfilter$Date <- AUS$date 
  
  for (i in 1:specAUS$K) {
    AUSfilter[,i] <- State(fit_spec_AUS)$FiltProb[, 1, i, drop = TRUE]
    colnames(AUSfilter)[i] <- paste("Regime", i,sep = "")
  }

  filter_probs_long_AUS <- gather(AUSfilter,Regime,Probability,Regime1,factor_key = T) 
  AUSplot <- ggplot(data = filter_probs_long_AUS)+geom_line(aes(x=Date,y=Probability))+facet_grid(.~Regime)+theme_bw()
  
#Canada
CANfilter <- as.data.frame(matrix(data = NA, nrow = nrow(CAN), ncol = specCAN$K)) 
CANfilter$Date <- CAN$date # Add the date column to make nicer plots 
  
  for (i in 1:specCAN$K) {
    CANfilter[,i] <- State(fit_spec_CAN)$FiltProb[, 1, i, drop = TRUE]
    colnames(CANfilter)[i] <- paste("Regime", i,sep = "")
    }
  
  filter_probs_long_CAN <- gather(CANfilter,Regime,Probability,Regime1,factor_key = T) 
  CANplot <- ggplot(data = filter_probs_long_CAN)+geom_line(aes(x=Date,y=Probability))+facet_grid(.~Regime)+theme_bw()
  
#IND
 NORfilter <- as.data.frame(matrix(data = NA, nrow = nrow(NOR), ncol = specNOR$K)) #
  NORfilter$Date <- NOR$date # Add the date column to make nicer plots 
  
  for (i in 1:specNOR$K) {
    NORfilter[,i] <- State(fit_spec_IND)$FiltProb[, 1, i, drop = TRUE]
    colnames(NORfilter)[i] <- paste("Regime", i,sep = "")
    }
  
  filter_probs_long_NOR <- gather(NORfilter,Regime,Probability,Regime1,factor_key = T) 
  NORplot <- ggplot(data = filter_probs_long_NOR)+geom_line(aes(x=Date,y=Probability))+facet_grid(.~Regime)+theme_bw()
```
These two code chunks plot the filtered probbailities for the countries of the two groups (AE and EM), and plot them on the same graph (ie the three filtered probabilities for the EM group on one graph, and the three for the AE on another graph).
```{r Figure 3, warning =  FALSE, fig.align = 'center', fig.cap = "Filtered Probabilities - Emerging economies \\label{Figure1}", fig.ext = 'png', fig.height = 4, fig.width = 7}
#plotting EM group
#------------------------------------
#EM group
#--------
probEM <- ZARfilter %>% select(Date, Regime1) %>% rename(SA = Regime1) %>% cbind((BRZfilter %>% select(Regime1) %>% rename(Brazil = Regime1)),(INDfilter %>% select(Regime1) %>% rename(India = Regime1)))
probEMlong <- probEM %>% gather(Name, Probability, -Date)
EMplot <- ggplot(data = probEMlong)+geom_line(aes(x=Date, y=Probability, colour = Name))
print(EMplot)

```
```{r  Figure 4, warning =  FALSE, fig.align = 'center', fig.cap = "Filtered Probabilities - Advanced economies \\label{Figure3}", fig.ext = 'png', fig.height = 4, fig.width = 7}
#plotting AE group
#-----------------
probAE <- AUSfilter %>% select(Date, Regime1) %>% rename(Australia = Regime1) %>% cbind((CANfilter %>% select(Regime1) %>% rename(Canada = Regime1)),(NORfilter %>% select(Regime1) %>% rename(Norway = Regime1)))
probAElong <- probAE %>% gather(Name, Probability, -Date)
AEplot <- ggplot(data = probAElong)+geom_line(aes(x=Date, y=Probability, colour=Name))
print(AEplot)
```
```{r Figure 5, warning =  FALSE, fig.align = 'center', fig.cap = "Conditional Volatilities - Emerging economies \\label{Figure5}", fig.ext = 'png', fig.height = 3, fig.width = 7}
#========================
#CONDITIONAL VOLATILITIES
#========================
#EM group
#--------
ZARcondvol <- as.data.frame(Volatility(fit_spec_ZAR)) 
colnames(ZARcondvol) <- "South Africa"
BRZcondvol <- as.data.frame(Volatility(fit_spec_BRZ))
colnames(BRZcondvol) <- "Brazil"
INDcondvol <- as.data.frame(Volatility(fit_spec_IND))
colnames(INDcondvol) <- "India"
EMcondvol <- cbind(ZARfilter$Date,ZARcondvol,BRZcondvol,INDcondvol) %>% tbl_df()
EMcondvol <- EMcondvol %>% rename(Date = `ZARfilter$Date`) %>% gather(Name, `Conditional Volatility`, -Date)
EMcondvolplot <- ggplot(data = EMcondvol)+geom_line(aes(x=Date, y=`Conditional Volatility`, colour=Name))
print(EMcondvolplot)
```

These two chunks plot the conditional volatilities provided by the model. Similar format to the filtered probabilities graph - the conditional volatilities of the three EMs on one graph, and those of the three AEs on a separate graph. Plotting multiple series on the same graph makes analysis easier.

```{r Figure 6, warning =  FALSE, fig.align = 'center', fig.cap = "Conditional Volatilities - Advanced economies \\label{Figure6}", fig.ext = 'png', fig.height = 3, fig.width = 7}
#AE group
#--------
AUScondvol <- as.data.frame(Volatility(fit_spec_AUS)) 
colnames(AUScondvol) <- "Australia"
CANcondvol <- as.data.frame(Volatility(fit_spec_CAN))
colnames(CANcondvol) <- "Canada"
NORcondvol <- as.data.frame(Volatility(fit_spec_NOR))
colnames(NORcondvol) <- "Norway"
AEcondvol <- cbind(ZARfilter$Date,AUScondvol,CANcondvol,NORcondvol) %>% tbl_df()
AEcondvol <- AEcondvol %>% rename(Date = `ZARfilter$Date`) %>% gather(Name, `Conditional Volatility`, -Date)
AEcondvolplot <- ggplot(data = AEcondvol)+geom_line(aes(x=Date, y=`Conditional Volatility`, colour=Name))
print(AEcondvolplot)
```
The next chunk provides the output for the appendix, which consists of the model estimation output for each country. 


```{r}
#=================
#ESTIMATION OUTPUT
#=================
#EM group
#--------
load_pkg("broom")
ZARoutput <- fit_spec_ZAR[["Inference"]][["MatCoef"]]
kable(ZARoutput, caption = "Model estimation - South Africa")
BRZoutput <- fit_spec_BRZ[["Inference"]][["MatCoef"]]
kable(BRZoutput, caption = "Model estimation - Brazil")
```

\newpage

```{r}
INDoutput <- fit_spec_IND[["Inference"]][["MatCoef"]]
kable(INDoutput, caption = "Model estimation - India")
```

## Output estimation -AE group
```{r}
AUSoutput <- fit_spec_AUS[["Inference"]][["MatCoef"]]
kable(AUSoutput, caption = "Model estimation - Australia")
```

\newpage

```{r}
CANoutput <- fit_spec_CAN[["Inference"]][["MatCoef"]]
kable(CANoutput, caption = "Model estimation - Canada")
NORoutput <- fit_spec_NOR[["Inference"]][["MatCoef"]]
kable(NORoutput, caption = "Model estimation - Norway")
