###Setting up the environment

This tutorial is meant to reproduce the graphs and tables of chapter 3 of Godley and Lavoie (2007, G&L for now on) and at the same time discover some of the tools provided by the package PK-SFC. 

Before doing any modelling, we need to load the package in the R environment. 
```{r}
library(PKSFC)
```

The, you need to download the two attached [SIM.txt](https://github.com/S120/PKSFC/files/436955/SIM.txt) and [SIMEX.txt](https://github.com/S120/PKSFC/files/436956/SIMEX.txt) file and save it in the folder of your choice. Make sure to set the working directory where you saved the downloaded file. In comand line this looks like this but if you use Rstudio, you can use the graphical interface as well (Session>Set Working Dirctory>Choose Directory)
```{r, eval=FALSE}
setwd("pathToYourDirectory")
```

###Loading the model

The first thig to do is to load the model anc check for completeness.
```{r}
simex<-sfc.model("SIMEX.txt",modelName="SIMplest model")
simex<-sfc.check(simex,fill=FALSE)
```

We are now ready to simulate the model
```{r}
datasimex<-simulate(simex)
```

This replicates table 3.6 page 81
```{r}
round(t(datasimex$baseline[c(1,2,3,66),c("G_d","Y","T_s","Yd","Yd_e","C_d","H_s","H_h")]),
      digits=1)
```

|     | 1945| 1946| 1947| 2010|
|:----|----:|----:|----:|----:|
|G_d  |   20|   20| 20.0|   20|
|Y    |   NA|   20| 36.0|  100|
|T_s  |   NA|    4|  7.2|   20|
|Yd   |    0|   16| 28.8|   80|
|Yd_e |   NA|    0| 16.0|   80|
|C_d  |   NA|    0| 16.0|   80|
|H_s  |    0|   16| 28.8|   80|
|H_h  |    0|   16| 28.8|   80|

This replicates figure 3.5 page 82
```{r}
plot(simex$time,datasimex$baseline[,"Yd"],type="l",xlab="",ylab="",lty=2)
lines(simex$time,datasimex$baseline[,"Yd_e"],lty=3)
lines(simex$time,vector(length=length(simex$time))+datasimex$baseline["2010","Yd"])
legend(x=1970,y=50,legend=c("Disposable Income","Expected Disposable Income","Steady State"),
       lty=c(2,3,1),bty="n")
```

![alt tag](https://cloud.githubusercontent.com/assets/11057808/17969058/12c02d30-6ac8-11e6-81df-eb36c8ed1f98.png)

###Gauss-Seidel and solving a PK-SFC model with the package
If you have run the SIM model, you probably have noted the difference in time needed to obtain the results for model SIM and model SIMEX. We can confirm this impression by computing the simulation time. We can thus load the SIM model and then compare both timing.

```{r}
sim<-sfc.model("SIM.txt",modelName="sim")

#Simulation SIM
ptm <- proc.time()
data1<-simulate(sim)
paste("Elapsed time is ",proc.time()[3]-ptm[3],"seconds")

#Simulation SIMEX
ptm <- proc.time()
dataex<-simulate(simex,tolValue = 1e-10)
paste("Elapsed time is ",proc.time()[3]-ptm[3],"seconds")
```

The reason behind these differences in time is based on the way of how the model are simulated. In a nutshell, the package solves a system of euquation for each period of the simulation. The solver used for the resolution of the system is based on the Gauss-Seidel (GS) algorithm. 

The GS allows to solve a linear system of equation which can be represented by $Ax=b,\, A\in\mathbb{R}^{n\times n},\, b\in \mathbb{R}^n$ via an iterative algorithm, where each iteration can be represented by $L x^{k+1} = b-Ux^{k},\, A=L+U$. The pseudo-code for the GS is the following:
<ol>
<li>Select initial values $x^0$</li>
<li>While $k<maxIter$ \& $\delta < tolValue$</li>
<ol>
<li>For each $i=1,...,n$: $$x_i^{k+1}=\frac{1}{a_{ii}}\left( b_i-\sum^{i-1}_{j=1}a_{ij} x_j^{k+1}-\sum_{j=i+1}^n a_{ij}x_j^k\right)$$</li>
<li>Compute $\delta$: $$\delta = \frac{x^{k+1}-x^k}{x^k}$$</li>
</ol>
</ol>
where the convergence of the GS towards the solution of the system is being captured by the difference between $\delta$ and $tolValue$ and where $maxIter$ allows to limit the number of iterations performed in cases where the convergence is not observed (note that converegence is ensured for a linear system of equations).

The two parameters, i.e. the convergence parameter and the maximum number of iterations can be specified in the `simulate` function. This obviously impacts the time needed to simulate the model:

```{r}
#Simulation 1
ptm <- proc.time()
data1<-simulate(sim)
paste("Elapsed time is ",proc.time()[3]-ptm[3],"seconds")

#Simulation 2
ptm <- proc.time()
data2<-simulate(sim,tolValue = 1e-3)
paste("Elapsed time is ",proc.time()[3]-ptm[3],"seconds")

#Simulation 3
ptm <- proc.time()
data3<-simulate(sim, maxIter=10)
paste("Elapsed time is ",proc.time()[3]-ptm[3],"seconds")
```

In this package, the GS algorithm has been improved in order to account for the fact that you might have various blocks of independent systems of equations (where a system might actually be composed of only one equation). If this is the case, you reduce significantly the number of iterations needed for the Gauss-Seidel algorithm to converge. You can obtain the various blocks of equations that the package finds in the model by printing the summary of a model.

```{r}
summary(sim)
summary(simex)
```

You observe that by changing the consumption function, the simex model has a radical block structure. Indeed, the model is not a system of simultaneous equation anymore while the model sim is. This allows the package to reduce significantly the number of iterations in the GS algorithm. You can observe these number of iteration per block in the datastruture generated by the simulations under the colomn `iter block x` where `x` is the number of the block in the block structure observed when looking at the summary of the model. We can thus compare the number of iteratins needed to obtain convergence in the case of the SIM and SIMEX simulations

```{r}
#Observing the results of the three simulations
round(t(data1$baseline[c(1,2,20,40,66),c("iter block 1","iter block 2")]),digits=3)
round(t(dataex$baseline[c(1,2,20,40,66),
                        c("iter block 1","iter block 2","iter block 3",
                          "iter block 4","iter block 5","iter block 6")]),digits=3)
```

|             | 1945| 1946| 1964| 1984| 2010|
|:------------|----:|----:|----:|----:|----:|
|iter block 1 |    0|    2|    1|    1|    1|
|iter block 2 |    0|  358|  322|  322|  322|

|             | 1945| 1946| 1964| 1984| 2010|
|:------------|----:|----:|----:|----:|----:|
|iter block 1 |    0|    2|    2|    2|    2|
|iter block 2 |    0|    2|    2|    2|    2|
|iter block 3 |    0|    2|    2|    2|    2|
|iter block 4 |    0|    2|    2|    2|    2|
|iter block 5 |    0|    2|    2|    2|    2|
|iter block 6 |    0|    2|    2|    2|    2|

###Back to simulations: Expectations mistake

Section 3.7 of G&L analyses the impacts of a constant expectation on disposable income. In order to do that, we need to change the expectation equation in the model. This can be done with the `sfc.editEqu` function. We also need to set a value for the parameter
```{r}
simex_b<-sfc.editEqu(simex,var="Yd_e",eq="Yd_fixed")
simex_b<-sfc.addVar(simex_b,var="Yd_fixed",init=80,desc="Constant expected disposable income")
simex_b<-sfc.check(simex_b,fill=F)
```

The model can be simulated and a scenario can be added as well, as done in Godley and Lavoie
```{r}
datasimex_b<-simulate(simex_b)
init = datasimex_b$baseline[66,]
simex_b<-sfc.addScenario(simex_b,"G_d",25,1960,2010,init)
datasimex_b<-simulate(simex_b)
```

This replicates figure 3.6 page 84
```{r}
plot(simex_b$time,datasimex_b$scenario_1[,"Y"],type="l",xlab="",ylab="",lty=2)
lines(simex_b$time,(simex_b$time>1959)*
        (datasimex_b$scenario_1["2010","Y"]-datasimex_b$scenario_1["1958","Y"])
      +datasimex_b$scenario_1["1958","Y"])
legend(x=1970,y=110,legend=c("GDP","Steady State GDP"),lty=c(2,1),bty="n")
```

![alt tag](https://cloud.githubusercontent.com/assets/11057808/17969060/12c36554-6ac8-11e6-9238-c1029d1128d5.png)

This replicates figure 3.7 page 84
```{r}
plot(simex_b$time,datasimex_b$scenario_1[,"H_s"],type="l",xlab="",ylab="")
lines(simex_b$time,datasimex_b$scenario_1[,"C_d"],lty=2)
lines(simex_b$time,datasimex_b$scenario_1[,"Yd"],lty=3)
lines(simex_b$time,datasimex_b$scenario_1[,"Yd_e"],lty=4)
legend(x=1944,y=130,legend=c("Wealth","Consumption","Disposable Income",
                             "Expecetd Disposable Income"),lty=c(1,2,3,4),bty="n")
```

![alt tag](https://cloud.githubusercontent.com/assets/11057808/17969059/12c114a2-6ac8-11e6-9e2a-216606ae6cdc.png)
