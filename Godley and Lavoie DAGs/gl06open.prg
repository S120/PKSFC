' MODEL OPEN for Eviews version 6
' from Wynne Godley & Marc Lavoie
' MONETARY ECONOMICS
' Chapter 6

' This program creates model OPEN, described in chapter 6, and simulates the model
' to produce results in par. 6.7 & 6.8


' ****************************************************************************
' Copyright (c) 2006 Gennaro Zezza
' Permission is hereby granted, free of charge, to any person obtaining a 
' copy of this software and associated documentation files (the "Software"),
' to deal in the Software without restriction, including without limitation
' the rights to use, copy, modify, merge, publish, distribute, sublicense, 
' and/or sell copies of the Software, and to permit persons to whom the 
' Software is furnished to do so, subject to the following conditions:
' 
' The above copyright notice and this permission notice shall be included in 
' all copies or substantial portions of the Software.
' 
' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
' FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
' IN THE SOFTWARE.
' ****************************************************************************

' Create a workfile, naming it OPEN, to hold annual data from 1945 to 2010

wfcreate(wf=open, page=annual) a 1945 2010

' Creates and documents series
series b_cb_n
b_cb_n.displayname Bills held by the Central bank in Country N
series b_cb_s
b_cb_s.displayname Bills held by the Central bank in Country S
series b_h_n
b_h_n.displayname Bills held by households, Country N
series b_h_s
b_h_s.displayname Bills held by households, Country S
series b_s_n
b_s_n.displayname Supply of government bills in Country N
series b_s_s
b_s_s.displayname Supply of government bills in Country S
series cons_n
cons_n.displayname Households consumption, Country N
series cons_s
cons_s.displayname Households consumption, Country S
series g_n
g_n.displayname Government expenditure, Country N
series g_s
g_s.displayname Government expenditure, Country S
series h_h_n
h_h_n.displayname Cash held by households, Country N
series h_h_s
h_h_s.displayname Cash held by households, Country S
series h_s_n
h_s_n.displayname Supply of cash in Country N
series h_s_s
h_s_s.displayname Supply of cash in Country S
series im_n
im_n.displayname Imports, Country N
series im_s
im_s.displayname Imports, Country S
series or_n
or_n.displayname Gold holding by Central bank in Country N
series or_s
or_s.displayname Gold holding by Central bank in Country S
series p_g_bar
p_g_bar.displayname Price of gold, set exogenously
series p_g_n
p_g_n.displayname Price of gold in Country N
series p_g_s
p_g_s.displayname Price of gold in Country S
series r_n
r_n.displayname Interest rate on bills in Country N
series r_s
r_s.displayname Interest rate on bills in Country S
series r_bar_n
r_bar_n.displayname Interest rate on bills set exogenously in Country N
series r_bar_s
r_bar_s.displayname Interest rate on bills set exogenously in Country S
series t_n
t_n.displayname Tax payments, Country N
series t_s
t_s.displayname Tax payments, Country S
series v_n
v_n.displayname Households wealth, Country N
series v_s
v_s.displayname Households wealth, Country S
series x_n
x_n.displayname Exports, Country N
series x_s
x_s.displayname Exports, Country S
series xr
xr.displayname Exchange rate (units of currency S for one unit of currency N)
series xr_bar
xr_bar.displayname Exchange rate, set exogenously
series y_n
y_n.displayname National income, Country N
series y_s
y_s.displayname National income, Country S
series yd_n
yd_n.displayname National disposable income, Country N
series yd_s
yd_s.displayname National disposable income, Country S

' Generate parameters
series alpha1_n
alpha1_n.displayname Propensity to consume out of income in Country N
series alpha1_s
alpha1_s.displayname Propensity to consume out of income in Country S
series alpha2_n
alpha2_n.displayname Propensity to consume out of wealth in Country N
series alpha2_s
alpha2_s.displayname Propensity to consume out of wealth in Country S
series lambda0_n
lambda0_n.displayname Parameter in asset demand function, Country N
series lambda0_s
lambda0_s.displayname Parameter in asset demand function, Country S
series lambda1_n
lambda1_n.displayname Parameter in asset demand function, Country N
series lambda1_s
lambda1_s.displayname Parameter in asset demand function, Country S
series lambda2_n
lambda2_n.displayname Parameter in asset demand function, Country N
series lambda2_s
lambda2_s.displayname Parameter in asset demand function, Country S
series mu_n
mu_n.displayname Import propensity, Country N
series mu_s
mu_s.displayname Import propensity, Country S
series theta_n
theta_n.displayname Tax rate in Country N
series theta_s
theta_s.displayname Tax rate in Country S


' Set sample size to all workfile range
smpl @all

' Assign values for
'   PARAMETERS
alpha1_n = 0.6
alpha1_s = 0.7
alpha2_n = 0.4
alpha2_s = 0.3
lambda0_n = 0.635
lambda0_s = 0.67
lambda1_n = 5
lambda1_s = 6
lambda2_n = 0.01
lambda2_s = 0.07
mu_n = 0.18781
mu_s = 0.18781
theta_n = 0.2
theta_s = 0.2
'   EXOGENOUS
g_n = 20
g_s = 20
p_g_bar = 1
r_bar_n = 0.025
r_n = r_bar_n
r_bar_s = 0.025
r_s = r_bar_s
xr_bar = 1
'   Starting values for stocks
b_cb_n = 11.622
b_cb_s = 11.622
b_h_n = 64.865
b_h_s = 64.865
b_s_n = 76.486
b_s_s = 76.486
or_n = 10
or_s = 10
v_n = 86.487
v_s = 86.487
h_h_n = v_n - b_h_n
h_h_s = v_s - b_h_s
h_s_n = h_h_n
h_s_s = h_h_s

' Create a model object, and name it open_mod

model open_mod

' Add equations to model OPEN

' Determination of national income in Country N - eq. 6.O.1
open_mod.append y_n = cons_n + g_n + x_n - im_n

' Determination of national income in Country S - eq. 6.O.2
open_mod.append y_s = cons_s + g_s + x_s - im_s

' Imports in Country N - eq. 6.O.3
open_mod.append im_n = mu_n*y_n

' Imports in Country S - eq. 6.O.4
open_mod.append im_s = mu_s*y_s

' Exports of Country N - eq. 6.O.5
open_mod.append x_n = im_s/xr

' Exports of Country S - eq. 6.O.6
open_mod.append x_s = im_n*xr

' Disposable income in Country N - eq. 6.O.7
open_mod.append yd_n = y_n - t_n + r_n(-1)*b_h_n(-1)

' Disposable income in Country S - eq. 6.O.8
open_mod.append yd_s = y_s - t_s + r_s(-1)*b_h_s(-1)

' Tax payments in Country N - eq. 6.O.9
open_mod.append t_n = theta_n*(y_n + r_n(-1)*b_h_n(-1))

' Tax payments in Country S - eq. 6.O.10
open_mod.append t_s = theta_s*(y_s + r_s(-1)*b_h_s(-1))

' Wealth accumulation in Country N - eq. 6.O.11
open_mod.append v_n = v_n(-1) + (yd_n - cons_n)

' Wealth accumulation in Country S - eq. 6.O.12
open_mod.append v_s = v_s(-1) + (yd_s - cons_s)

' Consumption function in Country N - eq. 6.O.13
open_mod.append cons_n = alpha1_n*yd_n + alpha2_n*v_n(-1)

' Consumption function in Country S - eq. 6.O.14
open_mod.append cons_s = alpha1_s*yd_s + alpha2_s*v_s(-1)

' Cash money held in Country N - eq. 6.O.15
open_mod.append h_h_n = v_n - b_h_n

' Cash money held in Country S - eq. 6.O.16
open_mod.append h_h_s = v_s - b_h_s

' Demand for government bills in Country N - eq. 6.O.17
open_mod.append b_h_n = v_n*(lambda0_n + lambda1_n*r_n - lambda2_n*(yd_n/v_n))

' Demand for government bills in Country S - eq. 6.O.18
open_mod.append b_h_s = v_s*(lambda0_s + lambda1_s*r_s - lambda2_s*(yd_s/v_s))

' Supply of government bills in Country N - eq. 6.O.19
open_mod.append b_s_n = b_s_n(-1) + (g_n + r_n(-1)*b_s_n(-1)) - (t_n + r_n(-1)*b_cb_n(-1))

' Supply of government bills in Country S - eq. 6.O.20
open_mod.append b_s_s = b_s_s(-1) + (g_s + r_s(-1)*b_s_s(-1)) - (t_s + r_s(-1)*b_cb_s(-1))

' Bills held by Central bank in Country N - eq. 6.O.21
open_mod.append b_cb_n = b_s_n - b_h_n

' Bills held by Central bank in Country S - eq. 6.O.22
open_mod.append b_cb_s = b_s_s - b_h_s

' Gold holding by Central bank in Country N - eq. 6.O.23
open_mod.append or_n = or_n(-1) + (h_s_n - h_s_n(-1) -(b_cb_n - b_cb_n(-1)))/p_g_n

' Gold holding by Central bank in Country S - eq. 6.O.24
open_mod.append or_s = or_s(-1) + (h_s_s - h_s_s(-1) -(b_cb_s - b_cb_s(-1)))/p_g_s

' Supply of cash in Country N - eq. 6.O.25
open_mod.append h_s_n = h_h_n

' Supply of cash in Country S - eq. 6.O.26
open_mod.append h_s_s = h_h_s

' Price of gold in Country N - eq. 6.O.27
open_mod.append p_g_n = p_g_bar

' Price of gold in Country S - eq. 6.O.28
open_mod.append p_g_s = p_g_n*xr

' Exchange rate - eq. 6.O.29
open_mod.append xr = xr_bar

' Interest rate in Country N - eq. 6.O.30
open_mod.append r_n = r_bar_n

' Interest rate in Country S - eq. 6.O.31
open_mod.append r_s = r_bar_s

' End of model

' Select the baseline scenario

open_mod.scenario baseline

' Drop first observation to get starting values for solving the model
smpl 1946 @last

' First experiment: increase in the propensity to import of Country S
smpl 1960 @last
mu_s = 0.20781
smpl @all

' Solve the model for the current sample

open_mod.solve(i=p)

' stores g_s for the current scenario
series g_s_0 = g_s

' Creates charts from simulated variables

' Creates the chart in Figure 6.8
graph fig6_8.line y_n_0 y_s_0
fig6_8.options linepat
fig6_8.setelem(1) lcolor(blue) lwidth(2) lpat(1)
fig6_8.setelem(2) lcolor(red) lwidth(2) lpat(2)
fig6_8.name(1) North Country GDP
fig6_8.name(2) South Country GDP
fig6_8.addtext(t,just(c)) Figure 6.8 Evolution of GDP in the N and S Country\n following an increase in the import propensity of the S Country

show fig6_8

' Creates the chart in Figure 6.9
smpl 1950 2000
graph fig6_9.line v_s_0-v_s_0(-1) t_s_0-(g_s_0+r_s_0*b_h_s_0(-1)) x_s_0-im_s_0
fig6_9.options linepat
fig6_9.setelem(1) lcolor(red) lwidth(2) lpat(1)
fig6_9.setelem(2) lcolor(green) lwidth(2) lpat(2)
fig6_9.setelem(3) lcolor(blue) lwidth(2) lpat(3)
fig6_9.name(1) Change in households wealth - S Country
fig6_9.name(2) Government balance with the S Country
fig6_9.name(3) Trade balance - S Country
fig6_9.addtext(t,just(c)) Figure 6.9: Evolution of balances in the S Country\n following an increase in its import propensity

show fig6_9

' Creates the chart in Figure 6.10
smpl 1950 2000
graph fig6_10.line d(b_cb_s_0) d(h_h_s_0) p_g_s_0*d(or_s_0)
fig6_10.options linepat
fig6_10.setelem(1) lcolor(red) lwidth(2) lpat(1)
fig6_10.setelem(2) lcolor(green) lwidth(2) lpat(2)
fig6_10.setelem(3) lcolor(blue) lwidth(2) lpat(3)
fig6_10.name(1) Change in the stock of bills held by the Central bank - S Country
fig6_10.name(2) Change in the S Country stock of money
fig6_10.name(3) Value of the change in gold reserves of the Central bank - S Country
fig6_10.addtext(t,just(c)) Figure 6.10: Evolution of 3 components of the CB balance sheet in the S Country\n following an increase in its import propensity

show fig6_10

' Second experiment: increase in the propensity to save in Country S
smpl @all

' get the propensity to import back to its stable value
mu_s = 0.18781

' Select the first alternative Scenario

open_mod.scenario "Scenario 1"

smpl 1960 @last
alpha1_s = 0.6
smpl @all

' Solve the model for the current sample

open_mod.solve(i=p)

' Creates the chart in Figure 6.11
smpl 1950 2000
graph fig6_11.line d(b_cb_s_1) d(h_h_s_1) p_g_s_1*d(or_s_1)
fig6_11.options linepat
fig6_11.setelem(1) lcolor(red) lwidth(2) lpat(1)
fig6_11.setelem(2) lcolor(green) lwidth(2) lpat(2)
fig6_11.setelem(3) lcolor(blue) lwidth(2) lpat(3)
fig6_11.name(1) Change in the stock of bills held by the Central bank - S Country
fig6_11.name(2) Change in the S Country stock of money
fig6_11.name(3) Value of the change in gold reserves of the Central bank - S Country
fig6_11.addtext(t,just(c)) Figure 6.11: Evolution of the 3 components of the CB balance sheet in the S Country\n following an increase in its propensity to save

show fig6_11

