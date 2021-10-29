*limpio la memoria y cargo los datos
use "C:\Users\lcaravaggio_mecon\Desktop\Personales\UBA\Maestria\Felicidad\csv\felicidad ver6.dta" , clear



*Creo una variable numérica para idenpa y así poder ordenar la serie de tiempo
egen id_num = group(id)


*Seteo la serie de tiempo
sort id_num year
tsset id_num year


*creo la variable promedio
gen promedio=(felicidad+actual+futura)/3


*creo las variables en tasa
gen tfelicidad=(felicidad-L.felicidad)/L.felicidad*100
gen tpromedio=(promedio-L.promedio)/L.promedio*100
gen tpbi=(pbi-L.pbi)/L.pbi*100
gen tactual=(actual-L.actual)/L.actual*100
gen tfutura=(futura-L.futura)/L.futura*100
gen tppp=(ppp-L.ppp)/L.ppp*100
gen tpbir=(pibr-L.pibr)/L.pibr*100

*Creo las variables en tasa de las variables estandarizadas
*Las variables estandarizadas fueron llevadas a media cero por país de forma de facilitar su comparación. 
gen tfelicidade=(felicidade-L.felicidade)/L.felicidade
gen tpromedioe=(promedioe-L.promedioe)/L.promedioe


*creo la variable Componente principal
pca felicidade futurae actuale
predict comp1 score


*Creo las variables con la transformada de Helmert
helm felicidad pbi crisis post actual futura tfelicidad tpbi tpromedio tfelicidade felicidade crisiso futurae promedio promedioe gini comp1 tpbir tppp




*Corro el Panel VAR con 200 repeticiones de Monte Carlo (por defecto) y 3 rezagos por variable

*Para Felicidad y Tasa de PBI
sort id_num year
pvar felicidade tpbi, lag(2) gmm impulse monte 1000 decomp

*Para Futura y Tasa de PBI
sort id_num year
pvar futurae tpbi, lag(3) gmm impulse monte decomp

*Para Felicidad y crisis objetiva
sort id_num year
pvar felicidade crisiso, lag(3) gmm impulse monte decomp

*Entre felicidad y futura
sort id_num year
pvar futurae felicidade, lag(3) gmm impulse monte decomp


*Para Felicidad y Tasa de PBI PPP
sort id_num year
pvar felicidade tppp, lag(2) gmm impulse monte 1000 decomp



*Inversión del orden de Cholesky

sort id_num year
pvar tpbi felicidade, lag(3) gmm impulse monte decomp

sort id_num year
pvar tpbi futurae, lag(3) gmm impulse monte decomp

sort id_num year
pvar crisiso felicidade, lag(3) gmm impulse monte decomp

sort id_num year
pvar felicidade futurae, lag(3) gmm impulse monte decomp



-----------
* Test de Raiz Unitaria para PBI


*SOBRE TODOS LOS PANELES
drop if year==1995
drop if year==1996
drop if id=="RepDominicana"
xtunitroot llc tpbi, lags(1)
drop if id=="Ecuador"
drop if id=="Panama"
xtunitroot llc felicidade, lags(1)




*PANEL POR PANEL (menos interesante)
* Fisher
xtunitroot fisher pbi , dfuller lags(1)
xtunitroot fisher tpbi , dfuller lags(1)


* Perron
xtunitroot fisher pbi , pperron lags(1)
xtunitroot fisher tpbi , pperron lags(1)

* Im-Pesaran-Shin
xtunitroot ips pbi
xtunitroot ips tpbi


* Hadri
drop if year<=1996
drop if id=="RepDominicana"
xtunitroot hadri pbi , trend
xtunitroot hadri tpbi , trend


drop if id=="Chile"
drop if id=="Peru"
drop if id=="Uruguay"
drop if id=="Bolivia"
drop if id=="Brasil"





--------------------
*VAR

irf set felicidadip

var felicidade tpbi if id=="Argentina", lags(1/2) small dfk
irf create Argentina
var felicidade tpbi if id=="Ecuador", lags(1/2) small dfk
irf create Ecuador
var felicidade tpbi if id=="Mexico", lags(1/2) small dfk
irf create Mexico
var felicidade tpbi if id=="Venezuela", lags(1/2) small dfk
irf create Venezuela

irf graph oirf, impulse(felicidade) response(tpbi) ustep(4)
irf graph oirf, impulse(tpbi) response(felicidade) ustep(4)



irf set promedioip

var promedioe tpbi if id=="Argentina", lags(1/2) small dfk
irf create Argentina
var promedioe tpbi if id=="Ecuador", lags(1/2) small dfk
irf create Ecuador
var promedioe tpbi if id=="Mexico", lags(1/2) small dfk
irf create Mexico
var promedioe tpbi if id=="Venezuela", lags(1/2) small dfk
irf create Venezuela

irf graph oirf, impulse(promedioe) response(tpbi) ustep(4)
irf graph oirf, impulse(tpbi) response(promedioe) ustep(4)





-------------------
*Gráficos de Impulso respuesta(?) y Granger sobre los datos de VAR de un país contra otro

var felarg tpbiarg felbra, lags(2)
vargranger
irf create Brasil
var felarg tpbiarg felchi, lags(2)
vargranger
irf create Chile
var felarg tpbiarg feluru, lags(1)
vargranger
irf create Uruguay
var felarg tpbiarg felpar, lags(3)
vargranger
irf create Paraguay

irf graph oirf, impulse(tpbiarg) response(felarg) ustep(4)




------------
*Viejos

*Para Tasa de Felicidad y Tasa de PBI

sort id_num year
pvar tfelicidade tpbi, lag(3) gmm impulse monte decomp

*Corro el Panel VAR con 200 repeticiones de Monte Carlo (por defecto) y 3 rezagos por variable
*Para Tasa de Indicador Promedio y Tasa de PBI

sort id_num year
pvar tpromedio tpbi, lag(3) gmm impulse monte decomp


---------
*La parte de los VAR por país
var felicidade tpbi if id=="Argentina", lags(1) small dfk
varnorm, jbera
varwle
varlmar, mlag(6)
pvargranger

