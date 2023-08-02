$TITLE Trade
$OFFSYMXREF OFFSYMLIST
*Model of EAC

*Definition sets and subscripts
Set i      countries    /bur, ken, rwa, uga, tan, con, row/;
set k(i)   excl row     /bur, ken, rwa, uga, tan, con/;
set l(k)   excl Congo   /bur, ken, rwa, uga, tan/;
set m(k)   Congo        /con/;

alias(i,ii);
alias(k,kk);


*Initial data
Table qtrade(i,ii)     trade volumes in kg
          bur        ken        rwa        uga     tan        con       row
bur         0          0          0          0       0       3000         0
ken         0          0     260205       2435       0          0    174839
rwa         0          0          0          0       0   59272803     21400
uga      2450          0          0          0       0   24998976  11096971
tan  12161782   76001151   62417315  233426216       0    7023300         0
con      5000          0       3153          0  183990          0         0
row   6512827  528448851  124104066   60997314  174066  110667976         0
;

Table qwimp(i,ii)     import prices in dollar per kg
          bur      ken       rwa      uga     tan       con      row
bur         0        0         0        0       0    0.9555        0
ken         0        0    0.7209   0.5380       0         0   0.7909
rwa         0        0         0        0       0    0.4537   0.6199
uga    0.3302        0         0        0       0    0.2481   0.5571
tan    0.3914   0.4586    0.6538   0.3325       0    0.1505        0
con    0.3966        0    0.2845        0  0.2475         0        0
row    0.4286   0.4082    0.5242   0.3598  0.4589    0.2292        0
;


Table qwexp(i,ii)     export prices in dollar per kg
          bur      ken       rwa      uga     tan       con      row
bur         0        0         0        0       0    0.8847        0
ken         0        0    0.6731   0.4950       0         0   0.7323
rwa         0        0         0        0       0    0.4174   0.5740
uga    0.3038        0         0        0       0    0.2283   0.5158
tan    0.3601   0.4255    0.5930   0.3059       0    0.1385        0
con    0.3649        0    0.2617        0  0.2292         0        0
row    0.3943   0.3755    0.4823   0.3310  0.4222    0.2109        0
;

parameter  qcons(k)  consumption in kg
/bur 186639059, ken 800408523, rwa 251912936, uga 289757278,
 tan 4532753292, con 1672555912/;

parameter  qproduc(k)  production in kg
/bur 167960000, ken 196396000, rwa 124422400, uga 31429710,
 tan 4923425000, con 1470782000/;

parameter qwcons(k)  prices consumtion
/bur 0.3302, ken 0.4950, rwa 0.4174, uga 0.3038, tan 0.3059, con 0.2292/;

parameter qwproduc(k)  prices production
/bur 0.3193, ken 0.7431, rwa 0.1917, uga 0.2473, tan 0.3113, con 0.2202/;



*Scaling
parameter
tradeo(i,ii)   initial trade volumes
wimpo(i,ii)    initial import prices
wexpo(i,ii)    initial export prices
conso(k)       initial consumption
produco(k)     initial production
wconso(k)      initial prices consumption
wproduco(k)    initial prices production
wpimpo(k)      initial world market prices imports
;

tradeo(i,ii) = qtrade(i,ii)/1000;
wimpo(i,ii) = qwimp(i,ii)*1000;
wexpo(i,ii) = qwexp(i,ii)*1000;
conso(k) = qcons(k)/1000;
produco(k) = qproduc(k)/1000;
wconso(k) = qwcons(k)*1000;
wproduco(k) = qwproduc(k)*1000;
wpimpo(k) = qwexp("row",k)*1000;


*Data check
parameter  test1(k);

test1(k) = (sum(i,tradeo(k,i))+conso(k))
           -(sum(i,tradeo(i,k))+produco(k));

display
test1, tradeo, wimpo, wexpo, conso, produco, wconso, wproduco;


*Calibration CET
parameter
mu(k)      transformation elasticity cet
rhos(k)    substitution parameter cet
aes(k,i)   distribution coefficient cet
acon(k)    distribution coefficient consumption cet
tacet(k)   sum distribution coefficients cet
gammas(k)  efficiency coefficient cet
;


mu(k) = -2;
rhos(k) = (1/mu(k))-1;
aes(k,i)$tradeo(k,i) = (wexpo(k,i)*tradeo(k,i)**(1/mu(k)))/
           (sum(ii,(wexpo(k,ii)*tradeo(k,ii)**(1/mu(k)))$tradeo(k,ii))+
           wconso(k)*conso(k)**(1/mu(k)) );
acon(k) =  (wconso(k)*conso(k)**(1/mu(k)))/
           (sum(ii,(wexpo(k,ii)*tradeo(k,ii)**(1/mu(k)))$tradeo(k,ii))+
           wconso(k)*conso(k)**(1/mu(k)) );

tacet(k) = sum(i,aes(k,i))+acon(k);


display
aes, acon, tacet;


*Calibration CES
parameter
sig(k)       substitution elasticity ces
rhod(k)      substitution parameter ces
bes(i,k)     distribution coefficient ces
bproduc(k)   distribution coefficient prodution ces
tbces(k)     sum distribution coefficients ces
gammad(k)    efficiency coefficient ces
;


sig(k) = 2;
rhod(k) = (1/sig(k))-1;
bes(i,k)$tradeo(i,k) = (wimpo(i,k)*tradeo(i,k)**(1/sig(k)))/
           (sum(ii,(wimpo(ii,k)*tradeo(ii,k)**(1/sig(k)))$tradeo(ii,k))+
           wproduco(k)*produco(k)**(1/sig(k)) );
bproduc(k) =  (wproduco(k)*produco(k)**(1/sig(k)))/
           (sum(ii,(wimpo(ii,k)*tradeo(ii,k)**(1/sig(k)))$tradeo(ii,k))+
           wproduco(k)*produco(k)**(1/sig(k)) );

tbces(k) = sum(i,bes(i,k))+bproduc(k);


display
bes, bproduc, tbces;


*sum value domestic consumption and exports
*sum value domestic production and imports
parameter
vldom(k)      value domestic supply input
vldoml(k)     value domestic supply output
difdom(k)     difference in values dom
wdomdo(k)     price domestic supply input
wdomso(k)     price domestic supply output
difwdomo(k)   price difference domestic
tdomo(k)      rate domestic prices
domo(k)       quantity domestic supply input
domlo(k)      quantity domestic supply output
to(k,kk)      tariffs etc causingf price wedge between import and export prices
trowo(k)      tariff imports row
;

vldom(k) = sum(i,wimpo(i,k)*tradeo(i,k))+wproduco(k)*produco(k);
vldoml(k) = sum(i,wexpo(k,i)*tradeo(k,i))+wconso(k)*conso(k);
difdom(k) = vldom(k)-vldoml(k);
wdomdo(k) = vldom(k)/(sum(i,tradeo(i,k))+produco(k));
wdomso(k) = vldoml(k)/(sum(i,tradeo(k,i))+conso(k));
difwdomo(k) = wdomdo(k) - wdomso(k);
tdomo(k) = (wdomdo(k) - wdomso(k))/wdomso(k);
domo(k) = vldom(k)/wdomdo(k);
domlo(k) = vldoml(k)/wdomso(k);
to(k,kk)$tradeo(k,kk) = (wimpo(k,kk) - wexpo(k,kk))/wexpo(k,kk);
trowo(k) = (wimpo("row",k)-wpimpo(k))/wpimpo(k);

display
vldom, vldoml, difdom, wdomdo, wdomso, difwdomo, tdomo, domo, domlo, to, trowo;


gammas(k) = domo(k)/( sum(i,aes(k,i)$tradeo(k,i)*(tradeo(k,i)**(-rhos(k)))
            $tradeo(k,i))+ acon(k)*conso(k)**(-rhos(k)) )**(-1/rhos(k)) ;


gammad(k) = domo(k)/( sum(i,bes(i,k)$tradeo(i,k)*(tradeo(i,k)**(-rhod(k)))
            $tradeo(i,k))+ bproduc(k)*produco(k)**(-rhod(k)) )**(-1/rhod(k)) ;

display
gammas, gammad;


*Checks
parameter
himpo(i,k)    help import demand
hproduco(k)   help production
hexpo(k,i)    help export supply
hconso(k)     help domestic consumption
;


himpo(i,k)$tradeo(i,k) = ( gammad(k)**(-1)*domo(k)*bes(i,k)**sig(k)*wimpo(i,k)**(-sig(k))*
      (sum(ii,(bes(ii,k)**sig(k))$tradeo(ii,k)*(wimpo(ii,k)**(1-sig(k)))$tradeo(ii,k) ) +
            (bproduc(k)**sig(k)*wproduco(k)**(1-sig(k)) ) )**(sig(k)/(1-sig(k)))   )-tradeo(i,k);


hproduco(k) = ( gammad(k)**(-1)*domo(k)*bproduc(k)**sig(k)*wproduco(k)**(-sig(k))*
      (sum(ii,(bes(ii,k)**sig(k))$tradeo(ii,k)*(wimpo(ii,k)**(1-sig(k)))$tradeo(ii,k) ) +
            (bproduc(k)**sig(k)*wproduco(k)**(1-sig(k)) ) )**(sig(k)/(1-sig(k)))   )-produco(k);


hexpo(k,i)$tradeo(k,i) = ( gammas(k)**(-1)*domo(k)*aes(k,i)**mu(k)*wexpo(k,i)**(-mu(k))*
      (sum(ii,(aes(k,ii)**mu(k))$tradeo(k,ii)*(wexpo(k,ii)**(1-mu(k)))$tradeo(k,ii) ) +
            (acon(k)**mu(k)*wconso(k)**(1-mu(k)) ) )**(mu(k)/(1-mu(k)))   )-tradeo(k,i);

hconso(k) = ( gammas(k)**(-1)*domo(k)*acon(k)**mu(k)*wconso(k)**(-mu(k))*
      (sum(ii,(aes(k,ii)**mu(k))$tradeo(k,ii)*(wexpo(k,ii)**(1-mu(k)))$tradeo(k,ii) ) +
            (acon(k)**mu(k)*wconso(k)**(1-mu(k)) ) )**(mu(k)/(1-mu(k)))   )-conso(k);



display
himpo, hproduco, hexpo, hconso;


*Import and export prices within union are linked via transport costs and tariffs
parameter
taxo(k,kk)    initial absolute tax
rtaxo(k,kk)   initial ad valorem tax
vltaxo(k,kk)  initial tax revenue per country
tvltaxo(k)    initial total tax revenue per country
hvltaxo(k)    help initial tax revenue per country should be zero
;

taxo(k,kk) = wimpo(k,kk) - wexpo(k,kk);
rtaxo(k,kk)$tradeo(k,kk) = taxo(k,kk)/wexpo(k,kk);
vltaxo(k,kk) = rtaxo(k,kk)*wexpo(k,kk)*tradeo(k,kk);
tvltaxo(k) = sum(kk,vltaxo(k,kk))+trowo(k)*wpimpo(k)*tradeo("row",k);
hvltaxo(k) = sum(kk,vltaxo(k,kk)) + vldoml(k) - vldom(k);


display
taxo, rtaxo, vltaxo, tvltaxo, hvltaxo;


*Extra checks
parameter
impo(i,k)    initial imports
expo(k,i)    initial exports
h1(k)        help 1
h2(k)        help 2
h3(k,kk)     help 3
h4(k,kk)     help 4
;


impo(i,k) = tradeo(i,k);
expo(k,i) = tradeo(k,i);
h1(k) = (wdomdo(k)*domo(k)) - (wproduco(k)*produco(k)+sum(i,wimpo(i,k)*impo(i,k)));
h2(k) = (wdomso(k)*domo(k)) - (wconso(k)*conso(k)+sum(i,wexpo(k,i)*expo(k,i)));
h3(k,kk)$tradeo(k,kk) = (wimpo(k,kk) - wexpo(k,kk)*(1+to(k,kk)));
h4(k,kk)$tradeo(k,kk) = (impo(k,kk) - expo(k,kk));

display
h1, h2, h3, h4;


*Calibration supply and consumer demand functions
parameter
elass(k)   price elasticities of supply
elasd(k)   price elasticities of demand
as(k)      intercept supply functions
ad(k)      intercept demand functions
bs(k)      slopes supply functions
bd(k)      slopes demand functions
hprod(k)   help production
hcons(k)   help consumption
;

elass(k) =  0.1;
elasd(k) = -0.3;

bs(k) = elass(k)*(produco(k)/wproduco(k));
as(k) = produco(k)-bs(k)*wproduco(k);
bd(k) = elasd(k)*(conso(k)/wconso(k));
ad(k) = conso(k)-bd(k)*wconso(k);

hprod(k) = ( as(k)+bs(k)*wproduco(k) )-produco(k);
hcons(k) = ( ad(k)+bd(k)*wconso(k) )-conso(k);

display
hprod, hcons;




*Here the actual model starts
Variables
imp(i,k)       imports of country k from country i
produc(k)      production in country k
domd(k)        domestic demand in country k

exp(k,i)       exports of country k from country i
cons(k)        consumption in country k
doms(k)        domestic supply in country k

wimp(i,k)      prices of imports of country k from country i
wproduc(k)     prices of production in country k
wdomd(k)       prices of domestic supply demand supply in country k

wexp(k,i)      prices of exports of country k from country i
wcons(k)       prices of consumption in country k
wdoms(k)       prices of domestic supply demand supply in country k

tdom(k)        rate between domestic prices
t(k,kk)        ad valorem price wedge
wpimp(k)       world market price imports from rest of the world
trow(k)        tariff rate for imports from the rest of the world

cost(k)        cost index to make function homogenous of degree zero
inc(k)         income index to make function homogenous of degree zero

hel            help variable
;


Equations
eimp(i,k)      eq imports of country k from country i
eproduc(k)     eq production in country k
evdem(k)       eq value imports plus production equals value domestic supply

eexp(k,i)      eq exports of country k from country i
econs(k)       eq consumption in country k
evsup(k)       eq value exports plus consumption equals value domestic supply

edom(k)        eq equilibrium domestic
emar(k,kk)     eq market equilibrium

esprod(k)      eq supply domestic production
edcon(k)       eq demand domestic consumption

ewdom(k)       eq link between domestic prices
eprice(k,kk)   eq price wedge
ewpimp(k)      eq world market price row

ehel           eq help
;

*demand and supply equations
eimp(i,k)..     imp(i,k)$tradeo(i,k) =e= gammad(k)**(-1)*domd(k)*(bes(i,k)**sig(k))$tradeo(i,k)*(wimp(i,k)**(-sig(k)))$tradeo(i,k)*
                (sum(ii,(bes(ii,k)**sig(k))$tradeo(ii,k)*(wimp(ii,k)**(1-sig(k)))$tradeo(ii,k) ) +
                (bproduc(k)**sig(k)*wproduc(k)**(1-sig(k)) ) )**(sig(k)/(1-sig(k)))   ;


eproduc(k)..    produc(k) =e= gammad(k)**(-1)*domd(k)*bproduc(k)**sig(k)*wproduc(k)**(-sig(k))*
                (sum(ii,(bes(ii,k)**sig(k))$tradeo(ii,k)*(wimp(ii,k)**(1-sig(k)))$tradeo(ii,k) ) +
                (bproduc(k)**sig(k)*wproduc(k)**(1-sig(k)) ) )**(sig(k)/(1-sig(k)))   ;

eexp(k,i)..     exp(k,i)$tradeo(k,i) =e= gammas(k)**(-1)*doms(k)*(aes(k,i)**mu(k))$tradeo(k,i)*(wexp(k,i)**(-mu(k)))$tradeo(k,i)*
                (sum(ii,(aes(k,ii)**mu(k))$tradeo(k,ii)*(wexp(k,ii)**(1-mu(k)))$tradeo(k,ii) ) +
                (acon(k)**mu(k)*wcons(k)**(1-mu(k)) ) )**(mu(k)/(1-mu(k)));

econs(k)..      cons(k) =e= gammas(k)**(-1)*doms(k)*acon(k)**mu(k)*wcons(k)**(-mu(k))*
                (sum(ii,(aes(k,ii)**mu(k))$tradeo(k,ii)*(wexp(k,ii)**(1-mu(k)))$tradeo(k,ii) ) +
                (acon(k)**mu(k)*wcons(k)**(1-mu(k)) ) )**(mu(k)/(1-mu(k)));


*supply and demand equations domestic production and consumption
esprod(k)..     produc(k) =e= as(k)+bs(k)*(wproduc(k)/cost(k));
edcon(k)..      cons(k) =e= ad(k)+bd(k)*(wcons(k)/inc(k));


*zero profit conditions
evdem(k)..      wdomd(k)*domd(k) =e= wproduc(k)*produc(k)+sum(i,wimp(i,k)*imp(i,k));

evsup(k)..      wdoms(k)*doms(k) =e= wcons(k)*cons(k)+sum(i,wexp(k,i)*exp(k,i));


*market equilibrium equations
edom(k)..       doms(k) =e= domd(k);
emar(k,kk)$tradeo(k,kk)..    imp(k,kk) =e= exp(k,kk);


*Price equations
ewdom(k)..      wdomd(k) =e= (1+tdom(k))*wdoms(k);
eprice(k,kk)$tradeo(k,kk)..  wimp(k,kk) =e= wexp(k,kk)*(1+t(k,kk));
ewpimp(k)..     wimp("row",k) =e= (1+trow(k))*wpimp(k);

*fake variable to be maximized
ehel..          hel =e= sum(k,wdomd(k)*domd(k));


*exogenous variables
wpimp.fx(k) = wpimpo(k);
wexp.fx(k,"row") = wexpo(k,"row");

tdom.fx(k) = tdomo(k);
cost.fx(k) = 1;
inc.fx(k) = 1;

*base scenario
trow.fx(k) = 0.75;
t.fx(k,kk)$tradeo(k,kk) = to(k,kk);
*Scenario 1
*trow.fx(k) = trowo(k);
t.fx(l,kk)$tradeo(l,kk) = 0;
t.fx(m,kk)$tradeo(m,kk) = to(m,kk);
*Scenario 2
*trow.fx(k) = trowo(k)*0.4;
*t.fx(l,kk)$tradeo(l,kk) = 0;
*t.fx(m,kk)$tradeo(m,kk) = to(m,kk)*0.5;
*Scenario 3
*trow.fx(k) = trowo(k);
*t.fx(l,kk)$tradeo(l,kk) = 0.3;
*t.fx(m,kk)$tradeo(m,kk) = to(m,kk)*0.5;
*Scenario 4
*trow.fx(k) = trowo(k);
*t.fx(l,kk)$tradeo(l,kk) = 0;
*t.fx(m,kk)$tradeo(m,kk) = 0;


*starting values
imp.l(i,k)$tradeo(i,k) = himpo(i,k);
produc.l(k) = produco(k);
domd.l(k) = domo(k);

exp.l(k,i)$tradeo(k,i) = hexpo(k,i);
cons.l(k) = conso(k);
doms.l(k) = domo(k);

wimp.l(i,k)$tradeo(i,k) = wimpo(i,k)+1;
wproduc.l(k) = wproduco(k);
wdomd.l(k) = wdomdo(k);

wexp.l(k,i)$tradeo(k,i) = wexpo(k,i)+1;
wcons.l(k) = wconso(k);
wdoms.l(k) = wdomso(k);

tdom.l(k) = tdomo(k);
t.l(k,kk)$tradeo(k,kk) = to(k,kk);


*The solver is asked to solve the model
MODEL EAC /ALL/;
SOLVE EAC Maximizing hel USING NLP;


display
imp.l, produc.l, domd.l, exp.l, cons.l,
wimp.l, wproduc.l, wexp.l, wcons.l, wdomd.l, tdom.l;


*With the help of model outcomes some extras are calcualted
parameter
dtax(k,kk)       tariff revenues detailed
tax(k)           tariff revenues by country
taxdom(k)        tax domestic
;

dtax(k,kk)$tradeo(k,kk) = t.l(k,kk)*wexp.l(k,kk)*imp.l(k,kk);
tax(k) = sum(kk,dtax(k,kk))+trow.l(k)*wpimp.l(k)*imp.l("row",k);
taxdom(k) = tdom.l(k)*wdoms.l(k)*domd.l(k);


display
dtax, tax, vltaxo, taxdom;


*Changes
parameter
chimp(i,k)       change imports of country k from country i
chproduc(k)      change production in country k
chdomd(k)        change domestic demand in country k

chexp(k,i)       change exports of country k from country i
chcons(k)        change consumption in country k
chdoms(k)        change domestic supply in country k

chwimp(i,k)      change prices of imports of country k from country i
chwproduc(k)     change prices of production in country k
chwdomd(k)       change prices of domestic supply demand supply in country k

chwexp(k,i)      change prices of exports of country k from country i
chwcons(k)       change prices of consumption in country k
chwdoms(k)       change prices of domestic supply demand supply in country k

chtdom(k)        change rate between domestic prices
cht(k,kk)        change ad valorem price wedge

chdtax(k,kk)     change detailed tariff revenue
chtax(k)         change total tariff revenue
;


chimp(i,k)$tradeo(i,k) = ((imp.l(i,k)-impo(i,k))/impo(i,k))*100;
chproduc(k) = ((produc.l(k)-produco(k))/produco(k))*100;
chdomd(k) = ((domd.l(k)-domo(k))/domo(k))*100;

chexp(k,i)$tradeo(k,i) = ((exp.l(k,i)-expo(k,i))/expo(k,i))*100;
chcons(k) = ((cons.l(k)-conso(k))/conso(k))*100;
chdoms(k) = ((doms.l(k)-domo(k))/domo(k))*100;

chwimp(i,k)$tradeo(i,k) = ((wimp.l(i,k)-wimpo(i,k))/wimpo(i,k))*100;
chwproduc(k) = ((wproduc.l(k)-wproduco(k))/wproduco(k))*100;
chwdomd(k) = ((wdomd.l(k)-wdomdo(k))/wdomdo(k))*100;

chwexp(k,i)$tradeo(k,i) = ((wexp.l(k,i)-wexpo(k,i))/wexpo(k,i))*100;
chwcons(k) = ((wcons.l(k)-wconso(k))/wconso(k))*100;
chwdoms(k) = ((wdoms.l(k)-wdomso(k))/wdomso(k))*100;

chtdom(k) = ((tdom.l(k)-tdomo(k))/tdomo(k))*100;
cht(k,kk)$tradeo(k,kk) = t.l(k,kk)-to(k,kk);

chdtax(k,kk)$tradeo(k,kk) = ((dtax(k,kk)-vltaxo(k,kk))/vltaxo(k,kk))*100;
chtax(k) = ((tax(k)-tvltaxo(k))/tvltaxo(k))*100;


display
chimp, chproduc, chdomd, chexp, chcons, chdoms,
chwimp, chproduc, chwdomd, chwexp, chwcons, chwdoms, chtdom, cht,
chdtax, chtax;


*Welfare effects
parameter
welcon(k)     change welfare consumption
welpro(k)     change welfare production
welbud(k)     change budget revenue
weltot(k)     change total welfare
;


welcon(k) = ( -ad(k)*(wcons.l(k)/inc.l(k))-0.5*bd(k)*(wcons.l(k)/inc.l(k))**2 )-
            ( -ad(k)*wconso(k)-0.5*bd(k)*wconso(k)**2);

welpro(k) = (as(k)*wproduc.l(k)/cost.l(k)+0.5*bs(k)*(wproduc.l(k)/cost.l(k))**2 ) -
            (as(k)*wproduco(k)+0.5*bs(k)*wproduco(k)**2);

welbud(k) = tax(k)-tvltaxo(k);

weltot(k) = welcon(k)+welpro(k)+welbud(k);


display
welcon, welpro, welbud, weltot;
