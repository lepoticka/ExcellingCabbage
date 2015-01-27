Functional Reactive Programming - mini Excel
=============================================
Naša naloga je bila ustvariti tabelo polj, podobno excelu, kjer tudi mi polja imenujemo celice. V celicah se lahko nahajajo vrednosti ali formule, ki pa se spreminjajo ob spremembi odvisnih polj. Torej dopuščamo možnost, da so lahko celice medsabo odvisne in če spremenimo eno celico, se bo istočasno spremenila tudi celica, ki je od nje odvisna. To smo naredili tako, da smo uporabili FRP programsko paradigmo.

### Parsec
V ExParser.hs smo uvozili knjižnico Parsec, s katero smo si pomagali sprejete stringe pretvoriti v drugo podatkovno strukturo.

### Funkcijsko Reaktivno programiranje-FRP
## Reaktivno programiranje
Reaktivno programiranje je programska paradigma, ki je usmerjena okoli podatkovnih tokov(pretoku podatkov) in širjenja sprememb. 
To pomeni, da bi bilo mogoče izraziti statične ali dinamične podatkovne tokove, ki bi bili z lahkoto uporabljeni v programskih jezikih in da bi temeljni izvršni model samodejno širil spremembe preko podatkovnih tokov.

Za lažje razumevanje si bomo ogledali enostaven primer.
Recimo, da imamo 3x3 excel tabelo. V celicah A1, A2, A3 se nahajajo zneski, ki jih želite vezati na banko. V celici B1 se nahaja obrestna mera=r, ki jo ponuja banka. Celice C1, C2, C3 kažejo vaš zaslužek po enem letu, torej C1 = A1*(1+B1) in podobno za C2 in C3. Vidimo, da so celice C odvisne od celice B1. 
Recimo, da se banka odloči povečati obrestno mero, torej spremenimo samo celico B1. Potem se magično spremenijo vse celice C(they update).
Torej mi nismo sami spremenili vrednosti celic C, ampak so se one same spremenile, ker se je vrednost od katere so odvisne(B1) spremenila -> Reaktivno programiranje

## FRP
FRP je programska paradigma za reaktivno programiranje z uporabo gradnikov funkcionalnega programiranja(npr.: map, reduce, filter).
FRP se uporablja za programiranje grafičnih uporabniških vmesnikov(GUI), robotike, animacij in glasbe, katerega namen je poenostaviti te težave z izrecnim modeliranjem časa.

Bistveno pri funkcijsko reaktivnem programiranju je da imamo dva pomembna podatkovna tipa, to sta DOGODEK(=Event) in OBNAŠANJE(=Behaviour) in različne načine, kako jih združiti.

1.) data Event a:
Dogodek predstavlja tok dogodkov, ki se pojavljajo v točno določenem času. Lahko si predstavljamo dogodek, kot neskončen seznam vrednosti, ki so označeni s pripadajočim ustreznim časom njihovega nastanka.
-> type Event a = [(Time, a)]

2.) data Behaviour a
Onbašanje predstavlja vrednost, ki se spreminja v času. Torej časovno spreminjajoča vrednost je obnašanje, ki ima lahko različne vrednosti ob različnih časih.
Predstavljamo si lahko na takšen način:
-> type Behaviour a = Time -> a

## ExReactive.hs 
Knjižnica Threepenny-gui, preko katere smo implementirali funkcijsko reaktivno programiranje, je knjižica, ki se uporablja za grafične uporabniške vmesnike(= graphical user interfaces).
GUI okvir(=framework) uporablja spletni brskalnik(=web brauser) za prikaz rezultatov.
Program napisan s Threepenny je v bistvu majhen spletni strežnik, ki prikazuje uporabniški vmesnik kot spletno stran brskalnika, s katero se povezuje.
Threepenny-gui vsebuje več modulov, med njimi tudi Graphics.UI.Threepenny.Core, ki vsebuje glavne funkcije. Pravtako tako smo uporabili funkcije iz modulov Reactive.Threepenny, Graphics.UI.Threepenny.Attributes in Graphics.UI.Threepenny.Elements.

Naša datoteka EXReactive.hs implementira vse potrebne funkcije, da se naredijo osnovni bloki reaktivnega excela z uporabo knjižice Threepenny-gui-ja.

## excell.hs
Naša datoteka excell.hs je glavna datoteka, ki kliče vse ostale knjižice in prikaže rezultat.

##
Da zaženemo naš program, najprej v terminalu(Linux) vtipkaš:
cabal configure, da zaženeš datoteko ExcellingCabbage.cabal
cabal install, da inštaliraš Cabal knjižice
cabal build
cabal run

Terminal ti vrne spletni vir http:/127.0.0.1:8023/ , ki jo vtipkaš v spletni brskalnik in rezultat se prikaže, v našem primeru mini excel tabela.

### Diagrama
Sliki excellingcabbage.pdf in display.pdf prikazujeta strukturo ozadja naše kode, kako deluje naš program.
Na slikah črke označujejo event ali behaviour, medtem ko poimenovani pravokotni prostorčki predstavljajo funkcije.

