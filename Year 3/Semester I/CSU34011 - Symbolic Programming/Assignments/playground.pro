woman(mia).
woman(jody).
woman(yolanda).
playsAirGuitar(jody).
party.


'Pat is happy every day it is sunny'.
happyPat(sunnyDay).
sunnyDay(pat,happy).
day(mon).
day(tues).
day(wed).
day(thur).
day(fri).
day(sat).
day(sun).
day(sunny).
sunnyDay(mon).
sunnyDay(wed).
sunnyDay(fri).
sunnyDay(sun).
happy(pat,X) :- sunnyDay(X).
pat(happy) :- day(sunny).


footmassage(tom, mia).
footmassage(mia, jenny).
footmassage(michael, mia).
killed(mars, X) :- footmassage(X, mia).


tasty(pizza).
tasty(apple).
tasty(lasagna).
tasty(cake).
nutr(fibre).
nutr(rice).
nutr(salad).
nutr(chicken).
eaten(jules, X) :- tasty(X); nutr(X).


wizard(ron).
hasWand(harry).
quidditchPlayer(harry).
wizard(X):-  hasBroom(X),  hasWand(X).
hasBroom(X):-  quidditchPlayer(X). 


wizard(ron).
witch(ron).
wizard(hermione).
witch(hermione).
wizard(harry).
wizard(Y).
witch(Y).


add(0, X, X).
add(succ(X), Y, succ(Z)) :- add(X, Y, Z).

di(olga, kat).
di(nat, olga).
di(irina, nat).

in(X, Y) :- di(X, Y).
in(X, Y) :- di(X, Z), in(Z, Y).


directTrain(saarbruecken,dudweiler).
directTrain(forbach,saarbruecken).
directTrain(freyming,forbach).
directTrain(stAvold,freyming).
directTrain(fahlquemont,stAvold).
directTrain(metz,fahlquemont).
directTrain(nancy,metz). 

tft(X, Y) :- directTrain(X, Y).
tft(X, Y) :- directTrain(X, Z), tft(Z, Y).


gt(succ(_), 0).
gt(succ(X), succ(Y)) :- gt(X, Y).
