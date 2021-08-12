%mago(Nombre,Caracteristicas,TipoDeSangre,CasaQueOdia).
mago(harry,[coraje, amistad, orgullo, inteligencia],mestiza,slytherin).
mago(draco,[inteligencia, orgullo],pura,hufflepuff).
mago(hermione,[inteligencia, orgullo, responsabilidad],impura,ninguna).

casaApropiada(gryffindor,[coraje]).
casaApropiada(slytherin,[inteligencia,orgullo]).
casaApropiada(hufflepuff,[amistad]).
casaApropiada(ravenclaw,[inteligencia,responsabilidad]).

% Punto 1

permiteEntrar(Nombre,Casa):-
    mago(Nombre,_,_,_),
    casa(Casa),
    Casa \= slytherin.
permiteEntrar(Nombre,slytherin):-
    not(sangreImpura(Nombre)).

sangreImpura(Nombre):-
    mago(Nombre,_,impura,_).

% Punto 2

caracterApropiado(Nombre,Casa):-
    mago(Nombre,CaracteristicasMago,_,_),
    casaApropiada(Casa,CaracteristicasCasa),
    intersection(CaracteristicasCasa,CaracteristicasMago,CaracteristicasCasa).

% Punto 3

podriaQuedar(Nombre,Casa):-
    caracterApropiado(Nombre,Casa),
    permiteEntrar(Nombre,Casa),
    not(odiariaEntrar(Nombre,Casa)).
podriaQuedar(hermione,gryffindor).

odiariaEntrar(Nombre,Casa):-
    mago(Nombre,_,_,Casa).

% Punto 4

cadenaDeAmistades([Mago|[]]):-
    amistoso(Mago).
cadenaDeAmistades([Primero,Segundo |Cola]):-
    amistoso(Primero),
    podriaQuedar(Primero,Casa),
    podriaQuedar(Segundo,Casa),
    cadenaDeAmistades([Segundo | Cola]).

amistoso(Nombre):-
    mago(Nombre,Caracteristicas,_,_),
    member(amistad,Caracteristicas).
    
% ------------------------------------------------------------- ELIPSIS ------------------------------------------------------------------------------------------------------------------
% Parte 2

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

accionRealizada(harry,irFueraDeCama).
%accionRealizada(hermione,irAlTercerPiso).
accionRealizada(hermione,irSeccionRestringidaBiblioteca).
accionRealizada(harry,irAlBosque).
accionRealizada(harry,irAlTercerPiso).
accionRealizada(draco,irALasMazmorras).
accionRealizada(ron,ganarAjedrezMagico).
accionRealizada(hermione,salvarAmigosDeMuerte).
accionRealizada(harry,ganarAVoldemort).
accionRealizada(hermione,respondio(dondeSeEncuentraUnBezoar,20,snape)).
accionRealizada(hermione,respondio(comoHacerLevitarUnaPluma,25,flitwick)).

puntaje(irFueraDeCama,-50).
puntaje(irAlTercerPiso,-75).
puntaje(irAlBosque,-50).
puntaje(irSeccionRestringidaBiblioteca,-10).
puntaje(ganarAjedrezMagico,50).
puntaje(salvarAmigosDeMuerte,50).
puntaje(ganarAVoldemort,60).
puntaje(respondio(_,Dificultad,snape),Puntos):-
    Puntos is Dificultad / 2.
puntaje(respondio(_,Dificultad,Profesor),Dificultad):-
    Profesor \= snape.

% Punto 1
% A)
buenAlumno(Nombre):-
    accionRealizada(Nombre,_),
    forall(accionRealizada(Nombre,Accion),not(malaAccion(Accion))).

malaAccion(Accion):-
    puntaje(Accion,Puntos),
    Puntos < 0.

% B)
recurrente(Accion):-
    accionRealizada(Mago1,Accion),
    accionRealizada(Mago2,Accion),
    Mago1 \= Mago2.

% Punto 2

puntosAcumulados(Casa,PuntajeTotal):-
    findall(PuntosAlumno,puntosDeMiembro(Casa,PuntosAlumno),Puntos),
    sum_list(Puntos, PuntajeTotal).

puntosDeMiembro(Casa,PuntosAlumno):-
    esDe(Nombre,Casa),
    accionRealizada(Nombre,Accion),
    puntaje(Accion,PuntosAlumno).

% Punto 3

casaGanadora(Casa):-
    casa(Casa),
    forall(casasRivales(Casa,CasaRival),tieneMasPuntos(Casa,CasaRival)).

casasRivales(Casa,CasaRival):-
    casa(CasaRival),
    Casa \= CasaRival.

casa(Casa):-
    casaApropiada(Casa,_).

tieneMasPuntos(Casa,CasaRival):-
    puntosAcumulados(Casa,PuntosCasa),
    puntosAcumulados(CasaRival,PuntosCasaRival),
    PuntosCasa > PuntosCasaRival.

% Punto 4

% agregue las lineas 72,73,82,83,84 y 85