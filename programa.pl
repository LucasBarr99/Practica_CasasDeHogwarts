%mago(Nombre,Caracteristicas,TipoDeSangre,CasaQueOdia).
mago(harry,[coraje, amistad, orgullo, inteligencia],mestiza,slytherin).
mago(draco,[inteligencia, orgullo],pura,hufflepuff).
mago(hermione,[inteligencia, orgullo, responsabilidad],impura,ninguna).

casaApropiada(gryffindor,[coraje]).
casaApropiada(slytherin,[inteligencia,orgullo]).
casaApropiada(hufflepuff,[amistad]).
casaApropiada(ravenclaw,[inteligencia,responsabilidad]).

% Punto 1

permiteEntrar(_,gryffindor).
permiteEntrar(_,hufflepuff).
permiteEntrar(_,ravenclaw).
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
    
    

