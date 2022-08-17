
:- module(_,_,[classic,assertions,regtypes]).
:- use_module(library(lists)).

 
author_data('Jimenez','Perez','Juan','b190204').         %Autor: Juan Jiménez Pérez 190204
:-doc(hide,[author_data/4]).

:-doc(title,"Practica 2 Programacion logica y declarativa").
:-doc(author,"Juan Jimenez Perez 190204").
:-doc(hide,[author_data/4,bind/1,hexd/1,byte/1,hex_byte/1,binary_byte/1]).
:-doc(module,"
@section{Pruebas de ejecucion }
@subsection{Predicado 1. Particiones M-arias de un numero.}
@begin{enumerate}

@item Predicado 1.1: pots/3:
@begin{verbatim}
?- pots(3,9,Ps).

Ps = [9,3,1] ? ;
no
?-

@end{verbatim}

@item Predicado 1.2: mpart/3:
@begin{verbatim}
?- mpart(3,9,P).

P = [9] ? ;
P = [3,3,3] ? ;
P = [3,3,1,1,1] ? ;
P = [3,1,1,1,1,1,1] ? ;
P = [1,1,1,1,1,1,1,1,1] ? ;
no
?-

@end{verbatim}

@item Predicado 1.3: maria/3:
@begin{verbatim}
?- maria(3,9,M).

M = 5.
?-

@end{verbatim}

@subsection{Predicado 2. Aranyas de expansion de un grafo.}
@item Predicado 2.1: guardar_grafo/1:
@begin{verbatim}
?- guardar_grafo(
[arista(d,a),
arista(d,b),
arista(d,c),
arista(d,e),
arista(a,x),
arista(b,y),
arista(c,z),
arista(a,b),
arista(y,c)]).

yes
?-

@end{verbatim}

@item Predicado 2.2: aranya/0:
@begin{verbatim}
?- aranya.

yes
?-

@end{verbatim}
@end{enumerate}
").

%Predicados ----------------------------------------------

%Predicado 1----------------------------------------------

%Predicado 1.1: pots/3

pots(1,_,[1]):-!.
pots(0,_,[1]):-!.

pots(M,N,Ps):-
    integer(M),
    integer(N),
    pots_aux(M,N,1,L),
    reverse(L,Ps).

:-pred pots(M,N,Ps) #"@var{Ps} lista de potencias de @var{M} menores o iguales que @var{N}.".
:-doc(pots/3,"Devuelve la lista de potencias de M menores o iguales que N: @includedef{pots/3}Llama al predicado pots_aux/3 para ir realizando las diferentes potencias hasta que el valor donde se guardan las potencias es mayor que N, en cuyo caso para y vuelve a pots donde se invierte la lista.").

pots_aux(_,N,I,[]):-
    I > N, !.

pots_aux(M,N,I,[I|L]):-
    I =< N,
    X is M * I,
    pots_aux(M,N,X,L).

:-doc(pots_aux/4,"@includedef{pots_aux/4}.").

%Predicado 1.2: mpart/3

mpart(M,N,P):-
    integer(M),
    integer(N),
    pots(M,N,L),
    mpart_aux(N,L,0,P).

:-pred mpart(M,N,P) #"@var{P} lista de todas las particiones @var{M}-arias de @var{N}.".
:-doc(mpart/3,"Devuelve la lista de todas las particiones M-arias de N: @includedef{mpart/3}Primero llama a pots/3 y con el resultado llama a mpart_aux/3, donde se van realizando sumas con el primer elemento de la lista hasta que el valor es igual a N, en caso de que el valor de la suma sea mayor que N se vuelve a llamar al predicado mpart_aux/3 eliminando el primer elemento de la lista.").

mpart_aux(N,[X|_],A,[X]):-
    A1 is A + X,
    A1 == N.

mpart_aux(N,[X|L],A,[X|L1]):-
    A1 is A + X,
    A1 < N,
    mpart_aux(N,[X|L],A1,L1).

mpart_aux(N,[_|L],A,L1):-
    mpart_aux(N,L,A,L1).

:-doc(mpart_aux/4,"@includedef{mpart_aux/4}.").

%Predicado 1.3: maria/3

maria(M,N,P):-
    integer(M),
    integer(N),
    setof(L,mpart(M,N,L),L1),
    length(L1,P).

:-pred maria(M,N,P) #"@var{P} numero de todas las particiones @var{M}-arias de @var{N}.".
:-doc(maria/3,"Devuelve el numero de todas las particiones M-arias de N: @includedef{maria/3}Se llama a setof/3 para obtener la lista de todas las soluciones de mpart y por ultimo se llama a length/2 para calcular la longitud de dicha lista.").

%Predicado 2---------------------------------------------

%Predicado 2.1: guardar_grafo/2

:-dynamic arista/2.

:-doc(arista/2,"@includedef{arista/2}.").

guardar_grafo(L):-
    retractall(arista(_,_)),
    guardar_grafo_aux(L).

:-pred guardar_grafo(G) #"@var{G} grafo representado por aristas a dejar asertados en la base de datos como hechos del predicado arista/2.".
:-doc(guardar_grafo/1,"Grafo representado por aristas a dejar asertados en la base de datos como hechos del predicado arista/2: @includedef{guardar_grafo/1}Primero se vacia la base de datos llamando a retractall/2 y despues se llama a guardar_grafo_aux/1 para recorrer la lista de aristas y hacer los assert/1 de dichas aristas.").

guardar_grafo_aux([]).
guardar_grafo_aux([X|L]):-
    assert(X),
    guardar_grafo_aux(L).

:-doc(guardar_grafo_aux/1,"@includedef{guardar_grafo_aux/1}.").

%Predicado 2.2: aranya/0
obtener_nodos([],[]).
obtener_nodos([arista(A,B)|L],[A,B|L1]):-
    obtener_nodos(L,L1).

:-doc(obtener_nodos/2,"@includedef{obtener_nodos/2}.").

aranya:-
    setof(arista(A,B),arista(A,B),L),
    obtener_nodos(L,L1),
    setof(X,member(X,L1),L2),
    aranya_aux(L2,L2,L,[]).

:-pred aranya #"".
:-doc(aranya/0,"Comprueba si el grafo de la base de datos contiene una aranya de expansion @includedef{aranya/0}Primero se saca la lista de aristas y de nodos del grafo y se llama a la funcion aranya_aux/4 con la lista de nodos, la lista de aristas por visitar y la lista de aristas visitadas. Una vez en el predicado auxiliar se selecciona una arista de la lista de aristas por visitar y se comprueba si los nodos de dicha arista pertenecen a la lista de nodos, en caso de que pertenezcan a la lista se incluye la arista a la lista de aristas visitadas y se borran ambos nodos de la lista de nodos. Una vez la lista de nodos es vacia, se llama a vertices_aux/3 con la lista de aristas visitadas donde se cuenta el grado de cada vertice y se comprueba que hay como maximo un vertice con grado superior o igual a tres.").


aranya_aux(N,N1,L,L1):-
    select(arista(X,Y),L,LR),
    (member(X,N1);member(Y,N1)),
    append(L1,[X,Y],L2),
    delete(N1,X,N2),
    delete(N2,Y,NR),!,
    aranya_aux(N,NR,LR,L2).

aranya_aux(N,[],_,L):-
    vertices_aux(N,L,0).

:-doc(aranya_aux/4,"@includedef{aranya_aux/4}.").

vertices_aux([],_,_).
vertices_aux([X|Y],L,B):-
    bagof(A,(member(A,L),A==X),L1),
    length(L1,P),
    (P>=3 -> B==0, B1 is 1;B1 is 0),
    vertices_aux(Y,L,B1).

:-doc(vertices_aux/3,"@includedef{vertices_aux/3}.").
    
    
    