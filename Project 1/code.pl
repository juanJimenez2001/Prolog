
:-module(_,_,[assertions]).

 
author_data('Jimenez','Perez','Juan','b190204').         %Autor: Juan Jiménez Pérez 190204

:-doc(title,"Practica 1 Programacion logica y declarativa").
:-doc(author,"Juan Jimenez Perez 190204").
:-doc(hide,[author_data/4,bind/1,hexd/1,byte/1,hex_byte/1,binary_byte/1]).
:-doc(module,"

@section{Pruebas de ejecucion }
@begin{enumerate}

@item Predicado 1 byte_list/1:
@begin{verbatim}
?- byte_list (L).

L = [[bind(0), bind(0), bind(0), bind(0), 
bind(0), bind(0), bind(0), bind(0)]] ?
yes
?-

@end{verbatim}

@item Predicado 2 byte_conversion/2:
@begin{verbatim}
?- byte_conversion ([hexd(a), hexd (1)], B).

B = [bind (1), bind (0), bind (1), bind (0), 
bind (0), bind (0), bind (0) , bind (1)] ?;
no
?-

@end{verbatim}

@item Predicado 3 byte_list_conversion/2:
@begin{verbatim}
?- byte_list_conversion ([[hexd (3), hexd (5)], 
[hexd (4), hexd(e)]], BL).

BL = [[bind (0), bind (0), bind (1), bind (1), 
bind (0), bind (1), bind (0), bind (1)], 
[bind (0), bind (1), bind (0), bind (0), 
bind (1), bind (1), bind (1), bind (0)]] ?;
no
?-

@end{verbatim}

@item Predicado 4 get_nth_bit_from_byte/3:
@begin{verbatim}
?- get_nth_bit_from_byte (s(s(s(s(s(0))))), 
[bind (1), bind (0), bind (1), bind (0), 
bind (1), bind (1), bind (0), bind (0)], B).

B = bind (1) ?;
no
?-

@end{verbatim}

@item Predicado 5 byte_list_clsh/2:
@begin{verbatim}
?- byte_list_clsh ([[hexd (5), hexd(a)], 
[hexd (2), hexd (3)], [hexd (5), hexd (5)], 
[hexd (3), hexd (7)]], L).

L = [[hexd(b), hexd (4)], [hexd (4), hexd (6)], 
[hexd(a), hexd(a)], [hexd (6), hexd(e)]] ?;
no
?-

@end{verbatim}

@item Predicado 6 byte_list_crsh/2:
@begin{verbatim}
?- byte_list_crsh ([[hexd(b), hexd (4)], 
[hexd (4), hexd (6)], [hexd(a), hexd(a)], 
[hexd (6), hexd(e)]], L).

L = [[hexd (5), hexd(a)], [hexd (2), hexd (3)], 
[hexd (5), hexd (5)], [hexd (3), hexd (7)]] ?;
no
?-

@end{verbatim}

@item Predicado 7 byte_xor/3:
@begin{verbatim}
?- byte_xor ([hexd (5) ,hexd(a)],
[hexd (2) ,hexd (3)], B3).

B3 = [hexd (7), hexd (9)] ?;
no
?-

@end{verbatim}
@end{enumerate}
").

%Tipos----------------------------------------------------

%Define a bynary digit type
bind(0).                                              
bind(1).                                                

%Define a bynary byte as a list of 8 binary digits
binary_byte([bind(B7),bind(B6),bind(B5),bind(B4),bind(B3),bind(B2),bind(B1),bind(B0)]):- 
    bind(B7),                                           
    bind(B6),                                            
    bind(B5),                                            
    bind(B4),                                            
    bind(B3),                                            
    bind(B2),                                           
    bind(B1),                                            
    bind(B0).                                            

%Define a hex digit (nibble) type
hexd(0).                                                 
hexd(1).                                                 
hexd(2).                                                 
hexd(3).                                                 
hexd(4).                                                  
hexd(5).                                                 
hexd(6).                                                 
hexd(7).                                                 
hexd(8).                                                 
hexd(9).                                                 
hexd(a).                                                 
hexd(b).                                                 
hexd(c).                                                 
hexd(d).                                                 
hexd(e).                                                 
hexd(f).                                                   

%Define a byte type either as a bynary byte or as a hex byte
byte(BB):-                                               
    binary_byte(BB).                                     
byte(HB):-                                               
    hex_byte(HB).                                        

%Define a hex byte as a list of 2 hex nibbles
hex_byte([hexd(H1), hexd(H0)]):-                         
    hexd(H1),                                            
    hexd(H0).                                             

%Predicados auxiliares------------------------------------

%Hex_a_byte: hex_a_byte/2
%Convierte de hexadecimal a binario o al reves
hex_a_byte(hexd(0), [bind(0),bind(0),bind(0),bind(0)]).  %Hecho para convertir el hexadecimal 0 a binario
hex_a_byte(hexd(1), [bind(0),bind(0),bind(0),bind(1)]).  %Hecho para convertir el hexadecimal 1 a binario
hex_a_byte(hexd(2), [bind(0),bind(0),bind(1),bind(0)]).  %Hecho para convertir el hexadecimal 2 a binario
hex_a_byte(hexd(3), [bind(0),bind(0),bind(1),bind(1)]).  %Hecho para convertir el hexadecimal 3 a binario
hex_a_byte(hexd(4), [bind(0),bind(1),bind(0),bind(0)]).  %Hecho para convertir el hexadecimal 4 a binario
hex_a_byte(hexd(5), [bind(0),bind(1),bind(0),bind(1)]).  %Hecho para convertir el hexadecimal 5 a binario
hex_a_byte(hexd(6), [bind(0),bind(1),bind(1),bind(0)]).  %Hecho para convertir el hexadecimal 6 a binario
hex_a_byte(hexd(7), [bind(0),bind(1),bind(1),bind(1)]).  %Hecho para convertir el hexadecimal 7 a binario
hex_a_byte(hexd(8), [bind(1),bind(0),bind(0),bind(0)]).  %Hecho para convertir el hexadecimal 8 a binario
hex_a_byte(hexd(9), [bind(1),bind(0),bind(0),bind(1)]).  %Hecho para convertir el hexadecimal 9 a binario
hex_a_byte(hexd(a), [bind(1),bind(0),bind(1),bind(0)]).  %Hecho para convertir el hexadecimal a a binario
hex_a_byte(hexd(b), [bind(1),bind(0),bind(1),bind(1)]).  %Hecho para convertir el hexadecimal b a binario
hex_a_byte(hexd(c), [bind(1),bind(1),bind(0),bind(0)]).  %Hecho para convertir el hexadecimal c a binario
hex_a_byte(hexd(d), [bind(1),bind(1),bind(0),bind(1)]).  %Hecho para convertir el hexadecimal d a binario
hex_a_byte(hexd(e), [bind(1),bind(1),bind(1),bind(0)]).  %Hecho para convertir el hexadecimal e a binario
hex_a_byte(hexd(f), [bind(1),bind(1),bind(1),bind(1)]).  %Hecho para convertir el hexadecimal f a binario

:-prop hex_a_byte(HB,BB) #"@var{BB} conversion del numero hexadecimal @var{HB} en binario.".
:-doc(hex_a_byte/2,"Convierte de hexadecimal a binario al reves: @includedef{hex_a_byte/2}").


%My_nth_bit_from_byte: my_nth_bit_from_byte/3
%Recorre la lista hasta encontrar el byte pasado como primer parametro
my_nth_bit_from_byte(0,[X|_],X).                         %Al llegar al 0 devuelve el elemento en dicha posicion  
my_nth_bit_from_byte(s(X),[_|C],B):-                     %Recorre la lista reduciendo el indice hasta llegar al 0
    my_nth_bit_from_byte(X,C,B).

:-pred my_nth_bit_from_byte(s(X),L,R) #"@var{R} bit situado en el indice @var{s(X)} de la lista @var{L}.".
:-doc(my_nth_bit_from_byte/3,"Recorre la lista hasta encontrar el byte pasado como primer parametro: @includedef{my_nth_bit_from_byte/3} Recorre la lista reduciendo el indice hasta llegar al 0. Al llegar al 0 devuelve el elemento en dicha posicion").

%Invertir: invertir/2
%Invierte la lista pasada como argumento
invertir([],[]).                                         %Cuando no hay más elementos en la lista esta se devuelve vacia
invertir([X|Y],L):-                                      %Recorre la lista con llamadas recursivas
    invertir(Y,L1),                                      
    concatenar(L1,[X],L).                                %Una vez llega al final empieza a concatenar los elementos de la lista

:-pred invertir(L,R) #"@var{R} resultado de invertir la lista @var{L}.".
:-doc(invertir/2,"Invierte la lista pasada como argumento: @includedef{invertir/2} Recorre la lista con llamadas recursivas hasta llegar a la lista vacia, una vez ahi, concatena los distintos elementos de la lista.").

%Concatenar: concatenar/3
%Concatena las listas pasadas como argumento
concatenar([],L,L).                                      %Cuando no hay mas elementos en la lista inicial devuelve la lista 
concatenar([X|Y],Z,[X|U]):-                              %Añade los elementos de la primera lista en la lista resultado 
    concatenar(Y,Z,U).                                   %y llama recursivamente al predicado

:-pred concatenar(L1,L2,R) #"@var{R} resultado de concatenar las listas @var{L1} y @var{L2}.".
:-doc(concatenar/3,"Concatena las listas pasadas como argumento: @includedef{concatenar/3} Añade los elementos de la primera lista en la lista resultado y llama recursivamente al predicado, cuando no hay mas elementos en la lista inicial devuelve la lista").

%Byte_bit: byte_bit/2
%Convierte de byte a bit o al reves
byte_bit([],[]).                                         %Cuando no hay mas elementos en la lista inicial devuelve la lista vacia
byte_bit([[B7,B6,B5,B4,B3,B2,B1,B0]|Bytes],
         [B7,B6,B5,B4,B3,B2,B1,B0|Bits]):-               %Transforma una lista de listas de ocho elementos en una unica lista 
    byte_bit(Bytes,Bits).

:-pred byte_bit(L,R) #"@var{R} resultado de transformar la lista de bytes @var{L} en una lista de bits.".
:-doc(byte_bit/2,"Convierte de byte a bit o al reves: @includedef{byte_bit/2} Transforma una lista de listas de ocho elementos en una unica lista.").
 
%Binary_list: binary_list/1
%Comprueba si la lista esta compuesta solo de elementos binarios
binary_list([]).                                         %Cuando no hay más elementos para 
binary_list([X|Y]):-                                     %Recorre todos los elementos de la lista para comprobar si son binarios
    binary_byte(X),                                      %Para ello va comprobando el primer elemento de la lista
    binary_list(Y).                                      %y despues llama recursivamente al predicado con el resto de la lista

:-pred binary_list(L) #"@var{L} lista pasada para comprobar si es de digitos binarios.".
:-doc(binary_list/1,"Comprueba si la lista esta compuesta solo de elementos binarios: @includedef{binary_list/1} Para comprobarlo recorre la lista comprobando si el primer elemento de la lista es binario, si lo es llama al predicado recursivamente con el resto de la lista hasta que no quedan mas elementos.").

%Hex_list: hex_list/1
%Comprueba si la lista esta compuesta solo de elementos hexadecimales
hex_list([]).                                            %Cuando no hay mas elementos para 
hex_list([X|Y]):-                                        %Recorre los elementos de la lista para comprobar si son hexadecimales
    hex_byte(X),                                         %Para ello va comprobando el primer elemento de la lista
    hex_list(Y).                                         %y despues llama recursivamente al predicado con el resto de la lista

:-pred hex_list(L) #"@var{L} lista pasada para comprobar si es de digitos hexadecimales.".
:-doc(hex_list/1,"Comprueba si la lista esta compuesta solo de elementos hexadecimales: @includedef{hex_list/1}Para comprobarlo recorre la lista comprobando si el primer elemento de la lista es hexadecimal, si lo es llama al predicado recursivamente con el resto de la lista hasta que no quedan mas elementos.").
   
%Xor: xor/3
%Operaciones xor  
xor(bind(0),bind(0),bind(0)).                            %Hecho que calcula el xor de 0 y 0
xor(bind(0),bind(1),bind(1)).                            %Hecho que calcula el xor de 0 y 1
xor(bind(1),bind(0),bind(1)).                            %Hecho que calcula el xor de 1 y 0
xor(bind(1),bind(1),bind(0)).                            %Hecho que calcula el xor de 1 y 1

:-prop xor(B1,B2,B3) #"@var{B3} resultado de hacer la operacion xor de @var{B1} y de @var{B2}.".
:-doc(xor/3,"Operaciones xor: @includedef{xor/3}").
  
%Byte_xor_aux: byte_xor_aux/3
%Dadas dos listas realiza la operacion xor elemento a elemento
byte_xor_aux([],[],_).                                   %Cuando no hay mas elmentos en la lista para
byte_xor_aux([X|Y],[Z|T],[A|B]):-                        %Selecciona el primer elemento de cada lista 
    xor(X,Z,A),                                          %Realiza la operacion del primer elemento de cada lista
    byte_xor_aux(Y,T,B).                                 %y llama recursivamente al predicado con el resto de la lista

:-pred byte_xor_aux(B1,B2,B3) #"@var{B3} lista resultado de operar las listas @var{B1} y @var{B2}.".
:-doc(byte_xor_aux/3,"Dadas dos listas realiza la operacion xor elemento a elemento: @includedef{byte_xor_aux/3} Selecciona el primer elemento de cada lista y realiza la operacion xor. Repite la operacion de forma recursiva con el resto de la lista hasta que ambas son vacias.").

%Predicados ----------------------------------------------

%Predicado 1: byte_list/1
byte_list([]).                                           %Cuando no hay mas elmentos en la lista para
byte_list([X|Y]):-                                       %Recorre los elementos de la lista 
    byte(X),                                             %Comprueba si el primer elemento es binario o hexadecimal
    byte_list(Y).                                        %y llama recursivamente al predicado con el resto de la lista

:-pred byte_list(L) #"@var{L} lista pasada para comprobar si es de digitos hexadecimales o binarios.".
:-doc(byte_list/1,"Predicado 1: @includedef{byte_list/1} Recorre los elementos de la lista comprobando si el primer elemento es binario o hexadecimal y llama recursivamente al predicado con el resto de la lista hasta que esta sea vacia.").

%Predicado 2: byte_conversion/2
byte_conversion([H1,H0],[B7,B6,B5,B4,B3,B2,B1,B0]):-     %Convierte de hexadecimal a binario
    hex_byte([H1,H0]),                                   %Comprueba que el valor pasado es hexadecimal
    hex_a_byte(H1,[B7,B6,B5,B4]),                        %Convierte el primer hexadecimal a binario 
    hex_a_byte(H0,[B3,B2,B1,B0]).                        %Convierte el segundo hexadecimal a binario

:-pred byte_conversion(HB,B) #"@var{B} resultado de convertir a binario @var{HB}.".
:-doc(byte_conversion/2,"Predicado 2: @includedef{byte_conversion/2} Comprueba que el argumento pasado es hexadecimal y despues los convierte a binario.").

%Predicado 3: byte_list_conversion/2
byte_list_conversion([],[]).                             %Cuando no hay mas elementos en la lista para
byte_list_conversion([X|Y],[Z|T]):-                      %Recorre los elementos de la lista 
    byte_conversion(X,Z),                                %Convierte el primer elemento a binario llamando al predicado 2
    byte_list_conversion(Y,T).                           %y llama recursivamente a la funcion con el reesto de la funcion

:-pred byte_list_conversion(HL,BL) #"@var{BL} lista resultado de convertir a binario la lista @var{HL}.".
:-doc(byte_list_conversion/2,"Predicado 3: @includedef{byte_list_conversion/2} Recorre los elementos de la lista convirtiendo el primer elemento de hexadecimal a binario y llama recursivamente al predicado con el resto de la lista hasta que esta sea vacia.").
  
%Predicado 4: get_nth_bit_from_byte/3
get_nth_bit_from_byte(A,[H1,H0],B):-                     
    hex_byte([H1,H0]),                                   %Comprueba que el elemento pasados son hexadecimales
    byte_conversion([H1,H0],C),                          %Convierte de hexadecimal a binario llamando al predicado 2
    invertir(C,R),                                       %Invierte la lista 
    my_nth_bit_from_byte(A,R,B).                         %y llama a la funcion auxiliar para buscar el elemento en la posicion A
    
get_nth_bit_from_byte(A,BB,B):-                          
    binary_byte(BB),                                     %Comprueba que el elemento pasados son binarios
    invertir(BB,R),                                      %Invierte la lista 
    my_nth_bit_from_byte(A,R,B).                         %y llama a la funcion auxiliar para buscar el elemento en la posicion A

:-pred get_nth_bit_from_byte(N,B,BN) #"@var{BN} bit situado en el indice @var{N} de la lista @var{B}.".
:-doc(get_nth_bit_from_byte/3,"Predicado 4: @includedef{get_nth_bit_from_byte/3} Comprueba que los datos pasados son validos, y si son hexadecimales los convierte a binarios llamando al predicado 2. Invierte la lista y llama al predicado my_nth_bit_from_byte/3 para buscar el elemento en la lista.").
 
%Predicado 5: byte_list_clsh/2
byte_list_clsh(L,CLShL):-                                
    hex_list(L),                                         %Comprueba que la lista pasada como argumento es de hexadecimales
    byte_list_conversion(L,R),                           %Convierte de hexadecimal a binario llamando al predicado 3
    byte_bit(R,[X|Y]),                                   %Convierte la lista de bytes en una unica lista de bits
    concatenar(Y,[X],A),                                 %Mueve el primer elemento a la ultima posicion
    byte_bit(B,A),                                       %Convierte la lista de bits en una lista de bytes
    byte_list_conversion(CLShL,B).                       %Convierte de binario a hexadecimal llamando al predicado 3

byte_list_clsh(L,CLShL):-                                  
    binary_list(L),                                      %Comprueba que la lista pasada como argumento es de binarios
    byte_bit(L,[X|Y]),                                   %Convierte la lista de bytes en una unica lista de bits
    concatenar(Y,[X],R),                                 %Mueve el primer elemento a la ultima posicion
    byte_bit(CLShL,R).                                   %Convierte la lista de bits en una lista de bytes

:-pred byte_list_clsh(L,CLShL) #"@var{CLShL} lista resultado de rotar la lista @var{L} a la izquierda.".
:-doc(byte_list_clsh/2,"Predicado 5: @includedef{byte_list_clsh/2} Comprueba que los elementos pasados son validos, y si estan en hexadecimal los convierte a binario llamando al predicado 3. Transforma la lista de bytes a una lista de bits, llamando a byte_bit/2, en la que mueve el primer elemento a la ultima posicion. Convierte de nuevo la lista de bits a bytes y si es hexadecimal la transforma de nuevo a hexadecimal.").
      
%Predicado 6: byte_list_crsh/2
byte_list_crsh(L,CRShL):-                                 
    hex_list(L),                                         %Comprueba que la lista pasada como argumento es de hexadecimales
    byte_list_conversion(L,A),                           %Convierte de hexadecimal a binario llamando al predicado 3
    byte_bit(A,B),                                       %Convierte la lista de bytes en una unica lista de bits  
    invertir(B,[X|Y]),                                   %Invierte la lista
    concatenar(Y,[X],C),                                 %Mueve el primer elemento a la ultima posicion
    invertir(C,D),                                       %Invierte la lista
    byte_bit(R,D),                                       %Convierte la lista de bits en una lista de bytes
    byte_list_conversion(CRShL,R).                       %Convierte de binario a hexadecimal llamando al predicado 3

byte_list_crsh(L,CRShL):-                                
    binary_list(L),                                      %Comprueba que la lista pasada como argumento es de binarios
    byte_bit(L,A),                                       %Convierte la lista de bytes en una unica lista de bits
    invertir(A,[X|Y]),                                   %Invierte la lista
    concatenar(Y,[X],B),                                 %Mueve el primer elemento a la ultima posicion
    invertir(B,R),                                       %Invierte la lista
    byte_bit(CRShL,R).                                   %Convierte la lista de bits en una lista de bytes

:-pred byte_list_crsh(L,CRShL) #"@var{CRShL} lista resultado de rotar la lista @var{L} a la derecha.".
:-doc(byte_list_crsh/2,"Predicado 6: @includedef{byte_list_crsh/2} Comprueba que los elementos pasados son validos, y si estan en hexadecimal los convierte a binario llamando al predicado 3. Transforma la lista de bytes a una lista de bits llamando a byte_bit/2 y la invierte llamando a invertir/2. Mueve el primer elemento a la ultima posicion y la invierte de nuevo. Convierte de nuevo la lista de bits a bytes y si es hexadecimal la transforma de nuevo a hexadecimal.").

%Predicado 7: byte_xor/3
byte_xor(H1,H2,H3):-                                       
    hex_byte(H1),                                        %Comprueba que el primer argumento es hexadecimal 
    hex_byte(H2),                                        %Comprueba que el segundo argumento es hexadecimal
    byte_conversion(H1,A),                               %Convierte el primer argumento a binario usando el predicado 2
    byte_conversion(H2,B),                               %Convierte el primer argumento a binario usando el predicado 2
    byte_xor_aux(A,B,C),                                 %Llama al predicado auxiliar para realizar la operacion xor
    byte_conversion(H3,C).                               %Convierte el resultado a hexadecimal usando el predicado 2

byte_xor(B1,B2,B3):-                                     
    binary_byte(B1),                                     %Comprueba que el primer argumento es binario
    binary_byte(B2),                                     %Comprueba que el segundo argumento es binario
    byte_xor_aux(B1,B2,B3),                              %Llama al predicado auxiliar para realizar la operacion xor
    binary_byte(B3).                                     %Comprueba que el resultado obtenido es binario

:-pred byte_xor(B1,B2,B3) #"@var{B3} resultado de hacer la operacion xor de @var{B2} y @var{B1}.".
:-doc(byte_xor/3,"Predicado 7: @includedef{byte_xor/3} Comprueba que ambos datos pasados son validos, y en caso de ser hexadecimales los transforma a binario llamando al predicado 2. Llama al predicado byte_xor_aux/3 para calcular la operacion xor y por ultimo en caso de ser binario comprueba que el resultado es correcto y en caso de ser hexadecimal lo vuelve a convertir a hexadecimal.").

    
    
    