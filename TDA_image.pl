% Dominio: 
%
% Meta Principal:
% Metas Secundarias:


% Contar elementos
% Dominio: lista (de cualquier tipo) X N (int)
%
% Meta Principal: contar
% Metas Secundarias:
% Por medio de una recursión, sumaremos 1 a N por cada elemento que exista
% Su condición base será si la lista es nula, por lo que no se entrega nada


%contar([], 0).
%contar(ListaIn, N).
%contar([1,2,3], N).
contar([],0).
contar([_|Resto], N) :- 
    contar(Resto, Acc),
    N is Acc + 1.

% contarSoloNumeros([1,2,3, "a"], N).
% Dominio: lista (de cualquier tipo) X N (int)
%
% Meta Principal: contarSoloNumeros
% Metas Secundarias: 
% Utilizaremos una recursión similar a la de contar, pero
% filtraremos sólo elementos que correspondan a números
% Si es de tipo number, se suma
% sino, se pasa al siguiente elemento
% Mismo caso base que contar
contarSoloNumeros([],0).
contarSoloNumeros([Elemento|Resto], N) :- 
    contarSoloNumeros(Resto, Acc),
    (  number(Elemento)  %if
    -> N is Acc + 1      %then, tambien se puede usar N = Acc + 1
    ;  N is Acc          %else, tambien se puede usar N = Acc + 1
    ).

% Insertar elemento 
% Dominio: Elemento (cualquier tipo) X Lista (cualquier tipo)
%
% Meta Principal: insertarAlPrincipio
% Metas Secundarias: 
% Se considera una nueva lista, tomando como head al Elemento  y como tail a la Lista
insertarAlPrincipio( Elemento, [], [Elemento] ).
insertarAlPrincipio( Elemento, Lista, [Elemento|Lista] ).

% Map 
% Dominio: lista (cualquier tipo) X F (regla) X lista de salida (cualquier tipo)
%
% Meta Principal: Map
% Metas Secundarias: call
% Utilizando una recursión, le daremos uso a call también
% para aplicar una meta al elemento de la lista y
% añadirlo a la lista que mostraremos
% map([1,2,3], plus(1), X).



map([], F, []).
map([H|T], F, [HO|TO]) :- 
    call(F, H, HO),
    map(T,F,TO).

% Hasta aquí, han sido sólo metas que podríamos utilizar
% como secundarias para las solicitadas por enunciado

% pixbit
% Dominio: X (int) X Y (int) X Bit (1|0) X Depth (int) X lista
%
% Meta Principal: pixbit
% Metas Secundarias:
% Tomamos los elementos y los ordenamos en una lista que asumirá el rol de pixel bit

pixbit(X, Y, Bit, Depth, [X, Y, Bit, Depth]).

% pixrgb
% Dominio: X (int) X Y (int) X R (int) X G (int) X B (int) X Depth (int) X lista
%
% Meta Principal: pixbit
% Metas Secundarias:
% Tomamos los elementos y los ordenamos en una lista que asumirá el rol de pixel rgb

pixrgb(X, Y, R, G, B, Depth, [X, Y, R, G, B, Depth]).

% pixbit
% Dominio: X (int) X Y (int) X Hex (string) X Depth (int) X lista
%
% Meta Principal: pixhex
% Metas Secundarias:
% Tomamos los elementos y los ordenamos en una lista que asumirá el rol de pixel hex
pixhex(X, Y, Hex, Depth, [X, Y, Hex, Depth]).


% TDA - image
% Dominio: Largo (int) X Ancho (int) X Pixeles (pixrgb|pixbit|pixhex) X lista (cualquier tipo)
%
% Meta Principal: image
% Metas Secundarias:
% Tomamos los elementos y los ordenamos en una lista que asumirá el rol de image

image(Largo, Ancho, Pixeles, [Largo, Ancho, Pixeles]).

%pixbitd( 0, 0, 1, 10, PA),
%pixbitd( 0, 1, 0, 20, PB), 
%pixbitd( 1, 0, 0, 30, PC), 
%pixbitd( 1, 1, 1, 4, PD), 
%image( 2, 2, [PA, PB, PC, PD], I),
%imageTobitmap?(I).
%
% I = [2, 2, [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0, 30], [1, 1, 1, 4]]]
%image(Largo, Ancho, Pixeles, [2, 2, [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0, 30], [1, 1, 1, 4]]]).
%Pixeles = [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0, 30], [1, 1, 1, 4]]]
% [Pixel|Rest] = [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0, 30], [1, 1, 1, 4]].
%nth0(1, [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0, 30], [1, 1, 1, 4]], Pixel).
%nth0(1, [0, 0, 1, 10], PixelValue),
%is_boolean(PixelValue).

%( condition -> then_clause ; else_clause )


% pixelsAreHexmap?
% Dominio: Hex (lista de pixhex)
%
% Meta Principal: pixelsAreHexmap?
% Metas Secundarias: pixhex
% Caso base: la lista está vacía, se corta la recursión
% Cualquier otro, se recorre elemento a elemento, comprobando que sea
% de tipo hex, comprobando que su valor Hex sea un string (puesto que es el único pixel que contiene string
% ESTA REGLA SERVIRÁ COMO META SECUNDARIA PARA COMPROBAR SI UNA IMAGEN ES DE TIPO HEX
pixelsAreHexmap?([]).
pixelsAreHexmap?([Hexmap | Rest]) :-
    pixhex(_, _, Hex, _, Hexmap),
    string(Hex),
    pixelsAreHexmap(Rest).

% pixelsAreBitmap?
% Dominio: Pixel (lista de pixbit)
%
% Meta Principal: pixelsAreBitmap?
% Metas Secundarias: pixbit
% Comprobamos por medio de una recursión que todos los elementos de la lista sean
% de tipo pixbit por medio del valor de su Bit, que debe ser 1 o 0
% ESTA REGLA SERVIRÁ COMO META SECUNDARIA PARA COMPROBAR SI UNA IMAGEN ES DE TIPO HEX

pixelsAreBitmap?([]).
pixelsAreBitmap?([Pixbit | Rest]) :-
    pixbit(_, _, Bit, _, Pixbit),
    (Bit == 0 ; Bit == 1),
    pixelsAreBitmap?(Rest).

imageTobitmap?(Image) :-
    image(_, _, Pixels, Image),
    pixelsAreBitmap?(Pixels).


%solo pixbit
% ancho es Y
movePixelH(Ancho, Pixel, PixelOut) :-
    pixbit(X, Y, Bit, Depth, Pixel),
    (  Y < Ancho
    -> NewY is Y + 1     
    ;  NewY is 0
    ),
    pixbit(X, NewY, Bit, Depth, PixelOut).

movePixelsHorizontally(Ancho, [Pixel|Resto], PixelsAcc, PixelsOut) :-
    movePixelH(Ancho, Pixel, PixelOut),
    insertarAlPrincipio(PixelOut, PixelsAcc, PixelsOut),
    
    

moveH(ImageIn, ImageOut) :-
    image(Largo, Ancho, PixelsIn, ImageIn),
    movePixelsHorizontally(Ancho, PixelsIn, PixelsOut),
	image(Largo, Ancho, PixelsOut, ImageOut).
