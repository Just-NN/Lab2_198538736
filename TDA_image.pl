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



% Insertar elemento 
% Dominio: Elemento (cualquier tipo) X Lista (cualquier tipo)
%
% Meta Principal: insertarAlPrincipio
% Metas Secundarias: 
% Se considera una nueva lista, tomando como head al Elemento  y como tail a la Lista
insertarAlPrincipio( Elemento, [], [Elemento] ).
insertarAlPrincipio( Elemento, Lista, [Elemento|Lista] ).

% car
% Dominio: lista (cualquier tipo) X X (atomo)
%
% Meta Principal: car
% Metas Secundarias: no aplica
% Se toma una lista, cuyo primer elemento será X

car([X|_], X).

% cadr
% Dominio: lista (cualquier tipo) X Y (atomo)
%
% Meta Principal: cadr
% Metas Secundarias: no aplica
% Se toma una lista, cuyo segundo elemento será Y

cadr([_,Y|_], Y).

% caddr
% Dominio: lista (cualquier tipo) X R (atomo)
%
% Meta Principal: caddr
% Metas Secundarias: no aplica
% Se toma una lista, cuyo tercer elemento será R

caddr([_,_,R|_], R).

% cadddr
% Dominio: lista (cualquier tipo) X G (atomo)
%
% Meta Principal: cadddr
% Metas Secundarias: no aplica
% Se toma una lista, cuyo cuarto elemento será G

cadddr([_,_,_,G|_], G).

% caddddr
% Dominio: lista (cualquier tipo) X B (atomo)
%
% Meta Principal: caddddr
% Metas Secundarias: no aplica
% Se toma una lista, cuyo quinto elemento será D

caddddr([_,_,_,_, B|_],B).
cadddddr([_,_,_,_,_, D|_], D).



% ---------------------------------------------------------
% Hasta aquí, han sido sólo metas que podríamos utilizar
% como secundarias para las solicitadas por enunciado
% ---------------------------------------------------------
% 
% pixbit
% Dominio: X (int) X Y (int) X Bit (1|0) X Depth (int) X lista
%
% Meta Principal: pixbit
% Metas Secundarias: no aplica
% Tomamos los elementos y los ordenamos en una lista que asumirá el rol de pixel bit

pixbit(X, Y, Bit, Depth, [X, Y, Bit, Depth]).


% pixrgb
% Dominio: X (int) X Y (int) X R (int) X G (int) X B (int) X Depth (int) X lista
%
% Meta Principal: pixbit
% Metas Secundarias:no aplica
% Tomamos los elementos y los ordenamos en una lista que asumirá el rol de pixel rgb

pixrgb(X, Y, R, G, B, Depth, [X, Y, R, G, B, Depth]).

% pixbit
% Dominio: X (int) X Y (int) X Hex (string) X Depth (int) X lista
%
% Meta Principal: pixhex
% Metas Secundarias: no aplica
% Tomamos los elementos y los ordenamos en una lista que asumirá el rol de pixel hex
pixhex(X, Y, Hex, Depth, [X, Y, Hex, Depth]).


% TDA - image
% Dominio: Largo (int) X Ancho (int) X Pixeles (pixrgb|pixbit|pixhex) X lista (cualquier tipo)
%
% Meta Principal: image
% Metas Secundarias: no aplica
% Tomamos los elementos y los ordenamos en una lista que asumirá el rol de image

image(Largo, Ancho, Pixeles, [Largo, Ancho, Pixeles]).

% ------------------------------- Pertenencia de los 3 TDA de pixel
% (pixhex, pixbit y pixrgb)
% pixelsAreHexmap
% Dominio: Hex (lista de pixhex)
%
% Meta Principal: pixelsAreHexmap
% Metas Secundarias: pixhex, string
% Caso base: la lista está vacía, se corta la recursión
% Cualquier otro, se recorre elemento a elemento, comprobando que sea
% de tipo hex, comprobando que su valor Hex sea un string (puesto que es el único pixel que contiene string
% ESTA REGLA SERVIRÁ COMO META SECUNDARIA PARA COMPROBAR SI UNA IMAGEN ES DE TIPO HEX
pixelsAreHexmap([]):-!.
pixelsAreHexmap([Hexmap | Rest]) :-
    pixhex(_, _, Hex, _, Hexmap),
    string(Hex),
    pixelsAreHexmap(Rest).

% pixelsAreBitmap
% Dominio: Pixel (lista de pixbit)
%
% Meta Principal: pixelsAreBitmap
% Metas Secundarias: pixbit
% Comprobamos por medio de una recursión que todos los elementos de la lista sean
% de tipo pixbit por medio del valor de su Bit, que debe ser 1 o 0
% ESTA REGLA SERVIRÁ COMO META SECUNDARIA PARA COMPROBAR SI UNA IMAGEN ES DE TIPO BIT


pixelsAreBitmap([]):-!.
pixelsAreBitmap([Pixbit|Rest]) :-
    pixbit(_, _, Bit, _, Pixbit),
    (Bit == 0; Bit == 1),
    pixelsAreBitmap(Rest).

% pixelsAreRGBmap
% Dominio: Pixel (lista de pixbit)
%
% Meta Principal: pixelsAreRGBmap
% Metas Secundarias: pixrgb
% Comprobamos por medio de una recursión que todos los elementos de la lista sean
% de tipo pirxrgb por medio de sus valores R, G, B, que deben de estar entre 0 y 255
% ESTA REGLA SERVIRÁ COMO META SECUNDARIA PARA COMPROBAR SI UNA IMAGEN ES DE TIPO RGB

pixelsAreRGBmap([]):-!.
pixelsAreRGBmap([Pixbit | Rest]) :-
    pixrgb(_, _, R, G, B, _, Pixbit),
    (R >= 0,
    R =< 255,
    G >= 0, 
    G =< 255, 
    B >= 0, 
    B =< 255
    ),  
    pixelsAreRGBmap(Rest). 



%------------------------------------------------------------------------------
% Aquí empieza la pertenencia del TDA image
% 
% 


% Dominio: Pixel(pixbit)
%
% Meta Principal: bit
% Metas Secundarias: no aplica


bit(Pixel):-
    caddr(Pixel, X),
    (   X == 1 ; X==0),
    contar(Pixel, N),
    (   N == 4).
    

% imageTobitmap?
% Dominio: Image (image)
%
% Meta Principal: imageTobitmap?
% Metas Secundarias: pixelsAreBitmap, bit, image
% Utiliza Pixels para comprobar que sus elementos sean de tipo pixbit

imageTobitmap(Image) :-
    image(_, _, Pixels, Image),
    maplist(bit, Pixels).

% imageTohexmap?
% Dominio: Image (image)
%
% Meta Principal: imageTohexmap?
% Metas Secundarias: pixelsAreHexmap, image
% Utiliza Pixels para comprobar que sus elementos sean de tipo hex

imageTohexmap(Image):-
	image(_, _, Pixels, Image),
	pixelsAreHexmap(Pixels).


% imageTorgbmap
% Dominio: Image (image)
%
% Meta Principal: imageTorgbmap?
% Metas Secundarias: pixelsAreRGBmap, image
% Utiliza Pixels para comprobar que sus elementos sean de tipo rgb

imageTorgbmap(Image):-
	image(_, _, Pixels, Image),
	pixelsAreRGBmap(Pixels).

% Dominio: Image (image)
%
% Meta Principal: imageIsCompressed
% Metas Secundarias: image, contar
% Decripción: revisa si la cantidad de elementos concuerda con
% las dimensiones dadas para la imagen

imageIsCompressed(Image):-
    image(Largo, Ancho, Pixeles, Image),
    contar(Pixeles, N),
    Largo*Ancho > N.

%---------------------------------------------
% Aquí empiezan reglas para modificar los pixeles

% Dominio: X (int) X PixIn (pixbit|pixhex|pixrgb) X PixOut(pixbit|pixhex|pixrgb)
%
% Meta Principal: changePixel
% Metas Secundarias: cadr, caddr, cadddr, caddddr, cadddddr,
% 					 contar, string, pixhex, pixbit, pixrgb
% Cambia la posición X por la posición Y para el pixel ingresado, 
%

changePixel(X, PixIn, PixOut):-
    cadr(PixIn, Y),
    caddr(PixIn, BH),
    cadddr(PixIn, Depth),
    contar(PixIn, N),
    (   N==4->  (   string(BH) ->  pixhex(X, Y, BH, Depth, PixOut)
    			;   pixbit(X, Y, BH, Depth, PixOut))
    ;   (   caddddr(PixIn, B),
            cadddddr(PixIn, D),
            pixrgb(X, Y, BH, Depth, B, D, PixOut))).
flipPixels(_,_,[],_).
flipPixels(W, W2, [PixIn|PixsIn], [PixOut|PixsOut]):-
    changePixel(W, PixIn, PixOut),
    (   W==0 ->  NewW is W2
    ;   NewW is W-1
    ),
    flipPixels(NewW, W2, PixsIn, PixsOut).

flipH(In, IOut):-
    image(Largo, Ancho, Pixlist, In),
    NewAncho is Ancho-1,
    flipPixels(NewAncho, NewAncho, Pixlist, PixsOut),
    image(Largo, Ancho, PixsOut, IOut).



%---------------------------------------------
% alto es X
% movePixBitH
% Dominio: Ancho (int), Pixel (pixbit| pixhex| pixrgb)
%
% Meta Principal: movePixBitH
% Metas Secundarias: pixbit
% Comprueba si el ancho está dentro del rango y, en ese caso
% se mueve la posición en 1 hacia la derecha. Sino, el valor será 0
% Básicamente rota todo hacia la derecha y, si es el último elemento, 
% lo envía al inicio





%
% Dominio: Ancho (int), Pixel (pixbit| pixhex| pixrgb)
%
% Meta Principal: movePixRgbH
% Metas Secundarias: pixrgb
% Comprueba si el ancho está dentro del rango y, en ese caso
% se mueve la posición en 1 hacia la derecha. Sino, el valor será 0
% Básicamente rota todo hacia la derecha y, si es el último elemento, 
% lo envía al inicio

changePixelV(Y, PixIn, PixOut):-
    car(PixIn, X),
    caddr(PixIn, BH),
    cadddr(PixIn, Depth),
    contar(PixIn, N),
    (   N==4->  (   string(BH) ->  pixhex(X, Y, BH, Depth, PixOut)
    			;   pixbit(X, Y, BH, Depth, PixOut))
    ;   (   caddddr(PixIn, B),
            cadddddr(PixIn, D),
            pixrgb(X, Y, BH, Depth, B, D, PixOut))).
flipPixelsV(_,_,[],_).
flipPixelsV(W, W2, [PixIn|PixsIn], [PixOut|PixsOut]):-
    changePixelV(W, PixIn, PixOut),
    (   W==0 ->  NewW is W2
    ;   NewW is W-1
    ),
    flipPixelsV(NewW, W2, PixsIn, PixsOut).

    
    

flipV(ImageIn, ImageOut) :-
    image(Largo, Ancho, PixelsIn, ImageIn),
    NewLargo is Largo-1,
    flipPixelsV(NewLargo, NewLargo, PixelsIn, PixelsOut),
	image(Largo, Ancho, PixelsOut, ImageOut).


%----------------------------- crop ---------



inRange(X1, Y1, X2, Y2, Pix):-
    	car(Pix, X),
    	cadr(Pix, Y),
        X >= X1,
    	X =< X2,
    	Y >= Y1,
    	Y =< Y2
    	.
    
    

cropPixels(_, _, _, _, [], Accum, Accum).
cropPixels(X1, Y1, X2, Y2, [PixIn|PixsIn], Accum, Output):-
    (   inRange(X1, Y1, X2, Y2, PixIn) ->	insertarAlPrincipio(PixIn, Accum, NewAccum)
    ;  NewAccum = Accum
    ),
    cropPixels(X1, Y1, X2, Y2, PixsIn, NewAccum, Output)
    .
    
crop(X1, Y1, X2, Y2, ImageIn, ImageOut):-
	image(Largo, Ancho, PixelsIn, ImageIn),
    cropPixels(X1, Y1, X2, Y2, PixelsIn, [], PixelsOut),
    image(Largo, Ancho, PixelsOut, ImageOut).

%---------------------------------------------------- rgb to hex
truncate(X,N,Result):- X >= 0, Result is floor(10^N*X)/10^N, !.


intToHex(X, Y):-
    (   X==0 ->  Y = "0"
    ;   X==1 ->  Y = "1"
    ;   X==2 ->  Y = "2"
    ;   X==3 ->  Y = "3"
    ;   X==4 ->  Y = "4"
    ;   X==5 ->  Y = "5"
    ;   X==6 ->  Y = "6"
    ;   X==7 ->  Y = "7"
    ;   X==8 ->  Y = "8"
    ;   X==9 ->  Y = "9"
    ;   X==10 ->  Y = "A"
    ;   X==11 ->  Y = "B"
    ;   X==12 ->  Y = "C"
    ;   X==13 ->  Y = "D"
    ;   X==14 ->  Y = "E"
    ;   X==15 ->  Y = "F"
    ;   X==16 ->  Y = "G"
    ;   X==0.0 ->  Y = "0"
    ;   X==1.0 ->  Y = "1"
    ;   X==2.0 ->  Y = "2"
    ;   X==3.0 ->  Y = "3"
    ;   X==4.0 ->  Y = "4"
    ;   X==5.0 ->  Y = "5"
    ;   X==6.0 ->  Y = "6"
    ;   X==7.0 ->  Y = "7"
    ;   X==8.0 ->  Y = "8"
    ;   X==9.0 ->  Y = "9"
    ;   X==10.0 ->  Y = "A"
    ;   X==11.0 ->  Y = "B"
    ;   X==12.0 ->  Y = "C"
    ;   X==13.0 ->  Y = "D"
    ;   X==14.0 ->  Y = "E"
    ;   X==15.0 ->  Y = "F"
    ;   X==16.0 ->  Y = "G").

getInt(X, Y):-
    truncate(X, 0, Y).

getDec(X, Y):-
    (   X < 1 ->  truncate(X, 2, Y)
    ;   (	getInt(X, Int),
            Y is X - Int)).

rgbToHex(X, Y):-
    X1 is X/16,
    getInt(X1, Int),
    getDec(X1, Dec),
    intToHex(Int, NewInt),
    NewDec is Dec*16,
    intToHex(NewDec, NewInt2),
    atom_concat(NewInt, NewInt2, Y).

pixsRGBToHex([], _):-!.
pixsRGBToHex([PixIn|PixsIn], [PixOut|PixsOut]):-
    pixrgb(X, Y, R, G, B, D, PixIn),
    rgbToHex(R, ROut),
    rgbToHex(G, GOut),
    rgbToHex(B, BOut),
    atom_concat("#", ROut, NewR),
    atom_concat(NewR, GOut, NewG),
    atom_concat(NewG, BOut, RGB),
    pixhex(X, Y, RGB, D, PixOut),
    pixsRGBToHex(PixsIn, PixsOut).

	
imgRGBToHex(ImageIn, ImageOut):-
    image(Largo, Ancho, PixelsIn, ImageIn),
    pixsRGBToHex(PixelsIn, PixelsOut),
    image(Largo, Ancho, PixelsOut, ImageOut).


%--------------histograma-----------------%
%% usando include y exclude sale más rápido
pixCheck(A, B):-
    caddr(A, ABH),
    caddr(B, BBH),
    contar(A, N),
    (   N==4 ->  ABH == BBH
    ;   (   cadddr(A, AG),
            cadddr(B, BG),
            cadddr(A, AB),
            cadddr(B, BB),
            AG == BG,
            AB == BB)).
cons(A, B, [A,B]).

getRGB(Pix, RGB):-
    caddr(Pix, R),
    cadddr(Pix, G),
    caddddr(Pix, B),
    atom_concat(R, ", ", Rcoma),
    atom_concat(Rcoma, G, RG),
    atom_concat(RG, ", ", RGcoma),
    atom_concat(RGcoma, B, RGB).

histogram([], _).
histogram([PixIn|PixsIn], [PixOut|PixsOut]):-
    contar(PixIn, Len),
    (   Len == 4 ->  caddr(PixIn, BH)
    ;   getRGB(PixIn, BH)
    ),
    include(pixCheck(PixIn), PixsIn, P1),
    contar(P1, N),
    NewN is N+1,
	cons(BH, NewN, PixOut),
    exclude(pixCheck(PixIn), PixsIn, P2),
    histogram(P2, PixsOut).


%-------------rotate90°-------------------%
bitOrHexTranspose(PixIn, PixOut):-
    car(PixIn, X),
    cadr(PixIn, Y),
    caddr(PixIn, BH),
    cadddr(PixIn, Depth),
    (   string(BH) ->  pixhex(Y, X, BH, Depth, PixOut)
    ;   pixbit(Y, X, BH, Depth, PixOut)).
pixCons(PixIn, PixOut):-
    car(PixIn, X),
    caddr(PixIn, Y),
    contar(PixIn, N),
    (   N == 4 ->  bitOrHexTranspose(PixIn, PixOut)
    ;   (   caddr(PixIn, R),
            cadddr(PixIn, G),
            caddddr(PixIn, B),
            cadddddr(PixIn, D),
            pixrgb(Y, X, R, G, B, D, PixOut))).
transpose([], _).
transpose([PixIn|PixsIn], [PixOut|PixsOut]):-
    pixCons(PixIn, PixOut),
    transpose(PixsIn, PixsOut).
rotate90(Img1, Img2):-
    image(Ancho, Largo, Pixslist, Img1),
    transpose(Pixslist, PixsOut),
    NewAncho is Ancho-1,
    flipPixels(NewAncho, NewAncho, PixsOut, PixsOutput),
    image(Largo, Ancho, PixsOutput, Img2).
    


%pixelsAreBitmap([]).
%pixelsAreBitmap([Pixbit | Rest]) :-
%    pixbit(_, _, Bit, _, Pixbit),
%    (Bit == 0 ; Bit == 1),
%    pixelsAreBitmap(Rest).
 

/** <examples>
?- pixbit( 0, 0, 1, 10, PA),
pixbit( 0, 1, 0, 20, PB), 
pixbit( 1, 0, 0, 30, PC), 
pixbit( 1, 1, 1, 4, PD),
image( 2, 2, [PA, PB, PC, PD], I),
moveH(I, X),
moveImageHorizontally(I, B).
?- pixbit( 0, 0, 1, 10, PA),
   pixbit( 0, 1, 0, 20, PB), 
   pixbit( 1, 0, 0, 30, PC), 
   pixbit( 1, 1, 1, 4, PD),
   image( 2, 2, [PA, PB, PC, PD], I),
   moveH(I, X).
?- pixbit( 0, 0, 1, 10, PA),
   pixbit( 0, 1, 0, 20, PB), 
   pixbit( 1, 0, 0, 30, PC), 
   pixbit( 1, 1, 1, 4, PD),
   image( 2, 2, [PA, PB, PC, PD], I),
   moveH(I, X).
?- pixbit( 0, 0, 1, 10, PA),
   pixbit( 0, 1, 0, 20, PB), 
   pixbit( 1, 0, 0, 30, PC), 
   pixbit( 1, 1, 1, 4, PD),
   image( 2, 2, [PA, PB, PC, PD], I),
   flipH(I, X).
?- trace, (pixrgb( 0, 0, 255, 246, 255, 10, PA),
   pixrgb( 1, 0, 250, 220, 145, 10, PB),
   pixrgb( 0, 1, 234, 160, 130, 10, PC),
   pixrgb( 1, 1, 240, 176, 135, 10, PD),
      image( 2, 2, [PA, PB, PC, PD], I),
      imgRGBToHex(I, X)).
*/
