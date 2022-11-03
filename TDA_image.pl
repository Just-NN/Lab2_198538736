
/* Para definir los TDA, se consideran los siguientes:
	pixbit, pixhex, pixrgb e image.
	A continuación, se detallan sus constructores y predicados.

% Contar elementos
% Dominio: lista (de cualquier tipo) X N (int)
%
% Meta Principal: contar
% Metas Secundarias:
% Por medio de una recursión, sumaremos 1 a N por cada elemento que exista
% Su condición base será si la lista es nula, por lo que no se entrega nada



%------------ Reglas generales que sirven en los TDA-------------
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

% Considerando que los pixeles funcionan de manera similar a una lista
% es fácil imaginar que estas reglas servirán para los primeros 4 elementos en caso de 
% ser pixhex o pixbit (su largo es 4) y el resto servirán sólo para pixrgb u otras listas.

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
% Cambia la posición X del pixel ingresado por
% un nuevo valor

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

%
% Dominio: W (int) X W2 (int) X PixlistIn (pixbit|pixhex|pixrgb) X PixlistOut (pixbit|pixhex|pixrgb) 
%
% Meta Principal: flipPixels
% Metas Secundarias: changePixel
% Descripción: se cambia la posición de los pixeles de acuerdo al ancho de la imagen, de modo
% que queden volteadas horizontalmente

flipPixels(_,_,[],_).
flipPixels(W, W2, [PixIn|PixsIn], [PixOut|PixsOut]):-
    changePixel(W, PixIn, PixOut),
    (   W==0 ->  NewW is W2
    ;   NewW is W-1
    ),
    flipPixels(NewW, W2, PixsIn, PixsOut).

% Dominio: In (image) X IOut (image)
%
% Meta Principal: flipH
% Metas Secundarias: image, flipPixels
% Descripción: se considera una nueva imagen que reconocerá como lista de pixeles
% a la lista original, pero volteada horizontalmente

flipH(In, IOut):-
    image(Largo, Ancho, Pixlist, In),
    NewAncho is Ancho-1,
    flipPixels(NewAncho, NewAncho, Pixlist, PixsOut),
    image(Largo, Ancho, PixsOut, IOut).



%---------------------------------------------
% Estas serían las metas para el flip vertical, que es 
% casi igual al horizontal



% Dominio: Y (int) X PixIn (pixbit|pixhex|pixrgb) X PixOut(pixbit|pixhex|pixrgb)
%
% Meta Principal: changePixelV
% Metas Secundarias: cadr, caddr, cadddr, caddddr, cadddddr,
% 					 contar, string, pixhex, pixbit, pixrgb
% Cambia la posición Y del pixel ingresado por
% un nuevo valor

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

%
% Dominio: W (int) X W2 (int) X PixlistIn (pixbit|pixhex|pixrgb) X PixlistOut (pixbit|pixhex|pixrgb) 
%
% Meta Principal: flipPixelsV
% Metas Secundarias: changePixelV
% Descripción: se cambia la posición de los pixeles de acuerdo al largo de la imagen, de modo
% que queden volteadas verticalmente

flipPixelsV(_,_,[],_).
flipPixelsV(W, W2, [PixIn|PixsIn], [PixOut|PixsOut]):-
    changePixelV(W, PixIn, PixOut),
    (   W==0 ->  NewW is W2
    ;   NewW is W-1
    ),
    flipPixelsV(NewW, W2, PixsIn, PixsOut).

% Dominio: In (image) X IOut (image)
%
% Meta Principal: flipV
% Metas Secundarias: image, flipPixels
% Descripción: se considera una nueva imagen que reconocerá como lista de pixeles
% a la lista original, pero volteada verticalmente   
   
flipV(ImageIn, ImageOut) :-
    image(Largo, Ancho, PixelsIn, ImageIn),
    NewLargo is Largo-1,
    flipPixelsV(NewLargo, NewLargo, PixelsIn, PixelsOut),
	image(Largo, Ancho, PixelsOut, ImageOut).


%----------------------------- crop ---------
% Desde aquí empiezan metas para realizar el crop

% Dominio: X1 (int) X Y1 (int) X X2 (int) X Y2 (int) X Pix (pixbit|pixhex|pixrgb)
%
% Meta Principal: inRange
% Metas Secundarias: car, cadr


inRange(X1, Y1, X2, Y2, Pix):-
    	car(Pix, X),
    	cadr(Pix, Y),
        X >= X1,
    	X =< X2,
    	Y >= Y1,
    	Y =< Y2.
    
% Dominio: X1 (int) X Y1 (int) X X2 (int) X Y2 (int) X Pixlist (lista de pixbit|pixhex|pixrgb) X 
%		   Accum (lista de pixbit|pixhex|pixrgb) X Output (lista de pixbit|pixhex|pixrgb)
%
% Meta Principal: cropPixels
% Metas Secundarias: insertarAlPrincipio
% Descripción: se consideran las coordenadas dentro del rango, checkeadas por inRange 
% para poder agregar a los pixeles que cumplan dentro del acumulador y, finalmente,
% considerarlo como el Output de la regla.

cropPixels(_, _, _, _, [], Accum, Accum).
cropPixels(X1, Y1, X2, Y2, [PixIn|PixsIn], Accum, Output):-
    (   inRange(X1, Y1, X2, Y2, PixIn) ->	insertarAlPrincipio(PixIn, Accum, NewAccum)
    ;  NewAccum = Accum
    ),
    cropPixels(X1, Y1, X2, Y2, PixsIn, NewAccum, Output).

% Dominio: X1 (int) X Y1 (int) X X2 (int) X Y2 (int) X ImageIn (image) X ImageOut(image)
%
% Meta Principal: crop
% Metas Secundarias: image, cropPixels
% Descripción: al igual que otras reglas, ésta ocupa las metas secundarias previamente para
% considerar una lista de pixeles bajo los criterios dados. En este caso, los pixeles dentro
% del rango solicitado en cada llamado
    
crop(X1, Y1, X2, Y2, ImageIn, ImageOut):-
	image(Largo, Ancho, PixelsIn, ImageIn),
    cropPixels(X1, Y1, X2, Y2, PixelsIn, [], PixelsOut),
    image(Largo, Ancho, PixelsOut, ImageOut).

%---------------------------------------------------- rgb to hex
% Aquí empiezan las metas para convertir rgb a hex

% Dominio: X (number) X N (int) X Result (number)
%
% Meta Principal: truncate
% Metas Secundarias: floor
% Descripción: en resumen, se considera un parámetro X
% que se truncará a una cantidad de N decimales y esto corresponderá
% a Result
truncate(X,N,Result):- X >= 0, Result is floor(10^N*X)/10^N, !.

% Dominio: X (int) X Y(char)
%
% Meta Principal:intToHex
% Metas Secundarias: no aplica

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

% Dominio: X (number) X Y (int)
%
% Meta Principal: getInt
% Metas Secundarias:truncate
% Descripción: Y corresponderá a la parte entera de X

getInt(X, Y):-
    truncate(X, 0, Y).

% Dominio: X (number) X Y (int)
%
% Meta Principal: getDec
% Metas Secundarias: truncate, getInt
% Descripción: Y corresponderá a la parte decimal de X

getDec(X, Y):-
    (   X < 1 ->  truncate(X, 2, Y)
    ;   (	getInt(X, Int),
            Y is X - Int)).

% Dominio: X (int) X Y (string)
%
% Meta Principal: rgbToHex
% Metas Secundarias: getInt, getDec,m intToHex, atom_concat
% Descripción: se considera un numero X que será dividido por 16, 
% para luego tomar su parte entera y su parte decimal.
% De esta forma, se convierten a hex ambos valores siguiendo 
% los valores correspondientes

rgbToHex(X, Y):-
    X1 is X/16,
    getInt(X1, Int),
    getDec(X1, Dec),
    intToHex(Int, NewInt),
    NewDec is Dec*16,
    intToHex(NewDec, NewInt2),
    atom_concat(NewInt, NewInt2, Y).

% Dominio: PixsIn (lista de pixbit|pixrgb|pixhex) X PixsOut (lista de pixbit|pixrgb|pixhex)
%
% Meta Principal:pixsRGBToHex
% Metas Secundarias: pixrgb, rgbToHex, atom_concat, pixhex
% Descripción: se aplica el proceso descrito hasta ahora para cada pixelsAreHexmap
% Caso base: lista vacía

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

% Dominio: ImageIn(image) X ImageOut(image)
%
% Meta Principal: imgRGBToHex
% Metas Secundarias: image, pixsRGBToHex
% Descripción: Se considera la regla anterior para la lista de pixeles dada por la imagen 
% de entrada, para así poder obtener la de salida

imgRGBToHex(ImageIn, ImageOut):-
    image(Largo, Ancho, PixelsIn, ImageIn),
    pixsRGBToHex(PixelsIn, PixelsOut),
    image(Largo, Ancho, PixelsOut, ImageOut).


%--------------histograma-----------------%
% Desde aquí, empiezan las metas para el histograma

% Dominio: A (pixbit|pixhex|pixrgb) X B(pixbit|pixhex|pixrgb) 
%
% Meta Principal: pixCheck
% Metas Secundarias: caddr, contar, cadddr
% Descripción: Consiste en comparar los valores de 2 pixeles.
% Dependiendo de la cantidad de elementos, se compararán
% el 3er elemento o R, G y B.

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

% Dominio: A (variable) X B (variable)
%
% Meta Principal: cons
% Metas Secundarias: no aplica
% Descripción: se considera un par de elementos dados

cons(A, B, [A,B]).

% Dominio: Pix (pixbit|pixrgb|pixhex) X RGB (string)
%
% Meta Principal: getRGB
% Metas Secundarias: caddr, cadddr, caddddr, atom_concat
% Descripción: Se arma un string con los valores RGB del pixel

getRGB(Pix, RGB):-
    caddr(Pix, R),
    cadddr(Pix, G),
    caddddr(Pix, B),
    atom_concat(R, ", ", Rcoma),
    atom_concat(Rcoma, G, RG),
    atom_concat(RG, ", ", RGcoma),
    atom_concat(RGcoma, B, RGB).

% Dominio: PixsIn(lista de pixbit|pixhex|pixrgb) X PixsIn(lista de pixbit|pixhex|pixrgb)
%
% Meta Principal: histogram
% Metas Secundarias: contar, getRGB, include, cons, exclude
% Descripción: se separan los casos para pixbit/hex y pixrgb
% dependiendo del largo, reconociendo su 3er valor para así
% poder considerar una lista sólo con los elementos que sean equivalentes
% y finalmente contarlos para calcular su frecuencia. Posterior a ello, se arma
% un par con el valor y la frecuencia, y esto se unificaría como el pixel de salida en la lista
% para luego borrar todos los pixeles cuyos valores

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
% Desde aquí empiezan las metas para rotate90

% Dominio: PixsIn (pixbit|pixhex|pixrgb) X PixOut (pixbit|pixhex|pixrgb)
%
% Meta Principal: bitOrHexTranspose
% Metas Secundarias: car, cadr, caddr, cadddr, string, pixhex, pixbit
% Descripción: se toman los valores del pixel para cambiar de orden las coordenadas X e Y

bitOrHexTranspose(PixIn, PixOut):-
    car(PixIn, X),
    cadr(PixIn, Y),
    caddr(PixIn, BH),
    cadddr(PixIn, Depth),
    (   string(BH) ->  pixhex(Y, X, BH, Depth, PixOut)
    ;   pixbit(Y, X, BH, Depth, PixOut)).

% Dominio: PixsIn (pixbit|pixhex|pixrgb) X PixOut (pixbit|pixhex|pixrgb)
%
% Meta Principal: pixCons
% Metas Secundarias: car, caddr, cadddr, caddddr, cadddddr, contar, bitOrHexTranspose, pixrgb
% Descripción: Se sigue la idea anterior para generalizar los casos

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

% Dominio: PixsIn(lista de pixbit|pixhex|pixrgb) X PixsIn(lista de pixbit|pixhex|pixrgb)
%
% Meta Principal: transpose
% Metas Secundarias: pixCons, transpose
% Descripción: se repite la meta anterior con el fin de que cada pixel tenga
% sus coordenadas X e Y cambiadas

transpose([], _).
transpose([PixIn|PixsIn], [PixOut|PixsOut]):-
    pixCons(PixIn, PixOut),
    transpose(PixsIn, PixsOut).

% Dominio: Img1(image) X Img2(image)
%
% Meta Principal: rotate90
% Metas Secundarias: image, transpose, flipPixels
% Descripción: Se considera una imagen de entrada,
% para luego transponer sus posiciones y, posterior a ello,
% voltear horizontalmente dichas posiciones.
% Esto sería equivalente a rotar las posiciones en 90° y se
% utiliza la lista de pixeles de salida para la nueva imagen

rotate90(Img1, Img2):-
    image(Ancho, Largo, Pixslist, Img1),
    transpose(Pixslist, PixsOut),
    NewAncho is Ancho-1,
    flipPixels(NewAncho, NewAncho, PixsOut, PixsOutput),
    image(Largo, Ancho, PixsOutput, Img2).
    



