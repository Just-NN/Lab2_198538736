
/* Para definir los TDA, se consideran los siguientes:
	pixbit, pixhex, pixrgb e image.
	A continuación, se detallan sus constructores y predicados.*/



%------------ Reglas generales que sirven en los TDA-------------
%
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

image(Ancho, Largo, Pixeles, [Ancho, Largo, Pixeles]).

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

imageIsBitmap(Image) :-
    image(_, _, Pixels, Image),
    maplist(bit, Pixels).

% imageTohexmap?
% Dominio: Image (image)
%
% Meta Principal: imageTohexmap?
% Metas Secundarias: pixelsAreHexmap, image
% Utiliza Pixels para comprobar que sus elementos sean de tipo hex

imageIsHexmap(Image):-
	image(_, _, Pixels, Image),
	pixelsAreHexmap(Pixels).


% imageTorgbmap
% Dominio: Image (image)
%
% Meta Principal: imageTorgbmap?
% Metas Secundarias: pixelsAreRGBmap, image
% Utiliza Pixels para comprobar que sus elementos sean de tipo rgb

imageIsPixmap(Image):-
	image(_, _, Pixels, Image),
	pixelsAreRGBmap(Pixels).

% Dominio: Image (image)
%
% Meta Principal: imageIsCompressed
% Metas Secundarias: image, contar
% Decripción: revisa si la cantidad de elementos concuerda con
% las dimensiones dadas para la imagen

imageIsCompressed(Image):-
    image(Ancho, Largo, Pixeles, Image),
    contar(Pixeles, N),
    Largo*Ancho > N.

%---------------------------------------------
% Aquí empiezan reglas para modificar los pixeles

% Dominio: X (int) X PixIn (pixbit|pixhex|pixrgb) X PixOut(pixbit|pixhex|pixrgb)
%
% Meta Principal: changePixelH
% Metas Secundarias: cadr, caddr, cadddr, caddddr, cadddddr,
% 					 contar, string, pixhex, pixbit, pixrgb
% Cambia la posición X del pixel ingresado por
% un nuevo valor

changePixelH(X, PixIn, PixOut):-
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
% Metas Secundarias: changePixelH
% Descripción: se cambia la posición de los pixeles de acuerdo al ancho de la imagen, de modo
% que queden volteadas horizontalmente

flipPixels(_,[],_).
flipPixels(W2, [PixIn|PixsIn], [PixOut|PixsOut]):-
    car(PixIn, X),
    W is W2-X,
    changePixelH(W, PixIn, PixOut),
    flipPixels(W2, PixsIn, PixsOut).

% Dominio: In (image) X IOut (image)
%
% Meta Principal: flipH
% Metas Secundarias: image, flipPixels
% Descripción: se considera una nueva imagen que reconocerá como lista de pixeles
% a la lista original, pero volteada horizontalmente

imageFlipH(In, IOut):-
    image(Ancho, Largo, Pixlist, In),
    NewAncho is Ancho-1,
    flipPixels(NewAncho, Pixlist, PixsOut),
    image(Ancho, Largo, PixsOut, IOut).



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

flipPixelsV(_,[],_).
flipPixelsV(W2, [PixIn|PixsIn], [PixOut|PixsOut]):-
    cadr(PixIn, Y),
    W1 is W2-Y,
    changePixelV(W1, PixIn, PixOut),
    flipPixelsV(W2, PixsIn, PixsOut).

% Dominio: In (image) X IOut (image)
%
% Meta Principal: flipV
% Metas Secundarias: image, flipPixels
% Descripción: se considera una nueva imagen que reconocerá como lista de pixeles
% a la lista original, pero volteada verticalmente   
   
imageFlipV(ImageIn, ImageOut) :-
    image(Ancho, Largo, PixelsIn, ImageIn),
    NewLargo is Largo-1,
    flipPixelsV(NewLargo, PixelsIn, PixelsOut),
	image(Ancho, Largo, PixelsOut, ImageOut).


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

cropPixels(X1, Y1, X2, Y2, PixsIn, Output):-
    include(inRange(X1, Y1, X2, Y2), PixsIn, Output).

% Dominio: X (int) X Y (int) X PixIn (pixbit|pixhex|pixrgb) X  PixOut (pixbit|pixhex|pixrgb)
%
% Meta Principal: changeCoordinates
% Metas Secundarias: caddr, cadddr, contar, string, pixbit, caddddr, cadddddr, pixrgb, pixbit
% Descripción: regla que permite cambiar las coordenadas del pixel entregado de acuerdo a los valores de X e Y

changeCoordinates(X, Y, PixIn, PixOut):-
    caddr(PixIn, BH),
    cadddr(PixIn, Depth),
    contar(PixIn, N),
    (   N==4->  (   string(BH) ->  pixhex(X, Y, BH, Depth, PixOut)
    			;   pixbit(X, Y, BH, Depth, PixOut))
    ;   (   caddddr(PixIn, B),
            cadddddr(PixIn, D),
            pixrgb(X, Y, BH, Depth, B, D, PixOut))).

% Dominio: X (int) X Y (int) X Ancho (int) X Largo (int) X PixIn (lista de pixbit|pixhex|pixrgb)X  PixOut (lista de pixbit|pixhex|pixrgb)
%
% Meta Principal: changeCoordinates
% Metas Secundarias: caddr, cadddr, contar, string, pixbit, caddddr, cadddddr, pixrgb, pixbit
% Descripción: regla que permite cambiar las coordenadas del pixel entregado de acuerdo a los valores de X e Y

resetPositions(_, _, _, _, [], _).
resetPositions(X, Y, Ancho, Largo, [PixIn|PixsIn], [PixOut|PixsOut]):-
    changeCoordinates(X, Y, PixIn, PixOut),
    (   Y>=Largo-1 ->	NewY is 0, NewX is X+1
    ;   NewY is Y+1, NewX is X),
    resetPositions(NewX, NewY, Ancho, Largo, PixsIn, PixsOut).
    

% Dominio: X1 (int) X Y1 (int) X X2 (int) X Y2 (int) X ImageIn (image) X ImageOut(image)
%
% Meta Principal: crop
% Metas Secundarias: image, cropPixels, resetPositions
% Descripción: al igual que otras reglas, ésta ocupa las metas secundarias previamente para
% considerar una lista de pixeles bajo los criterios dados. En este caso, los pixeles dentro
% del rango solicitado en cada llamado
    
imageCrop(ImageIn, X1, Y1, X2, Y2, ImageOut):-
	image(_, _, PixelsIn, ImageIn),
    cropPixels(X1, Y1, X2, Y2, PixelsIn, PixelsOut),
    NewAncho is X2-X1+1,
    NewLargo is Y2-Y1+1,
    resetPositions(0, 0, NewAncho, NewLargo, PixelsOut, PixelsOut2),
    image(NewAncho, NewLargo, PixelsOut2, ImageOut).

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
    (   X>16 ->  X1 is X/16
    ;   X1 is X),
    getInt(X1, Int),
    getDec(X1, Dec),
    intToHex(Int, NewInt),
    NewDec is Dec*16,
    intToHex(NewDec, NewInt2),
    atom_concat(NewInt, NewInt2, Y0),
    atom_string(Y0, Y).

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
    atom_string(RGB, RGB1),
    pixhex(X, Y, RGB1, D, PixOut),
    pixsRGBToHex(PixsIn, PixsOut).

% Dominio: ImageIn(image) X ImageOut(image)
%
% Meta Principal: imgRGBToHex
% Metas Secundarias: image, pixsRGBToHex
% Descripción: Se considera la regla anterior para la lista de pixeles dada por la imagen 
% de entrada, para así poder obtener la de salida

imageRGBToHex(ImageIn, ImageOut):-
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

% Dominio: PixsIn (pixbit|pixhex|pixrgb) X PixOut (pixbit|pixhex|pixrgb)
% 
% Meta principal: pixtogram
% Metas secundarias: contar, getRGB, include, cons, exclude
% Descripción: se separan los casos para pixbit/hex y pixrgb
% dependiendo del largo, reconociendo su 3er valor para así
% poder considerar una lista sólo con los elementos que sean equivalentes
% y finalmente contarlos para calcular su frecuencia. Posterior a ello, se arma
% un par con el valor y la frecuencia, y esto se unificaría como el pixel de salida en la lista
% para luego borrar todos los pixeles cuyos valores

pixtogram([], _).
pixtogram([PixIn|PixsIn], [PixOut|PixsOut]):-
    contar(PixIn, Len),
    (   Len == 4 ->  caddr(PixIn, BH)
    ;   getRGB(PixIn, BH)
    ),
    include(pixCheck(PixIn), PixsIn, P1),
    contar(P1, N),
    NewN is N+1,
	cons(BH, NewN, PixOut),
    exclude(pixCheck(PixIn), PixsIn, P2),
    pixtogram(P2, PixsOut).

% Dominio: Img1 (image) X Histogram (listade pares)
%
% Meta Principal: ImageToHistogram
% Metas Secundarias: pixtogram
% Descripción: aplico la meta secundaria para conseguir una lista con pares
% de valor y frecuencia
findMax([], _).
findMax(Histogram, ActualValue, ActualColor):-
    car(Histogram, Par),
    car(Par, Color),
    cadr(Par, Value),
    (   Value>ActualValue -> (   NewValue is Value, NewColor is Color)
    ;   (   NewValue is ActualValue, NewColor is ActualColor)),
    findMax(Histogram, NewValue, NewColor).
imageToHistogram(Img1, Histogram):-
    image(_, _, Pixlist, Img1),
   	pixtogram(Pixlist, Histogram).


%-------------rotate90°-------------------%
% Desde aquí empiezan las metas para rotate90

% Dominio: 
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

transpose([], _):-!.
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

imageRotate90(Img1, Img2):-
    image(Ancho, Largo, Pixslist, Img1),
    transpose(Pixslist, PixsOut),
    NewAncho is Ancho-1,
    flipPixels(NewAncho, NewAncho, PixsOut, PixsOutput),
    image(Largo, Ancho, PixsOutput, Img2),!.



% Dominio: Img1(image) X PIn (pixbit|pixhex|pixrgb) X Img2(image)
%
% Meta Principal: imageChangePixel
% Metas Secundarias: image, checkCoordinates
% Descripción: Se considera una imagen de entrada y un pixel de entrada
% se irá revisando cada pixel de la Img1 para ver si las coordenadas calzan con las del nuevo pixel
% En caso de ser así, agrega PixIn en vez del original, y se se termina de crear la nueva Img2 en base a la nueva lista


% Dominio: PixIn (pixbit|pixhex|pixrgb) X PixOut (pixbit|pixhex|pixrgb)
%
% Meta Principal: copyPix
% Metas Secundarias: car, cadr, caddr, cadddr, contar, string, pixhex, pixbit, pixrgb, caddddr, cadddddr
% Descripción: Se evalúa el tipo de pixel y se reconstruye en el PixOut para poder usarlo
copyPix(PixIn, PixOut):-
    car(PixIn, X),
    cadr(PixIn, Y),
    caddr(PixIn, BH),
    cadddr(PixIn, Depth),
    contar(PixIn, N),
    (   N==4->  (   string(BH) ->  pixhex(X, Y, BH, Depth, PixOut)
    			;   pixbit(X, Y, BH, Depth, PixOut))
    ;   (   caddddr(PixIn, B),
            cadddddr(PixIn, D),
            pixrgb(X, Y, BH, Depth, B, D, PixOut))).

% Dominio: P1 (pixbit|pixhex|pixrgb) X P2 (pixbit|pixhex|pixrgb)
%
% Meta Principal: checkCoordinates
% Metas Secundarias: car, cadr
% Descripción: Se comprueba que ambos pares de coordenadas calcen

checkCoordinates(P1, P2):-
    car(P1, X1),
    car(P2, X2),
    cadr(P1, Y1),
    cadr(P2, Y2),
    X1 == X2,
    Y1 == Y2.

% Dominio: PixlistIn (lista de pixbit|pixhex|pixrgb) X PIn (pixbit|pixhex|pixrgb) X PixOut (pixbit|pixhex|pixrgb)
%
% Meta Principal: pixlistChangePixel
% Metas Secundarias: checkCoordinates, copyPix
% Descripción: Se checkean las coordenadas para copiar el pixel en la posición adecuada, sino, se copia el pixel que va originalmente en lugar de reemplazarlo
% Utiliza recursión, y un caso base en el cual 
pixlistChangePixel([], _, _).
pixlistChangePixel([PixIn|PixsIn], PIn, [PixOut|PixsOut]):-
    (   checkCoordinates(PixIn, PIn) ->  copyPix(PIn, PixOut)
    ;   copyPix(PixIn, PixOut)),
    pixlistChangePixel(PixsIn, PIn, PixsOut).

% Dominio: Img1(image) X PIn (pixbit|pixhex|pixrgb) X Img2(image)
%
% Meta Principal: imageChangePixel
% Metas Secundarias: image, checkCoordinates
% Descripción: Se considera una imagen de entrada y un pixel de entrada
% se irá revisando cada pixel de la Img1 para ver si las coordenadas calzan con las del nuevo pixel
% En caso de ser así, agrega PixIn en vez del original, y se se termina de crear la nueva Img2 en base a la nueva lista

imageChangePixel(Img1, PIn, Img2):-
    image(Width, Large, Pixlist1, Img1),
    pixlistChangePixel(Pixlist1, PIn, Pixlist2),
    image(Width, Large, Pixlist2, Img2).

% Dominio: PixIn (pixbit|pixhex|pixrgb) X StrOut (string)
%
% Meta Principal: PixToString
% Metas Secundarias: caddr, contar, atom_string, atom_concat, cadddr, caddddr, cadddddr
% Descripción: Regla que permite convertir en string el color de un pixel 


pixToString(PixIn, StrOut):-
    caddr(PixIn, BH),
    contar(PixIn, N),
    (   N==4->  atom_string(BH, StrOut)
    ;   (   cadddr(PixIn, B),
            caddddr(PixIn, D),
            cadddddr(PixIn, Depth),
            atom_concat(BH, ", ", R),
            atom_concat(R, B, RG),
            atom_concat(RG, ", ", RG2),
            atom_concat(RG2, D, RGB),
            atom_concat(RGB, ", ", RGB2),
            atom_concat(RGB2, Depth, RGB3),
            atom_string(RGB3, StrOut))).

% Dominio: S1 (string) X S2 (string) X S3 (string)
%
% Meta Principal: StrAppend
% Metas Secundarias: atom_concat, atom_string
% Descripción: Regla que permite agregar un string al final de otro

strAppend(S1, S2, S3):-
    atom_concat(S1, S2, SE),
    atom_string(SE, S3).

% Dominio: Width (int) X Width2 (int) X PixlistIn (lista de pixbit|pixhex|pixrgb) X StrOut (string) X Final (string)
%
% Meta Principal: pixlistToString
% Metas Secundarias: pixToString, strAppend
% Descripción: Regla que permite convertir en string toda una lista de pixeles en un formato similar a cómo se mostraría una imagen
% Usa recursión y un caso base, el cual, ocurre cuando la lista se vacía, y se iguala Final al StrOut

pixlistToString(_, _, [], StrOut, Final):-
    copy_term(StrOut, Final).
pixlistToString(Width, Width2, [PixIn|PixsIn], StrOut, Final):-
    pixToString(PixIn, C),
    (   Width =\= 0 ->	(   strAppend(C, "\t", NewC),
                            strAppend(StrOut, NewC, NewStrOut),
                            NewW is Width-1)
    ;   (   strAppend(C, " \n", NewC),
            strAppend(StrOut, NewC, NewStrOut),
        	NewW is Width2
        )
    ),
    
    pixlistToString(NewW, Width2, PixsIn, NewStrOut, Final).

% Dominio: I (image) X Sout (string)
%
% Meta Principal: imageToString
% Metas Secundarias: image, pixlistToString
% Descripción: Regla que permite convertir en string la lista de pixeles de una imagen, utilizando su ancho para poder realizar los saltos de linea
% y considerando una tabulacion entre cada pixel

imageToString(I, Sout):-
    image(W, _, Pixs, I),
    pixlistToString(W, W, Pixs, "", Sout).

/*
Ejemplo 1 bitmap y predicado funcionando plenamente e incluso imageToString
pixbit( 0, 0, 1, 10, PA), 
pixbit( 0, 1, 0, 20, PB), 
pixbit( 1, 0, 0, 30, PC), 
pixbit( 1, 1, 1, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsBitmap(I),
imageToString(I, Str),
write(Str).


Ejemplo 1.a isbitmap funciona correctamente
pixbit( 0, 0, 1, 10, PA), 
pixbit( 0, 1, 0, 20, PB), 
pixbit( 1, 0, 0, 30, PC), 
pixbit( 1, 1, 1, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsBitmap( I ).

Ejemplo 1.b bitmap arroja falso cuando es hex
pixhex( 0, 0, "#123456", 10, PA), 
pixhex( 0, 1, "#CF1054", 20, PB), 
pixhex( 1, 0, "#BCDEF1", 30, PC), 
pixhex( 1, 1, "#123433", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsBitmap( I ).

Ejemplo 1.c bitmap arroja falso cuando es rgb
pixrgb( 0, 0, 200, 200, 200, 10, PA),
pixrgb( 0, 1, 200, 200, 200, 20, PB),
pixrgb( 1, 0, 190, 190, 190, 30, PC),
pixrgb( 1, 1, 190, 190, 190, 4, PD),
image( 2, 2, [PA, PB, PC, PD], I),
imageIsBitmap( I ).

Ejemplo 2 image con hex funciona correctamente
pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 1, 0, "#0000FF", 30, PC), 
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I),
imageIsHexmap( I ),
imageToString(I, Str),
write(Str).

Ejemplo 2.a el predicado de hex arroja verdadero
pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 1, 0, "#0000FF", 30, PC), 
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsHexmap( I ).

Ejemplo 2.b el predicado de hex arroja falso con bit
pixbit( 0, 0, 1, 10, PA), 
pixbit( 0, 1, 0, 20, PB), 
pixbit( 1, 0, 0, 30, PC), 
pixbit( 1, 1, 1, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsHexmap( I ).

Ejemplo 2.c el predicado de hex arroja falso con rgb
pixrgb( 0, 0, 200, 200, 200, 10, PA), 
pixrgb( 0, 1, 200, 200, 200, 20, PB), 
pixrgb( 1, 0, 190, 190, 190, 30, PC), 
pixrgb( 1, 1, 190, 190, 190, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsHexmap( I ).

Ejemplo 3 image funciona correctamente con rgb
pixrgb( 0, 0, 255, 0, 0, 10, PA), 
pixrgb( 0, 1, 255, 0, 0, 20, PB), 
pixrgb( 1, 0, 0, 0, 255, 30, PC), 
pixrgb( 1, 1, 0, 0, 255, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I).

Ejemplo 3.a el predicado de rgb funciona correctamente con rgb
pixrgb( 0, 0, 200, 200, 200, 10, PA), 
pixrgb( 0, 1, 200, 200, 200, 20, PB), 
pixrgb( 1, 0, 190, 190,190, 30, PC), 
pixrgb( 1, 1, 190, 190, 190, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsPixmap( I ).

Ejemplo 3.b el predicado de rgb arroja falso con bit
pixbit( 0, 0, 1, 10, PA), 
pixbit( 0, 1, 0, 20, PB), 
pixbit( 1, 0, 0, 30, PC), 
pixbit( 1, 1, 1, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsPixmap( I ).

Ejemplo 3.c el predicado de rgb arroja falso con hex
pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 1, 0, "#0000FF", 30, PC), 
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsPixmap( I ).

Ejemplo 4 rgbToHex funcionando bien
pixrgb( 0, 0, 200, 200, 200, 10, PA),
pixrgb( 0, 1, 200, 200, 200, 20, PB),
pixrgb( 1, 0, 190, 190,190, 30, PC),
pixrgb( 1, 1, 190, 190, 190, 4, PD),
image( 2, 2, [PA, PB, PC, PD], I),
imageIsPixmap( I ),
imageRGBToHex(I, I2),
imageIsHexmap(I2),
imageToString(I2, Str), 
write(Str). 

Ejemplo 5 Rotate90 funcionando bien
pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 1, 0, "#0000FF", 30, PC), 
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I),
imageRotate90(I, I2),
imageRotate90(I2, I3),
imageRotate90(I3, I4),
imageRotate90(I4, I5).

Ejemplo 6 rotate90 funcionando bien de acuerdo a lo descrito por el script
pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 1, 0, "#0000FF", 30, PC), 
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageRotate90(I, I2).

Ejemplo 7 flipV funciona de acuerdo a lo descrito por el script
pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 1, 0, "#0000FF", 30, PC), 
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I),
imageFlipV(I, I2), 
imageFlipV(I2, I3).

Ejemplo 8 flipH funciona de acuerdo a lo descrito por el script
pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 1, 0, "#0000FF", 30, PC), 
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I),
imageFlipH(I, I2), 
imageFlipH(I2, I3).

Ejemplo 9 crop funciona de acuerdo a lo descrito por el script
pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 0, 2, "#0000FF", 30, PC), 
pixhex( 1, 0, "#0000FF", 4, PD), 
pixhex( 1, 1, "#FF0000", 4, PE), 
pixhex( 1, 2, "#FF0000", 4, PF), 
pixhex( 2, 0, "#FF0000", 4, PG), 
pixhex( 2, 1, "#FF0000", 4, PH), 
pixhex( 2, 2, "#FF0000", 4, PI), 
image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), 
imageCrop( I, 1, 1, 2, 2, I2), 
pixhex( 0, 0, "#FF0000", 4, PE2), 
pixhex( 0, 1, "#FF0000", 4, PF2), 
pixhex( 1, 0, "#FF0000", 4, PH2), 
pixhex( 1, 1, "#FF0000", 4, PI2), 
image( 2, 2, [PE2, PF2, PH2, PI2], I3).

Ejemplo 10.a Histograma usando bit
pixbit( 0, 0, 1, 10, PA), 
pixbit( 0, 1, 0, 20, PB), 
pixbit( 1, 0, 0, 30, PC), 
pixbit( 1, 1, 1, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageToHistogram(I, H).

Ejemplo 10.b Histograma usando hex
pixhex( 0, 0, "#123456", 10, PA), 
pixhex( 0, 1, "#CF1054", 20, PB), 
pixhex( 1, 0, "#BCDEF1", 30, PC), 
pixhex( 1, 1, "#123433", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageToHistogram(I, H).

Ejemplo 10.c Histograma usando rgb
pixrgb( 0, 0, 200, 200, 200, 10, PA), 
pixrgb( 0, 1, 200, 200, 200, 20, PB), 
pixrgb( 1, 0, 190, 190, 190, 30, PC), 
pixrgb( 1, 1, 190, 190, 190, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageToHistogram(I, H).

Ejemplo 11 ChangePixel funciona
pixrgb( 0, 0, 200, 200, 200, 10, PA),
pixrgb( 0, 1, 200, 200, 200, 20, PB),
pixrgb( 1, 0, 190, 190,190, 30, PC),
pixrgb( 1, 1, 190, 190, 190, 4, PD),
image( 2, 2, [PA, PB, PC, PD], I),
pixrgb( 1, 1, 120, 150, 160, 4, PE),
imageChangePixel(I, PE, I2).

*/

