#lang racket

#|
- Ing(c): Juan Fernando Cano Duque
- Nombre del lenguaje utilizado
- Versión del lenguaje utilizado
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
|#

( require graphics/graphics )
( open-graphics )
( define chess ( open-viewport "Ajedrez" 1000 640 ) )
( ( draw-solid-rectangle chess ) ( make-posn 0 0 ) 1000 640 "Brown" )
( ( ( draw-pixmap-posn "title.png" ) chess ) ( make-posn 640 0 ) )
( ( ( draw-pixmap-posn "turnoBlancas.png" ) chess ) ( make-posn 810 295 ) )
( define ( drawChess x y colorCounter ) ;Función para dibujar los cuadros del tablero
       ( if ( < y 640 )
            ( if ( < x 640 )
                ( if ( = colorCounter 1 )
                     ;Si el contador es 1 dibuja una casilla de color más claro
                     ( begin
                        ( ( draw-solid-rectangle chess ) ( make-posn x y ) 80 80 "Bisque" )
                        ( drawChess ( + 80 x ) y ( - colorCounter 1 ) )
                        );Fin begin
                 ;De lo contrario dibuja una casilla de color más oscuro
                     ( begin
                        ( ( draw-solid-rectangle chess ) ( make-posn x y ) 80 80 "peru" )
                        ( drawChess ( + 80 x ) y ( + colorCounter 1 ) )
                        );Fin begin
                     );Fin ( if ( = colorCounter 1 ) )
                ;De lo contrario
                ( if ( = colorCounter 1 )
                     ( drawChess 0 ( + 80 y ) 0 )
                     ( drawChess 0 ( + 80 y ) 1 )
                     );Fin ( if ( = colorCounter 1 ) )
                ); Fin ( if ( <= x 640 ) )
            ;De lo contrario 
            ( void )
            );Fin ( if ( <= y 640 ) )
   );fin función ( drawGame x y colorCounter )
( drawChess 0 0 1 );Dibuja el tablero sin las piezas
;Funcion que recibe un string con el orden de las piezas y dibuja las piezas sobre el tablero
( define ( Piezas str pos x y )
   ( if ( < pos ( - ( string-length str ) 1 ) )
        ;Si el contador de posición es menor al final de la cadena 
        ( if ( equal? "bb" ( substring str pos ( + pos 2 ) ) )
             ;Si hay un espacio en blanco "bb" salta al siguiente tramo del string sin dibujar nada
             ( Piezas str ( + 2 pos ) ( if ( = x 560 ) 0 ( + x 80 ) ) ( if ( = x 560 ) ( + y 80 ) y ) )
        ;De lo contrario
             ( begin
                ;Dibuja la imagen que tiene como nombre el substring de la posición actual
                ( ( ( draw-pixmap-posn ( string-append ( substring str pos ( + pos 2 ) ) ".png" ) ) chess ) ( make-posn x y ) )
                ( Piezas str ( + 2 pos ) ( if ( = x 560 ) 0 ( + x 80 ) ) ( if ( = x 560 ) ( + y 80 ) y ) );Llamada recursiva
                );Fin begin
             );Fin ( if ( equal? "bb" ( substring str pos ( + pos 2 ) ) ) )
    ;De lo contrario
        ( void )
        );Fin ( if ( < pos ( - ( string-length str ) 1 ) ) )
   );Fin funcion ( Piezas str pos x y )

;Función que hace el camvio en el string
( define ( Play str pos1 pos2 )
   ( if ( < pos1 pos2 )
        ( string-append ( substring str 0 pos1 ) "bb" ( substring str ( + pos1 2 ) pos2 )
                   ( substring str pos1 ( + pos1 2 ) ) ( substring str ( + pos2 2 ) ) )
        ;De lo contrario
        ( string-append ( substring str 0 pos2 ) ( substring str pos1 ( + pos1 2 ) )
                        ( substring str ( + pos2 2 ) pos1 ) "bb" ( substring str ( + pos1 2 ) ) )
        );Fin if ( < pos1 pos2 )
   );Fin función Play

;Función que dado un string y las posiciones jugadas revisa si hay piezas entre las posiciones que invaliden el movimiento
( define ( CleanPath? table turn x1 y1 x2 y2 pos1 pos2 )
   ( define turnOponent ( if ( char=? turn #\B ) #\N #\B ) );Define el turno del oponente
   ;Usamos el incremento para llamar recursivamente a la función y revisar, dependiendo de la pieza, si en el camino hay algún obstaculo
   ( define increment ( if ( = x1 x2 )
                         ( * 16 ( if ( > y2 y1 ) 1 -1 ) )
                         ;De lo contrario
                         ( if ( = y1 y2 )
                            ( if ( > x2 x1 ) 2 -2 )
                            ;De lo contrario
                            ( if ( = ( abs ( - x2 x1 ) ) ( abs ( - y2 y1 ) ) )
                               ( if ( and ( > x2 x1 ) ( > y2 y1 ) )
                                  18
                                  ;De lo contrario
                                  ( if ( and ( < x2 x1 ) ( < y2 y1 ) )
                                     -18
                                     ;De lo contrario
                                     ( if ( and ( > x2 x1 ) ( < y2 y1 ) )
                                        -14
                                        ;De lo contrario
                                        ( if ( and ( < x2 x1 ) ( > y2 y1 ) )
                                           14
                                           ;De lo contrario
                                           ( - pos2 pos1 )
                                           );Fin if ( and ( < x2 x1 ) ( > y2 y1 ) )
                                        );Fin if ( and ( > x2 x1 ) ( < y2 y1 ) )
                                     );Fin if ( and ( < x2 x1 ) ( < y2 y1 ) )
                                  );Fin if ( and ( > x2 x1 ) ( > y2 y1 ) )
                               ;De lo contrario
                               ( void )
                               );Fin if ( = ( abs ( - x2 x1 ) ) ( abs ( - y2 y1 ) ) )
                            );Fin if ( = y1 y2 )
                         );Fin if ( = x1 x2 )
      );Fin definición del identificador incremento
   ( if ( = pos1 pos2 )
      #t
      ;De lo contrario
      ( if ( or ( char=? ( string-ref table ( + pos1 increment 1 ) ) turn ) ( char=? ( string-ref table ( + pos1 1 ) ) turnOponent ) )
         #f
         ;De lo contrario
         ( CleanPath? table turn x1 y1 x2 y2 ( + pos1 increment ) pos2 )
         );Fin if ( or ( char=? ( string-ref table ( + pos1 increment 1 ) ) turn ) ... )
      );Fin if ( = pos1 pos2 )
   );Fin función CleanPath?

;Función que valida si el movimiento de la pieza es valido
( define ( ValidPlay? table piece x1 y1 x2 y2 pos1 pos2 )
   ( if ( char=? ( string-ref piece 0 ) #\T )
      ( if ( and ( or ( = x1 x2 ) ( = y1 y2 ) ) ( CleanPath? table ( string-ref piece 1 ) x1 y1 x2 y2 pos1 pos2 ) )
         #t
         ;De lo contrario
         #f
         );Fin if ( and ( or ( = x1 x2 ) ( = y1 y2 ) ) ( CleanPlath? ... ) )
      ;De lo contrario
      ( if ( char=? ( string-ref piece 0 ) #\C )
         ( if ( and ( not ( char=? ( string-ref table ( + 1 pos2 ) ) ( string-ref piece 1 ) ) ) ( = 3 ( + ( abs ( - x2 x1 ) ) ( abs ( - y2 y1 ) ) ) )
                    ( not ( = ( abs ( - x2 x1 ) ) 3 ) ) ( not ( = ( abs ( - y2 y1 ) ) 3 ) ) )
            #t
            ;De lo contrario
            #f
            );Fin if ( ( not ( char=? ( string-ref table ( + 1 pos2 ) ) ( string-ref piece 1 ) ) ) ( = 3 ... ) ( not ... ) )
         ;De lo contrario
         ( if ( char=? ( string-ref piece 0 ) #\A )
            ( if ( and ( = ( abs ( - x2 x1 ) ) ( abs ( - y2 y1 ) ) ) ( CleanPath? table ( string-ref piece 1 ) x1 y1 x2 y2 pos1 pos2 ) )
               #t
               ;De lo contrario
               #f
               );Fin if ( and ( = ( abs ( - x2 x1 ) ) ( abs ( - y2 y1 ) ) ) ( CleanPath? ... ) )
            ;De lo contrario
            ( if ( char=? ( string-ref piece 0 ) #\D )
               ( if ( and ( or ( = x1 x2 ) ( = y1 y2 ) ( = ( abs ( - x2 x1 ) ) ( abs ( - y2 y1 ) ) ) ) ( CleanPath? table ( string-ref piece 1 ) x1 y1 x2 y2 pos1 pos2 ) )
                  #t
                  ;De lo contrario
                  #f
                  );Fin if ( and ( or ( = x1 x2 ) ( = y1 y2 ) ( = ( abs ( - x2 x1 ) ) ( abs ( - y2 y1 ) ) ) ) ( CleanPath? ... ) )
               ;De lo contrario
               ( if ( char=? ( string-ref piece 0 ) #\P )
                  ( if ( or ( and ( char=? #\N ( string-ref piece 1 ) ) ( char=? #\B ( string-ref table ( + pos2 1 ) ) ) ( > y2 y1 )
                                  ( = 2 ( + ( - y2 y1 ) ( abs ( - x2 x1 ) ) ) ) ( not ( = ( abs ( - x2 x1 ) ) 2 ) ) ( not ( = ( abs ( - y2 y1 ) ) 2 ) ) )
                            ( and ( char=? #\B ( string-ref piece 1 ) ) ( char=? #\N ( string-ref table ( + pos2 1 ) ) ) ( > y1 y2 )
                                  ( = 2 ( + ( - y1 y2 ) ( abs ( - x2 x1 ) ) ) ) ( not ( = ( abs ( - x2 x1 ) ) 2 ) ) ( not ( = ( abs ( - y2 y1 ) ) 2 ) ) )
                            ( and ( = x2 x1 ) ( = y2 ( + y1 1 ) ) ( char=? #\N ( string-ref piece 1 ) ) ( char=? #\b ( string-ref table pos2 ) ) )
                            ( and ( = x2 x1 ) ( = y2 ( - y1 1 ) ) ( char=? #\B ( string-ref piece 1 ) ) ( char=? #\b ( string-ref table pos2 ) ) )
                            ( and ( = y1 1 ) ( = x2 x1 ) ( = y2 ( + y1 2 ) ) ( char=? #\b ( string-ref table pos2 ) ) )
                            ( and ( = y1 6 ) ( = x2 x1 ) ( = y2 ( - y1 2 ) ) ( char=? #\b ( string-ref table pos2 ) ) )
                            )
                     #t
                     ;De lo contrario
                     #f
                     );Fin if ( or ( and ... ) ( and ... ) ( and ... ) ( and ... ) ( and ... ) ( and ... )
                  ;De lo contrario
                  ( if ( char=? ( string-ref piece 0 ) #\R )
                     ( if ( and ( not ( char=? ( string-ref piece 1 ) ( string-ref table ( + pos2 1 ) ) ) )
                                ( or ( and ( = x1 x2 ) ( or ( = y2 ( + y1 1 ) ) ( = y2 ( - y1 1 ) ) ) )
                                     ( and ( = y1 y2 ) ( or ( = x2 ( + x1 1 ) ) ( = x2 ( - x1 1 ) ) ) )
                                     ( and ( = y2 ( + y1 1 ) ) ( = x2 ( + x1 1 ) ) ) ( and ( = y2 ( + y1 1 ) ) ( = x2 ( - x1 1 ) ) )
                                     ( and ( = y2 ( - y1 1 ) ) ( = x2 ( - x1 1 ) ) ) ( and ( = y2 ( - y1 1 ) ) ( = x2 ( + x1 1 ) ) ) ) )
                        #t
                        ;De lo contrario
                        #f
                        );Fin if ( and ( not ... ) ( or ( and ... ) ( and ... ) ( and ... ) ( and ... ) ) )
                     ;De lo contrario
                     #f
                     );Fin if ( char=? ( string-ref piece 0 ) #\R )
                  );Fin if ( char=? ( string-ref piece 0 ) #\P )
               );Fin if ( char=? ( string-ref piece 0 ) #\D )
            );Fin if ( char=? ( string-ref piece 0 ) #\A )
         );Fin if ( char=? ( string-ref piece 0 ) #\C )
      );Fin if ( char=? ( string-ref piece 0 ) #\T )
   );Fin función ValidPay?

;Función que revisa si el tablero está en jaque usando la posición del rey
( define ( Jaque? table posI posR )
   ( if ( < posI 128 )
      ( if ( = posI posR )
         ( Jaque? table ( + posI 2 ) posR )
         ;Llamamos recursivamente a la función sin validar la pieza que está con el rey
         ;De lo contrario
         ( if ( ValidPlay? table ( substring table posI ( + posI 2 ) ) ( remainder ( / posI 2 ) 8 )
              ( quotient ( / posI 2 ) 8 ) ( remainder ( / posR 2 ) 8 ) ( quotient ( / posR 2 ) 8 ) posI posR )
            #t
            ;De lo contrario
            ( Jaque? table ( + posI 2 ) posR );Llamamos recursivamente a la función
            );Fin if ( ValidPlay? ... )
         );Fin if ( = posI posR )
      ;De lo contrario
      #f
      );Fin if ( < posI 128 )
   );Fin función Jaque?

;Función que revisa si el jaque es jaque mate, revisando si cada jugada posible hace que el tablero deje de estar en jaque
( define ( Mate? table color pos posR )
   ;Función interna que dada una posición busca si moviendo la pieza en dicha posición el tablero deja de estar en jaque
   ( define ( BreakJaque posF )
      ( if ( <= posF 126 )
         ( if ( char=? color ( string-ref table ( + 1 pos ) ) )
            ( if ( or ( = pos posF ) ( = posF posR ) )
               ( BreakJaque ( + 2 posF ) );Llamamos a la función si la posición inicial es igual a la final o la final a la del rey
               ;De lo contrario
               ( if ( and ( ValidPlay? table ( substring table pos ( + 2 pos ) ) ( remainder ( / pos 2 ) 8 ) ( quotient ( / pos 2 ) 8 )
                          ( remainder ( / posF 2 ) 8 ) ( quotient ( / posF 2 ) 8 ) pos posF ) ( not ( Jaque? ( Play table pos posF ) 0 posR ) ) )
                    #t
                    ;De lo contrario
                    ( BreakJaque ( + posF 2 ) );Llamamos a la función con posF + 2 si no es valido el movimiento o si sigue en jaque el tablero
                    );Fin if ( and ( ValidPlay? ... ) ( not ( Jaque? ... ) ) )
               );Fin if ( or ( = pos posF ) ( = posF posR ) )
            ( BreakJaque ( + posF 2 ) )
            );Fin if ( char=? color ( string-ref table ( + 1 pos ) ) )
         #f
         );Fin if ( <= posF 126 )
      );Fin función interna BreakJaque
   ( if ( <= pos 126 )
      ( if ( BreakJaque 0 );Busca si la pieza en la posición rompe el jaque
         #f
         ;De lo contrario
         ( Mate? table color ( + 2 pos ) posR );Llama a la función conn pos + 2 para revisar la pieza siguiente
         );Fin if ( BreakJaque 0 )
      ;De lo contrario
      #t
      );Fin if ( <= pos 126 )
   );Fin función Mate

;Función que devuelve la posición del rey
( define ( PosR table color x y )
   ( if ( char=? #\R ( string-ref table ( + x ( * y 8 ) ) ) )
      ( if ( char=? color ( string-ref table ( + x ( * y 8 ) 1 ) ) )
           ( + x ( * y 8 ) )
           ;De lo contrario
           ( PosR table color ( if ( = x 16 ) 0 ( + x 2 ) ) ( if ( = x 16 ) ( + y 2 ) y ) )
           );Fin if ( char=? color ( string-ref table ( + x ( * y 8 ) 1 ) ) )
      ;De lo contrario
      ( if ( < y 16 )
         ( if ( < x 16 )
              ( PosR table color ( + 2 x ) y )
              ;De lo contrario
              ( PosR table color 0 ( + 2 y ) )
              );Fin if ( < x 16 )
         ;De lo contrario
         ( void )
         );Fin if ( < y 16 )
      );Fin if ( char=? #\R ( string-ref table ( + x ( * y 8 ) ) ) )
   );Fin función PosR

;Función que hace la coronación de peones
( define ( Coronation table color click pos1 pos2 )
   ( ( ( draw-pixmap-posn ( if ( char=? #\B color ) "CoronacionB.png" "CoronacionN.png" ) ) chess ) ( make-posn 640 0 ) )
   ;Pone la imagen con las opciones a coronar el peón, dependiendo de si el peón a coronar es negro o blanco
   ( define cl ( get-mouse-click chess ) );Definimos cl como el click nuevo para escoger las opciones
   ( ( draw-solid-rectangle chess ) ;Dibuja el rectangulo en la casilla en la que está el peón para borrarlo
     ( make-posn ( * 80 ( quotient ( posn-x ( mouse-click-posn click ) ) 80 ) ) ( * 80 ( quotient ( posn-y ( mouse-click-posn click ) ) 80 ) ) )
     80 80 ( if ( = 0 ( remainder ( + ( quotient ( / pos2 2 ) 8 ) ( remainder ( / pos2 2 ) 8 ) ) 2 ) ) "Bisque" "peru" ) )
   ( ( ( draw-pixmap-posn ;Dibuja en el cuadro en que estaba el peón la pieza que haya escogido
      ( string-append ( if ( and ( >= ( posn-x ( mouse-click-posn cl ) ) 672 ) ( < ( posn-x ( mouse-click-posn cl ) ) 737 )
                                 ( >= ( posn-y ( mouse-click-posn cl ) ) 432 ) ( < ( posn-y ( mouse-click-posn cl ) ) 497 ) )
                         "D"
                         ;De lo contrario
                         ( if ( and ( >= ( posn-x ( mouse-click-posn cl ) ) 750 ) ( < ( posn-x ( mouse-click-posn cl ) ) 815 )
                                    ( >= ( posn-y ( mouse-click-posn cl ) ) 432 ) ( < ( posn-y ( mouse-click-posn cl ) ) 497 ) )
                            "A"
                            ;De lo contrario
                            ( if ( and ( >= ( posn-x ( mouse-click-posn cl ) ) 826 ) ( < ( posn-x ( mouse-click-posn cl ) ) 891 )
                                       ( >= ( posn-y ( mouse-click-posn cl ) ) 432 ) ( < ( posn-y ( mouse-click-posn cl ) ) 497 ) )
                               "C"
                               ;De lo contrario
                               ( if ( and ( >= ( posn-x ( mouse-click-posn cl ) ) 901 ) ( < ( posn-x ( mouse-click-posn cl ) ) 966 )
                                          ( >= ( posn-y ( mouse-click-posn cl ) ) 432 ) ( < ( posn-y ( mouse-click-posn cl ) ) 497 ) )
                                  "T"
                                  ;De lo contrario
                                  ( Coronation table color click pos1 pos2 )
                                  );Fin if ( and ... )
                               );Fin if ( and ... )
                            );Fin if ( and ... )
                         );Fin if ( and ... )
                      ( ~a color ) ".png" ) ) chess ) ( make-posn ( * 80 ( remainder ( / pos2 2 ) 8 ) ) ( * 80 ( quotient ( / pos2 2 ) 8 ) ) ) )
   ;Define i como la pieza que se haya escogido
   ( define i ( if ( < ( quotient ( posn-x ( mouse-click-posn cl ) ) 75 ) 10 ) 1
                 ;De lo contrario
                 ( if ( < ( quotient ( posn-x ( mouse-click-posn cl ) ) 75 ) 11 ) 2
                    ;De lo contrario
                      ( if ( < ( quotient ( posn-x ( mouse-click-posn cl ) ) 75 ) 12 ) 3 4 ) ) ) )
   ;Define newTable como el string con el cambio en la coronación
   ( define newTable ( string-append ( substring table 0 pos2 ) ( if ( = 1 i ) "D" ( if ( = 2 i ) "A" ( if ( = 3 i ) "C" "T" ) ) ) ( ~a color ) ( substring table ( + 2 pos2 ) ) ) )
   ( ( ( draw-pixmap-posn ( if ( char=? color #\N ) "turnoBlancas.png" "turnoNegras.png" ) ) chess ) ( make-posn 810 295 ) );Cambia el mensaje del turno
   ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 120 "Brown" );Borra la interfaz con las opciones para la coronación
   ( if ( Jaque? newTable 0 ( PosR newTable ( if ( char=? color #\B ) #\N #\B ) 0 0 ) );Revisa si con el cambio de pieza el tablero queda en jaque
      ( if ( Mate? newTable ( if ( char=? color #\N ) #\B #\N ) 0 ( PosR newTable ( if ( char=? color #\N ) #\B #\N ) 0 0 ) ); revisa si hay mate
         ( begin
            ( ( ( draw-pixmap-posn "JaqueMate.png" ) chess ) ( make-posn 720 410 ) );Si hay mate, pone el mensaje de jaque mate
            ( End );Llama a la función para terminar el juego
            );Fin begin
         ;De lo contrario
         ( begin
            ( ( ( draw-pixmap-posn "Jaque.png" ) chess ) ( make-posn 720 410 ) );Si no hay mate pero si jaque, pone el mensaje de jaque
            ( Game newTable ( if ( char=? color #\N ) 0 1 ) 0 0 0 0 #t );Llama a la función principal con jaque en #t para continuar
            );Fin begin
         );Fin ir ( Mate? ... )
      ;De lo contrario
      ( Game newTable ( if ( char=? color #\B ) 1 0 ) 0 0 0 0 #f );Llama a la función principal con jaque en #f para continuar
      );Fin if ( Jaque? ... )
   );Fin función Coronation

;Función que hace el movimiento gráfico de las piezas
( define ( MovePiece table click previousClick pos )
   ;Borra la pieza en la casilla de llegada
   ( ( draw-solid-rectangle chess )
     ( make-posn ( * 80 ( quotient ( posn-x ( mouse-click-posn click ) ) 80 ) ) ( * 80 ( quotient ( posn-y ( mouse-click-posn click ) ) 80 ) ) )
     80 80 ( if ( = 0 ( remainder ( + ( quotient ( / pos 2 ) 8 ) ( remainder ( / pos 2 ) 8 ) ) 2 ) ) "Bisque" "peru" ) )
   ;Dibuja la pieza en la casilla de llegada
   ( ( ( draw-pixmap-posn ( string-append ( substring table previousClick ( + previousClick 2 ) ) ".png" ) ) chess )
     ( make-posn ( * 80 ( quotient ( posn-x ( mouse-click-posn click ) ) 80 ) ) ( * 80 ( quotient ( posn-y ( mouse-click-posn click ) ) 80 ) ) ) )
   ;Borra la pieza en la casilla de salida
   ( ( draw-solid-rectangle chess )
     ( make-posn ( * 80 ( remainder ( / previousClick 2 ) 8 ) ) ( * 80 ( quotient ( / previousClick 2 ) 8 ) ) ) 80 80
     ( if ( = 0 ( remainder ( + ( quotient ( / previousClick 2 ) 8 ) ( remainder ( / previousClick 2 ) 8 ) ) 2 ) ) "Bisque" "peru" ) )
   );Fin función MovePiece

;Función que termina el juego
( define ( End )
   ( define click ( get-mouse-click chess ) )
   ( if ( and ( >= ( posn-x ( mouse-click-posn click ) ) 704 ) ( < ( posn-x ( mouse-click-posn click ) ) 934 )
              ( >= ( posn-y ( mouse-click-posn click ) ) 536 ) ( < ( posn-y ( mouse-click-posn click ) ) 584 ) )
        ( close-viewport chess )
        ;De lo contrario
        ( End );Si no se presiona en el botón terminar vuelve a llamar a la funcion
        );Fin if ( and ... ) 
   );Fin función End

;Funcion principal del juego
( define ( Game table turn count x0 y0 previousClick jaque )
   ( if ( = 1 count ) ( printf "\n~a" table ) ( void ) )
   ( define click ( get-mouse-click chess ) )
   ( define x ( quotient ( posn-x ( mouse-click-posn click ) ) 80 ) )
   ( define y ( quotient ( posn-y ( mouse-click-posn click ) ) 80 ) )
   ( define pos ( * 2 ( + x ( * 8 y ) ) ) )
   ( define piece ( substring table previousClick ( + 2 previousClick ) ) )
   ( if ( < ( * x 80 ) 640 )
      ( if ( and ( or ( = y 0 ) ( = y 7 ) ) ( char=? #\P ( string-ref piece 0 ) ) ( not jaque ) ( ValidPlay? table piece x0 y0 x y previousClick pos ) )
         ( begin
            ( MovePiece table click previousClick pos )
            ( Coronation ( Play table previousClick pos ) ( string-ref piece 1 ) click previousClick pos )
            );Fin begin
         ;De lo contrario
         ( if ( and ( char=? #\b ( string-ref table pos ) ) ( = 0 count ) )
            ( begin
               ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 100 "Brown" )
               ( Game table turn 0 0 0 0 jaque )
               );Fin begin
            ;De lo contrario
            ( if ( = count 0 )
               ( if ( or ( and ( char=? ( string-ref table ( + 1 pos ) ) #\B ) ( = turn 0 ) ) ( and ( char=? ( string-ref table ( + 1 pos ) ) #\N ) ( = turn 1 ) ) )
                  ( begin
                     ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 100 "Brown" )
                     ( Game table turn 1 x y pos jaque )
                     );Fin begin
                  ;De lo contrario
                  ( begin
                     ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 100 "Brown" )
                     ( ( ( draw-pixmap-posn "JugadaInvalida.png" ) chess ) ( make-posn 720 410 ) )
                     ( Game table turn 0 0 0 0 jaque )
                     );Fin begin
                  );Fin if ( or ( and ... ) ( and ... ) )
               ;De lo contrario
               ( if ( = previousClick pos )
                  ( begin
                     ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 100 "Brown" )
                     ( Game table turn 1 x y pos jaque )
                     );Fin begin
                  ;De lo contrario
                  ( if ( char=? ( string-ref table ( + 1 previousClick ) ) ( string-ref table ( + 1 pos ) ) )
                     ( begin
                        ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 100 "Brown" )
                        ( ( ( draw-pixmap-posn "JugadaInvalida.png" ) chess ) ( make-posn 720 410 ) )
                        ( Game table turn 0 0 0 0 jaque )
                        );Fin begin
                     ;De lo contrario
                     ( if jaque
                        ( if ( and ( ValidPlay? table piece x0 y0 x y previousClick pos )
                             ( not ( Jaque? ( Play table previousClick pos ) 0 ( PosR ( Play table previousClick pos ) ( if ( = turn 1 ) #\N #\B ) 0 0 ) ) ) )
                           ( begin
                              ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 100 "Brown" )
                              ( MovePiece table click previousClick pos )
                              ( ( ( draw-pixmap-posn ( if ( = 1 turn ) "turnoBlancas.png" "turnoNegras.png" ) ) chess ) ( make-posn 810 295 ) )
                              ( Game ( Play table previousClick pos ) ( if ( = turn 1 ) 0 1 ) 0 0 0 0 #f )
                              );Fin begin
                           ;De lo contrario
                           ( begin
                              ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 100 "Brown" )
                              ( ( ( draw-pixmap-posn "InvalidaPorJaque.png" ) chess ) ( make-posn 720 410 ) )
                              ( Game table turn 0 0 0 0 jaque )
                              );Fin begin
                           );Fin if ( and ( ValidPlay? ... ) ( Jaque? ... ) )
                        ;De lo contrario
                        ( if ( and ( ValidPlay? table piece x0 y0 x y previousClick pos )
                                   ( Jaque? ( Play table previousClick pos ) 0 ( PosR ( Play table previousClick pos ) ( if ( = turn 0 ) #\N #\B ) 0 0 ) ) )
                           ( if ( Mate? ( Play table previousClick pos ) ( if ( = turn 1 ) #\B #\N ) 0 ( PosR ( Play table previousClick pos ) ( if ( = turn 0 ) #\N #\B ) 0 0 ) )
                              ( begin
                                 ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 100 "Brown" )
                                 ( MovePiece table click previousClick pos )
                                 ( ( ( draw-pixmap-posn "JaqueMate.png" ) chess ) ( make-posn 720 410 ) )
                                 ( End )
                                 );Fin begin
                              ;De lo contrario
                              ( begin
                                 ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 100 "Brown" )
                                 ( MovePiece table click previousClick pos )
                                 ( ( ( draw-pixmap-posn "Jaque.png" ) chess ) ( make-posn 720 410 ) )
                                 ( ( ( draw-pixmap-posn ( if ( = 1 turn ) "turnoBlancas.png" "turnoNegras.png" ) ) chess ) ( make-posn 810 295 ) )
                                 ( Game ( Play table previousClick pos ) ( if ( = turn 1 ) 0 1 ) 0 0 0 0 #t )
                                 );Fin begin
                              );Fin if ( and ( ValidPlay? ... ) ( Jaque? ... ) )
                           ;De lo contrario
                           ( if ( and ( not ( Jaque? ( Play table previousClick pos ) 0 ( PosR ( Play table previousClick pos ) ( if ( = turn 1 ) #\N #\B ) 0 0 ) ) )
                                   ( ValidPlay? table piece x0 y0 x y previousClick pos ) )
                              ( begin
                                 ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 100 "Brown" )
                                 ( MovePiece table click previousClick pos )
                                 ( ( ( draw-pixmap-posn ( if ( = 1 turn ) "turnoBlancas.png" "turnoNegras.png" ) ) chess ) ( make-posn 810 295 ) )
                                 ( Game ( Play table previousClick pos ) ( if ( = turn 1 ) 0 1 ) 0 0 0 0 jaque )
                                 );Fin begin
                              ;De lo contrario
                              ( begin
                                 ( ( draw-solid-rectangle chess ) ( make-posn 640 400 ) 360 100 "Brown" )
                                 ( ( ( draw-pixmap-posn "JugadaInvalida.png" ) chess ) ( make-posn 720 410 ) )
                                 ( Game table turn 0 0 0 0 jaque )
                                 );Fin begin
                              );Fin if ( and ( not ( Jaque? ... ) ) ( ValidPlay? ) )
                           );Fin if ( and ( ValidPla? ... ) ( Jaque? ... ) )
                        );Fin if jaque
                     );Fin if ( char=? ( string-ref table ( + 1 previousClick ) ) ( string-ref table ( + 1 pos ) ) )
                  );Fin if ( = previousClick pos )
               );Fin if ( = count 0 )
            );Fin if ( and ( char=? #\b ( string-ref table pos ) ) ( = 0 count ) )
         );Fin if ( and ( or ( = y 0 ) ( = y 7 ) ) ( char=? #\P ( string-ref piece 0 ) ) ( not jaque ) ( ValidPlay? ... ) )
      ;De lo contrario
      ( if ( or ( and ( >= ( posn-x ( mouse-click-posn click ) ) 704 ) ( < ( posn-x ( mouse-click-posn click ) ) 934 )
                      ( >= ( posn-y ( mouse-click-posn click ) ) 536 ) ( < ( posn-y ( mouse-click-posn click ) ) 584 ) ) )
         ( close-viewport chess )
         ;De lo contrario
         ( Game table turn count x0 y0 previousClick jaque )
         );Fin if ( or ( and ... ) )
      );Fin if ( < ( * x 80 ) 640 )
   );Fin función Game

( define piezas "TNCNANDNRNANCNTNPNPNPNPNPNPNPNPNbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbPBPBPBPBPBPBPBPBTBCBABDBRBABCBTB" )
;Llama a la función para que imprima las piezas
( Piezas piezas 0 0 0 )
;Inicia el juego
( Game piezas 0 0 0 0 0 #f )