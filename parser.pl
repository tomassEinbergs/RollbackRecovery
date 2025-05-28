
% ===========================
% Parser para leer las trazas
% ===========================

:- module(parser, [leer_traza/2]).

leer_traza(Fichero, Acciones) :-
    open(Fichero, read, Stream),
    leer_acciones(Stream, Acciones),
    close(Stream).

leer_acciones(Stream, Acciones) :-
    read(Stream, Term),
    ( Term == end_of_file ->
        Acciones = []
    ; convertir_accion(Term, Accion) ->
        leer_acciones(Stream, Resto),
        Acciones = [Accion | Resto]
    ; leer_acciones(Stream, Acciones)  % ignora líneas inválidas
    ).

convertir_accion({send, Dest, Tag}, send(Dest, Tag)).
convertir_accion({receive, Tag}, receive(Tag)).
convertir_accion({check, ID}, check(ID)).
convertir_accion({commit, ID}, commit(ID)).
convertir_accion({rollback, ID}, rollback(ID)).
