
% =============================
% Ejecución de las evaluaciones
% =============================

:- module(exec, [exec/1, run/1]).
:- use_module(eval, [eval_any/2]).
:- use_module(constructor, [estado_desde_trazas/2]).

% Estado inicial
run(Archivos) :-
    estado_desde_trazas(Archivos, Estado),
    ansi_format([bold], '--- Estado inicial ---~n', []),
    mostrar_estado(Estado),
    exec(Estado).

% Bucle de ejecución
exec(S) :-
    ansi_format([bold], '--- Estado actual ---~n', []),
    mostrar_estado(S),
    eval_any(S, S2),
    !,
    exec(S2).

exec(S) :-
    ansi_format([bold], '--- FIN ---~n', []),
    mostrar_estado(S).

% Mostrar el estado del sistema
mostrar_estado(estado(Procesos, Mensajes)) :-
    sort_procesos(Procesos, ProcesosOrdenados),
    ansi_format([bold], '--- Procesos ---~n', []),
    forall(member(proceso(P, E, Hist, Cola), ProcesosOrdenados), (
        format('~w: estado=~w~n', [P, E]),
        format('  Historial: ~w~n', [Hist]),
        format('  Cola:      ~w~n', [Cola])
    )),
    ansi_format([bold], '--- Mensajes en transito ---~n', []),
    forall(member(mensaje(Origen, Dest, Tag), Mensajes), (
        format('~w -> ~w :: ~w~n', [Origen, Dest, Tag])
    )),
    nl.

% Ordenar procesos por su PID
sort_procesos(Procesos, Ordenados) :-
    map_list_to_pairs(proceso_id, Procesos, Pares),
    keysort(Pares, ParesOrd),
    pairs_values(ParesOrd, Ordenados).

proceso_id(proceso(Pid, _, _, _), Pid).