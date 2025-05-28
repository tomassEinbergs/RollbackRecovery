
% ===================
% Reglas de ejecución
% ===================

:- module(eval, [eval/2, eval_any/2]).

% Envío de mensaje: send(Dest, Tag)
eval(
  estado(
    [proceso(P, Estado, Hist, [send(Dest, Tag)|RestoEv]) | Otros],
    Mensajes
  ),
  estado(
    [proceso(P, Estado, [send(Dest, Tag)|Hist], RestoEv) | Otros],
    [mensaje(P, Dest, Tag) | Mensajes]
  )
).

% Recepción de mensaje: receive(Tag)
eval(
  estado(
    [proceso(P, _, Hist, [receive(Tag)|RestoEv]) | Otros],
    Mensajes
  ),
  estado(
    [proceso(P, Tag, [receive(Tag)|Hist], RestoEv) | Otros],
    NuevosMensajes
  )
) :-
  select(mensaje(_, P, Tag), Mensajes, NuevosMensajes).

% Checkpoint: check(ID)
eval(
  estado(
    [proceso(P, Estado, Hist, [check(ID)|RestoEv]) | Otros],
    Mensajes
  ),
  estado(
    [proceso(P, Estado, [check(ID, Estado)|Hist], RestoEv) | Otros],
    Mensajes
  )
).

% Commit: commit(ID)
eval(
  estado(
    [proceso(P, Estado, Hist, [commit(ID)|RestoEv]) | Otros],
    Mensajes
  ),
  estado(
    [proceso(P, Estado, [commit(ID)|Hist], RestoEv) | Otros],
    Mensajes
  )
).

% Rollback: rollback(ID)
eval(
  estado(
    [proceso(P, Estado, Hist, [rollback(ID)|RestoEv]) | Otros],
    Mensajes
  ),
  estado(
    [proceso(P, Estado, [rollback(ID)|Hist], RestoEv) | Otros],
    Mensajes
  )
).

% ===========
% Round Robin
% ===========

eval_any(estado([], _), _) :- !, fail.

eval_any(estado([P | Ps], Ms), estado(ProcesosRes, MsRes)) :-
  ( eval(estado([P], Ms), estado([P2], Ms1)) ->
      append(Ps, [P2], ProcesosRes),
      MsRes = Ms1
  ; eval_any(estado(Ps, Ms), estado(Ps2, MsRes)),
    append(Ps2, [P], ProcesosRes)
  ).
