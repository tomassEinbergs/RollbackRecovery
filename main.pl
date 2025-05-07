% ==============================
% Simulador de Rollback Recovery
% ==============================

:- dynamic proceso/3.
:- dynamic mensaje/4.
:- dynamic pid_contador/1.
:- dynamic checkpoint_id/1.

pid_contador(2).
checkpoint_id(0).

% Genera un identificador único para un nuevo proceso.
% Usa y actualiza el contador global dinámico 'pid_contador/1'.
nuevo_pid(Pid) :-
    retract(pid_contador(N)),
    N1 is N + 1,
    assert(pid_contador(N1)),
    atom_concat(p, N1, Pid).

% Genera un identificador único para un nuevo checkpoint.
% Usa y actualiza el contador global dinámico 'checkpoint_id/1'.
nuevo_checkpoint_id(ID) :-
    retract(checkpoint_id(N)),
    N1 is N + 1,
    assert(checkpoint_id(N1)),
    atom_concat(tau, N1, ID).

cargar_entrada(F) :- open(F, read, S), leer_lineas(S), close(S).
leer_lineas(S) :- read_term(S, T, []), (T == end_of_file -> true ; assert(T), leer_lineas(S)).

% Crea un nuevo checkpoint y lo guarda en el historial del proceso
check(Pid, ID) :-
    nuevo_checkpoint_id(ID),
    proceso(Pid, Estado, Hist),
    NuevoHist = [check(ID, Estado)|Hist],
    actualizar_proceso(Pid, Estado, NuevoHist).

% Elimina el checkpoint indicado del historial del proceso
commit(Pid, ID) :-
    proceso(Pid, Estado, Hist),
    eliminar_checkpoint(Hist, ID, NuevoHist),
    actualizar_proceso(Pid, Estado, NuevoHist).

% Restaura el estado del proceso al guardado en el checkpoint y reinicia el historial
rollback(Pid, ID) :-
    proceso(Pid, _, Hist),
    buscar_checkpoint(Hist, ID, EstadoRestaurado),
    NuevoHist = [check(ID, EstadoRestaurado)],
    actualizar_proceso(Pid, EstadoRestaurado, NuevoHist).

% Envía un mensaje de un proceso a otro, registrando el envío en el historial y adjuntando los checkpoints activos
enviar(PidOrigen, PidDestino, Etiqueta, Valor) :-
    proceso(PidOrigen, Estado, Hist),
    chks(Hist, Chks),
    assert(mensaje(Chks, PidOrigen, PidDestino, {Etiqueta, Valor})),
    NuevoHist = [send(PidDestino, Etiqueta)|Hist],
    actualizar_proceso(PidOrigen, Estado, NuevoHist).

% El proceso receptor recibe el mensaje con la etiqueta determinada y actualiza su estado e historial
recibir(Pid, EtiquetaEsperada) :-
    mensaje(Chks, Origen, Pid, {EtiquetaEsperada, Valor}),
    retract(mensaje(Chks, Origen, Pid, {EtiquetaEsperada, Valor})),
    proceso(Pid, _, Hist),
    NuevoHist = [rec(Chks, Origen, Pid, {EtiquetaEsperada, Valor})|Hist],
    actualizar_proceso(Pid, Valor, NuevoHist).

% Crea un nuevo proceso que hereda los checkpoints activos del proceso padre como forzados
spawn(PidPadre, EstadoInicial, NuevoPid) :-
    nuevo_pid(NuevoPid),
    proceso(PidPadre, EstadoPadre, HistPadre),
    chks(HistPadre, ChksPadre),
    NuevoHistPadre = [spawn(NuevoPid)|HistPadre],
    actualizar_proceso(PidPadre, EstadoPadre, NuevoHistPadre),
    forzar_checkpoints(ChksPadre, ListaChks),
    actualizar_proceso(NuevoPid, EstadoInicial, ListaChks).

% ====================
% Predicados auxiliares
% ====================

% Actualiza la entrada del proceso por una nueva versión
actualizar_proceso(Pid, Estado, Historia) :-
    retractall(proceso(Pid, _, _)),
    assert(proceso(Pid, Estado, Historia)).

% Convierte una lista de IDs de checkpoints en entradas forzadas en el historial de un nuevo proceso
forzar_checkpoints([], []).
forzar_checkpoints([ID|R], [check(ID, null)|RR]) :- forzar_checkpoints(R, RR).

% Devuelve la lista de identificadores de checkpoints activos en el historial
chks([], []).
chks([check(ID,_)|T], [ID|R]) :- chks(T, R).
chks([_|T], R) :- chks(T, R).

% Auxiliar de commit: elimina la entrada check() del historial
eliminar_checkpoint([], _, []).
eliminar_checkpoint([check(ID,_)|T], ID, T) :- !.
eliminar_checkpoint([H|T], ID, [H|R]) :- eliminar_checkpoint(T, ID, R).
	
% Busca en el historial el checkpoint con identificador ID y devuelve el estado asociado
buscar_checkpoint([check(ID, Estado)|_], ID, Estado) :- !.
buscar_checkpoint([_|T], ID, Estado) :- buscar_checkpoint(T, ID, Estado).

% Muestra por pantalla todos los procesos y mensajes existentes
mostrar_estado :-
    writeln('--- Procesos ---'),
    forall(proceso(P, E, H), (write(P), write(': '), write(E), write(', Hist='), writeln(H))),
    writeln('--- Mensajes ---'),
    forall(mensaje(C, De, Para, M), (write(C), write(': '), write(De), write(' -> '), write(Para), write(' :: '), writeln(M))).
