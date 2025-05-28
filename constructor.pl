
% ==================================
% Constructor para el estado inicial
% ==================================

:- module(constructor, [estado_desde_trazas/2]).
:- use_module(parser).

estado_desde_trazas(Ficheros, estado(Procesos, [])) :-
    construir_procesos(Ficheros, Procesos).

% Por cada fichero, construye un término proceso(PID, Estado, Historial, Cola)
construir_procesos([], []). 
construir_procesos([Fichero|RestoArchivos], [proceso(PID, s0, [], Acciones)|RestoProcesos]) :-
    archivo_a_pid(Fichero, PID),
    parser:leer_traza(Fichero, Acciones),
    construir_procesos(RestoArchivos, RestoProcesos).

archivo_a_pid(Fichero, PID) :-
    file_base_name(Fichero, Base),
    file_name_extension(Nombre, _, Base),
    atom_string(PID, Nombre).
