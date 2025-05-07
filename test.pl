% ==================================================
% Script de prueba: ejecución paso a paso de main
% ==================================================

:- consult('main.pl').

run_test :-
	reset_sistema,
    cargar_entrada('entrada.txt'),
    writeln('--- Estado inicial ---'), mostrar_estado,

    % Crear un checkpoint en p1
    check(p1, ID),
    format('Checkpoint creado en p1: ~w~n', [ID]),
	writeln('--- Despues del checkpoint ---'), mostrar_estado,
    
    % Enviar mensaje de p1 a p2
    enviar(p1, p2, hola, mundo),
    writeln('--- Despues de enviar ---'), mostrar_estado,

    % Recibir mensaje en p2
    recibir(p2, hola),
    writeln('--- Despues de recibir ---'), mostrar_estado,

    % Ejecutar rollback en p1
    rollback(p1, ID),
    writeln('--- Despues de rollback ---'), mostrar_estado,
	
	% Crear un nuevo proceso hijo desde p1
    spawn(p1, estado_inicial_hijo, NuevoPid),
    format('--- Proceso ~w creado con spawn desde p1 ---~n', [NuevoPid]),
	writeln('--- Despues del spawn ---'),	mostrar_estado.


reset_sistema :-
    retractall(proceso(_, _, _)),
    retractall(mensaje(_, _, _, _)),
    retractall(pid_contador(_)),
    retractall(checkpoint_id(_)),
    assert(pid_contador(2)),
    assert(checkpoint_id(0)).