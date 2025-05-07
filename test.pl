% ==================================================
% Script de prueba: ejecución paso a paso de main
% ==================================================

:- consult('main.pl').

run_test :-
    reset_sistema,
    cargar_entrada('entrada.txt'),
    writeln('\n=== ESTADO INICIAL DEL SISTEMA ==='),
    mostrar_estado,

    % 1. Crea un checkpoint en p1
    check(p1, ID),
    format('\n-> Checkpoint creado en p1 con ID: ~w~n', [ID]),
    writeln('Estado tras crear el checkpoint en p1:'),
    mostrar_estado,

    % 2. p1 envía un mensaje a p2
    enviar(p1, p2, hola, mundo),
    writeln('\n-> Mensaje {hola, mundo} enviado de p1 a p2'),
    writeln('Estado tras el envio:'),
    mostrar_estado,

    % 3. p2 recibe el mensaje
    recibir(p2, hola),
    writeln('\n-> p2 ha recibido el mensaje {hola, mundo}'),
    writeln('Estado tras la recepcion:'),
    mostrar_estado,

    % 4. Ejecuta rollback en p1
    rollback(p1, ID),
    format('\n-> Rollback ejecutado en p1 al checkpoint ~w~n', [ID]),
    writeln('Estado tras el rollback de p1:'),
    mostrar_estado,

    % 5. Crea un nuevo proceso desde p1 con spawn
    spawn(p1, estado_inicial_hijo, NuevoPid),
    format('\n-> Proceso ~w creado mediante spawn desde p1~n', [NuevoPid]),
    writeln('Estado tras crear el nuevo proceso:'),
    mostrar_estado.

% --------------------------------------
% Limpieza del sistema entre ejecuciones
% --------------------------------------

reset_sistema :-
    retractall(proceso(_, _, _)),
    retractall(mensaje(_, _, _, _)),
    retractall(pid_contador(_)),
    retractall(checkpoint_id(_)),
    assert(pid_contador(2)),
    assert(checkpoint_id(0)).
