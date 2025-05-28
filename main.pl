
% ==============================
% Simulador de Rollback Recovery
% ==============================

:- [parser].
:- [constructor].
:- [eval].
:- [exec].

% Ejecutar: main(['p1.trace', 'p2.trace']).
main(Archivos) :- run(Archivos).
