% Define os predicados dinâmicos para permitir a modificação em tempo de execução.
:- dynamic availability/3.                % Disponibilidade de médicos ou salas.
:- dynamic agenda_staff/3.                % Agenda inicial de médicos.
:- dynamic agenda_staff1/3.               % Agenda temporária de médicos durante o cálculo.
:- dynamic agenda_operation_room/3.       % Agenda inicial da sala de operação.
:- dynamic agenda_operation_room1/3.      % Agenda temporária da sala de operação durante o cálculo.
:- dynamic better_sol/5.                  % Melhor solução encontrada para as cirurgias.

% Define as agendas iniciais dos médicos no formato (ID, Data, Lista de Tarefas).
agenda_staff(d001, 20241028, [(720, 790, m01), (1080, 1140, c01)]). % Médico d001, tarefas em dois intervalos.
agenda_staff(d002, 20241028, [(850, 900, m02), (901, 960, m02), (1380, 1440, c02)]). % Médico d002.
agenda_staff(d003, 20241028, [(720, 790, m01), (910, 980, m02)]).  % Médico d003.

% Define os horários de trabalho disponíveis para cada médico.
timetable(d001, 20241028, (480, 1200)). % Médico d001: 8h00 às 20h00.
timetable(d002, 20241028, (500, 1440)). % Médico d002: 8h20 às 24h00.
timetable(d003, 20241028, (520, 1320)). % Médico d003: 8h40 às 22h00.

% Define os tipos de cirurgias e o tempo necessário para anestesia, cirurgia e limpeza.
surgery(so2, 45, 60, 45). % Cirurgia so2: anestesia 45 min, cirurgia 60 min, limpeza 45 min.
surgery(so3, 45, 90, 45). % Cirurgia so3.
surgery(so4, 45, 75, 45). % Cirurgia so4.

% Associa códigos de cirurgia aos seus tipos.
surgery_id(so100001, so2). % Código so100001 para cirurgia do tipo so2.
surgery_id(so100002, so3). % Código so100002 para cirurgia do tipo so3.

% Define a alocação de médicos para cirurgias específicas.
assignment_surgery(so100001, d001). % Médico d001 está alocado para so100001.
assignment_surgery(so100002, d002). % Médico d002 está alocado para so100002.

% Define a agenda inicial da sala de operação. Nenhuma cirurgia agendada inicialmente.
agenda_operation_room(or1, 20241028, []).

% Função que retorna os intervalos livres para uma agenda ocupada.
free_agenda0([], [(0, 1440)]). % Se a agenda estiver vazia, todo o dia está livre.
free_agenda0([(0, Tfin, _)|LT], LT1) :- !, free_agenda1([(0, Tfin, _)|LT], LT1). % Começa no início do dia.
free_agenda0([(Tin, Tfin, _)|LT], [(0, T1)|LT1]) :- 
    T1 is Tin - 1,                       % Define o início do intervalo livre.
    free_agenda1([(Tin, Tfin, _)|LT], LT1). % Processa o restante.

% Ajusta o final dos intervalos livres.
free_agenda1([( _, Tfin, _)], [(T1, 1440)]) :- 
    Tfin \== 1440, !,                    % Se o intervalo não termina no final do dia.
    T1 is Tfin + 1.                      % Início do próximo intervalo livre.
free_agenda1([( _, _, _)], []).          % Sem mais intervalos ocupados.
free_agenda1([( _, T, _), (T1, Tfin2, _)|LT], LT1) :- 
    Tx is T + 1, T1 == Tx, !,            % Intervalos consecutivos sem tempo livre.
    free_agenda1([(T1, Tfin2, _)|LT], LT1).
free_agenda1([( _, Tfin1, _), (Tin2, Tfin2, _)|LT], [(T1, T2)|LT1]) :-
    T1 is Tfin1 + 1, T2 is Tin2 - 1,     % Define o intervalo livre entre ocupações.
    free_agenda1([(Tin2, Tfin2, _)|LT], LT1).

% Ajusta os intervalos livres conforme os horários de trabalho.
adapt_timetable(D, Date, LFA, LFA2) :-
    timetable(D, Date, (InTime, FinTime)), % Obtém o horário total do médico.
    treatin(InTime, LFA, LFA1),            % Ajusta o início.
    treatfin(FinTime, LFA1, LFA2).         % Ajusta o final.

% Ajusta o início dos intervalos livres ao horário de início do trabalho.
treatin(InTime, [(In, Fin)|LFA], [(In, Fin)|LFA]) :-
    InTime =< In, !.                      % O início do intervalo já está dentro do horário.
treatin(InTime, [(_, Fin)|LFA], LFA1) :-
    InTime > Fin, !,                      % O intervalo está totalmente antes do início do horário.
    treatin(InTime, LFA, LFA1).
treatin(InTime, [(_, Fin)|LFA], [(InTime, Fin)|LFA]). % Ajusta o início.

% Ajusta o final dos intervalos livres ao horário de final do trabalho.
treatfin(FinTime, [(In, Fin)|LFA], [(In, Fin)|LFA1]) :-
    FinTime >= Fin, !,                    % O final do intervalo já está dentro do horário.
    treatfin(FinTime, LFA, LFA1).
treatfin(FinTime, [(In, _)|_], []) :- 
    FinTime =< In, !.                     % O intervalo está totalmente após o final do horário.
treatfin(FinTime, [(In, _)|_], [(In, FinTime)]). % Ajusta o final.

% Obtém a interseção de agendas de médicos para identificar intervalos disponíveis para todos.
intersect_all_agendas([Name], Date, LA) :-
    !, availability(Name, Date, LA).      % Para um único médico, a disponibilidade é direta.
intersect_all_agendas([Name|LNames], Date, LI) :-
    availability(Name, Date, LA),         % Obtém a disponibilidade do primeiro médico.
    intersect_all_agendas(LNames, Date, LI1), % Interseção com os demais.
    intersect_2_agendas(LA, LI1, LI).     % Calcula a interseção.

% Calcula a interseção de dois conjuntos de intervalos.
intersect_2_agendas([], _, []).           % Sem interseção possível se uma das listas estiver vazia.
intersect_2_agendas([D|LD], LA, LIT) :-  
    intersect_availability(D, LA, LI, LA1), % Calcula a interseção do intervalo atual.
    intersect_2_agendas(LD, LA1, LID),      % Processa os demais intervalos.
    append(LI, LID, LIT).                   % Junta os intervalos interseccionados.

% Calcula a interseção de dois intervalos.
intersect_availability((_, _), [], [], []). % Sem interseção se a segunda lista estiver vazia.
intersect_availability((_, Fim), [(Ini1, Fim1)|LD], [], [(Ini1, Fim1)|LD]) :- 
    Fim < Ini1, !.                         % Intervalo sem sobreposição.
intersect_availability((Ini, Fim), [(_, Fim1)|LD], LI, LA) :- 
    Ini > Fim1, !,                         % Intervalo sem sobreposição.
    intersect_availability((Ini, Fim), LD, LI, LA).
intersect_availability((Ini, Fim), [(Ini1, Fim1)|LD], [(Imax, Fmin)], [(Fim, Fim1)|LD]) :-
    Fim1 > Fim, !,                         % Sobreposição parcial.
    min_max(Ini, Ini1, _, Imax),           % Ajusta o início.
    min_max(Fim, Fim1, Fmin, _).           % Ajusta o final.

% Função auxiliar para calcular mínimo e máximo.
min_max(I, I1, I, I1) :- I < I1, !.
min_max(I, I1, I1, I).


% Calcula o tempo total necessário para uma cirurgia, somando anestesia, cirurgia e limpeza.
sumTemp(OperationType, TResult) :-
    surgery(OperationType, TAnas, TSur, TClean), % Obtém os tempos de anestesia, cirurgia e limpeza do tipo de cirurgia.
    TResult is TAnas + TSur + TClean.            % Calcula o tempo total necessário.

% Obtém os médicos disponíveis para realizar uma cirurgia específica.
% Este código não foi testado para integração, mas teoricamente funcional.
get_staff_for_surgeries(OpCode, Specialization, LStaff) :-
    surgery_id(OpCode, OpType),                  % Determina o tipo de cirurgia a partir do código.
    findall(StaffId,                             % Encontra todos os IDs de médicos:
        (staff(StaffId, _, Specialization, Specializations), % Verifica a especialidade do médico.
         member(OpType, Specializations)),       % Garante que o médico está apto para o tipo de cirurgia.
        LStaff).                                 % Retorna a lista de médicos encontrados.

% Agenda todas as cirurgias em uma sala de operações para um dia específico.
schedule_all_surgeries(Room, Day) :-
    retractall(agenda_staff1(_, _, _)),          % Remove agendas temporárias de médicos.
    retractall(agenda_operation_room1(_, _, _)), % Remove agendas temporárias de salas.
    retractall(availability(_, _, _)),           % Remove dados de disponibilidade temporária.
    findall(_,                                   % Atualiza as agendas temporárias de médicos com base nas agendas iniciais.
        (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))),
        _),
    agenda_operation_room(Room, Day, Agenda),    % Obtém a agenda inicial da sala de operação.
    assertz(agenda_operation_room1(Room, Day, Agenda)), % Armazena temporariamente a agenda da sala.
    findall(_,                                   % Calcula a disponibilidade ajustada de cada médico.
        (agenda_staff1(D, Day, L),
         free_agenda0(L, LFA),                   % Obtém intervalos livres na agenda.
         adapt_timetable(D, Day, LFA, LFA2),     % Ajusta os intervalos com base no horário de trabalho.
         assertz(availability(D, Day, LFA2))),  % Armazena a disponibilidade ajustada.
        _),
    findall(OpCode, surgery_id(OpCode, _), LOpCode), % Lista todos os códigos de cirurgia.
    availability_all_surgeries(LOpCode, Room, Day),  % Planeja todas as cirurgias.
    !.

% Planeja cada cirurgia sequencialmente.
availability_all_surgeries([], _, _).              % Se não houver cirurgias, termina.
availability_all_surgeries([OpCode|LOpCode], Room, Day) :-
    surgery_id(OpCode, OpType),                    % Obtém o tipo de cirurgia pelo código.
    surgery(OpType, _, TSurgery, _),               % Obtém o tempo necessário para a cirurgia.
    sumTemp(OpType, TResult),                      % Calcula o tempo total da cirurgia.
    availability_operation(OpCode, Room, Day, LPossibilities, LDoctors), % Determina os intervalos possíveis.
    schedule_first_interval(TResult, LPossibilities, (TinS, TfinS)), % Escolhe o primeiro intervalo adequado.
    retract(agenda_operation_room1(Room, Day, Agenda)), % Remove a agenda temporária da sala.
    insert_agenda((TinS, TfinS, OpCode), Agenda, Agenda1), % Insere a cirurgia na agenda da sala.
    assertz(agenda_operation_room1(Room, Day, Agenda1)), % Atualiza a agenda da sala.
    insert_agenda_doctors((TinS, TfinS, OpCode), Day, LDoctors), % Atualiza a agenda dos médicos.
    availability_all_surgeries(LOpCode, Room, Day). % Planeja as cirurgias restantes.

% Determina os intervalos de tempo possíveis para realizar uma cirurgia.
availability_operation(OpCode, Room, Day, LPossibilities, LDoctors) :-
    surgery_id(OpCode, OpType),                    % Obtém o tipo de cirurgia pelo código.
    findall(Doctor, assignment_surgery(OpCode, Doctor), LDoctors), % Lista os médicos alocados à cirurgia.
    intersect_all_agendas(LDoctors, Day, LA),     % Obtém a interseção das agendas dos médicos.
    agenda_operation_room1(Room, Day, LAgenda),   % Obtém a agenda temporária da sala.
    free_agenda0(LAgenda, LFAgRoom),              % Determina os intervalos livres na agenda da sala.
    intersect_2_agendas(LA, LFAgRoom, LIntAgDoctorsRoom), % Calcula a interseção com a disponibilidade da sala.
    sumTemp(OpType, TResult),                     % Calcula o tempo total necessário para a cirurgia.
    remove_unf_intervals(TResult, LIntAgDoctorsRoom, LPossibilities). % Remove intervalos insuficientes.

% Remove intervalos que não são suficientemente longos para realizar a cirurgia.
remove_unf_intervals(_, [], []).                  % Nenhum intervalo restante, termina.
remove_unf_intervals(TSurgery, [(Tin, Tfin)|LA], [(Tin, Tfin)|LA1]) :-
    DT is Tfin - Tin + 1,                         % Calcula a duração do intervalo.
    TSurgery =< DT, !,                            % Mantém o intervalo se for suficientemente longo.
    remove_unf_intervals(TSurgery, LA, LA1).      % Processa os intervalos restantes.
remove_unf_intervals(TSurgery, [_|LA], LA1) :-
    remove_unf_intervals(TSurgery, LA, LA1).      % Descarta o intervalo e processa os restantes.

% Seleciona o primeiro intervalo disponível.
schedule_first_interval(TSurgery, [(Tin, _)|_], (Tin, TfinS)) :-
    TfinS is Tin + TSurgery - 1.                  % Calcula o tempo final do intervalo.

% Insere uma cirurgia na agenda.
insert_agenda((TinS, TfinS, OpCode), [], [(TinS, TfinS, OpCode)]). % Insere em uma agenda vazia.
insert_agenda((TinS, TfinS, OpCode), [(Tin, Tfin, OpCode1)|LA], [(TinS, TfinS, OpCode), (Tin, Tfin, OpCode1)|LA]) :-
    TfinS < Tin, !.                               % Insere antes de um intervalo existente.
insert_agenda((TinS, TfinS, OpCode), [(Tin, Tfin, OpCode1)|LA], [(Tin, Tfin, OpCode1)|LA1]) :-
    insert_agenda((TinS, TfinS, OpCode), LA, LA1).% Insere em uma posição posterior.

% Atualiza a agenda dos médicos com a nova cirurgia.
insert_agenda_doctors(_, _, []).                  % Nenhum médico restante, termina.
insert_agenda_doctors((TinS, TfinS, OpCode), Day, [Doctor|LDoctors]) :-
    retract(agenda_staff1(Doctor, Day, Agenda)),  % Remove a agenda temporária do médico.
    insert_agenda((TinS, TfinS, OpCode), Agenda, Agenda1), % Insere a cirurgia na agenda do médico.
    assertz(agenda_staff1(Doctor, Day, Agenda1)), % Atualiza a agenda temporária do médico.
    insert_agenda_doctors((TinS, TfinS, OpCode), Day, LDoctors). % Processa os demais médicos.

% Encontra a melhor solução para o planejamento de cirurgias.
obtain_better_sol(Room, Day, AgOpRoomBetter, LAgDoctorsBetter, TFinOp) :-
    get_time(Ti),                                 % Registra o tempo inicial.
    (obtain_better_sol1(Room, Day); true),        % Busca soluções possíveis.
    retract(better_sol(Day, Room, AgOpRoomBetter, LAgDoctorsBetter, TFinOp)), % Obtém a melhor solução.
    write('Final Result: AgOpRoomBetter='), write(AgOpRoomBetter), nl,
    write('LAgDoctorsBetter='), write(LAgDoctorsBetter), nl,
    write('TFinOp='), write(TFinOp), nl,
    get_time(Tf),                                 % Registra o tempo final.
    T is Tf - Ti,                                 % Calcula o tempo de execução.
    write('Tempo de geracao da solucao:'), write(T), nl.

% Busca todas as permutações possíveis e calcula as soluções.
obtain_better_sol1(Room, Day) :-
    asserta(better_sol(Day, Room, _, _, 1441)),   % Inicializa a melhor solução com o pior tempo possível.
    findall(OpCode, surgery_id(OpCode, _), LOC), % Lista todos os códigos de cirurgia.
    permutation(LOC, LOpCode),                   % Gera todas as permutações dos códigos.
    retractall(agenda_staff1(_, _, _)),          % Reseta os dados temporários.
    retractall(agenda_operation_room1(_, _, _)),
    retractall(availability(_, _, _)),
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))), _), % Inicializa os dados temporários.
    agenda_operation_room(Room, Day, Agenda),

%-------------------------------------------------------------------------------------------------------------%

% Heuristic 1: Earliest-Available-Time (EAT)
% Esta heurística agenda as cirurgias de forma a priorizar aquelas que podem começar mais cedo.
schedule_eat(Room, Day) :-
    % Remove todas as agendas temporárias para garantir que começamos do zero.
    retractall(agenda_staff1(_, _, _)),
    retractall(agenda_operation_room1(_, _, _)),
    retractall(availability(_, _, _)),

    % Recupera todas as agendas de profissionais para o dia e as armazena como fatos temporários.
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))), _),

    % Recupera a agenda da sala de operação e a armazena como fato temporário.
    agenda_operation_room(Room, Day, AgendaRoom), assertz(agenda_operation_room1(Room, Day, AgendaRoom)),

    % Gera os intervalos livres para todos os profissionais no dia especificado e os armazena como disponibilidade.
    findall(_, (agenda_staff1(D, Day, L), free_agenda0(L, LFA), adapt_timetable(D, Day, LFA, LFA2), assertz(availability(D, Day, LFA2))), _),

    % Recupera todos os códigos de cirurgias disponíveis.
    findall(OpCode, surgery_id(OpCode, _), LOpCode),

    % Agenda as cirurgias sequencialmente com base na estratégia EAT.
    schedule_eat_surgeries(LOpCode, Room, Day).

% Caso base para parar o agendamento quando não há mais cirurgias a serem processadas.
schedule_eat_surgeries([], _, _).

% Regra recursiva que agenda uma cirurgia e continua com o restante.
schedule_eat_surgeries([OpCode | LOpCode], Room, Day) :-
    % Recupera o tipo de cirurgia associado ao código.
    surgery_id(OpCode, OpType),

    % Calcula o tempo total necessário para a cirurgia (anestesia, cirurgia e limpeza).
    sumTemp(OpType, TResult),

    % Determina os intervalos possíveis para realizar a cirurgia.
    availability_operation(OpCode, Room, Day, LPossibilities, LDoctors),

    % Escolhe o primeiro intervalo disponível para a cirurgia.
    schedule_first_interval(TResult, LPossibilities, (TinS, TfinS)),

    % Atualiza a agenda da sala de operação com o intervalo escolhido.
    retract(agenda_operation_room1(Room, Day, Agenda)),
    insert_agenda((TinS, TfinS, OpCode), Agenda, Agenda1),
    assertz(agenda_operation_room1(Room, Day, Agenda1)),

    % Atualiza as agendas dos médicos associados à cirurgia.
    insert_agenda_doctors((TinS, TfinS, OpCode), Day, LDoctors),

    % Chama a função recursivamente para processar as cirurgias restantes.
    schedule_eat_surgeries(LOpCode, Room, Day).

%-------------------------------------------------------------------------------------------------------------%
% Heuristic 2: Greedy-Minimum-Idle-Time (GMIT)
% Esta heurística agenda as cirurgias para minimizar o tempo ocioso entre elas.
schedule_gmit(Room, Day) :-
    % Remove todas as agendas temporárias para garantir que começamos do zero.
    retractall(agenda_staff1(_, _, _)),
    retractall(agenda_operation_room1(_, _, _)),
    retractall(availability(_, _, _)),

    % Recupera todas as agendas de profissionais para o dia e as armazena como fatos temporários.
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))), _),

    % Recupera a agenda da sala de operação e a armazena como fato temporário.
    agenda_operation_room(Room, Day, AgendaRoom), assertz(agenda_operation_room1(Room, Day, AgendaRoom)),

    % Gera os intervalos livres para todos os profissionais no dia especificado e os armazena como disponibilidade.
    findall(_, (agenda_staff1(D, Day, L), free_agenda0(L, LFA), adapt_timetable(D, Day, LFA, LFA2), assertz(availability(D, Day, LFA2))), _),

    % Recupera todos os códigos de cirurgias disponíveis.
    findall(OpCode, surgery_id(OpCode, _), LOpCode),

    % Ordena as cirurgias por duração (do maior para o menor).
    prioritize_surgeries(LOpCode, PrioritizedLOpCode),

    % Agenda as cirurgias sequencialmente com base na estratégia GMIT.
    schedule_gmit_surgeries(PrioritizedLOpCode, Room, Day).

% Ordena as cirurgias por duração (do maior para o menor).
prioritize_surgeries(LOpCode, PrioritizedLOpCode) :-
    % Associa cada código de cirurgia com seu tempo total necessário.
    findall((OpCode, TResult), (member(OpCode, LOpCode), surgery_id(OpCode, OpType), sumTemp(OpType, TResult)), LDurations),

    % Ordena os pares (código, duração) pela duração em ordem decrescente.
    sort(2, @>=, LDurations, SortedLDurations),

    % Extrai os códigos ordenados em uma lista.
    findall(OpCode, member((OpCode, _), SortedLDurations), PrioritizedLOpCode).

% Caso base para parar o agendamento quando não há mais cirurgias a serem processadas.
schedule_gmit_surgeries([], _, _).

% Regra recursiva que agenda uma cirurgia e continua com o restante.
schedule_gmit_surgeries([OpCode | LOpCode], Room, Day) :-
    % Recupera o tipo de cirurgia associado ao código.
    surgery_id(OpCode, OpType),

    % Calcula o tempo total necessário para a cirurgia (anestesia, cirurgia e limpeza).
    sumTemp(OpType, TResult),

    % Determina os intervalos possíveis para realizar a cirurgia.
    availability_operation(OpCode, Room, Day, LPossibilities, LDoctors),

    % Seleciona o intervalo ótimo que minimiza o tempo ocioso.
    find_optimal_slot(LPossibilities, (TinS, TfinS)),

    % Atualiza a agenda da sala de operação com o intervalo escolhido.
    retract(agenda_operation_room1(Room, Day, Agenda)),
    insert_agenda((TinS, TfinS, OpCode), Agenda, Agenda1),
    assertz(agenda_operation_room1(Room, Day, Agenda1)),

    % Atualiza as agendas dos médicos associados à cirurgia.
    insert_agenda_doctors((TinS, TfinS, OpCode), Day, LDoctors),

    % Chama a função recursivamente para processar as cirurgias restantes.
    schedule_gmit_surgeries(LOpCode, Room, Day).

% Seleciona o intervalo que minimiza o tempo ocioso.
find_optimal_slot([Slot], Slot). % Caso base: um único intervalo disponível.
find_optimal_slot([(Tin1, Tfin1), (Tin2, Tfin2) | Slots], BestSlot) :-
    % Calcula o tempo ocioso gerado por cada intervalo.
    Idle1 is Tin1 - Tfin1, Idle2 is Tin2 - Tfin2,

    % Compara os intervalos e escolhe aquele com menor tempo ocioso.
    (Idle1 =< Idle2 -> find_optimal_slot([(Tin1, Tfin1) | Slots], BestSlot) ; find_optimal_slot([(Tin2, Tfin2) | Slots], BestSlot)).

    
    

