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


% Calcula o tempo total necessário para uma cirurgia, somando os tempos de anestesia, cirurgia e limpeza.
sumTemp(OperationType, TResult):-  
    surgery(OperationType, TAnas, TSur, TClean),  % Obtém os tempos de anestesia, cirurgia e limpeza.
    TResult is TAnas + TSur + TClean.            % Calcula o tempo total necessário.

% Função auxiliar para obter os médicos para uma cirurgia, filtrando por especialidade.
get_staff_for_surgeries(OpCode, Specializatity, LStaff) :-
    surgery_id(OpCode, OpType),                  % Obtém o tipo de cirurgia a partir do código.
    findall(StaffId, (staff(StaffId, _, Specializatity, Specializations), member(OpType, Specializations)), LStaff).  
    % Encontra todos os médicos (StaffId) que possuem a especialização necessária para a cirurgia (OpType).

% Função principal que organiza todas as cirurgias de um dia específico para uma sala.
schedule_all_surgeries(Room, Day):-  
    retractall(agenda_staff1(_, _, _)),           % Limpa todas as agendas temporárias de médicos.
    retractall(agenda_operation_room1(_, _, _)),  % Limpa todas as agendas temporárias da sala de operação.
    retractall(availability(_, _, _)),             % Limpa todas as disponibilidades temporárias de médicos.
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))), _),  % Copia as agendas dos médicos para a agenda temporária.
    agenda_operation_room(Or, Date, Agenda), assertz(agenda_operation_room1(Or, Date, Agenda)),  % Copia a agenda da sala de operação.
    findall(_, (agenda_staff1(D, Date, L), free_agenda0(L, LFA), adapt_timetable(D, Date, LFA, LFA2), assertz(availability(D, Date, LFA2))), _),  % Atualiza a disponibilidade dos médicos.
    findall(OpCode, surgery_id(OpCode, _), LOpCode),  % Obtém todos os códigos de cirurgia.
    availability_all_surgeries(LOpCode, Room, Day), !.  % Planeja todas as cirurgias para o dia na sala de operação.

% Função recursiva para agendar todas as cirurgias, uma por vez.
availability_all_surgeries([], _, _).  % Caso base: se não houver mais cirurgias, termina a recursão.
availability_all_surgeries([OpCode | LOpCode], Room, Day):- 
    surgery_id(OpCode, OpType),           % Obtém o tipo da cirurgia.
    surgery(OpType, _, TSurgery, _),      % Obtém o tempo necessário para a cirurgia.
    sumTemp(OpType, TResult),              % Calcula o tempo total da cirurgia.
    availability_operation(OpCode, Room, Day, LPossibilities, LDoctors),  % Obtém os intervalos de tempo possíveis para a cirurgia.
    schedule_first_interval(TResult, LPossibilities, (TinS, TfinS)),  % Obtém o primeiro intervalo disponível.
    retract(agenda_operation_room1(Room, Day, Agenda)),  % Remove a agenda anterior da sala.
    insert_agenda((TinS, TfinS, OpCode), Agenda, Agenda1),  % Insere a cirurgia na agenda da sala.
    assertz(agenda_operation_room1(Room, Day, Agenda1)),  % Atualiza a agenda da sala.
    insert_agenda_doctors((TinS, TfinS, OpCode), Day, LDoctors),  % Insere a cirurgia nas agendas dos médicos.
    availability_all_surgeries(LOpCode, Room, Day).  % Continua agendando as outras cirurgias.

% Função que calcula os intervalos de tempo possíveis para uma cirurgia, considerando a disponibilidade dos médicos e da sala.
availability_operation(OpCode, Room, Day, LPossibilities, LDoctors) :-
    surgery_id(OpCode, OpType),                  % Obtém o tipo da cirurgia.
    findall(Doctor, assignment_surgery(OpCode, Doctor), LDoctors),  % Obtém os médicos alocados para a cirurgia.
    intersect_all_agendas(LDoctors, Day, LA),     % Obtém a interseção das agendas dos médicos.
    agenda_operation_room1(Room, Day, LAgenda),   % Obtém a agenda da sala de operação.
    free_agenda0(LAgenda, LFAgRoom),               % Obtém os intervalos livres na agenda da sala.
    intersect_2_agendas(LA, LFAgRoom, LIntAgDoctorsRoom),  % Calcula a interseção entre os intervalos dos médicos e da sala.
    sumTemp(OpType, TResult),                      % Calcula o tempo total necessário para a cirurgia.
    remove_unf_intervals(TResult, LIntAgDoctorsRoom, LPossibilities).  % Filtra os intervalos possíveis.

% Filtra os intervalos onde o tempo necessário para a cirurgia não cabe.
remove_unf_intervals(_, [], []).  % Caso base: sem intervalos disponíveis.
remove_unf_intervals(TSurgery, [(Tin, Tfin) | LA], [(Tin, Tfin) | LA1]) :-
    DT is Tfin - Tin + 1, TSurgery =< DT, !,  % Verifica se o tempo da cirurgia cabe no intervalo.
    remove_unf_intervals(TSurgery, LA, LA1).   % Continua a filtrar os próximos intervalos.
remove_unf_intervals(TSurgery, [_ | LA], LA1) :- 
    remove_unf_intervals(TSurgery, LA, LA1).   % Se o intervalo não couber, continua a filtrar os próximos.

% Função que encontra o primeiro intervalo disponível para agendar a cirurgia.
schedule_first_interval(TSurgery, [(Tin, _) | _], (Tin, TfinS)) :-
    TfinS is Tin + TSurgery - 1.  % Calcula o final do intervalo com base na duração da cirurgia.

% Função para inserir uma cirurgia na agenda de uma sala de operação.
insert_agenda((TinS, TfinS, OpCode), [], [(TinS, TfinS, OpCode)]).  % Se a agenda estiver vazia, insere o intervalo.
insert_agenda((TinS, TfinS, OpCode), [(Tin, Tfin, OpCode1) | LA], [(TinS, TfinS, OpCode), (Tin, Tfin, OpCode1) | LA]) :-
    TfinS < Tin, !.  % Se o intervalo da cirurgia é antes de outro, insere antes.
insert_agenda((TinS, TfinS, OpCode), [(Tin, Tfin, OpCode1) | LA], [(Tin, Tfin, OpCode1) | LA1]) :-
    insert_agenda((TinS, TfinS, OpCode), LA, LA1).  % Se não, continua buscando a posição.

% Função para inserir a cirurgia na agenda dos médicos.
insert_agenda_doctors(_, _, []).  % Caso base: se não houver mais médicos, termina.
insert_agenda_doctors((TinS, TfinS, OpCode), Day, [Doctor | LDoctors]) :-
    retract(agenda_staff1(Doctor, Day, Agenda)),  % Obtém a agenda do médico.
    insert_agenda((TinS, TfinS, OpCode), Agenda, Agenda1),  % Insere a cirurgia na agenda do médico.
    assert(agenda_staff1(Doctor, Day, Agenda1)),  % Atualiza a agenda do médico.
    insert_agenda_doctors((TinS, TfinS, OpCode), Day, LDoctors).  % Continua com os próximos médicos.

% Função principal para encontrar a melhor solução de agendamento de cirurgias.
obtain_better_sol(Room, Day, AgOpRoomBetter, LAgDoctorsBetter, TFinOp) :-  
    get_time(Ti),  % Obtém o tempo inicial.
    (obtain_better_sol1(Room, Day); true),  % Calcula a melhor solução através das permutações de cirurgias.
    retract(better_sol(Day, Room, AgOpRoomBetter, LAgDoctorsBetter, TFinOp)),  % Remove a solução atual.
    write('Final Result: AgOpRoomBetter='), write(AgOpRoomBetter), nl,  % Exibe o resultado.
    write('LAgDoctorsBetter='), write(LAgDoctorsBetter), nl,
    write('TFinOp='), write(TFinOp), nl,
    get_time(Tf),  % Obtém o tempo final.
    T is Tf - Ti,  % Calcula o tempo de execução.
    write('Tempo de geracao da solucao:'), write(T), nl.  % Exibe o tempo de execução.

% Função para encontrar a melhor solução por meio de permutações das cirurgias.
obtain_better_sol1(Room, Day) :-  
    asserta(better_sol(Day, Room, _, _, 1441)),  % Inicializa a melhor solução com um tempo muito alto.
    findall(OpCode, surgery_id(OpCode, _), LOC),  % Obtém todos os códigos de cirurgia.
    permutation(LOC, LOpCode),  % Gera todas as permutações possíveis das cirurgias.
    retractall(agenda_staff1(_, _, _)),  % Limpa as agendas temporárias.
    retractall(agenda_operation_room1(_, _, _)),  % Limpa as agendas da sala de operação.
    retractall(availability(_, _, _)),  % Limpa as disponibilidades.
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))), _),  % Atualiza as agendas dos médicos.
    agenda_operation_room(Room, Day, Agenda), assert(agenda_operation_room1(Room, Day, Agenda)),  % Atualiza a agenda da sala.
    findall(_, (agenda_staff1(D, Day, L), free_agenda0(L, LFA), adapt_timetable(D, Day, LFA, LFA2), assertz(availability(D, Day, LFA2))), _),  % Atualiza as disponibilidades.
    availability_all_surgeries(LOpCode, Room, Day),  % Agende todas as cirurgias.
    agenda_operation_room1(Room, Day, AgendaR),
    update_better_sol(Day, Room, AgendaR, LOpCode),  % Atualiza a melhor solução.
    fail.  % Força a recursão até todas as permutações serem testadas.

% Atualiza a melhor solução se uma solução melhor for encontrada.
update_better_sol(Day, Room, Agenda, LOpCode) :-
    better_sol(Day, Room, _, _, FinTime),  % Obtém o tempo final da solução atual.
    reverse(Agenda, AgendaR),  % Reverte a agenda para a ordem original.
    evaluate_final_time(AgendaR, LOpCode, FinTime1),  % Avalia o tempo final da nova solução.
    write('Analysing for LOpCode='), write(LOpCode), nl,
    write('now: FinTime1='), write(FinTime1), write(' Agenda='), write(Agenda), nl,
    FinTime1 < FinTime,  % Se a nova solução for melhor, atualiza.
    write('best solution updated'), nl,
    retract(better_sol(_, _, _, _, _)),  % Remove a solução antiga.
    findall(Doctor, assignment_surgery(_, Doctor), LDoctors1),  % Obtém os médicos.
    remove_equals(LDoctors1, LDoctors),  % Remove duplicatas de médicos.
    list_doctors_agenda(Day, LDoctors, LDAgendas),  % Lista as agendas dos médicos.
    asserta(better_sol(Day, Room, Agenda, LDAgendas, FinTime1)).  % Armazena a nova melhor solução.

% Função que calcula o tempo final após todas as cirurgias.
evaluate_final_time([], _, 1441).  % Caso base: sem mais cirurgias, o tempo final é 1441.
evaluate_final_time([( _, Tfin, OpCode) | _], LOpCode, Tfin) :- member(OpCode, LOpCode), !.  % Se a cirurgia estiver na lista de cirurgias, retorna o tempo.
evaluate_final_time([_ | AgR], LOpCode, Tfin) :- evaluate_final_time(AgR, LOpCode, Tfin).  % Continua a busca pelos intervalos.

% Lista as agendas dos médicos após o agendamento.
list_doctors_agenda(_, [], []).  
list_doctors_agenda(Day, [D | LD], [(D, AgD) | LAgD]) :- 
    agenda_staff1(D, Day, AgD), list_doctors_agenda(Day, LD, LAgD).  % Obtém as agendas dos médicos e as lista.

% Remove duplicatas de uma lista.
remove_equals([], []).  
remove_equals([X | L], L1) :- member(X, L), !, remove_equals(L, L1).  % Se o elemento já está na lista, ignora.
remove_equals([X | L], [X | L1]) :- remove_equals(L, L1).  % Caso contrário, mantém o elemento.


%-------------------------------------------------------------------------------------------------------------%

% Heuristic 1: Earliest-Available-Time (EAT)
% Esta heurística agenda as cirurgias de forma a priorizar aquelas que podem começar mais cedo.
schedule_eat(Room, Day) :-  
    % Remove todas as agendas temporárias para garantir que começamos do zero.
    retractall(agenda_staff1(_, _, _)),          % Remove todas as agendas temporárias dos médicos.
    retractall(agenda_operation_room1(_, _, _)), % Remove todas as agendas temporárias da sala de operação.
    retractall(availability(_, _, _)),           % Remove todas as disponibilidades temporárias de médicos.

    % Recupera todas as agendas de profissionais para o dia e as armazena como fatos temporários.
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))), _),  
    % Para cada médico (D) e sua agenda (Agenda) no dia especificado, armazena-as como fatos temporários (agenda_staff1).

    % Recupera a agenda da sala de operação e a armazena como fato temporário.
    agenda_operation_room(Room, Day, AgendaRoom), assertz(agenda_operation_room1(Room, Day, AgendaRoom)),  
    % Obtém a agenda da sala de operação para o dia especificado e a armazena como fato temporário (agenda_operation_room1).

    % Gera os intervalos livres para todos os profissionais no dia especificado e os armazena como disponibilidade.
    findall(_, (agenda_staff1(D, Day, L), free_agenda0(L, LFA), adapt_timetable(D, Day, LFA, LFA2), assertz(availability(D, Day, LFA2))), _),  
    % Para cada médico (D) com sua agenda (L), calcula os intervalos livres e os armazena como disponibilidade (availability).

    % Recupera todos os códigos de cirurgias disponíveis.
    findall(OpCode, surgery_id(OpCode, _), LOpCode),  
    % Obtém todos os códigos de cirurgia disponíveis (OpCode) a partir do banco de dados de cirurgias.

    % Agenda as cirurgias sequencialmente com base na estratégia EAT.
    schedule_eat_surgeries(LOpCode, Room, Day).  
    % Chama a função recursiva para agendar as cirurgias, uma a uma, com base na heurística EAT.

% Caso base para parar o agendamento quando não há mais cirurgias a serem processadas.
schedule_eat_surgeries([], _, _).  
% Caso base da recursão: se não houver mais cirurgias (LOpCode está vazio), a função retorna e termina a recursão.

% Regra recursiva que agenda uma cirurgia e continua com o restante.
schedule_eat_surgeries([OpCode | LOpCode], Room, Day) :-  
    % Recupera o tipo de cirurgia associado ao código.
    surgery_id(OpCode, OpType),  
    % Obtém o tipo de cirurgia (OpType) a partir do código de cirurgia (OpCode).

    % Calcula o tempo total necessário para a cirurgia (anestesia, cirurgia e limpeza).
    sumTemp(OpType, TResult),  
    % Calcula o tempo total necessário para a cirurgia (soma os tempos de anestesia, cirurgia e limpeza).

    % Determina os intervalos possíveis para realizar a cirurgia.
    availability_operation(OpCode, Room, Day, LPossibilities, LDoctors),  
    % Obtém os intervalos de tempo possíveis para agendar a cirurgia, considerando a disponibilidade dos médicos (LDoctors) e da sala.

    % Escolhe o primeiro intervalo disponível para a cirurgia.
    schedule_first_interval(TResult, LPossibilities, (TinS, TfinS)),  
    % Escolhe o primeiro intervalo possível para a cirurgia, com base no tempo necessário (TResult) e nos intervalos disponíveis.

    % Atualiza a agenda da sala de operação com o intervalo escolhido.
    retract(agenda_operation_room1(Room, Day, Agenda)),  
    % Remove a agenda anterior da sala de operação.
    insert_agenda((TinS, TfinS, OpCode), Agenda, Agenda1),  
    % Insere o intervalo escolhido para a cirurgia na agenda da sala de operação.
    assertz(agenda_operation_room1(Room, Day, Agenda1)),  
    % Atualiza a agenda da sala de operação com o novo intervalo.

    % Atualiza as agendas dos médicos associados à cirurgia.
    insert_agenda_doctors((TinS, TfinS, OpCode), Day, LDoctors),  
    % Insere o intervalo da cirurgia nas agendas dos médicos (LDoctors) no dia especificado.

    % Chama a função recursivamente para processar as cirurgias restantes.
    schedule_eat_surgeries(LOpCode, Room, Day).  
    % Chama a função recursivamente para agendar a próxima cirurgia da lista (LOpCode).

%-------------------------------------------------------------------------------------------------------------%
% Heuristic 2: Greedy-Minimum-Idle-Time (GMIT)
% Esta heurística agenda as cirurgias para minimizar o tempo ocioso entre elas.
schedule_gmit(Room, Day) :-  
    % Remove todas as agendas temporárias para garantir que começamos do zero.
    retractall(agenda_staff1(_, _, _)),         % Remove todas as agendas temporárias dos médicos.
    retractall(agenda_operation_room1(_, _, _)),% Remove todas as agendas temporárias da sala de operação.
    retractall(availability(_, _, _)),          % Remove todas as disponibilidades temporárias de médicos.

    % Recupera todas as agendas de profissionais para o dia e as armazena como fatos temporários.
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))), _),  
    % Para cada médico (D) e sua agenda (Agenda) no dia especificado, armazena-as como fatos temporários (agenda_staff1).

    % Recupera a agenda da sala de operação e a armazena como fato temporário.
    agenda_operation_room(Room, Day, AgendaRoom), assertz(agenda_operation_room1(Room, Day, AgendaRoom)),  
    % Obtém a agenda da sala de operação para o dia especificado e a armazena como fato temporário (agenda_operation_room1).

    % Gera os intervalos livres para todos os profissionais no dia especificado e os armazena como disponibilidade.
    findall(_, (agenda_staff1(D, Day, L), free_agenda0(L, LFA), adapt_timetable(D, Day, LFA, LFA2), assertz(availability(D, Day, LFA2))), _),  
    % Para cada médico (D) com sua agenda (L), calcula os intervalos livres e os armazena como disponibilidade (availability).

    % Recupera todos os códigos de cirurgias disponíveis.
    findall(OpCode, surgery_id(OpCode, _), LOpCode),  
    % Obtém todos os códigos de cirurgia disponíveis (OpCode) a partir do banco de dados de cirurgias.

    % Ordena as cirurgias por duração (do maior para o menor).
    prioritize_surgeries(LOpCode, PrioritizedLOpCode),  
    % Ordena os códigos de cirurgia pela duração, do maior para o menor, chamando a função prioritize_surgeries.

    % Agenda as cirurgias sequencialmente com base na estratégia GMIT.
    schedule_gmit_surgeries(PrioritizedLOpCode, Room, Day).  
    % Chama a função recursiva para agendar as cirurgias com a lista ordenada de cirurgias (PrioritizedLOpCode).

% Ordena as cirurgias por duração (do maior para o menor).
prioritize_surgeries(LOpCode, PrioritizedLOpCode) :-  
    % Associa cada código de cirurgia com seu tempo total necessário.
    findall((OpCode, TResult), (member(OpCode, LOpCode), surgery_id(OpCode, OpType), sumTemp(OpType, TResult)), LDurations),  
    % Para cada cirurgia (OpCode) da lista (LOpCode), calcula o tempo total (TResult) necessário (soma dos tempos de anestesia, cirurgia e limpeza).

    % Ordena os pares (código, duração) pela duração em ordem decrescente.
    sort(2, @>=, LDurations, SortedLDurations),  
    % Ordena a lista de pares (OpCode, Resultado do Tempo) pela duração da cirurgia, do maior para o menor.

    % Extrai os códigos ordenados em uma lista.
    findall(OpCode, member((OpCode, _), SortedLDurations), PrioritizedLOpCode).  
    % Extrai os códigos de cirurgia (OpCode) da lista ordenada e os coloca em uma nova lista (PrioritizedLOpCode).

% Caso base para parar o agendamento quando não há mais cirurgias a serem processadas.
schedule_gmit_surgeries([], _, _).  
% Caso base da recursão: se não houver mais cirurgias (LOpCode está vazio), a função retorna e termina a recursão.

% Regra recursiva que agenda uma cirurgia e continua com o restante.
schedule_gmit_surgeries([OpCode | LOpCode], Room, Day) :-  
    % Recupera o tipo de cirurgia associado ao código.
    surgery_id(OpCode, OpType),  
    % Obtém o tipo de cirurgia (OpType) a partir do código de cirurgia (OpCode).

    % Calcula o tempo total necessário para a cirurgia (anestesia, cirurgia e limpeza).
    sumTemp(OpType, TResult),  
    % Calcula o tempo total necessário para a cirurgia (soma os tempos de anestesia, cirurgia e limpeza).

    % Determina os intervalos possíveis para realizar a cirurgia.
    availability_operation(OpCode, Room, Day, LPossibilities, LDoctors),  
    % Obtém os intervalos de tempo possíveis para agendar a cirurgia, considerando a disponibilidade dos médicos (LDoctors) e da sala.

    % Seleciona o intervalo ótimo que minimiza o tempo ocioso.
    find_optimal_slot(LPossibilities, (TinS, TfinS)),  
    % Chama a função find_optimal_slot para encontrar o intervalo com o menor tempo ocioso.

    % Atualiza a agenda da sala de operação com o intervalo escolhido.
    retract(agenda_operation_room1(Room, Day, Agenda)),  
    % Remove a agenda anterior da sala de operação.
    insert_agenda((TinS, TfinS, OpCode), Agenda, Agenda1),  
    % Insere o intervalo escolhido para a cirurgia na agenda da sala de operação.
    assertz(agenda_operation_room1(Room, Day, Agenda1)),  
    % Atualiza a agenda da sala de operação com o novo intervalo.

    % Atualiza as agendas dos médicos associados à cirurgia.
    insert_agenda_doctors((TinS, TfinS, OpCode), Day, LDoctors),  
    % Insere o intervalo da cirurgia nas agendas dos médicos (LDoctors) no dia especificado.

    % Chama a função recursivamente para processar as cirurgias restantes.
    schedule_gmit_surgeries(LOpCode, Room, Day).  
    % Chama a função recursivamente para agendar a próxima cirurgia da lista (LOpCode).

% Seleciona o intervalo que minimiza o tempo ocioso.
find_optimal_slot([Slot], Slot).  
% Caso base: quando há apenas um intervalo disponível, esse intervalo é escolhido.

find_optimal_slot([(Tin1, Tfin1), (Tin2, Tfin2) | Slots], BestSlot) :-  
    % Calcula o tempo ocioso gerado por cada intervalo.
    Idle1 is Tin1 - Tfin1, Idle2 is Tin2 - Tfin2,  

    % Compara os intervalos e escolhe aquele com menor tempo ocioso.
    (Idle1 =< Idle2 -> find_optimal_slot([(Tin1, Tfin1) | Slots], BestSlot) ; find_optimal_slot([(Tin2, Tfin2) | Slots], BestSlot)).  
    % Compara o tempo ocioso dos dois primeiros intervalos e escolhe o que gera o menor tempo ocioso. Se houver mais intervalos, a função é chamada recursivamente.


