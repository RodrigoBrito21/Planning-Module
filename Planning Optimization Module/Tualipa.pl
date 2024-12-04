:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:- dynamic agenda_operation_room/3.
:- dynamic agenda_operation_room1/3.
:- dynamic better_sol/5.

agenda_staff(d001,20241028,[(720,790,m01),(1080,1140,c01)]).
agenda_staff(d002,20241028,[(850,900,m02),(901,960,m02),(1380,1440,c02)]).
agenda_staff(d003,20241028,[(720,790,m01),(910,980,m02)]).

timetable(d001,20241028,(480,1200)).
timetable(d002,20241028,(500,1440)).
timetable(d003,20241028,(520,1320)).

% first example
%agenda_staff(d001,20241028,[(720,840,m01),(1080,1200,c01)]).
%agenda_staff(d002,20241028,[(780,900,m02),(901,960,m02),(1080,1440,c02)]).
%agenda_staff(d003,20241028,[(720,840,m01),(900,960,m02)]).

%timetable(d001,20241028,(480,1200)).
%timetable(d002,20241028,(720,1440)).
%timetable(d003,20241028,(600,1320)).

staff(d001,doctor,orthopaedist,[so2,so3,so4]).
staff(d002,doctor,orthopaedist,[so2,so3,so4]).
staff(d003,doctor,orthopaedist,[so2,so3,so4]).

%surgery(SurgeryType,TAnesthesia,TSurgery,TCleaning).

surgery(so2,45,60,45).
surgery(so3,45,90,45).
surgery(so4,45,75,45).

surgery_id(so100001,so2).
surgery_id(so100002,so3).
%surgery_id(so100003,so4).
%surgery_id(so100004,so2).
%surgery_id(so100005,so4).


assignment_surgery(so100001,d001).
assignment_surgery(so100002,d002).
%assignment_surgery(so100003,d003).
%assignment_surgery(so100004,d001).
%assignment_surgery(so100004,d002).
%assignment_surgery(so100005,d002).
%assignment_surgery(so100005,d003).

agenda_operation_room(or1,20241028,[]).


free_agenda0([],[(0,1440)]).
free_agenda0([(0,Tfin,)|LT],LT1):-!,free_agenda1([(0,Tfin,)|LT],LT1).
free_agenda0([(Tin,Tfin,_)|LT],[(0,T1)|LT1]):- T1 is Tin-1,
    free_agenda1([(Tin,Tfin,_)|LT],LT1).

free_agenda1([(,Tfin,)],[(T1,1440)]):-Tfin\==1440,!,T1 is Tfin+1.
free_agenda1([(,,_)],[]).
free_agenda1([(,T,),(T1,Tfin2,_)|LT],LT1):-Tx is T+1,T1==Tx,!,
    free_agenda1([(T1,Tfin2,_)|LT],LT1).
free_agenda1([(,Tfin1,),(Tin2,Tfin2,_)|LT],[(T1,T2)|LT1]):-T1 is Tfin1+1,T2 is Tin2-1,
    free_agenda1([(Tin2,Tfin2,_)|LT],LT1).


adapt_timetable(D,Date,LFA,LFA2):-timetable(D,Date,(InTime,FinTime)),treatin(InTime,LFA,LFA1),treatfin(FinTime,LFA1,LFA2).

treatin(InTime,[(In,Fin)|LFA],[(In,Fin)|LFA]):-InTime=<In,!.
treatin(InTime,[(_,Fin)|LFA],LFA1):-InTime>Fin,!,treatin(InTime,LFA,LFA1).
treatin(InTime,[(_,Fin)|LFA],[(InTime,Fin)|LFA]).
treatin(_,[],[]).

treatfin(FinTime,[(In,Fin)|LFA],[(In,Fin)|LFA1]):-FinTime>=Fin,!,treatfin(FinTime,LFA,LFA1).
treatfin(FinTime,[(In,)|],[]):-FinTime=<In,!.
treatfin(FinTime,[(In,)|],[(In,FinTime)]).
treatfin(_,[],[]).


intersect_all_agendas([Name],Date,LA):-!,availability(Name,Date,LA).
intersect_all_agendas([Name|LNames],Date,LI):-
    availability(Name,Date,LA),
    intersect_all_agendas(LNames,Date,LI1),
    intersect_2_agendas(LA,LI1,LI).

intersect_2_agendas([],_,[]).
intersect_2_agendas([D|LD],LA,LIT):-	intersect_availability(D,LA,LI,LA1),
					intersect_2_agendas(LD,LA1,LID),
					append(LI,LID,LIT).

intersect_availability((,),[],[],[]).

intersect_availability((_,Fim),[(Ini1,Fim1)|LD],[],[(Ini1,Fim1)|LD]):-
		Fim<Ini1,!.

intersect_availability((Ini,Fim),[(_,Fim1)|LD],LI,LA):-
		Ini>Fim1,!,
		intersect_availability((Ini,Fim),LD,LI,LA).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)],[(Fim,Fim1)|LD]):-
		Fim1>Fim,!,
		min_max(Ini,Ini1,_,Imax),
		min_max(Fim,Fim1,Fmin,_).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)|LI],LA):-
		Fim>=Fim1,!,
		min_max(Ini,Ini1,_,Imax),
		min_max(Fim,Fim1,Fmin,_),
		intersect_availability((Fim1,Fim),LD,LI,LA).

min_max(I,I1,I,I1):- I<I1,!.
min_max(I,I1,I1,I).

sumTemp(OperationType, TResult):-
surgery(OperationType, TAnas, TSur, TClean),
TResult is TAnas + TSur + TClean.

% Não conseguimos ligar mas funciona
get_staff_for_surgeries(OpCode, Specializatity, LStaff) :-
surgery_id(OpCode, OpType),
findall(StaffId, ( staff(StaffId, _, Specializatity, Specializations), member(OpType, Specializations) ), LStaff).

schedule_all_surgeries(Room,Day):-
    retractall(agenda_staff1(,,_)),
    retractall(agenda_operation_room1(,,_)),
    retractall(availability(,,_)),
    findall(,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),),
    agenda_operation_room(Or,Date,Agenda),assert(agenda_operation_room1(Or,Date,Agenda)),
    findall(,(agenda_staff1(D,Date,L),free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),),
    findall(OpCode,surgery_id(OpCode,_),LOpCode),

    availability_all_surgeries(LOpCode,Room,Day),!.

availability_all_surgeries([],,).
availability_all_surgeries([OpCode|LOpCode],Room,Day):-
    surgery_id(OpCode,OpType),surgery(OpType,,TSurgery,),
    sumTemp(OpType, TResult),
    availability_operation(OpCode,Room,Day,LPossibilities,LDoctors),
    schedule_first_interval(TResult,LPossibilities,(TinS,TfinS)),
    retract(agenda_operation_room1(Room,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assertz(agenda_operation_room1(Room,Day,Agenda1)),
    insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors),
    availability_all_surgeries(LOpCode,Room,Day).



availability_operation(OpCode,Room,Day,LPossibilities,LDoctors):-
    surgery_id(OpCode,OpType),
    findall(Doctor,assignment_surgery(OpCode,Doctor),LDoctors),
    intersect_all_agendas(LDoctors,Day,LA),
    agenda_operation_room1(Room,Day,LAgenda),
    free_agenda0(LAgenda,LFAgRoom),
    intersect_2_agendas(LA,LFAgRoom,LIntAgDoctorsRoom),
    sumTemp(OpType, TResult),
    remove_unf_intervals(TResult,LIntAgDoctorsRoom,LPossibilities).


remove_unf_intervals(_,[],[]).
remove_unf_intervals(TSurgery,[(Tin,Tfin)|LA],[(Tin,Tfin)|LA1]):-DT is Tfin-Tin+1,TSurgery=<DT,!,
    remove_unf_intervals(TSurgery,LA,LA1).
remove_unf_intervals(TSurgery,[_|LA],LA1):- remove_unf_intervals(TSurgery,LA,LA1).


schedule_first_interval(TSurgery,[(Tin,)|],(Tin,TfinS)):-
    TfinS is Tin + TSurgery - 1.

insert_agenda((TinS,TfinS,OpCode),[],[(TinS,TfinS,OpCode)]).
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(TinS,TfinS,OpCode),(Tin,Tfin,OpCode1)|LA]):-TfinS<Tin,!.
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(Tin,Tfin,OpCode1)|LA1]):-insert_agenda((TinS,TfinS,OpCode),LA,LA1).

insert_agenda_doctors(,,[]).
insert_agenda_doctors((TinS,TfinS,OpCode),Day,[Doctor|LDoctors]):-
    retract(agenda_staff1(Doctor,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assert(agenda_staff1(Doctor,Day,Agenda1)),
    insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors).



obtain_better_sol(Room,Day,AgOpRoomBetter,LAgDoctorsBetter,TFinOp):-
		get_time(Ti),
		(obtain_better_sol1(Room,Day);true),
		retract(better_sol(Day,Room,AgOpRoomBetter,LAgDoctorsBetter,TFinOp)),
            write('Final Result: AgOpRoomBetter='),write(AgOpRoomBetter),nl,
            write('LAgDoctorsBetter='),write(LAgDoctorsBetter),nl,
            write('TFinOp='),write(TFinOp),nl,
		get_time(Tf),
		T is Tf-Ti,
		write('Tempo de geracao da solucao:'),write(T),nl.


obtain_better_sol1(Room,Day):-
    asserta(better_sol(Day,Room,,,1441)),
    findall(OpCode,surgery_id(OpCode,_),LOC),!,
    permutation(LOC,LOpCode),
    retractall(agenda_staff1(,,_)),
    retractall(agenda_operation_room1(,,_)),
    retractall(availability(,,_)),
    findall(,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),),
    agenda_operation_room(Room,Day,Agenda),assert(agenda_operation_room1(Room,Day,Agenda)),
    findall(,(agenda_staff1(D,Day,L),free_agenda0(L,LFA),adapt_timetable(D,Day,LFA,LFA2),assertz(availability(D,Day,LFA2))),),
    availability_all_surgeries(LOpCode,Room,Day),
    agenda_operation_room1(Room,Day,AgendaR),
		update_better_sol(Day,Room,AgendaR,LOpCode),
		fail.

update_better_sol(Day,Room,Agenda,LOpCode):-
                better_sol(Day,Room,,,FinTime),
                reverse(Agenda,AgendaR),
                evaluate_final_time(AgendaR,LOpCode,FinTime1),
             write('Analysing for LOpCode='),write(LOpCode),nl,
             write('now: FinTime1='),write(FinTime1),write(' Agenda='),write(Agenda),nl,
		FinTime1<FinTime,
             write('best solution updated'),nl,
                retract(better_sol(,,,,_)),
                findall(Doctor,assignment_surgery(_,Doctor),LDoctors1),
                remove_equals(LDoctors1,LDoctors),
                list_doctors_agenda(Day,LDoctors,LDAgendas),
		asserta(better_sol(Day,Room,Agenda,LDAgendas,FinTime1)).

evaluate_final_time([],_,1441).
evaluate_final_time([(,Tfin,OpCode)|],LOpCode,Tfin):-member(OpCode,LOpCode),!.
evaluate_final_time([_|AgR],LOpCode,Tfin):-evaluate_final_time(AgR,LOpCode,Tfin).

list_doctors_agenda(_,[],[]).
list_doctors_agenda(Day,[D|LD],[(D,AgD)|LAgD]):-agenda_staff1(D,Day,AgD),list_doctors_agenda(Day,LD,LAgD).

remove_equals([],[]).
remove_equals([X|L],L1):-member(X,L),!,remove_equals(L,L1).
remove_equals([X|L],[X|L1]):-remove_equals(L,L1).
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

    
    

