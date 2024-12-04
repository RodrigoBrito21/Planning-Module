# Planning-Module

## Explicação do código das PLs

Definição das Agendas
Definição de Recursos Humanos
Definição de Cirurgias

Manipulação de Agendas Livres
Funções free_agenda0/2 e free_agenda1/2
Essas funções geram intervalos livres na agenda de um profissional ou sala, considerando as ocupações já registradas.

free_agenda0: Inicializa com um intervalo total de 0 a 1440 minutos (1 dia).
free_agenda1: Processa os intervalos ocupados para gerar uma lista de intervalos disponíveis.

Adaptação ao Horário Total
adapt_timetable/4, treatin/3 e treatfin/3
Essas funções ajustam os intervalos livres conforme o horário total de trabalho do profissional:

treatin: Alinha o início dos intervalos livres com o início do horário de trabalho.
treatfin: Alinha o final dos intervalos livres com o final do horário de trabalho.


Interseção de Agendas
intersect_all_agendas/3 e intersect_2_agendas/3
Essas funções calculam os intervalos de tempo em que todos os profissionais necessários para uma cirurgia estão simultaneamente disponíveis.

Planejamento de Cirurgias
schedule_all_surgeries/2
Essa função principal organiza todas as cirurgias em uma sala para um dia específico:

Inicializa as agendas e disponibilidades.
Calcula os intervalos livres.
Planeja cada cirurgia sequencialmente com availability_all_surgeries/3.
availability_operation/5
Determina os intervalos de tempo possíveis para uma cirurgia específica, considerando:

Disponibilidade dos médicos.
Disponibilidade da sala.
schedule_first_interval/3
Seleciona o primeiro intervalo adequado para agendamento.

Inserção de Cirurgias nas Agendas
insert_agenda/3 e insert_agenda_doctors/3
Essas funções inserem a cirurgia nos intervalos calculados, atualizando a agenda da sala e dos médicos.

Obtenção da Melhor Solução
obtain_better_sol/5 e obtain_better_sol1/2
Gera todas as permutações de cirurgias.
Avalia o tempo final da última cirurgia em cada permutação.
Armazena a solução com o menor tempo final.
update_better_sol/4
Atualiza a solução atual, se uma melhor for encontrada.

Funções Auxiliares
evaluate_final_time/3: Calcula o tempo final da última cirurgia.
remove_equals/2: Remove duplicatas de uma lista.
list_doctors_agenda/3: Lista as agendas dos médicos após o planejamento.

Fluxo Geral
Inicialização: Definir dados (agendas, cirurgias e horários).
Planejamento: Identificar intervalos disponíveis. Planejar cirurgias em sequência.
Otimização: Testar todas as permutações para encontrar a solução ótima.
Resultado: Exibir a melhor solução encontrada.


## Explicação do código Adaptado e Heurísticos

### Heurística 1: Earliest-Available-Time (EAT)

Objetivo: Escolher a cirurgia que pode ser iniciada mais cedo, baseada na disponibilidade de médicos e da sala de operação.
Adiciona Função:

schedule_eat/2
schedule_eat_surgeries/3

Estas funções realizam o planejamento sequencial das cirurgias com base na disponibilidade inicial de tempo.

### Heurística 2: Greedy-Minimum-Idle-Time (GMIT)

Objetivo: Escolher cirurgias de maneira a minimizar os períodos de inatividade entre elas.
Adiciona Função:
schedule_gmit/2
schedule_gmit_surgeries/3
prioritize_surgeries/2
find_optimal_slot/2

Estas funções priorizam cirurgias mais longas e procuram o intervalo ideal para reduzir a ociosidade.


### Adição de Ordenação de Cirurgias
O código agora introduz a função prioritize_surgeries/2, que organiza as cirurgias por duração em ordem decrescente para a heurística GMIT

Objetivo: Garantir que cirurgias mais longas sejam realizadas primeiro, alinhando-se com a estratégia de ocupação eficiente.

Implementa o seguinte:
prioritize_surgeries(LOpCode, PrioritizedLOpCode) :-
findall((OpCode, TResult), (member(OpCode, LOpCode), surgery_id(OpCode, OpType), sumTemp(OpType, TResult)), LDurations),
sort(2, @>=, LDurations, SortedLDurations),
findall(OpCode, member((OpCode, _), SortedLDurations), PrioritizedLOpCode).

Isso ordena os códigos de cirurgia pelo tempo total necessário

### Cálculo do Intervalo Ótimo

Objetivo: Identificar o intervalo de tempo ideal para uma cirurgia, minimizando períodos de inatividade.
Implementa:

find_optimal_slot([Slot], Slot).
find_optimal_slot([(Tin1, Tfin1), (Tin2, Tfin2) | Slots], BestSlot) :-
Idle1 is Tin1 - Tfin1, Idle2 is Tin2 - Tfin2,
(Idle1 =< Idle2 -> find_optimal_slot([(Tin1, Tfin1) | Slots], BestSlot) ; find_optimal_slot([(Tin2, Tfin2) | Slots], BestSlot)).

A escolha do intervalo é baseada na quantidade de tempo ocioso gerada.

### Funções Adicionais e Ajustes Gerais

Reduz redundância, centralizando o cálculo:

sumTemp(OperationType, TResult) :-
surgery(OperationType, TAnas, TSur, TClean),
TResult is TAnas + TSur + TClean.

### Parâmetros com Formato Genérico

Diferença:
Diversas funções agora utilizam parâmetros genéricos para melhorar a legibilidade e evitar erros, como nas funções availability_operation/5 e schedule_all_surgeries/2.

Explicação:
Por exemplo, em availability_operation/5:

availability_operation(OpCode, Room, Day, LPossibilities, LDoctors) :-
surgery_id(OpCode, OpType),
findall(Doctor, assignment_surgery(OpCode, Doctor), LDoctors),
intersect_all_agendas(LDoctors, Day, LA),
agenda_operation_room1(Room, Day, LAgenda),
free_agenda0(LAgenda, LFAgRoom),
intersect_2_agendas(LA, LFAgRoom, LIntAgDoctorsRoom),
sumTemp(OpType, TResult),
remove_unf_intervals(TResult, LIntAgDoctorsRoom, LPossibilities).

A implementação genérica facilita futuras modificações.

### Retração e Assertividade de Dados

As funções de manipulação de fatos foram unificadas e otimizadas com uso extensivo de retractall/1 e assertz/1.

Melhorias:
Retractall/1 remove todos os fatos antes de recalcular as disponibilidades ou planejar cirurgias.
Assertz/1 insere dados no final da base para manter a ordem correta.

### Resumo

As principais diferenças incluem:

- Novas heurísticas (EAT e GMIT): Introduzidas para diferentes estratégias de planejamento.
- Ordenação de cirurgias por duração: Facilitando o planejamento estratégico.
- Cálculo do intervalo ótimo: Focado em minimizar a ociosidade.
- Ajustes gerais: Tornando o código mais eficiente e modular.

Essas mudanças melhoram a flexibilidade e o desempenho do sistema, ao mesmo tempo em que oferecem
diferentes opções de planejamento para atender a várias demandas de hospitais.







