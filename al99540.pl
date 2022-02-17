% 99540 Pedro Lameiras

:- [codigo_comum].
:- [testesPublicos].

%-------------------------------------------------------------------------------
% 1 - extrai_ilhas_linha(N_L, Linha, Ilhas)
% Ilhas corresponde a lista ordenada das ilhas presentes na Linha
% com numero N_L. Uma ilha possui a estrutura 
% Ilha(numero de pontes, (Linha, Coluna)).
%-------------------------------------------------------------------------------

extrai_ilhas_linha(N_L, Linha, Ilhas):- 
    length(Linha, Comp_Linha),
    extrai_ilhas_linha(N_L, Linha, Ilhas, [], 0, Comp_Linha).

extrai_ilhas_linha(_, [], Ilhas, Ilhas, Comp_Linha, Comp_Linha) :- !.

extrai_ilhas_linha(N_L, [P|L], Ilhas, Res, Count, Comp_Linha):- 
    Count < Comp_Linha,
    NewCount is Count + 1,
    % uma ilha corresponde a uma entrada maior que 0 que devera ser adicionada.
    (P > 0 -> 
    append(Res, [ilha(P, (N_L, NewCount))], NewRes),
    extrai_ilhas_linha(N_L, L, Ilhas, NewRes, NewCount, Comp_Linha);
    extrai_ilhas_linha(N_L, L, Ilhas, Res, NewCount, Comp_Linha) 
    ).

%-------------------------------------------------------------------------------
% 2 - ilhas(Puz, Ilhas)
% Ilhas corresponde a lista ordenada das ilhas do puzzle. Corresponde a
% a aplicar o predicado extrai_ilhas_linha a cada linha do puzzle.
%-------------------------------------------------------------------------------

ilhas(Puz, Ilhas):-
    % numero de entradas do puzzle corresponde ao numero de linhas.
    length(Puz, Num_Linhas),
    ilhas(Puz, Ilhas, [], 0, Num_Linhas).

ilhas([], Ilhas, Ilhas, Num_Linhas, Num_Linhas) :- !.

ilhas([P|L], Ilhas, Res, Count, Num_Linhas):- 
    Count < Num_Linhas,
    % nova linha do puzzle.
    NewCount is Count + 1,
    extrai_ilhas_linha(NewCount, P, Ilhas_Linha),
    append(Res, Ilhas_Linha, NewRes),
    ilhas(L, Ilhas, NewRes, NewCount, Num_Linhas).

%-------------------------------------------------------------------------------
% 3 - vizinhas(Ilhas, Ilha, Vizinhas)
% Dado um conjunto de Ilhas, Vizinhas corresponde as ilhas mais proximas
% horizontalmente e verticalmente. O predicado faz a adicao destas ilhas
% que podem ser elementos vazios e no fim faz a eliminacao desses elementos
%-------------------------------------------------------------------------------

% predicado que retorna o primeiro elemento duma lista que pode ser
% um elemento vazio se a lista estiver vazia
primeiro_elemento([H|_], H).
primeiro_elemento([], []).

% predicado que retornar o ultimo elemento de uma lista que pode ser
% um elemento vazio se a lista estiver vazia
ultimo_elemento(X,[X]).
ultimo_elemento(X,[_|L]) :- ultimo_elemento(X,L).
ultimo_elemento([], []).

vazio([]).

vizinhas(Ilhas, Ilha, Vizinhas):-
    vizinhas(Ilhas, Ilha, Vizinhas, 0, []).

vizinhas(_, _, Vizinhas, 2, Vizinhas).

% primeiro sao encontradas as ilhas vizinhas horizontalmente
vizinhas(Ilhas, Ilha, Vizinhas, 0, _):-
    arg(2, Ilha, Posicao),
    Posicao = (Linha, Coluna),
    findall(X1, (member(X1, Ilhas), arg(2, X1, Pos), Pos = (Linha, B1), B1 < Coluna), Ilhas_Vizinhas_Esquerda),
    findall(X2, (member(X2, Ilhas), arg(2, X2, Pos), Pos = (Linha, B2), B2 > Coluna), Ilhas_Vizinhas_Direita),
    ultimo_elemento(Ilha_Vizinha_Esquerda, Ilhas_Vizinhas_Esquerda),
    primeiro_elemento(Ilhas_Vizinhas_Direita, Ilha_Vizinha_Direita),
    Ilha_Esquerda_Direita = [Ilha_Vizinha_Esquerda, Ilha_Vizinha_Direita],
    vizinhas(Ilhas, Ilha, Vizinhas, 1, Ilha_Esquerda_Direita).

% segundamente sao determinadas as ilhas vizinhas verticalmente e 
% os elementos vazios sao removidos
vizinhas(Ilhas, Ilha, Vizinhas, 1, Ilhas_Vizinhas):-
    arg(2, Ilha, Posicao),
    Posicao = (Linha, Coluna),
    findall(X1, (member(X1, Ilhas), arg(2, X1, Pos), Pos = (B1, Coluna), B1 < Linha), Ilhas_Vizinhas_Cima),
    findall(X2, (member(X2, Ilhas), arg(2, X2, Pos), Pos = (B2, Coluna), B2 > Linha), Ilhas_Vizinhas_Baixo),
    ultimo_elemento(Ilha_Vizinha_Cima, Ilhas_Vizinhas_Cima),
    primeiro_elemento(Ilhas_Vizinhas_Baixo, Ilha_Vizinha_Baixo),
    append([Ilha_Vizinha_Cima], Ilhas_Vizinhas, Ilhas_Sem_baixo),
    append(Ilhas_Sem_baixo, [Ilha_Vizinha_Baixo], Ilhas_Todas),
    exclude(vazio, Ilhas_Todas, Ilhas_Res),
    vizinhas(Ilhas, Ilha, Vizinhas, 2, Ilhas_Res).

%-------------------------------------------------------------------------------
% 4 - estado(Ilhas, Estado)
% Dado um conjunto de ilhas e retornado um Estado, que corresponde a um
% conjunto de entradas, cada uma composta pela ilha da entrada, as suas
% vizinhas e a lista de pontes dessa ilha, inicialmente vazia.
%-------------------------------------------------------------------------------

% Forma inicial de cada entrada da lista
appendlist(P, Q, [P, Q, []]).

estado(Ilhas, Estado):-
    estado(Ilhas, Ilhas, Estado, []).

% Processo decorre ate todas as ilhas terem sido processadas
estado(_, [], Estado, Estado).

estado(Ilhas, [P|Q], Estado, Aux):-
    vizinhas(Ilhas, P, Viz),
    appendlist(P, Viz, Ilha_Viz),
    append(Aux, [Ilha_Viz], NewAux),
    estado(Ilhas, Q, Estado, NewAux).

%-------------------------------------------------------------------------------
% 5 - posicoes_entre(Pos1, Pos2, Posicoes)
% Posicoes corresponde a lista ordenada de posicoes (com duas coordenadas,
% linha e coluna) entre duas posicoes Pos1 e Pos2. O resultado e false
% se as posicoes nao se encontrarem na mesma linha ou coluna. Posicoes
% encontra-se ordenado por ordem crescente.
%-------------------------------------------------------------------------------

% duas posicoes encontram se em linhas e colunas diferentes se 
% nenhuma das suas coordenadas for igual
linha_coluna(A1, B1, A2, B2) :-  A1 =\= A2 , B1 =\= B2. 

% caso as duas posicoes sejam iguais nao existem posicoes entre elas
posicoes_entre(Pos, Pos, []):- !.

posicoes_entre(Pos1, Pos2, Posicoes):-
    Pos1 = (A1, B1),
    Pos2 = (A2, B2),
    \+linha_coluna(A1, B1, A2, B2),
    %tratamentos diferentes para caminhos horizontais ou verticais
    (A1 =\= A2 ->
    posicoes_entre(Pos1, Pos2, Posicoes, [], 1);
    posicoes_entre(Pos1, Pos2, Posicoes, [], 2)).
    

posicoes_entre(A, A, Pos, Pos, _).

% Caso a segunda coordenada seja igual vai-se incrementando a primeira
% ate as coordenadas serem iguais
posicoes_entre(Pos1, Pos2, Posicoes, Pos, 1):-
    Pos1 = (A1, B1),
    Pos2 = (A2, B2),
    % verificar qual das coordenadas que difere e menor e consequentemente
    % deve ser incrementada ate atingir a outra
    (A1 < A2 ->
    NewA1 is A1 + 1,
    append(Pos, [(NewA1, B1)], NewPos),
    NewPos1 = (NewA1, B1),
    NewPos2 = Pos2;
    NewA2 is A2 + 1,
    append(Pos, [(NewA2, B2)], NewPos),
    NewPos2 = (NewA2, B2),
    NewPos1 = Pos1),
    (NewPos1 = NewPos2 ->
    append(Aux, [_], NewPos);
    Aux = NewPos),
    posicoes_entre(NewPos1, NewPos2, Posicoes, Aux, 1).

% Por outro lado, caso as primeiras coordenadas sejam iguais vai-se 
% incrementando as segundas ate as coordenadas serem iguais
posicoes_entre(Pos1, Pos2, Posicoes, Pos, 2):-
    Pos1 = (A1, B1),
    Pos2 = (A2, B2),
    % logica semelhante ao referido previamente
    (B1 < B2 ->
    NewB1 is B1 + 1,
    append(Pos, [(A1, NewB1)], NewPos),
    NewPos1 = (A1, NewB1),
    NewPos2 = Pos2;
    NewB2 is B2 + 1,
    append(Pos, [(A2, NewB2)], NewPos),
    NewPos2 = (A2, NewB2),
    NewPos1 = Pos1),
    (NewPos1 = NewPos2 ->
    append(Aux, [_], NewPos);
    Aux = NewPos),
    posicoes_entre(NewPos1, NewPos2, Posicoes, Aux, 2).

%-------------------------------------------------------------------------------
% 6 - cria_ponte(Pos1, Pos2, Ponte)
% E criada uma ponte entre Pos1 e Pos2 com a estrutura ponte(Pos1, Pos2)
%-------------------------------------------------------------------------------

cria_ponte(Pos1, Pos2, Ponte):-
    Pos1 = (A1, B1),
    Pos2 = (A2, B2),
    (B1 < B2; B1 = B2, A1 < A2), 
    Ponte =.. [ponte, Pos1, Pos2], !.

cria_ponte(Pos1, Pos2, Ponte):- 
    Ponte =.. [ponte, Pos2, Pos1].

%-------------------------------------------------------------------------------
% 7 - caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
% Dadas duas posicoes Pos1 e Pos2 e as Posicoes entre elas, averigua-se
% se existe um caminho livre entre elas. Ou seja, se existe alguma
% intersecao entre os caminhos do primeiro e segundo par
%-------------------------------------------------------------------------------

caminho_livre(Pos1, Pos2, Posicoes, I, Vz):-
    posicoes_entre(Pos1, Pos2, Posicoes),
    I =.. [ilha, _, (A1, B1)],
    Vz =.. [ilha, _, (A2, B2)],
    PosI = (A1, B1),
    PosViz = (A2, B2),
    (linha_coluna(A1, B1, A2, B2)->
    % nao existe caminho livre se I e Vz nao poderem ser ligadas
    !, fail;
    posicoes_entre(PosI, PosViz, Posicoes2),
    findall(Pos, (member(Pos, Posicoes), member(Pos, Posicoes2)), Intercecao),
    % se os caminhos nao intersetarem significa que o caminho esta livre
    % se os caminhos forem iguais significa que se trata do mesmo par de ilhas
    ((Intercecao == [] ; Intercecao == Posicoes)->
    true;
    !, fail)).

%-------------------------------------------------------------------------------
% 8 - actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
% Dadas duas posicoes Pos1 e Pos2, as posicoes entre elas e uma Entrada, a
% Nova_Entrada corresponde a entrada com a lista atualizada das ilhas
% vizinhas da ilha cujo caminho permanece livre mesmo apos a adicao de uma ponte 
% entre Pos1 e Pos2.
%-------------------------------------------------------------------------------

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada):-
    posicoes_entre(Pos1, Pos2, Posicoes),
    Entrada = [I, Vz, A],
    include(caminho_livre(Pos1, Pos2, Posicoes, I), Vz, NewVizinhas),
    Nova_Entrada = [I, NewVizinhas, A].

%-------------------------------------------------------------------------------
% 9 - actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
% Sendo Pos1 e Pos2 as posicoes entre as quais foi adicionada uma ponte,
% e o Estado um conjunto de entradas, o Novo_estado corresponde a aplicar
% o predicado actualiza_vizinhas_entrada a cada uma dessas entradas, atualizando
% a lista de ilhas vizinhas de cada entrada.
%-------------------------------------------------------------------------------

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado):-
    posicoes_entre(Pos1, Pos2, Posicoes),
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes), Estado, Novo_estado).

%-------------------------------------------------------------------------------
% 10 - ilhas_terminadas(Estado, Ilhas_term)
% Dado um certo Estado, Ilhas_term corresponde ao conjunto de ilhas cujo
% numero de elementos da lista de pontes e igual ao numero de pontes dessa
% ilha e o numero de pontes e diferente de 'X'.
%-------------------------------------------------------------------------------

% aplicado a cada entrada do estado
entrada_terminada(Entrada):-
    Entrada = [I, _, A],
    length(A, C),
    I =.. [ilha, N_pontes, _],
    (N_pontes \== 'X', N_pontes == C->
    true;
    !, fail).    

extrai_ilhas(Entrada, I):-
    Entrada = [I, _, _].

ilhas_terminadas(Estado, Ilhas_term):-
    include(entrada_terminada, Estado, Entradas_Terminadas),
    maplist(extrai_ilhas, Entradas_Terminadas, Ilhas_term).

%-------------------------------------------------------------------------------
% 11 - tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% Dada uma Entrada, a Nova_entrada corresponde a retirar as ilhas terminadas
% da lista de ilhas vizinhas da entrada.
%-------------------------------------------------------------------------------

tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada):-
    Entrada = [I, Viz, A],
    findall(Ilha, (member(Ilha, Viz), (\+ member(Ilha, Ilhas_term))), Viz2),
    Nova_entrada = [I, Viz2, A].

%-------------------------------------------------------------------------------
% 12 - tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% O Novo_estado corresponde a aplicar a cada entrada do Estado o predicado
% tira_ilhas_terminadas_entrada com as Ilhas terminadas.
%-------------------------------------------------------------------------------

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

%-------------------------------------------------------------------------------
% 13 - marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% A Nova_entrada corresponde a entrada cuja ilha da entrada e marcada com
% um 'X' caso pertence a lista de ilhas terminadas
%-------------------------------------------------------------------------------

marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada):-
    Entrada = [I, Viz, A],
    I =.. [ilha, _, Cord],
    % verificar se a ilha da entrada pertence as ilhas terminadas
    (include(=(I), Ilhas_term, [])->
    Nova_entrada = Entrada;
    Novo_I =.. [ilha, 'X', Cord]),
    Nova_entrada = [Novo_I, Viz, A].

%-------------------------------------------------------------------------------
% 14 - marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% Corresponde a aplicar a todas as entradas do Estado, dado um conjunto de
% Ilhas terminadas, o predicado marca_ilhas_terminadas_entrada, obtendo
% um novo estado.
%-------------------------------------------------------------------------------

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

%-------------------------------------------------------------------------------
% 15 - trata_ilhas_terminadas(Estado, Novo_estado)
% Ao selecionar as ilhas terminadas de cada entrada do estado, marcar cada
% ilha terminada, obtendo-se um estado intermedio, e retirando as ilhas
% terminadas obtem-se o Novo_estado
%-------------------------------------------------------------------------------

trata_ilhas_terminadas(Estado, Novo_estado):-
    ilhas_terminadas(Estado, Ilhas_term),
    marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado_Aux),
    tira_ilhas_terminadas(Novo_estado_Aux, Ilhas_term, Novo_estado).

%-------------------------------------------------------------------------------
% 16 - junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado)
%-------------------------------------------------------------------------------

% To Do