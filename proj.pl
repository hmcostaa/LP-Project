:-[codigo_comum].

% Este predicado recebe o numero de uma determinada linha do puzzle, uma
% lista dessa linha com as varias ilhas contidas nela e devolve uma
% lista Ilhas com todas as ilhas dessa linha do puzzle onde tambem
% podemos observar a presenca do numero de pontes que essa ilha pode ter
% e tambem as suas coordenadas.
extrai_ilhas_linha(N_L, Linha, Ilhas) :- extrai_ilhas_linha(N_L, Linha, [], 1, Ilhas).

extrai_ilhas_linha(_, [], Ilhas, _, Ilhas).

extrai_ilhas_linha(N_L, [Ilha1|Resto], Aux, Col, Ilhas):-
 Ilha1 > 0,
 append(Aux,[ilha(Ilha1, (N_L, Col))], Novo_Aux),
 Novo_Col is Col + 1,
 extrai_ilhas_linha(N_L, Resto, Novo_Aux, Novo_Col, Ilhas).

extrai_ilhas_linha(N_L, [Ilha1|Resto], Aux, Col, Ilhas):-
 Ilha1 =< 0,
 Novo_Col is Col + 1,
 extrai_ilhas_linha(N_L, Resto, Aux, Novo_Col, Ilhas).

% O predicado ilhas recebe um puzzle e devolve uma lista ordenada
% (cima para baixo e da esquerda para a direita) dessas ilhas.
ilhas(Puz, Ilhas) :- ilhas(Puz, [], 1, Ilhas).

ilhas([], Ilhas, _, Ilhas).

ilhas([Ilha1|Resto], Aux, L, Ilhas):-
 extrai_ilhas_linha(L, Ilha1, Lista_Ilhas),
 append(Aux, Lista_Ilhas, Novo_Aux),
 L_Novo is L + 1,
 ilhas(Resto, Novo_Aux, L_Novo, Ilhas).

% Recebe a lista de ilhas ordenadas de um determinado puzzle, uma ilha
% pertencente a essa lista e devolve uma lista com todas as listas
% vizinhas a essa ilha.
% Ou seja, devolve as ilhas que se encontram mais proximas dessa
% determinada ilha por cima, direita, baixo e esquerda.
vizinhas(Ilhas, Ilha, Vizinhos):- vizinhas(Ilhas, Ilha, [], [], [], [], [], Vizinhos).

vizinhas([], _, [], [], [], [], Vizinhos, Vizinhos).

vizinhas([ilha(N2, (Linha2, Coluna2))|T], ilha(N, (Linha, Coluna)), C_C, L_T, L_F, C_B, Aux, Vizinhos):-
 Coluna2 == Coluna,
 Linha2 < Linha
 % Aplica se esta determinada ilha estiver na mesma coluna da ilha dada e tiver acima dela.
 ->
 Novo_C_C = [ilha(N2, (Linha2, Coluna2))],
 vizinhas(T, ilha(N, (Linha, Coluna)), Novo_C_C, L_T, L_F, C_B, Aux, Vizinhos);
 Linha2 == Linha,
 Coluna2 < Coluna
 % Aplica se esta determinada ilha estiver na mesma linha da ilha dada mas se encontrar a esquerda desta.
 ->
 Novo_L_T = [ilha(N2, (Linha2, Coluna2))],
 vizinhas(T, ilha(N, (Linha, Coluna)), C_C, Novo_L_T, L_F, C_B, Aux, Vizinhos);
 Linha2 == Linha,
 Coluna2 > Coluna,
 length(L_F, 0)
 % Aplica se esta determinada ilha se encontrar na mesma linha da ilha dada e estiver a direita desta sendo
 % que se a lista L_F ja tiver algum elemento la dentro ignora este procedimento.
 ->
 Novo_L_F = [ilha(N2, (Linha2, Coluna2))],
 vizinhas(T, ilha(N, (Linha, Coluna)), C_C, L_T, Novo_L_F, C_B, Aux, Vizinhos);
 Coluna2 == Coluna,
 Linha2 > Linha,
 length(C_B, 0)
 % Aplica se esta determinada ilha se encontrar na mesma coluna da ilha dada e estiver abaixo desta sendo
 % que se a lista C_B ja tiver algum elemnto la dentro ignora este procedimento.
 ->
 Novo_C_B = [ilha(N2, (Linha2, Coluna2))],
 vizinhas(T, ilha(N, (Linha, Coluna)), C_C, L_T, L_F, Novo_C_B, Aux, Vizinhos);
 vizinhas(T, ilha(N, (Linha, Coluna)), C_C, L_T, L_F, C_B, Aux, Vizinhos).

vizinhas([], ilha(N, (Linha, Coluna)), C_C, L_T, L_F, C_B, _, Vizinhos):-
 append([C_C, L_T, L_F, C_B], Novo_Aux),
 vizinhas([], ilha(N, (Linha, Coluna)), [], [], [], [], Novo_Aux, Vizinhos).

% O predicado estado recebe uma lista ordenada de ilhas e devolve uma
% lista de ilhas igualmente ordenadas que cada ilha foi transformada
% numa Entrada.
% Entrada = [ilha(N_Pontes, (Coordenadas)), [lista das vizinhas dessa
% ilha], [lista contendo as suas pontes]].
estado(Ilhas, Estado):- estado(Ilhas, Ilhas, [], Estado).

estado([], _, Estado, Estado).

estado([H|T], Ilhas2, Acc, Estado):-
 vizinhas(Ilhas2, H, Vizinhos),
 append(Acc, [[H, Vizinhos, []]], Novo_Acc),
 estado(T, Ilhas2, Novo_Acc, Estado).

% Recebe duas posicoes e devolve a lista Posicoes que contem as posicoes
% entre estas duas dadas.
% A primeira parte verifica se as posicoes estao na mesma linha.
posicoes_entre((X1, Y1), (X2, Y2), Posicoes):-
 X1 == X2,
 Y1 < Y2
 ->
 findall(Y, (between(Y1, Y2, Y), Y\== Y1, Y \== Y2), Possiveis),
 maplist(cria_posicoes_Y(X1), Possiveis, Posicoes);
 X1 == X2,
 findall(Y, (between(Y2, Y1, Y), Y\== Y1, Y \== Y2), Possiveis),
 maplist(cria_posicoes_Y(X1), Possiveis, Posicoes).

% A segunda parte verifica se as posicoes estao na mesma coluna.
posicoes_entre((X1, Y1), (X2, Y2), Posicoes):-
 Y1 == Y2,
 X1 < X2
 ->
 findall(X, (between(X1, X2, X), X\== X1, X \== X2), Possiveis),
 maplist(cria_posicoes_X(Y1), Possiveis, Posicoes);
 Y1 == Y2,
 findall(X, (between(X2, X1, X), X\== X1, X \== X2), Possiveis),
 maplist(cria_posicoes_X(Y1), Possiveis, Posicoes).

% Estas funcoes auxiliares como o proprio nome indica criam posicoes de
% acordo se pretendemos posicoes na mesma coluna ou na mesma linha.
cria_posicoes_Y(X1, Y, Coord):-
 Coord = (X1, Y).

cria_posicoes_X(Y1, X, Coord):-
 Coord = (X, Y1).

% Recebe duas posicoes e devolve uma ponte entre essas duas posicoes.
% Posicoes que se encontrem na mesma linha.
cria_ponte((X1, Y1), (X2, Y2), Ponte):-
 X1 == X2,
 Y1 < Y2
 ->
 Ponte = ponte((X1, Y1), (X2, Y2));
 X1 == X2,
 Ponte = ponte((X2, Y2), (X1, Y1)).

% Posicoes que se encontrem na mesma coluna.
cria_ponte((X1, Y1), (X2, Y2), Ponte):-
 Y1 == Y2,
 X1 < X2
 ->
 Ponte = ponte((X1, Y1), (X2, Y2));
 Y1 == Y2,
 Ponte = ponte((X2, Y2), (X1, Y1)).

% Recebe duas Posicoes, uma lista Ilhas com as posicoes ordenadas entre
% as duas posicoes dadas, recebe uma determinada Ilha e uma das suas
% vizinhas.
% O objetivo do predicado e verificar se criando uma ponte entre as duas
% posicoes dadas, a ilha dada e a sua vizinha tem alguma ponte
% atravessada entre elas, ou seja, se o caminho entre elas e livre ou
% nao.
caminho_livre(Pos1, Pos2, _, ilha(N, Pos1), ilha(N, Pos2)).

caminho_livre(Pos1, Pos2, _, ilha(N, Pos2), ilha(N, Pos1)).

caminho_livre(_, _, Ilhas, ilha(N, (X1, Y1)), ilha(N, (X2, Y2))):-
 posicoes_entre((X1, Y1), (X2, Y2), Outras_Posicoes),
 findall(X, (member(X, Outras_Posicoes), member(X, Ilhas)), Intrusos),
 length(Intrusos, 0).

% Este predicado recebe duas posicoes, uma lista de posicoes ordendas
% entre essas duas, uma Entrada e devolve essa mesma Entrada removendo
% da primeira lista dessa Entrada as ilhas que deixaram de ser vizinhas
% do primeiro elemento dessa Entrada devido a criacao de uma ponte entre
% as duas posicoes dadas.
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [ilha(N, (X1, Y1)), [H|T], Ponte], Nova_Entrada):-
 actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [ilha(N, (X1, Y1)), [H|T], Ponte], [], Nova_Entrada).

actualiza_vizinhas_entrada(_, _, _, [ilha(N, (X1, Y1)), [], Ponte], Acc, Nova_Entrada):-
 Nova_Entrada = [ilha(N, (X1, Y1)), Acc, Ponte].


actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [ilha(N, (X1, Y1)), [H|T], Ponte], Acc, Nova_Entrada):-
 caminho_livre(Pos1, Pos2, Posicoes, ilha(_, (X1, Y1)), H),
 append(Acc, [H], Novo_Acc),
 actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [ilha(N, (X1, Y1)), T, Ponte], Novo_Acc, Nova_Entrada).

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [ilha(N, (X1, Y1)), [_|T], Ponte], Acc, Nova_Entrada):-
 actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [ilha(N, (X1, Y1)), T, Ponte], Acc, Nova_Entrada).

% Utilizando as funcionalidades do predicado anterior este predicado vai
% receber um Estado e aplicar o predicado anterior a cada Entrada
% contida nesse Estado de forma a remover todas as vizinhas que foram
% afetadas pela criacao de uma ponte entre as posicoes Pos1 e Pos2
% dadas.
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado):-
 actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, [], Novo_Estado).

actualiza_vizinhas_apos_pontes([], _, _, Novo_Estado, Novo_Estado).

actualiza_vizinhas_apos_pontes([H|T], Pos1, Pos2, Acc, Novo_Estado):-
 posicoes_entre(Pos1, Pos2, Posicoes),
 actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, H, Nova_Entrada),
 append(Acc, [Nova_Entrada], Novo_Acc),
 actualiza_vizinhas_apos_pontes(T, Pos1, Pos2, Novo_Acc, Novo_Estado).

% Recebe um Estado e devolve uma lista com o primeiro elemento de cada
% Entrada (uma ilha), que esteja terminada.
% Ilha terminda = Ilha em que o numero que corresponde ao numero de
% pontes que essa ilha pode ter for igual ao numero de pontes na Entrada
% em que essa Ilha encontra.
ilhas_terminadas(Estado, Ilhas_term):-
 ilhas_terminadas(Estado, [], Ilhas_term).

ilhas_terminadas([], Ilhas_term, Ilhas_term).

% Sao ignoradas as Ilhas que ao inves de um numero indicando o numero de
% pontes que essa ilha pode ter, tem um X.
ilhas_terminadas([[ilha('X', _), _, _]|T], Acc, Ilhas_term):-
 ilhas_terminadas(T, Acc, Ilhas_term).

ilhas_terminadas([[ilha(N, (X1, Y1)),_, Pontes]|T], Acc, Ilhas_term):-
 length(Pontes, Comprimento),
 N == Comprimento
 ->
 append(Acc, [ilha(N,(X1, Y1))], Novo_Acc),
 ilhas_terminadas(T, Novo_Acc, Ilhas_term);
 ilhas_terminadas(T, Acc, Ilhas_term).

% Recebe Ilhas_term que e uma lista com ilhas terminadas e recebe uma
% Entrada.
% O resultado deste predicado e uma Nova_Entrada em que as Ilhas em
% Vizinhas coincidam com as ilhas presentes na lista Ilhas_term sejam
% removidas da lista Vizinhas.
tira_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vizinhas, Pontes], Nova_Entrada):-
 findall(X, (member(X, Vizinhas), \+member(X, Ilhas_term)), Acc),
 Nova_Entrada = [Ilha, Acc, Pontes].

% Utilizando as funcionalidades do predicado anterior este predicado vai
% aplicar o mesmo processo de remocao de Ilhas termindas as Vizinhas de
% cada Entrada deste Estado.
tira_ilhas_terminadas(Estado, IlhasTerm, Novo_Estado):-
 tira_ilhas_terminadas(Estado, IlhasTerm, [], Novo_Estado).

tira_ilhas_terminadas([], _, Novo_Estado, Novo_Estado).

tira_ilhas_terminadas([H|T], IlhasTerm, Acc, Novo_Estado):-
 tira_ilhas_terminadas_entrada(IlhasTerm, H, Novo_H),
 append(Acc, [Novo_H], Novo_Acc),
 tira_ilhas_terminadas(T, IlhasTerm, Novo_Acc, Novo_Estado).

% Este predicado recebe mais uma vez a lista das ilhas terminadas, uma
% Entrada e vai devolver uma Nova_Entrada.
% Esta nova Entrada apenas se alterara se a ilha (primeiro elemento)
% dessa Entrada pertenca a lista Ilhas_term, nesse caso o valor de N
% deve ser alterado para um 'X'.
marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N,(X1, Y1))|T], Nova_Entrada):-
 member(ilha(N,(X1, Y1)), Ilhas_term)
 ->
 append([ilha('X',(X1, Y1))], T, Nova_Entrada);
 Nova_Entrada = [ilha(N,(X1, Y1))|T].

% Utilizando as funcionalidades do predicado anterior este predicado vai
% aplicar o mesmo processo de marcacao com o 'X' nas ilhas que se
% verifiquem pertencentes as Ilhas_term, a todas as Entradas contidas em
% Estado devolvendo o Novo_Estado em que cada Entrada sofreu as
% alteracoes necessarias.
marca_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado):-
 marca_ilhas_terminadas(Estado, Ilhas_term, [], Novo_Estado).

marca_ilhas_terminadas([], _, Novo_Estado, Novo_Estado).

marca_ilhas_terminadas([H|T], Ilhas_term, Acc, Novo_Estado):-
 marca_ilhas_terminadas_entrada(Ilhas_term, H, Aux),
 append(Acc, [Aux], Novo_Acc),
 marca_ilhas_terminadas(T, Ilhas_term, Novo_Acc, Novo_Estado).

% Este predicado utiliza as funcionalidades dos predicados
% tira_ilhas_terminadas e marca_ilhas_terminadas e aplica cada uma delas
% a todas as Entradas em Estado, sendo o Novo_Estado a lista de Entradas
% resultantes da aplicacao desses dois predicados a cada uma dessas
% Entradas.
trata_ilhas_terminadas(Estado, Novo_Estado):-
 ilhas_terminadas(Estado, Terminadas),
 tira_ilhas_terminadas(Estado, Terminadas, Fora_Terminadas),
 marca_ilhas_terminadas(Fora_Terminadas, Terminadas, Novo_Estado).

% O predicado junta_pontes recebe um Estado, Ilha1 e Ilha2 (que sao duas
% ilhas), Num_Pontes que e o numero de pontes a adicionar entre essas
% duas Ilhas e devolve Novo_Estado que e o resultado da adicao das
% pontes as Entradas necessarias em Estado.
% Para isso recorreu-se a um predicado auxiliar, adiciona_pontes.
adiciona_pontes(Estado, Ponte, Num_Pontes, Ilha1, Ilha2, Novo_Estado):-
 adiciona_pontes(Estado, Ponte, Num_Pontes, Ilha1, Ilha2, [], Novo_Estado).

adiciona_pontes([], _, _, _, _, Novo_Estado, Novo_Estado).

% Esta parte do predicado verifica se a Entrada identificada em Estado e
% uma das Ilhas a aplicar a adicao de pontes, neste caso se e igual a
% Ilha1. Se vericado a igualdade, procede-se a criacao de uma lista com
% 'Num_Pontes' elementos sendo que esses elementos sao igual a Ponte.
% De seguida junta-se essa lista com a lista ja existente na Entrada que
% guardava as Pontes criando a lista Nova_Ponte10 que de seguida e
% introduzida na Entrada.
adiciona_pontes([[ilha(N, (X, Y)), Vz, Ponte10]|T], Ponte, Num_Pontes, Ilha1, Ilha2, Acc, Novo_Estado):-
 ilha(N, (X, Y)) == Ilha1,
 length(L, Num_Pontes),
 maplist(=(Ponte), L),
 append(Ponte10, L, Nova_Ponte10),
 append(Acc, [[ilha(N, (X, Y)), Vz, Nova_Ponte10]], Novo_Acc),
 adiciona_pontes(T, Ponte, Num_Pontes, Ilha1, Ilha2, Novo_Acc, Novo_Estado).

% Esta parte do predicado tem a mesma funcionalidade da parte anterior
% mas desta vez verica se a Entrada identificada em Estado contem a ilha
% igual a Ilha2 que por consequencia vai ser aplicado os mesmos
% procedimentos que seriam aplicados na parte anterior.
adiciona_pontes([[ilha(N, (X, Y)), Vz, Ponte10]|T], Ponte, Num_Pontes, Ilha1, Ilha2, Acc, Novo_Estado):-
 ilha(N, (X, Y)) == Ilha2,
 length(L, Num_Pontes),
 maplist(=(Ponte), L),
 append(Ponte10, L, Nova_Ponte10),
 append(Acc, [[ilha(N, (X, Y)), Vz, Nova_Ponte10]], Novo_Acc),
 adiciona_pontes(T, Ponte, Num_Pontes, Ilha1, Ilha2, Novo_Acc, Novo_Estado).

% Se a Entrada identificada em Estado nao for ilegivel para alteracao,
% ou seja, a Ilha contida nessa Entrada nao for igual nem a Ilha1 nem a
% Ilha2, nao se procede a nenhuma alteracao a Entrada e apenas e
% adiciona a lista Novo_Acc que e a lista que por agora vai armazenar as
% Entradas.
adiciona_pontes([[ilha(N, (X, Y)), Vz, Ponte10]|T], Ponte, Num_Pontes, Ilha1, Ilha2, Acc, Novo_Estado):-
 append(Acc, [[ilha(N, (X, Y)), Vz, Ponte10]], Novo_Acc),
 adiciona_pontes(T, Ponte, Num_Pontes, Ilha1, Ilha2, Novo_Acc, Novo_Estado).

% O predicado junta_pontes ja explicado anteriormente vai depois de
% adicionar_pontes recorrer a dois outros predicados,
% actualiza_vizinhas_apos_pontes e trata_ilhas_terminadas cujas
% funcionalidades ja foram explicadas anteriormente, devolvendo
% finalmente Novo_Estado depois de aplicado a todas as Entradas estes
% dois ultimos predicados mencionados.
junta_pontes(Estado, Num_Pontes, ilha(N1,(X1, Y1)), ilha(N2,(X2, Y2)), Novo_Estado):-
 cria_ponte((X1, Y1), (X2, Y2), Ponte),
 adiciona_pontes(Estado, Ponte, Num_Pontes, ilha(N1,(X1, Y1)), ilha(N2,(X2, Y2)), Aux),
 actualiza_vizinhas_apos_pontes(Aux, (X1, Y1), (X2, Y2), Novo_Aux),
 trata_ilhas_terminadas(Novo_Aux, Novo_Estado).
