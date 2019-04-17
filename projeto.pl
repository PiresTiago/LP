%|/////////\\\\\\\\\\|
%|					 |
%|Tiago Pires N89544 |
%| 					 |
%|\\\\\\\\\//////////|

%-------------------------------------------------------------------------------
% PROPAGA

% O predicado_aux encontra a lista que contem a posicao(Pos) dada no predicado 
% propaga. O predicado_n_elementos remove as posicoes da lista que se encontram 
% depois de Pos, pois a propagacao e feita do inicio do termometro ate Pos.
%-------------------------------------------------------------------------------

propaga([H|_],Pos,Posicoes):-
    propaga_aux(H,Pos,Temp),
    nth1(Index_Pos,Temp,Pos),
    length(Temp,Len),
    N is Len-Index_Pos,
	remove_n_posicoes(N, Temp, Temp_Sem_N),
    sort(Temp_Sem_N,Posicoes).

propaga_aux([],_,_).
propaga_aux([H|T],Pos,Temp):- 
    member(Pos,H),Temp=H;
    propaga_aux(T,Pos,Temp).

remove_n_posicoes(N, Lista1, Lista2) :-  
    remove_n_posicoes_aux(N, Lista1, Lista2).
remove_n_posicoes_aux(N, Lista1, []) :-
    length(Lista1, N).
remove_n_posicoes_aux(N, [H|T], [H|T1]) :-
    remove_n_posicoes_aux(N, T, T1).


%-------------------------------------------------------------------------------
% NAO_ALTERA_LINHAS ANTERIORES

% O predicado nao_altera_aux verifica se as posicoes de linhas anteriores da  
% lista Posicoes se encontram na lista das posicoes Ja_Preenchidas,se sim, 
% o predicado retorna True pois nao e feita nenhuma alteracao `as linhas  
% anteriores, caso contrario retorna False.
%-------------------------------------------------------------------------------

nao_altera_linhas_anteriores([],_,_).
nao_altera_linhas_anteriores([H|T],L,Ja_Preenchidas):-
	nao_altera_aux(H,L,Ja_Preenchidas),
	nao_altera_linhas_anteriores(T,L,Ja_Preenchidas).

nao_altera_aux((X,Y),L,Ja_Preenchidas):-
	X<L,
	member((X,Y),Ja_Preenchidas).
nao_altera_aux((X,_),L,_):-
	X>=L.


%-------------------------------------------------------------------------------
% VERIFICA_PARCIAL

% O predicado verifica_parcial_aux conta quantas posicoes da Lista_posicoes
% pertencem a determinada coluna e depois e feita uma comparacao no predicado
% verifica_parcial com o maximo de posicoes para essa coluna de forma a verificar 
% se nenhuma coluna fica com um numero de posicoes preenchidas superior ao seu total.
%-------------------------------------------------------------------------------

verifica_parcial([_,_,C],Ja_Preenchidas,_,Poss):-
	union(Ja_Preenchidas,Poss,Lista_posicoes),
	verifica_parcial([_,_,C],Lista_posicoes, 1).

verifica_parcial([_,_,[]],_,_).
verifica_parcial([_,_,[H|T]],Lista_posicoes, Coluna):-
	Total_Coluna=H,
	verifica_parcial_aux(Coluna,Lista_posicoes,0,Num_pos_coluna),!,
	Num_pos_coluna =< Total_Coluna,
	Coluna1 is Coluna + 1,
	verifica_parcial([_,_,T],Lista_posicoes,Coluna1).

verifica_parcial_aux(_,[],Cnt_pos_aux,Cnt_pos_aux).
verifica_parcial_aux(Coluna,[H|T],Cnt_pos_aux,Num_pos_coluna):-
	((_,Y)=H,
	Y=Coluna,
	Cnt_pos_aux1 is Cnt_pos_aux + 1,
	verifica_parcial_aux(Coluna,T,Cnt_pos_aux1,Num_pos_coluna));
	verifica_parcial_aux(Coluna, T, Cnt_pos_aux,Num_pos_coluna).


%-------------------------------------------------------------------------------
% POSSIBILIDADES_LINHA

% O predicado combinacao e o predicado findall criam uma lista com todas as
% combinacoes de posicoes possiveis para uma dada linha.
% O predicado possibilidades_aux_pos cria uma Lista com as propagacoes de
% cada posicao de uma dada combinacao.
% O predicado possibilidades_aux faz as verificacoes `a lista de propagacoes
% (de uma combinacao), caso a lista passe nas verificacoes, e uma possibilidade  
% valida e e adicionada `a lista Possibilidades_L.
%-------------------------------------------------------------------------------

possibilidades_linha(Puz,Posicoes_linha,Total,Ja_Preenchidas,
	Sorted_Possibilidades_L):-
	findall(Posicao_random,combinacao(Total,Posicoes_linha,Posicao_random), 
		Combinacoes),
	[Primeira_pos|_]=Posicoes_linha,
	(L,_)=Primeira_pos,
	possibilidades_aux(Puz,L,Total,Combinacoes,Ja_Preenchidas,[],
		Possibilidades_L),
	sort(Possibilidades_L,Sorted_Possibilidades_L).

possibilidades_aux_pos(_,[],Posicoes_propaga,Posicoes_propaga).
possibilidades_aux_pos(Puz,[H|T],Posicoes_Aux,Lista):-
	propaga(Puz,H,Posicoes),!,
	union(Posicoes,Posicoes_Aux,Posicoes_propaga),
	possibilidades_aux_pos(Puz,T,Posicoes_propaga,Lista).

possibilidades_aux(_,_,_,[],_,Possibilidades_L_aux,Possibilidades_L_aux).
possibilidades_aux(Puz,L,Total,[H|T],Ja_Preenchidas,Lista_aux,
	Possibilidades_L):-
	possibilidades_aux_pos(Puz,H,[],Lista_propaga),
	findall((L,X),member((L,X),Lista_propaga),Lista_elem_Linha_L),
	findall((L,X),member((L,X),Ja_Preenchidas),Lista_elem_Ja_Pree),
	union(Lista_elem_Ja_Pree,Lista_elem_Linha_L,Lista_com_elem_L),
	length(Lista_com_elem_L,N_elem_L),
	N_elem_L == Total,
	nao_altera_linhas_anteriores(Lista_propaga,L,Ja_Preenchidas),
	verifica_parcial(Puz,Ja_Preenchidas,_,Lista_propaga),
	sort(Lista_propaga,Sorted_lista_propaga),
	append([Sorted_lista_propaga],Lista_aux,Possibilidades_L_aux),
	possibilidades_aux(Puz,L,Total,T,Ja_Preenchidas,Possibilidades_L_aux,
		Possibilidades_L);
	possibilidades_aux(Puz,L,Total,T,Ja_Preenchidas,Lista_aux,Possibilidades_L).

combinacao(0, _, []).
combinacao(N, L, [E | C_L_E]) :-
	N > 0,
	append(_, [E | L_apos_E], L),
	N_1 is N - 1,
	combinacao(N_1, L_apos_E, C_L_E).


%-------------------------------------------------------------------------------
% RESOLVE

% O predicado resolve calcula as variaveis necessarias para a execucao do
% predicado possibilidades_linha calculando depois com o auxilio do predicado
% member a solucao do puzzle.
%-------------------------------------------------------------------------------

resolve(Puz,Solucao):-
	[_,L,C]=Puz,
	length(C,Num_Colunas),
	length(L,Linha_Max_Aux),
	Linha_Maxima is Linha_Max_Aux+1,
	resolve(Puz,Solucao,Num_Colunas,Linha_Maxima,1,[]).

resolve(_,Ja_Preenchidas_sorted,_,Linha_Maxima,Linha_Maxima,
	Ja_Preenchidas_sorted).
resolve(Puz,Solucao,Num_Colunas,Linha_Maxima,Linha,Ja_Preenchidas):-
	[_,L,_]=Puz,
	nth1(Linha,L,Total),
	descobre_Posicoes_Linha_aux(Num_Colunas,Linha,[],Posicoes_linha),
	possibilidades_linha(Puz,Posicoes_linha,Total,Ja_Preenchidas,
		Possibilidades_L),!,
	member(Uma_Possibilidade,Possibilidades_L),
	sort(Uma_Possibilidade,Uma_Possibilidade_sorted),
	union(Uma_Possibilidade_sorted,Ja_Preenchidas,Ja_Preenchidas_Aux),
	sort(Ja_Preenchidas_Aux,Ja_Preenchidas_sorted),
	Proxima_Linha is Linha+1,
	resolve(Puz,Solucao,Num_Colunas,Linha_Maxima,Proxima_Linha,
		Ja_Preenchidas_sorted).

descobre_Posicoes_Linha_aux(0,_,Posicoes_linha_temp,Posicoes_linha_temp).
descobre_Posicoes_Linha_aux(Num_Colunas,Linha,Posicoes_linha_aux,Posicoes_linha):-
	append([(Linha,Num_Colunas)],Posicoes_linha_aux,Posicoes_linha_temp),
	Num_Colunas_Aux is Num_Colunas-1,
	descobre_Posicoes_Linha_aux(Num_Colunas_Aux,Linha,Posicoes_linha_temp,
		Posicoes_linha).