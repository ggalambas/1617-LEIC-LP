% 86430 GUILHEMRE GALAMBAS

% LOGICA PARA PROGRAMACAO
% 2016/2017 - 2o SEMESTRE
% SOLUCIONADOR DE PROBLEMAS DE SUDOKU

:- include('SUDOKU').


%/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
%
%	PREDICADOS PARA A PROPAGACAO DE MUDANCAS
%
%\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


%		tira_num_aux/4
% N_Puz e' o puzzle resultante de tirar Num da posicao Pos do puzzle Puz.

tira_num_aux(Num, Puz, Pos, N_Puz) :-
	puzzle_ref(Puz, Pos, Cont),
	member(Num, Cont), !,
	subtract(Cont, [Num], N_Cont),
	puzzle_muda_propaga(Puz, Pos, N_Cont, N_Puz).

tira_num_aux(_, Puz, _, Puz).												/* Se Num nao pertence ao conteudo da posicao Pos, N_Puz = Puz */


%		tira_num/4
% N_Puz e' o puzzle resultante de tirar Num de todas as posicoes do puzzle Puz.

tira_num(Num, Puz, Posicoes, N_Puz) :-
	percorre_muda_Puz(Puz, tira_num_aux(Num), Posicoes, N_Puz).				/* Aplica tira_num_aux/4 a todas a posicoes da lista Posicoes */


%		puzzle_muda_propaga/4
% Substitui o conteudo da posicao Pos do puzzle Puz por Cont
% No caso de Cont ser uma lista unitaria, retira o numero em Cont de todas as posicoes relacionadas.

puzzle_muda_propaga(Puz, _, [], Puz) :- !. 									/* Se Cont for uma lista vazia, N_Puz = Puz */

puzzle_muda_propaga(Puz, Pos, Cont, N_Puz) :-
	length(Cont, 1), !,
	puzzle_muda(Puz, Pos, Cont, Puz_Aux),
	posicoes_relacionadas(Pos, Posicoes),
	nth0(0, Cont, Num),														/* Num = Conteudo da lista unitaria Cont */
	tira_num(Num, Puz_Aux, Posicoes, N_Puz). 								/* Propaga a mudanca */

puzzle_muda_propaga(Puz, Pos, Cont, N_Puz) :-								/* Se Cont nao for uma lista unitaria, a mudanca nao e' propagada */
	puzzle_muda(Puz, Pos, Cont, N_Puz).


%/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
%
%		PREDICADOS PARA A INICIALIZACAO DE PUZZLES
%
%\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


%		possibilidades/3
% Poss e' a lista de numeros possiveis para a posicao Pos do puzzle Puz.

possibilidades(Pos, Puz, Poss) :-
	puzzle_ref(Puz, Pos, Poss),												/* Caso seja o conteudo das posicao Pos de Puz uma lista unitaria, Poss = Cont */
	length(Poss, 1), !.

possibilidades(Pos, Puz, Poss) :-
	posicoes_relacionadas(Pos, Posicoes),
	conteudos_posicoes(Puz, Posicoes, Conteudos),							/* Ve o conteudo das posicoes relacionadas com Pos */
	transf_em_unitaria(Conteudos, N_Conteudos),								/* Retira os conteudos nao unitarios da lista */
	append(N_Conteudos, L_Conteudos),
	numeros(L),																/* L = Lista com todas os conteudos possiveis */
	subtract(L, L_Conteudos, Poss).											/* Retira da lista L todas os conteudos das posicoes relacionadas, decobre Poss */


%		transf_em_unitaria/2
% Auxiliar que transforma uma lista de conteudos numa lista de conteudos unitarios.

transf_em_unitaria([], []) :- !.											/* Quando todos os conteudos tiverem sido vistos, acaba */

transf_em_unitaria([P|R], [P|N_R]) :-
	length(P, 1), !,														/* Se a lista for unitaria, adiciona a nova lista de conteudos */
	transf_em_unitaria(R, N_R).

transf_em_unitaria([_|R], Cont) :-											/* Se a lista nao for unitaria, passa para a proxima */ 
	transf_em_unitaria(R, Cont).


%		inicializa_aux/3
% N_Puz e' o puzzle resultante de colocar na posicao Pos de Puz a lista de possibilidades.

inicializa_aux(Puz, Pos, N_Puz) :-
	possibilidades(Pos, Puz, Poss),
	puzzle_muda_propaga(Puz, Pos, Poss, N_Puz).								/* Propaga a mudanca */


%		inicializa/2
% N_Puz e' o puzzle resultante de inicializar o puzzle Puz.

inicializa(Puz, N_Puz) :-
	todas_posicoes(Posicoes),												/* Lista com todas as posicoes do puzzle */
	percorre_muda_Puz(Puz, inicializa_aux, Posicoes, N_Puz).


%/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
%
%		PREDICADOS PARA A INSPECCAO DE PUZZLES
%
%\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


%		so_aparece_uma_vez/4
% Ve se Num so aparece uma vez numa das posicoes da lista Posicoes do puzzle Puz.
% Pos_Num e' a posicao de Num.

so_aparece_uma_vez(Puz, Num, Posicoes, Pos_Num) :-
	conteudos_posicoes(Puz, Posicoes, Conteudos),							/* Lista de conteudos das posicoes inseridas */
	encontra_num(Num, Conteudos, Posicoes, Num_Rep, Pos_Num),				/* Procura por Num e guarda as posicoes onde aparece */
	Num_Rep =:= 1.															/* Testa se so aparece uma vez */


%		encontra_num/5
% Auxiliar que procura por um numero.
% Retorna as posicoes e o numero de vez que aparece.

encontra_num(_, [], [], 0, _) :- !.										 	/* Quando todos os conteudos tiverem sido vistos, acaba */

encontra_num(Num, [PC|RC], [PP|RP], Num_Rep, Pos_Num) :-
	member(Num, PC), !,
	Pos_Num = PP,															/* Guarda a posicao onde apareceu o numero */
	encontra_num(Num, RC, RP, Num_Rep1, Pos_Num),
	Num_Rep is Num_Rep1 + 1.												/* Guarda o numero de vezes que este apareceu */

encontra_num(Num, [_|RC], [_|RP], Num_Rep, Pos_Num) :-						/* Se o numero nao estiver num conteudo, procura no proximo */
	encontra_num(Num, RC, RP, Num_Rep, Pos_Num).


%		inspecciona_num/4
% N_Puz e' o resultado de inspeccionar o grupo das posicoes dadas.
% Conteudo da posicao e' mudado para Num.

inspecciona_num(Posicoes, Puz, Num, N_Puz) :-
	so_aparece_uma_vez(Puz, Num, Posicoes, Pos_Num),						/* Testa se Num so ocorre numa das posicoes */
	puzzle_muda_propaga(Puz, Pos_Num, [Num], N_Puz), !.						/* Muda o conteudo para Num e propaga a mudanca */

inspecciona_num(_, Puz, _, Puz).											/* Caso nao ocorra uma vez ou o conteudo for unitario, N_Puz = Puz */


%		inspecciona_grupo/3
% Inspecciona as posicoes Gr de Puz para cada um dos numeros possiveis, resultando N_Puz.

inspecciona_grupo(Puz, Gr, N_Puz) :-
	numeros(L),																/* L = Lista com todas os conteudos possiveis */
	percorre_lista(Puz, L, Gr, N_Puz).										/* Aplica insecciona_num/4 a todos os numeros de L */ 


%		percorre_lista/4
% Aplica insecciona_num/4 a todos os numeros da lista dada.

percorre_lista(Puz, [], _, Puz) :- !.										/* Quando todos os numero tiverem sido vistos, acaba */

percorre_lista(Puz, [P|R], Gr, N_Puz) :-
	inspecciona_num(Gr, Puz, P, Puz_Aux),
	percorre_lista(Puz_Aux, R, Gr, N_Puz).


%		inspecciona/2
% Inspecciona cada um dos grupos existente em Puz para cada um dos numero possiveis.

inspecciona(Puz, N_Puz) :-
	grupos(Gr),																/* Gr = Lista com todas os grupos existentes */
	percorre_muda_Puz(Puz, inspecciona_grupo, Gr, N_Puz).					/* Aplica inspecciona_grupo/3 a todos os grupos de Gr */


%/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
%
%		PREDICADOS PARA A VERIFICACAO DE SOLUCOES
%
%\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


%		grupo_correcto/3
% Significa que o grupo de Puz cujas posicoes sao de Gr contem todos os numeros de Nums.

grupo_correcto(Puz, Nums, Gr) :-
	conteudos_posicoes(Puz, Gr, Conteudos),									/* Lista de conteudos das posicoes de Gr */
	transf_em_unitaria(Conteudos, N_Conteudos),								/* Retira os conteudos nao unitarios da lista */
	append(N_Conteudos, L_Conteudos),
	msort(L_Conteudos, Nums).												/* Confirma se todos os numeros de Nums estao presentes */


%		solucao/2
% Testa se todos os grupos de Puz contem todos os numeros possiveis sem repeticoes.

solucao(Puz) :-
	numeros(L),																/* L = Lista com todas os conteudos possiveis */
	grupos(Gr),																/* Gr = Lista com todas os grupos existentes */
	chama_gr_correcto(Puz, L, Gr).											/* Aplica grupo_correcto/3 para todos os grupos de Gr */


%		chama_gr_correcto/3
% Auxiliar que chama grupo_correcto/3 para todos os grupos de Gr.

chama_gr_correcto(_, _, []) :- !.											/* Quando todos os grupos tiverem sido vistos, acaba */

chama_gr_correcto(Puz, L, [P|R]) :-
	grupo_correcto(Puz, L, P),
	chama_gr_correcto(Puz, L, R).


%/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
%
%		PREDICADO RESOLVE
%
%\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


%		resolve/2
% Sol e' uma solucao do puzzle Puz.

resolve(Puz, Sol) :-
	inicializa(Puz, Puz_Aux),												/* Inicializa o puzzle */
	inspecciona(Puz_Aux, N_Puz),											/* Inspeciona os grupos do puzzle */
	resolve_aux(N_Puz, Sol).												/* Procura uma solucao */


%		resolve_aux/2
% Auxiliar que procura uma solucao para o puzzle.

resolve_aux(Puz, Puz) :-
	solucao(Puz), !.														/* Teste se o puzzle esta terminado */

resolve_aux(Puz, Sol) :-
	todas_posicoes(Posicoes),
	encontra_pos(Puz, Posicoes, Pos),										/* Encontra uma posicao nao unitaria no puzzle */
	puzzle_ref(Puz, Pos, Cont),
	member(Num, Cont),														/* Testa com todos os numeros pertencentes a posicao encontrada */
	puzzle_muda_propaga(Puz, Pos, [Num], N_Puz),							/* Muda o conteudo e propaga a mudanca */
	resolve_aux(N_Puz, Sol).


%		encontra_pos/3
% Auxiliar que devolve a primeira posicao nao unitaria do puzzle.

encontra_pos(Puz, [P|_], P) :-
	puzzle_ref(Puz, P, Cont),
	\+length(Cont, 1), !.

encontra_pos(Puz, [_|R], Pos) :-
	encontra_pos(Puz, R, Pos).												/* Caso seja unitaria, passa para a proxima posicao */
