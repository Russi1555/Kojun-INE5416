blusa(amarela).
blusa(azul).
blusa(branca).
blusa(verde).
blusa(vermelha).

nome(aline). %sem letra maiuscula
nome(carol). %sem letra maiuscula
nome(fernanda). %sem letra maiuscula
nome(juliana). %sem letra maiuscula
nome(natalia). %sem letra maiuscula

esqueceu(amaciante).
esqueceu(frutas).
esqueceu(leite).
esqueceu(pao). %lembrar de não usar acento em nada
esqueceu(presunto).

pagamento(cheque).
pagamento(credito). %sem acento
pagamento(debito). %sem acento
pagamento(dinheiro).
pagamento(vale).

foi_com(filho).
foi_com(irma). %sem acento
foi_com(mae). %sem acento
foi_com(marido).
foi_com(namorada).

carro(crossover).
carro(hatch).
carro(pickup).
carro(sedan).
carro(suv). %sem letra maiuscula

todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).


aEsquerda(X,Y,Lista) :- 
    nth0(IndexX,Lista,X), 
    nth0(IndexY,Lista,Y),
	IndexX < IndexY.

aDireita(X,Y,Lista) :- 
    aEsquerda(Y,X,Lista).

aoLado(X,Y,Lista) :- 
    nextto(X,Y,Lista);nextto(Y,X,Lista).

noCanto(X,Lista) :- last(Lista,X).
noCanto(X,[X|_]).
%caixa(blusa,nome,esqueceu,pagamento,foi_com,carro)

solucao(Solucao) :-
    Solucao = [
              caixa(Blusa1, Nome1, Esqueceu1, Pagamento1, Foi_com1, Carro1),
              caixa(Blusa2, Nome2, Esqueceu2, Pagamento2, Foi_com2, Carro2),
              caixa(Blusa3, Nome3, Esqueceu3, Pagamento3, Foi_com3, Carro3),
              caixa(Blusa4, Nome4, Esqueceu4, Pagamento4, Foi_com4, Carro4),
              caixa(Blusa5, Nome5, Esqueceu5, Pagamento5, Foi_com5, Carro5)
              ],
    %A mulher que esqueceu o Amaciante está exatamente à esquerda da que foi dirigindo um Sedan.
	aEsquerda(caixa(_, _, amaciante, _, _, _), caixa(_, _, _, _, _, sedan), Solucao),
    
    %Quem foi num Crossover está exatamente à direita de quem vai pagar no cartão de Débito.
    aDireita(caixa(_,_,_,_,_,crossover), caixa(_,_,_,debito, _, _), Solucao),
    
    %A mulher que foi com o Namorado foi fazer as compras dirigindo uma Pickup.
    member(caixa(_,_,_,_,namorado,pickup),Solucao),
    
    %A dona do Sedan está exatamente à esquerda da dona do SUV.
    aEsquerda(caixa(_, _, _, _, _, sedan), caixa(_, _, _, _, _, suv), Solucao),
    
    %Quem esqueceu o Pão foi com a Mãe.
    member(caixa(_,_,pao,_,mae,_), Solucao),
    
    %Aline está ao lado da mulher que foi ao supermercado com o filho
    aoLado(caixa(_,aline,_,_,_,_),caixa(_,_,_,_,filho,_), Solucao),

    %Em um dos caixas das pontas está a mulher que foi ao supermercado com o Marido
    noCanto(caixa(_,_,_,_,marido,_), Solucao),
    
    %No caixa da quarta posição está a mulher que vai pagar com Cheque.
    Pagamento4 = cheque,
    
    %Quem vai pagar com Dinheiro está em um dos caixas das pontas
    noCanto(caixa(_,_,_,dinheiro,_,_), Solucao),
    
    %A mulher que vai pagar com o cartão de Débito está exatamente à esquerda de quem vai pagar com Vale.
    aEsquerda(caixa(_,_,_,debito,_,_), caixa(_,_,_,vale,_,_), Solucao),
    
    %Juliana foi ao supermercado com a Mãe.
    member(caixa(_,juliana,_,_,mae,_),Solucao),
    
    %Quem esqueceu o Presunto vai pagar com o cartão de Débito.
    member(caixa(_,_,presunto,debito,_,_),Solucao),
    
    %A mulher da blusa Amarela está ao lado da que esqueceu as Frutas.
    aoLado(caixa(amarela,_,_,_,_,_), caixa(_,_,frutas,_,_,_), Solucao),
    
    %Quem esqueceu o Pão foi ao supermercado dirigindo um SUV.
    member(caixa(_,_,pao,_,_,suv), Solucao),
    
    %Fernanda foi para o supermercado com o Filho.
    member(caixa(_,fernanda,_,_,filho,_),Solucao),
    
    %Carol está exatamente à direita da mulher que esqueceu o Amaciante.
    aDireita(caixa(_,carol,_,_,_,_),caixa(_,_,amaciante,_,_,_),Solucao),        
            
    %A mulher da blusa Azul está no quarto caixa.
    Blusa4 = azul,
    
    %A mulher da blusa Verde está em algum lugar à esquerda da de blusa Vermelha.
    aEsquerda(caixa(verde,_,_,_,_,_), caixa(vermelha,_,_,_,_,_), Solucao),
    
    %Quem esqueceu as Frutas está ao lado de quem esqueceu o Presunto.
    aoLado(caixa(_,_,frutas,_,_,_), caixa(_,_,presunto,_,_,_), Solucao),
    
    %A mulher da blusa Amarela está em algum lugar entre a que foi com o Marido e a de blusa Verde, nessa ordem.
    aDireita(caixa(amarela,_,_,_,_,_), caixa(_,_,_,_,marido,_), Solucao),
    aEsquerda(caixa(amarela,_,_,_,_,_), caixa(verde,_,_,_,_,_), Solucao),
    
    %Quem vai pagar com Dinheiro está ao lado de quem foi de Sedan ao supermercado.
    aoLado(caixa(_,_,dinheiro,_,_,_), caixa(_,_,_,_,_,sedan), Solucao),
    
    blusa(Blusa1),
	blusa(Blusa2),
	blusa(Blusa3),
	blusa(Blusa4),
	blusa(Blusa5),
	todosDiferentes([Blusa1,Blusa2,Blusa3,Blusa4,Blusa5]),

	nome(Nome1),
	nome(Nome2),
	nome(Nome3),
	nome(Nome4),
	nome(Nome5),
	todosDiferentes([Nome1,Nome2,Nome3,Nome4,Nome5]),

	esqueceu(Esqueceu1),
	esqueceu(Esqueceu2),
	esqueceu(Esqueceu3),
	esqueceu(Esqueceu4),
	esqueceu(Esqueceu5),
	todosDiferentes([Esqueceu1,Esqueceu2,Esqueceu3,Esqueceu4,Esqueceu5]),
    
	pagamento(Pagamento1),
	pagamento(Pagamento2),
	pagamento(Pagamento3),
	pagamento(Pagamento4),
	pagamento(Pagamento5),
	todosDiferentes([Pagamento1,Pagamento2,Pagamento3,Pagamento4,Pagamento5]),
    
	foi_com(Foi_com1),
	foi_com(Foi_com2),
	foi_com(Foi_com3),
	foi_com(Foi_com4),
	foi_com(Foi_com5),
	todosDiferentes([Foi_com1,Foi_com2,Foi_com3,Foi_com4,Foi_com5]),
    
    carro(Carro1),
	carro(Carro2),
	carro(Carro3),
	carro(Carro4),
	carro(Carro5),
	todosDiferentes([Carro1,Carro2,Carro3,Carro4,Carro5]).
