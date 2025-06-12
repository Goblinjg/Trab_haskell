# Trab_haskell
trabalho fundamentado em funções de programação em haskell


>>3. unica_ocorrencia: recebe um elemento e uma lista e verifica se existe uma única ocorrência 
>>do elemento na lista.
>>ex.: unica_ocorrencia 2 [1,2,3,2]   False⇒
>>unica_ocorrencia 2 [3,1]   False⇒
>>unica_ocorrencia 2 [2]   True

>>6. remove: recebe um elemento e uma lista e retorna a lista sem a primeira ocorrência do 
>>elemento (se o elemento não estiver na lista, não há resposta possível).

>>9. gera_sequencia: recebe um número inteiro n positivo e retorna a lista [1,-1,2,-2,3,-3, ... ,n,-n]

>>12. reverso: recebe uma lista e retorna outra, que contém os mesmos elementos da primeira, em 
>>ordem contrária.

>>15. somatorio: recebe uma lista de números e retorna a soma deles.
>>ex.: somatorio [] => 0
>>somatorio [2,3] => 5 

>>18. interseccao: recebe duas listas sem elementos repetidos e retorna uma lista com os 
>>elementos que são comuns às duas.
>>ex.:  interseccao [3,6,5,7] [9,7,5,1,3]   [3,5,7]

>>21. ordenada: recebe uma lista e verifica se seus itens estão ordenados (ordem crescente).
>>ex.: ordenada [3,7,7,8,9]   True

>>24. picos: recebe uma lista de números e retorna os números que são maiores que seus vizinhos. 
>>Considere que a lista é circular, ou seja, o início e o fim estão ligados.
>>ex.: picos [2,3,5,10,5,5,6,2,3]   [10,6,3] 

>>27. todas_maiusculas: Recebe uma string qualquer e retorna outra string onde todas as letras são 
>>maiúsculas. Pode ser útil saber os seguintes códigos de representação de caracteres: a=97, 
>>z=122, A=65, Z=90, 0=48, 9=57, espaço=32. 
>>ex.: todas_maiusculas "abc 123" = "ABC 123"

>>30. variancia: recebe uma lista de números e retorna a variância (populacional) deles.
>>ex.: variancia [6,2,9,0,8,3,0,2]   10.6875

>>33. separa: separa os elementos de uma lista de números nas posições com zero.
>>ex.: separa [3,4,7,-1,0,4,7,3,0,0,9,8]   [[3,4,7,-1],[4,7,3],[],[9,8]] 

>>36. soma_digitos: recebe um número natural e retorna a soma de seus dígitos.
>>ex.: soma_digitos 328464584658   63 ⇒

>>39. Dizemos que um quadrado perfeito é um número cuja raiz quadrada é um número inteiro. 
>>Sabemos o que a raiz quadrada é um cálculo lento quando comparado à operações como 
>>adição ou multiplicação. Implemente uma função que verifica se um número é um quadrado 
>>perfeito sem usar uma função que calcula raiz quadrada
