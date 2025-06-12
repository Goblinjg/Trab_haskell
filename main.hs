-- Grupo:
-- João Gabriel Salomão Baldim
-- José Victor Miranda de Oliveira
-- Questões grupo 0 (3,6,9,12,15,18,21,24,27,30,33,36,39)

-- Funções Auxliares
estanalista:: Eq f => f -> [f] -> Bool 
estanalista _ [] = False
estanalista e (ahead:atail)
    |e == ahead = True
    |otherwise = estanalista e atail
    -- Verifica se um elemento "e" esta numa lista (ahead:atail)

xMod :: (Ord f,Num f)  => f -> f -> f 
xMod x y
    | x < y     = x
    | otherwise = xMod (x - y) y
    -- Função pra calculo de Mod, executa o algoritmo da divisão até que o valor
    -- Seja inferior ao divisor, ai retorna o resto

xDiv :: (Ord f,Num f)  => f -> f -> f
xDiv x y
    | x < y     = 0
    | otherwise = 1 + xDiv (x - y) y
    -- Econtra o diivisor inteiro de x

sumnLista:: Num f=> [f] -> f
sumnLista [] = 0
sumnLista (ahead:atail) = ahead + sumnLista atail
-- Somatorio da lista



-- Questões do Trabalho

-- Questão 3
unica_ocorrencia:: Eq f => f-> [f] -> Bool
unica_ocorrencia _ [] = False -- se o elemento não estiver na lista, não há resposta possível, 
-- coloquei False por que não vai ter nenhuma ocorrencia, logo não tem unica tambem
unica_ocorrencia e (ahead:atail)
    |e == ahead = not(estanalista e atail) 
    |otherwise = unica_ocorrencia e atail
-- Inverte o Booleano retornado em "estanalista", ou seja, 
-- caso esteja na lista, não tem unica ocorrencia e vice versa

-- Questão 6
remove:: Eq f => f -> [f] -> [f]
remove _ [] = [] -- Caso base, se a lista estiver vazia, nada pra remover
remove f (ahead:atail)
    | f == ahead  = atail
-- Retira a cabeça da lista caso seja o elemento a ser removido
    | otherwise = ahead : remove f atail 
-- Chama recursivamente até encontrar o elemento ou a lista acabar

-- Questão 9
gera_sequencia:: (Num f ,Enum f) => f -> [f]
gera_sequencia n = intercala [1..n] [-1,-2..(-n)]
    where
        intercala atail1 [] = atail1 -- Não tem como intercalar com lista vazia
        intercala [] atail2 = atail2
        intercala (ahead1:atail1) (ahead2:atail2) = ahead1 : ahead2 : intercala atail1 atail2
        -- intercala as cabeças da lista as concatenando com a intercalação de suas caudas, 
        -- e assim por sucessivamente

-- Questão 12
reverso:: [f] -> [f]
reverso [] = [] -- Caso esteja vazia, o reverso é ela mesma
reverso (ahead:atail) = reverso atail ++ [ahead] -- coloca o elemento da cabeça no final da lista
-- e chama a função pra cauda recursivamente


-- Questão 15
somatorio:: Num f=> [f] -> f
somatorio [] = 0 -- Lista vazia, somatorio = 0
somatorio (ahead:atail) = ahead + somatorio atail
--  Soma recursivamente a cabeça da lista

-- Questão 18
interseccao :: Eq f => [f] -> [f] -> [f]
interseccao [] _ = [] -- intersecção de nada com alguma coisa é nada
interseccao _ [] = [] -- trata o contrario do de cima
interseccao (ahead:atail) y
  | estanalista ahead y = ahead : interseccao atail y
  | otherwise   = interseccao atail y
-- retorna uma lista dos elementos de (ahead:atail) que estão em y

-- Questão 21
ordenada:: Ord f => [f] -> Bool
ordenada (atual:prox:atail)
    |atual > prox = False
    |otherwise = ordenada (ahead:atail)
ordenada _ = True
-- Para se o elemento anterior é maior que o proximo
-- Caso não pare, a lista esta ordenada



-- Questão 24 
picos:: Ord f => [f] -> [f]
picos f = achapicos(concatrev f)
    where
        ultimoLista (ahead:atail)
            |atail == [] = ahead
            |otherwise = ultimoLista atail
        -- retorna o ultiimo elemento da lista

        concatrev (ahead:atail) = [ultimoLista(ahead:atail)] ++ (ahead:atail) ++ [ahead]
        -- Concatenação especial para a simulação de uma lista circular

        achapicos (ante:atual:prox:atail) -- numa lista [a,b,c] transforma em 
        --[c,a,b,c,a] coloca o ultimo no começo e o primero no final,
        -- simulando uma lista circular
            | atual > ante && atual > prox = atual : achapicos (atual:prox:atail) -- verificação dos picos
            |otherwise = achapicos (atual:prox:atail)
        achapicos _ = [] -- se não achar, não teve picos

-- Questão 27
todas_maiusculas :: String -> String
todas_maiusculas [] = []  -- Caso base: string vazia
todas_maiusculas (ahead:atail)
  | 97 <= fromEnum ahead && fromEnum ahead <= 122 = toEnum(fromEnum ahead - 32) : todas_maiusculas atail  
  | otherwise = ahead : todas_maiusculas atail -- se não for letra, é ignorado
-- Subtrai 32 do codigo das Minusculas para buscar sua versão maiuscula



-- Questão 30
variancia :: Fractional f => [f] -> f
variancia xq = desvPadrao xq (media xq) / tamLista xq -- desvio padrao/tamanho
  where
    tamLista [] = 0
    tamLista (_:atail) = 1 + tamLista atail -- enquanto ouver cauda é somado em 1 o tamanho 

    media yq = somatorio yq / tamLista yq -- Calcula a media da lista

    desvPadrao [] _ = 0
    desvPadrao (ahead:atail) vMedia = (ahead - vMedia)^2 + desvPadrao atail vMedia -- Calcula a variancia baseada na
    -- Formula somatorio[(x - media)^2]


         
-- Questão 33
separa:: (Eq f, Num f) => [f] -> [[f]]
separa [] = [[]]
separa g = separaZeros g [] -- Passa o acumulo como uma lista vazia
    where
        separaZeros [] acumulo = [reverso acumulo] -- manter ordem original
        separaZeros (ahead:atail) acumulo 
            |ahead == 0 = reverso acumulo : separaZeros atail [] 
            -- Quando encontra o 0, retorna a lista de valores acumulados
            -- ao contrario para manter a ordem
            |otherwise = separaZeros atail (ahead:acumulo) 
            -- Enquanto não encontra, 
            -- concatena na lista o acumulo que sera retornado depois
    

-- Questão 36
soma_digitos:: Integral f => f -> f
soma_digitos x 
    | x < 10 = x -- caso base. se o numero for menor que 10, nao precisamos quebra-lo
    |otherwise = soma_digitos (xDiv x 10) + (xMod x 10) -- soma a recursivamente a divisão inteira por 10 
    -- com o resto 



-- Questão 39
quadrado_perfeito:: Integral f => f -> Bool
quadrado_perfeito 0 = True  -- 0 é um quadrado perfeito
quadrado_perfeito num =  achaDivisor num 1 -- começa os testes a partir do numero 1
    where
        achaDivisor num testeDivisor -- funcao para iterar sobre o divisor
            | xDiv num testeDivisor  == testeDivisor && xMod num testeDivisor== 0 = True 
            -- verifica se o numero inserido tem resultado igual ao divisor e se seu modulo é 0
            | testeDivisor * testeDivisor > num = False -- caso o quadrado do divisor ultrapasse o valor inserido, para os testes por ai
            | otherwise = achaDivisor num (testeDivisor+1) -- chama a função recursivamente, aumentando de 1 em 1 o divisor


