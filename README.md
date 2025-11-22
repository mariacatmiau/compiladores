# compiladores Proj 2

Maria Gabriela 10409037

Raphaela polonis 10408843

Link GitHub: https://github.com/mariacatmiau/compiladores

📌 Sobre o projeto

Esse projeto é a Fase 2 do compilador da linguagem PasKenzie.
Aqui eu fiz:

Análise Léxica

Análise Sintática

Análise Semântica (variáveis duplicadas ou não declaradas)

Geração de Código MEPA

Mantive a mesma gramática da Fase 1, como pedido no enunciado.

🧠 O que eu implementei
✔ Mini tabela de símbolos (Hash)

Guarda as variáveis declaradas, usando hashMack().
Cada variável recebe um endereço (0, 1, 2…).

✔ Verificação semântica

O compilador detecta:

variável não declarada

variável declarada duas vezes

Se acontecer, mostra um erro com a linha e para.

✔ Geração de MEPA

Durante a análise sintática, já imprime as instruções da MEPA:

INPP, AMEM

LEIT, IMPR

CRCT, CRVL, ARMZ

SOMA, MULT, SUBT, DIVI

Comparações: CMEG, CMME, CMMA etc

DSVF, DSVS

Rótulos L1, L2, L3…

PARA

No final também imprime a tabela de símbolos.

▶️ Como compilar
Linux
gcc -Wall -g -Og projeto.c hashMack.o -o projeto

Windows
gcc -Wall -g -Og projeto.c hashMack.obj -o projeto.exe

▶️ Como executar

Meu arquivo de teste chama teste.pzk, então rodo assim:

./projeto teste.pzk


Se estiver no Windows:

projeto.exe teste.pzk

📄 O que o compilador mostra

Se não tiver erro:

primeiro imprime o código MEPA

depois a tabela de símbolos

Se tiver erro:

mostra qual é (léxico, sintático ou semântico)

mostra a linha

e para imediatamente
