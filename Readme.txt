Ep3 - Programação Funcional

João Machini de Miranda
Pedro Morhy Borges Leal

Como compilar e rodar:
    make
    scala (fazer import do pacote pfc)
    
Como rodar os testes que fizemos:
    make tests
    
Como Limpar:
    make clean
    
Observação sobre executável scala:
    O Makefile assume que o nome do aplicativo scala se chama "scala" e o compilador "scalac"


Observação sobre o Implicit:
    Como foi perguntado no Paca, ao definirmos a conversão implicita doubleToPol dentro do object Pol,
    infelizmente não funcionam as operações do tipo Double + Pol, mesmo utilizando scala-2.8. 
    Talvez isso aconteça por conta de estarmos utilizando o MacOS.
    De qualquer forma colocamos dentro do object Pol e esperamos que na sua máquina funcione.