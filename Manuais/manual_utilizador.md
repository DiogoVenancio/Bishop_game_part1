# 1. Inteligência Artificial
## **JOGO DO BISPO - PARTE 1**

![alt text](https://pcatunda.files.wordpress.com/2014/06/xadrez.jpg "IA imagem")

### **Docente:**

- Filipe Mariano

### **Alunos:**

- Diogo Venâncio - 160221076
- André Gonçalves - 170221015

<br>

# **Indíce**

- [Objetivo do programa](#objetivo-do-programa)
- [Funcionamento](#funcionamento)
	- [Menu inicial](#menu-inicial)
	- [Menu dos algoritmos](#menu-dos-algoritmos)
	- [Menu dos boards](#menu-dos-boards)
	- [Menu dos movimentos](#menu-dos-movimentos)
  - [Menu dos pontos objetivo](#menu-dos-pontos-objetivo)
  - [Menu das heuristicas](#menu-das-heuristicas)
- [Estatística](#estatística)
- [Limitações do programa](#limitações-do-programa)

<br>

# **Objetivo do programa**

Este programa tem como objetivos:

1. Os algoritmos conseguirem resolver todos os boards;
2. As estatísticas ficarem registadas num ficheiro.

<br>

# **Funcionamento**

## **Menu inicial**

No menu inicial o jogador simplesmente escolhe se quer iniciar um jogo.

```lisp
 -------------------------------------
              Jogo do Bispo            
                                       
              1 - Começar              
                                       
              s - Sair                 
 -------------------------------------
```

---

## **Menu dos algoritmos**

No menu dos algoritmos o jogador escolhe qual o algoritmo que pretende executar.

```lisp
----------------------------------------------------------    
             Jogo do Bispo - Escolha o algoritmo              
                                                              
                   1 - Breadth First                          
                   2 - Depth First                            
                   3 - A*                                     
                   0 - Voltar                                 
                                                              
                   s - Sair                                   
                                                              
 ---------------------------------------------------------
```

---

## **Menu dos boards**

No menu dos board o jogador escolhe qual o board que pretende.

```lisp
 -------------------------------------
   Jogo do Bispo - Escolha o board   
            1 - Board 1            
            2 - Board 2            
            3 - Board 3            
            4 - Board 4            
            5 - Board 5            
         6 - Board random                
                                        
           0 - Voltar                    
 -------------------------------------
```

---

## **Menu dos movimentos**

No menu dos movimentos o jogador escolhe o nº maximo de movimentos que o algoritmo pode executar.

```lisp
 -------------------------------------- 
 Jogo do Bispo - Numero max de moves   
                                       
             n - Não definir           
            -1 - Voltar                
                                       
             s - Sair                  
 -------------------------------------
```

---

## **Menu dos pontos objetivo**

No menu dos pontos objetivo o jogador escolhe o nº maximo de pontos que o algoritmo pode efetuar, o valor solução.

```lisp
 -------------------------------------- 
   Jogo do Bispo - Definir objetivo    
                                       
             n - Não definir           
            -1 - Voltar                
                                       
             s - Sair                  
 -------------------------------------
```

---

## **Menu das heuristicas**

No menu das heuristica o jogador escolhe qual a heuristica que quer usar com o algoritmo A*.

```lisp
 -------------------------------------- 
  Jogo do Bispo - Escolher heuristica  
                                       
        1 - Heuristica Enuncia         
        2 - Heuristica Criada          
            0 - Voltar                 
                                       
             s - Sair                  
 -------------------------------------
```

<br>

## **Estatística**

No final da execução de um algoritmo, as estatisticas são escritas para um ficheiro.

Exemplo: Board 6 com o Algoritmo A* com a heuristica dada

```lisp
** Board - Stats 6 **
 > Algoritmo: A*
 > Inicio: 19:2:6
 > Fim: 19:2:6
 > Nos gerados: 96
 > Nos expandidos: 21
 > Profundidade max: 20
 > Objetivo: 1500
 > Penetrância: 5/24
 > Comprimento: 20
 > Pontos totais: 1169
 > Heuristica: HEURISTICA
```

<br>

# **Limitações do programa**

1. Quando se volta atrás no menu dos movimentos, e se o jogador escolher executar um algoritmo, o menu aparece novamente.