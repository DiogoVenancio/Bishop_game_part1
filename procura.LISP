;; Contem a implementação dos algoritmos de procura
;; Autor: Diogo Venâncio - 160221076
;; Autor: André Gonçalves - 170221015
;; Ano letivo 19/20

;******* Nó *******

;Função para criar a estrutura de dados para um no
(defun create-no (board pai &optional (g 0) (h 0))
  (list board pai g h)
)

;Função que retorna o estado board de um no
(defun estado-no(no)
  (first no)
)

;Função que retorna o no pai de um no
(defun pai-no(no)
  (second no)
)

;Função que retorna o valor g de um no
(defun g-no(no)
  (third no)
)

;Função que retorna o valor h de um no
(defun h-no(no)
  (fourth no)
)

;Função que retorna o valor f de um no
(defun f-no(no)
  (+ (g-no no) (h-no no))
)

;Verifica se o nó existe na lista
(defun existe-no (no lista-no)
	(eval (cons 'or (mapcar #'(lambda(aux-no) (if (equal aux-no no) T NIL)) lista-no)))
)

;Função que calcula a profundidade de um no
(defun profun-no(no)
  (cond
   ((null (pai-no no)) 0)
   (t (1+ (profun-no (pai-no no)))))
)

;**** FUNÇÕES ALGORITMOS ****

;Lista de abertos para o dfs
(defun lista-abertos-dfs(lista sucessor)
  (cond
   ((null lista) sucessor)
   (t (append lista sucessor)))
)

;Lista de abertos para o bfs
(defun lista-abertos-bfs(lista sucessor)
  (cond
   ((null lista) sucessor)
   (t (append lista sucessor)))
)

;Função para calcular o valor de f
(defun f-calcula(no)
  (cond
   ((null no) nil)
   (t (+ (g-no no) (h-no no))))
)

;Função que retorna o melhor board segundo o valor f
(defun f-best(lista-no solucao &optional best-no best-f)
  (let ((f (f-calcula (car lista-no))))
    (cond
     ((or (null lista-no) (null solucao)) best-no)
     ((null best-no) (f-best (cdr lista-no) solucao (car lista-no) f))
     ((> f best-f) (f-best (cdr lista-no) solucao (car lista-no) f))
     (t (f-best (cdr lista-no) solucao best-no best-f))))
)

;Função para calcular a penetrancia (comprimento do caminho / numero total de nos gerados)
(defun penetrancia(lista)
  (/ (profun-no (car lista)) (+ (cadr lista) (caddr lista)))
)

;****** ALGORITMOS ******

;BFS
;1. Nó inicial -> Abertos
;2. Se Abertos vazio, falha
;3. Remove o primeiro no de Abertos (n) e coloca-o em fechados
;4. Expande o no n. Coloca os sucessores no fim de Abertos, colocando os ponteiros para n.
;5. Se algum dos sucessores é um no objetivo sai, e dá a solução. Caso contrário vai chamar novamente a função
(defun bfs(sucessor abertos &optional moves solucao fechados)
  (let* ((atual (car abertos)) ;No atual
         (lista (posicoes-jogadas-possiveis (jogadas-possiveis (car atual)) (car atual))) ;Lista das jogadas possiveis
         (sucessores (funcall sucessor atual 'bfs lista)) ;Sucessores do no atual
         (abertos-new (lista-abertos-bfs (cdr abertos) sucessores)) ;Adiciona á lista de nos abertos
         (fechados-new (cons atual fechados))) ;Adiciona o no atual á lista de fechados
   (cond
    ((null abertos) nil)
    ((equal moves (profun-no atual)) (list atual (length abertos-new) (length fechados-new))) ;Se os movimentos forem iguais á profundidade do no atual
    ((null abertos-new) (list atual (length abertos-new) (length fechados-new))) ;Se a atual lista de abertos estiver vazia
    ((and (not (null solucao)) (<= solucao (g-no atual))) (list atual (length abertos-new) (length fechados-new))) ;Se a solucao não for null e for menor ou igual ao valor g do no atual
    (t (bfs sucessor abertos-new moves solucao fechados-new))))
)

;DFS
;1. Nó inicial -> Abertos
;2. Se Abertos vazio, falha
;3. Remove o primeiro no de Abertos (n) e coloca-o em Fechados
;4. Se a profundidade de n é maior que d vai chamar novamente a função
;5. Expande o no n. Coloca os sucessores no inicio de Abertos, colocando os ponteiros para n.
;6. Se algum dos sucessores é um no objetivo sai, e dá a solução. Caso contrário vai chamar novamente a função
(defun dfs(sucessor abertos &optional moves solucao fechados)
  (let* ((atual (car abertos)) ;No atual
         (lista (posicoes-jogadas-possiveis (jogadas-possiveis (car atual)) (car atual))) ;Lista das jogadas possiveis
         (sucessores (funcall sucessor atual 'dfs lista)) ;Sucessores do no atual
         (abertos-new (lista-abertos-dfs sucessores (cdr abertos))) ;Adiciona á lista de nos abertos
         (fechados-new (cons atual fechados))) ;Adiciona o no atual á lista de fechados
   (cond
    ((null abertos) nil)

    ;Se os movimentos forem menores que a profundidade do no atual (Se a profundidade de atual é maior que o movimentos atribuidos vai para 2)
    ((> (profun-no atual) moves) (dfs sucessor abertos-new moves solucao fechados-new))
    ((null abertos-new) (list atual (length abertos-new) (length fechados-new))) ;Se a atual lista de abertos estiver vazia
    ((and (not (null solucao)) (<= solucao (g-no atual))) (list atual (length abertos-new) (length fechados-new))) ;Se a solucao não for null e for menor ou igual ao valor g do no atual
    (t (dfs sucessor abertos-new moves solucao fechados-new))))
)

;A*
;1. Nó inicial -> Abertos.
;2. Se Abertos vazio, falha
;3. Remove o nó de Abertos (n) com menor custo de f e coloca-o em Fechados
;4. Expande o nó n. Calcula o f de cada um dos sucessores
;5. Coloca os sucessores que não existem em Abertos nem Fechados na lista de Abertos, por ordem de f colocando os ponteiros para n
;6. Se algum sucessor for um nó objetivo termina e dá a solução
;7. Vai chamar novamente a função
(defun a*(sucessor heuristica abertos solucao &optional (moves 10) fechados)
  (let* ((atual (f-best abertos solucao)) ;No atual com melhor valor de f segundo a solucao pretendida
         (lista (posicoes-jogadas-possiveis (jogadas-possiveis (car atual)) (car atual))) ;Lista das jogadas possiveis
         (sucessores (funcall sucessor atual 'a* lista moves solucao heuristica)) ;Sucessores do no atual
         (abertos-new (append (cdr abertos) sucessores)) ;Adiciona á lista de nos abertos
         (fechados-new (cons atual fechados)) ;Adiciona o no atual á lista de fechados
         (best-fechado (f-best fechados solucao))) ;Verifica qual é o no fechado com melhor valor de f
    (cond
     ((null abertos) nil) ;Se a lista de abertos estiver vazia
     ((null abertos-new) (list atual (length abertos-new) (length fechados-new))) ;Se a nova lista de abertos estiver vazia
     ((equal moves (profun-no atual)) (list atual (length abertos-new) (length fechados-new))) ;Se o nº de movimentos for igual á profundidade do no atual
     ((<= solucao (g-no atual)) (list atual (length abertos-new) (length fechados-new))) ;Se a solucao atribuida for menor ou igual ao valor g do no atual
     ((and (not (null best-fechado)) (> (f-calcula best-fechado) (f-calcula atual))) ;Se o melhor no fechado nao estiver vazio, se o melhor no fechado com o valor f for maior que o valor f do atual no, chama novamente o algoritmo a* mas na nova lista de abertos, é adicionado o melhor no fechado com valor f e em fechados o mesmo é removido
      (a* sucessor heuristica (cons best-fechado abertos-new) solucao moves (remover-se #'(lambda (x) (if (equal x best-fechado) x)) fechados-new)))
     (t (a* sucessor heuristica abertos-new solucao moves fechados-new))))
)