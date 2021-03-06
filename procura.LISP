;; Contem a implementa��o dos algoritmos de procura
;; Autor: Diogo Ven�ncio - 160221076
;; Autor: Andr� Gon�alves - 170221015
;; Ano letivo 19/20

;******* N� *******

;Fun��o para criar a estrutura de dados para um no
(defun create-no (board pai &optional (g 0) (h 0))
  (list board pai g h)
)

;Fun��o que retorna o estado board de um no
(defun estado-no(no)
  (first no)
)

;Fun��o que retorna o no pai de um no
(defun pai-no(no)
  (second no)
)

;Fun��o que retorna o valor g de um no
(defun g-no(no)
  (third no)
)

;Fun��o que retorna o valor h de um no
(defun h-no(no)
  (fourth no)
)

;Fun��o que retorna o valor f de um no
(defun f-no(no)
  (+ (g-no no) (h-no no))
)

;Verifica se o n� existe na lista
(defun existe-no (no lista-no)
	(eval (cons 'or (mapcar #'(lambda(aux-no) (if (equal aux-no no) T NIL)) lista-no)))
)

;Fun��o que calcula a profundidade de um no
(defun profun-no(no)
  (cond
   ((null (pai-no no)) 0)
   (t (1+ (profun-no (pai-no no)))))
)

;**** FUN��ES ALGORITMOS ****

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

;Fun��o para calcular o valor de f
(defun f-calcula(no)
  (cond
   ((null no) nil)
   (t (+ (g-no no) (h-no no))))
)

;Fun��o que retorna o melhor board segundo o valor f
(defun f-best(lista-no solucao &optional best-no best-f)
  (let ((f (f-calcula (car lista-no))))
    (cond
     ((or (null lista-no) (null solucao)) best-no)
     ((null best-no) (f-best (cdr lista-no) solucao (car lista-no) f))
     ((> f best-f) (f-best (cdr lista-no) solucao (car lista-no) f))
     (t (f-best (cdr lista-no) solucao best-no best-f))))
)

;Fun��o para calcular a penetrancia (comprimento do caminho / numero total de nos gerados)
(defun penetrancia(lista)
  (/ (profun-no (car lista)) (+ (cadr lista) (caddr lista)))
)

;****** ALGORITMOS ******

;BFS
;1. N� inicial -> Abertos
;2. Se Abertos vazio, falha
;3. Remove o primeiro no de Abertos (n) e coloca-o em fechados
;4. Expande o no n. Coloca os sucessores no fim de Abertos, colocando os ponteiros para n.
;5. Se algum dos sucessores � um no objetivo sai, e d� a solu��o. Caso contr�rio vai chamar novamente a fun��o
(defun bfs(sucessor abertos &optional moves solucao fechados)
  (let* ((atual (car abertos)) ;No atual
         (lista (posicoes-jogadas-possiveis (jogadas-possiveis (car atual)) (car atual))) ;Lista das jogadas possiveis
         (sucessores (funcall sucessor atual 'bfs lista)) ;Sucessores do no atual
         (abertos-new (lista-abertos-bfs (cdr abertos) sucessores)) ;Adiciona � lista de nos abertos
         (fechados-new (cons atual fechados))) ;Adiciona o no atual � lista de fechados
   (cond
    ((null abertos) nil)
    ((equal moves (profun-no atual)) (list atual (length abertos-new) (length fechados-new))) ;Se os movimentos forem iguais � profundidade do no atual
    ((null abertos-new) (list atual (length abertos-new) (length fechados-new))) ;Se a atual lista de abertos estiver vazia
    ((and (not (null solucao)) (<= solucao (g-no atual))) (list atual (length abertos-new) (length fechados-new))) ;Se a solucao n�o for null e for menor ou igual ao valor g do no atual
    (t (bfs sucessor abertos-new moves solucao fechados-new))))
)

;DFS
;1. N� inicial -> Abertos
;2. Se Abertos vazio, falha
;3. Remove o primeiro no de Abertos (n) e coloca-o em Fechados
;4. Se a profundidade de n � maior que d vai chamar novamente a fun��o
;5. Expande o no n. Coloca os sucessores no inicio de Abertos, colocando os ponteiros para n.
;6. Se algum dos sucessores � um no objetivo sai, e d� a solu��o. Caso contr�rio vai chamar novamente a fun��o
(defun dfs(sucessor abertos &optional moves solucao fechados)
  (let* ((atual (car abertos)) ;No atual
         (lista (posicoes-jogadas-possiveis (jogadas-possiveis (car atual)) (car atual))) ;Lista das jogadas possiveis
         (sucessores (funcall sucessor atual 'dfs lista)) ;Sucessores do no atual
         (abertos-new (lista-abertos-dfs sucessores (cdr abertos))) ;Adiciona � lista de nos abertos
         (fechados-new (cons atual fechados))) ;Adiciona o no atual � lista de fechados
   (cond
    ((null abertos) nil)

    ;Se os movimentos forem menores que a profundidade do no atual (Se a profundidade de atual � maior que o movimentos atribuidos vai para 2)
    ((> (profun-no atual) moves) (dfs sucessor abertos-new moves solucao fechados-new))
    ((null abertos-new) (list atual (length abertos-new) (length fechados-new))) ;Se a atual lista de abertos estiver vazia
    ((and (not (null solucao)) (<= solucao (g-no atual))) (list atual (length abertos-new) (length fechados-new))) ;Se a solucao n�o for null e for menor ou igual ao valor g do no atual
    (t (dfs sucessor abertos-new moves solucao fechados-new))))
)

;A*
;1. N� inicial -> Abertos.
;2. Se Abertos vazio, falha
;3. Remove o n� de Abertos (n) com menor custo de f e coloca-o em Fechados
;4. Expande o n� n. Calcula o f de cada um dos sucessores
;5. Coloca os sucessores que n�o existem em Abertos nem Fechados na lista de Abertos, por ordem de f colocando os ponteiros para n
;6. Se algum sucessor for um n� objetivo termina e d� a solu��o
;7. Vai chamar novamente a fun��o
(defun a*(sucessor heuristica abertos solucao &optional (moves 10) fechados)
  (let* ((atual (f-best abertos solucao)) ;No atual com melhor valor de f segundo a solucao pretendida
         (lista (posicoes-jogadas-possiveis (jogadas-possiveis (car atual)) (car atual))) ;Lista das jogadas possiveis
         (sucessores (funcall sucessor atual 'a* lista moves solucao heuristica)) ;Sucessores do no atual
         (abertos-new (append (cdr abertos) sucessores)) ;Adiciona � lista de nos abertos
         (fechados-new (cons atual fechados)) ;Adiciona o no atual � lista de fechados
         (best-fechado (f-best fechados solucao))) ;Verifica qual � o no fechado com melhor valor de f
    (cond
     ((null abertos) nil) ;Se a lista de abertos estiver vazia
     ((null abertos-new) (list atual (length abertos-new) (length fechados-new))) ;Se a nova lista de abertos estiver vazia
     ((equal moves (profun-no atual)) (list atual (length abertos-new) (length fechados-new))) ;Se o n� de movimentos for igual � profundidade do no atual
     ((<= solucao (g-no atual)) (list atual (length abertos-new) (length fechados-new))) ;Se a solucao atribuida for menor ou igual ao valor g do no atual
     ((and (not (null best-fechado)) (> (f-calcula best-fechado) (f-calcula atual))) ;Se o melhor no fechado nao estiver vazio, se o melhor no fechado com o valor f for maior que o valor f do atual no, chama novamente o algoritmo a* mas na nova lista de abertos, � adicionado o melhor no fechado com valor f e em fechados o mesmo � removido
      (a* sucessor heuristica (cons best-fechado abertos-new) solucao moves (remover-se #'(lambda (x) (if (equal x best-fechado) x)) fechados-new)))
     (t (a* sucessor heuristica abertos-new solucao moves fechados-new))))
)