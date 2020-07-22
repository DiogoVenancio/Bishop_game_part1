;; Código relacionado com o problema
;; Autor: Diogo Venâncio - 160221076
;; Autor: André Gonçalves - 170221015
;; Ano letivo 19/20

;******************* BOARD *******************

;Retorna a lista reverse
;;(get-board-reverse (board-a))
(defun get-board-reverse (board)
  (cond
   ((null board) nil)
   (t (append (get-board-reverse (cdr  board)) (list (car board)))))
)

;******************* SELECTORES *******************

;Retornar uma linha do tabuleiro
;;(get-linha 1 (get-board-reverse (board-a)))
(defun get-linha (i board)
  (cond
   ((null board) nil)
   ((equal i 1) (car board))
  (t (get-linha (1- i) (cdr board)))
 )
)

;Retorna uma celula segundo a linha, a coluna e o board
;;(get-celula 1 2 (get-board-reverse (board-a)))
(defun get-celula (x y board)
  (cond
   ((null board) nil)
   (t (labels ((num (i l)
                 (cond
                  ((null l) nil)
                  ((equal i 1) (car l))
                  (t (num (1- i) (cdr l))))))
        (num y (get-linha x  board)))))
)

;******************* POSICIONAMENTO *******************

;Verifica se já existe um bispo jogado
;;(verifica-bispo (get-linha 1 (get-board-reverse (board-a))))
(defun verifica-bispo (lista)
  (cond
   ((null lista) 0)
    ((equal 'T (car lista)) (+ 1 (verifica-bispo (cdr lista))))
   (t (verifica-bispo (cdr lista))))
)

;Função que recebe um indice, uma lista e um valor e substitui pelo valor pretendido nessa posição
;;(substitui-pos 2 (get-linha 1 (get-board-reverse (board-a))) T)
(defun substitui-pos (x l &optional (v nil))
  (cond
   ((null l) nil)
   ((equal x 1) (cons v (cdr l)))
   (t (cons (car l) (substitui-pos (- x 1) (cdr l) v)))
   )
)

;Função que recebe dois indices, o board e um valor. Retorna o board com a celula substituida pelo valor pretendido
;;(substituir 1 2 (get-board-reverse (board-a)) T)
(defun substituir(x y board &optional (v nil))
  (cond
   ((or (null board)) nil)
   ((equal x 1) (cons (substitui-pos y (car board) v)
    (substituir (- x 1) y (cdr board) v)))
   (t (cons (car board) (substituir (- x 1) y (cdr board) v)))
   )
)

;Função que verifica se a posição recebida está dentro dos limites do board e retorna o valor que se encontra na casa
;;(verifica-pos 3 3 (get-board-reverse (board-a)))
(defun verifica-pos(x y board)
  (if
   (and (>= x 0) (<= x (length board)) (>= y 0) (<= y (length board)) (not (null board)))
    (nth (- y 1) (nth (- x 1) board)) nil)
)

;Função que recebe o tabuleiro e devolve a posição (x y) em que se encontra o bispo
;;(posicao-bispo (substituir 1 2 (get-board-reverse (board-a)) T))
(defun posicao-bispo(board)
  (cond
   ((null board) nil)
   ((null (find T (car board))) (posicao-bispo (cdr board)))
   (t (list (+ (- (length (car board)) (length board)) 1) (+ (- (length (car board)) (length (member T (car board)))) 1))))
)

;Função que recebe o valor a pesquisar e a linha ao qual ele se encontra. Retorna o numero da coluna
;;(get-coluna-valor 54 (get-linha 3 (get-board-reverse (board-a))))
(defun get-coluna-valor(v linha &optional (y 1))
  (cond
   ((or (null v) (null linha)) nil)
   ((equal v (car linha)) y)
   (t (get-coluna-valor v (cdr linha) (1+ y))))
)

;Função que recebe um valor e um board. Retorna a posicao(x y) do valor introduzido
;;(get-valor-pos 45 (get-board-reverse (board-a)))
(defun get-valor-pos(v board &optional (x 1))
  (let ((y (get-coluna-valor v (car board))))
  (cond
   ((or (null v) (null board)) nil)
   ((null y) (get-valor-pos v (cdr board) (1+ x)))
   (t (cons x (cons y nil)))))
)

;******************* OPERADORES *******************

;Função que, ao receber um número, neste caso, entre 11 e 88, separa-os para dentro de uma lista
;;(separa-num 33)
(defun separa-num(n)
  (loop for c across (write-to-string n) collect (digit-char-p c))
)

;Função que recebe uma linha e retorna o maior duplo dessa linha
;;(duplo-maior-linha (get-linha 4 (get-board-reverse (board-b))))
(defun duplo-maior-linha(linha &optional (duplo nil))
  (let ((numeros (separa-num (car linha))))
    (cond
     ((null linha) duplo)

     ;Caso o duplo seja nil, a primeira casa da linha não seja o bispo e os numeros forem iguais, vai passar o resto da linha
     ((and (null duplo) (not (equal T (car linha))) (equal (car numeros) (cadr numeros))) 
      (duplo-maior-linha (cdr linha) (car linha)))

     ;Caso o duplo não seja nil, a cabeça da linha não esteja vazia, o maior duplo não seja igual ao bispo, a cabeça da linha não seja igual ao bispo, os valores serem iguais e se o duplo for menor que a cabeça da linha
     ((and (not (null duplo)) (not (null (car linha))) (not (equal T duplo)) (not (equal T (car linha))) (equal (car numeros) (cadr numeros)) (< duplo (car linha)))
      (duplo-maior-linha (cdr linha) (car linha)))

     (t (duplo-maior-linha (cdr linha) duplo))))
)

;Função que recebe um board e retorna o maior duplo que contem
;;(duplo-maximo (get-board-reverse (board-b)))
(defun duplo-maximo(board &optional (duplo nil))
  (let ((maior-linha (duplo-maior-linha (car board))))
    (cond
     ((null board) duplo)

     ;Caso o duplo seja nil, o maior duplo não seja nil e não seja igual ao bispo
     ((and (null duplo) (not (null maior-linha)) (not (equal T maior-linha)))
      (duplo-maximo (cdr board) maior-linha))

     ;Caso o duplo não seja nil, o maior duplo não seja nil, não seja igual ao bispo, a cabeça da linha não seja o bispo e o duplo seja menor que o maior da linha
     ((and (not (null duplo)) (not (null maior-linha)) (not (equal T duplo)) (not (equal T maior-linha)) (< duplo maior-linha))
      (duplo-maximo (cdr board) maior-linha))

     (t (duplo-maximo (cdr board) duplo))))
)

;Função que retorna o valor da posição y do valor simetrico de n numa determina linha, se existir
;;(simetrico-pos-linha 31 (get-linha 1 (get-board-reverse (board-b))))
(defun simetrico-pos-linha(n linha &optional (y 1))
  (let* ((numeros (separa-num n)) (simetrico (separa-num (car linha))))
    (cond
     ((null linha) nil)
     ((equal (get-board-reverse numeros) simetrico) y)
     (t (simetrico-pos-linha n (cdr linha) (1+ y)))))
)

;Função que retorna a posição (x y) do numero simetrico de n  num determinado board, caso exista
;;(simetrico-pos 54 (get-board-reverse (board-b)))
(defun simetrico-pos(n board &optional (x 1))
  (let ((simetrico (simetrico-pos-linha n (car board))))
    (cond
     ((null board) nil)

     ; Caso o n seja um numero duplo ou seja menor que 11 ou maior que 88, vai buscar o maior duplo por default
     ((or (equal (car (separa-num n)) (cadr (separa-num n))) (< n 11) (> n 88)) (get-valor-pos (duplo-maximo board) board))

     ((not (null simetrico)) (cons x (cons simetrico nil)))
     (t (simetrico-pos n (cdr board) (1+ x)))))
)

;Função responsável em colocar o bispo no board
;;(coloca-bispo (get-board-reverse (board-a)))
(defun coloca-bispo(board)
  (let ((bispo-pos (posicao-bispo board))
        (bispo-pos-y (nth (random (length (get-valores-linha board))) (get-valores-linha board))))
    (cond
     ((and (null bispo-pos)
           (not (null (verifica-pos 1 bispo-pos-y board)))
           (not (equal 'T (verifica-pos 1 bispo-pos-y board))))
      (let* ((board-t (substituir 1 bispo-pos-y board 'T))
             (simetrico (simetrico-pos (get-celula 1 bispo-pos-y board-t) board-t)))
        (cond
         ((null simetrico) (substituir 1 bispo-pos-y board-t 'T))
         (t (substituir (car simetrico) (cadr simetrico) board-t 'NIL))
         )
        ))
     ((null bispo-pos) (coloca-bispo board))
     (t board)))
)

;Função que retorna a posicao y do primeiro valor da primeira linha
;;(primeiro-valor-y (get-board-reverse (board-b)))
(defun primeiro-valor-y(board &optional (y 1))
  (let ((pos (verifica-pos 1 y board)))
    (cond
     ((or (equal 'NIL pos) (equal 'T pos)) (primeiro-valor-y board (+ y 1)))
     (t y)))
)

;Função que retorna os valores  da primeira linha
;;(get-valores-linha (get-board-reverse (board-a)))
(defun get-valores-linha(board &optional (linha (get-linha 1 board)))
  (cond
   ((or (null board) (null linha)) nil)
   (t (let* ((valor (car linha))
             (pos (cadr (get-valor-pos valor board))))
     (cond
      ((equal 'NIL valor) (get-valores-linha board (cdr linha)))
      (t (cons pos (get-valores-linha board (cdr linha))))))))
)

;Função que conta a ocorrencia de um determinado elemento
;;(conta-nil '(NIL NIL 54 NIL) 'NIL)
(defun conta-nil(lista element)
  (cond
   ((null lista) 0)
   ((equal (car lista) element) (1+ (conta-nil (cdr lista) element)))
   (t (conta-nil (cdr lista) element)))
)                           

;**** FUNÇÕES OPERADORES ****

;;(diagonal-1-pos (get-board-reverse (board-a)))
(defun diagonal-1-pos(board &optional (bispo-pos (posicao-bispo board)))
  (cond
   ((null bispo-pos) (diagonal-1-pos (coloca-bispo board)))
   ((or (< (car bispo-pos) 0) (< (cadr bispo-pos) 0) (> (car bispo-pos) 8) (> (cadr bispo-pos) 8) (equal (car bispo-pos) 0) (equal (cadr bispo-pos) 0)) nil)
   (t (cons bispo-pos (diagonal-1-pos (cdr board) (list (- (car bispo-pos) 1) (+ (cadr bispo-pos) 1))))))
)

;;(diagonal-2-pos (get-board-reverse (board-a)))
(defun diagonal-2-pos(board &optional (bispo-pos (posicao-bispo board)))
  (cond
   ((null bispo-pos) (diagonal-2-pos (coloca-bispo board)))
   ((or (< (car bispo-pos) 0) (< (cadr bispo-pos) 0) (> (car bispo-pos) 8) (> (cadr bispo-pos) 8) (equal (car bispo-pos) 0) (equal (cadr bispo-pos) 0))  nil)
   (t (cons bispo-pos (diagonal-2-pos (cdr board) (list (- (car bispo-pos) 1) (- (cadr bispo-pos) 1))))))
)

;;(diagonal-3-pos (get-board-reverse (board-a)))
(defun diagonal-3-pos(board &optional (bispo-pos (posicao-bispo board)))
  (cond
   ((null bispo-pos) (diagonal-3-pos (coloca-bispo board)))
   ((or (< (car bispo-pos) 0) (< (cadr bispo-pos) 0) (> (car bispo-pos) 8) (> (cadr bispo-pos) 8) (equal (car bispo-pos) 0) (equal (cadr bispo-pos) 0))  nil)
   (t (cons bispo-pos (diagonal-3-pos (cdr board) (list (+ (car bispo-pos) 1) (- (cadr bispo-pos) 1))))))
)

;;(diagonal-4-pos (get-board-reverse (board-a)))
(defun diagonal-4-pos(board &optional (bispo-pos (posicao-bispo board)))
  (cond
   ((null bispo-pos) (diagonal-4-pos (coloca-bispo board)))
   ((or (< (car bispo-pos) 0) (< (cadr bispo-pos) 0) (> (car bispo-pos) 8) (> (cadr bispo-pos) 8) (equal (car bispo-pos) 0) (equal (cadr bispo-pos) 0))  nil)
   (t (cons bispo-pos (diagonal-4-pos (cdr board) (list (+ (car bispo-pos) 1) (+ (cadr bispo-pos) 1))))))
)

;Função que retorna os valores das posições recebidas
;;(get-valor-posicao (diagonal-1-pos (get-board-reverse (board-a))) (get-board-reverse (board-a))) 
(defun get-valor-posicao(lista board)
  (cond
   ((null lista) nil)
   ((null board) nil)
   (t (let* ((posicao (car lista))
          (valor (get-celula (car posicao) (cadr posicao) board)))
        (cond
         ((equal 'T valor) (get-valor-posicao (remove-item 0 lista) board))
         (t (cons valor (get-valor-posicao (cdr lista) board)))))))
)

;Remover um item de uma lista
;;(remove-item 0 '(T 1 2 3))
(defun remove-item (item lista)
  (if (> item 0)
      (cons (car lista) (remove-item (1- item) (cdr lista)))
    (cdr lista))
)

;Remove todos os NIL da lista
;;(remove-nil '(NIL NIL NIL NIL NIL NIL 11 NIL 77))
(defun remove-nil (lista)
  (cond
   ((null lista) NIL)
   ((null (car lista)) (remove-nil (cdr lista)))
   ((atom (car lista)) (cons (car lista) (remove-nil (cdr lista))))
   (t (cons (remove-nil (car lista)) (remove-nil (cdr lista)))))
)


;Devolve o valor de um determinado indice
;;(get-item-index 1 '(1 2 3 4))
(defun get-item-index(n lista)
  (if (zerop n)
      (car lista)
    (get-item-index (1- n) (cdr lista)))
)                          

;Função que coloca o bispo na nova casa jogada
;;(print-board (jogar-bispo (posicoes-jogadas-possiveis (jogadas-possiveis (get-board-reverse (board-b))) (get-board-reverse (board-b))) (get-board-reverse (board-b))))
;(jogar-bispo lista (coloca-bispo board))
(defun jogar-bispo(lista board)
  (cond
   ((and (null lista) (null board)) nil)
   (t (let* ((posicao lista)
         (bispo-pos (posicao-bispo board)))         
     (cond
      ((null bispo-pos) (coloca-bispo board))
      ((null posicao) nil)
      (t (let ((valor (get-celula (car posicao) (cadr posicao) board)))
        (cond
         ((null valor) nil)
         (t (let* ((board-t (substituir (car bispo-pos) (cadr bispo-pos) board 'NIL))
                (board-t-1 (substituir (car posicao) (cadr posicao) board-t 'T))
                (simetrico (simetrico-pos valor board-t-1))
                (duplo (duplo-maximo board-t-1)))
           (cond
            ((and (null simetrico) (null duplo) board-t-1))
            ((null simetrico) (substituir (get-valor-pos duplo board-t-1) board-t-1 'NIL))
            (t (substituir (car simetrico) (cadr simetrico) board-t-1 'NIL))))))))))))
)

;Função que retorna todas as posicoes das diagonais
;;(jogadas-possiveis (get-board-reverse (board-b)))
(defun jogadas-possiveis-total(board)
  (cond
   ((null board) nil)
   (t (let ((op-1 (remove-item 0 (diagonal-1-pos board)))
            (op-2 (remove-item 0 (diagonal-2-pos board)))
            (op-3 (remove-item 0 (diagonal-3-pos board)))
            (op-4 (remove-item 0 (diagonal-4-pos board))))
        (append op-1 op-2 op-3 op-4)))) 
)

;Jogadas possiveis que pode ser realizadas pelo jogador
;;(jogadas-possiveis (get-board-reverse (board-b)))
(defun jogadas-possiveis(board)
  (cond
   ((null board) nil)
   (t (let* ((valores (get-valor-posicao (jogadas-possiveis-total board) board))
          (val (remove-nil valores)))
     (cond
      ((null val) nil)
      (val)))))
)

;Função que retorna todas as posicoes possiveis para o jogador jogar
;;(posicoes-jogadas-possiveis (jogadas-possiveis (get-board-reverse (board-b))) (get-board-reverse (board-b)))
(defun posicoes-jogadas-possiveis(lista board)
  (cond
   ((null lista) nil)
   (t (cons (get-valor-pos (car lista) board) (posicoes-jogadas-possiveis (cdr lista) board))))
)         

;*********** HEURISTICA ***********

;Função para juntas todas as linhas de um board
;;(append-board (get-board-reverse (board-b)))
(defun append-board(board)
  (cond
   ((null board) nil)
   (t (append (car board) (append-board (cdr board)))))
)

;Função que irá retornar a média por casa dos pontos que constam no board x
;;(pontos-media (get-board-reverse (board-b)))
(defun pontos-media (board &optional primeira)
  (cond
   ((null board) 0.0)
   ((null primeira)
    (let ((linha (remover-se #'(lambda (x) (or (null x) (equal 'T x))) (append-board board))))
      (cond
       ((or (equal 0 (length linha)) (null linha)) 1)
       (t (/ (+ (car linha) (pontos-media (cdr linha) 1)) (length linha)))
       )))
   (t (+ (car board) (pontos-media (cdr board) 1))))
)

;Heuristica base
;;Vai visitar as casas com o valor mais alto
(defun heuristica(no sucessor-g obj)
  (cond
   ((or (null no) (null obj)) nil)
   (t (/ (* (/ 28 15) (- obj sucessor-g)) (pontos-media (estado-no no)))))
)

;Heuristica criada
;;Heuristica que é centralizada nos movimentos possiveis do bispo e menos no valor das casas em si.
;;h(x) = (o(x) * 0.2) + (l(x) * 0.8)

;; o(x) -  é o número de pontos que faltam para atingir o valor definido como objetivo.
;; l(x) - é o tamanho da lista de todas as jogadas possiveis que o bispo pode efetuar
(defun heuristica-criada(no sucessor-g obj)
  (cond
   ((or (null no) (null obj)) nil)
   (t (+ (* (- obj sucessor-g) 0.2) (* (length (jogadas-possiveis (estado-no no))) 0.8))))
)

;******************* SUCESSORES *******************

;Função que retorna o valor da casa que vai corresponder á proxima jogada
;;(pos-g (create-no (jogar-bispo '(4 5) (get-board-reverse (board-b))) (get-board-reverse (board-b))) (jogar-bispo '(6 3) (jogar-bispo '(4 5) (get-board-reverse (board-b)))))
(defun pos-g(no sucessor)
  (let ((bispo-pos (posicao-bispo sucessor)))
    (cond
     ((null bispo-pos) nil)
     (t (verifica-pos (car bispo-pos) (cadr bispo-pos) (estado-no no)))))
)

;Função que cria um novo sucessor a partir de um nó
;;(new-sucessor (create-no (jogar-bispo '(4 5) (get-board-reverse (board-b))) (get-board-reverse (board-b))) 'jogar-bispo '(6 3))
;;(new-sucessor (create-no (jogar-bispo '(4 5) (get-board-reverse (board-b))) (get-board-reverse (board-b))) 'jogar-bispo '(6 3) 100 'heuristica)
(defun new-sucessor(no jogada posicao &optional obj heuristica)
  (let* ((sucessor (create-no (funcall jogada posicao (estado-no no)) no)) 
         (g (pos-g no (estado-no sucessor))))
    (cond
     ((or (null no) (null sucessor) (null g)) nil)
     ((null heuristica) (create-no (estado-no sucessor) no (+ (g-no no) g) 0))
     (t (create-no (estado-no sucessor) no (+ (g-no no) g) (funcall heuristica sucessor (+ (g-no no) g) obj)))))
)

;Função que retorna os sucessores de um no através de um determinado algoritmo
;;(sucessores (create-no (get-board-reverse (board-b)) nil) 'bfs (posicoes-jogadas-possiveis (jogadas-possiveis (get-board-reverse (board-b))) (get-board-reverse (board-b))))
;;(sucessores (create-no (get-board-reverse (board-b)) nil) 'a* (posicoes-jogadas-possiveis (jogadas-possiveis (get-board-reverse (board-b))) (get-board-reverse (board-b))) 10 100 'heuristica)
(defun sucessores(no algoritmo lista &optional maxProfundidade obj heuristica)
  (cond
   ((or (null no) (null lista)) nil)
   (t (let ((posicao (car lista)))
        (cond
         ((and (equal algoritmo 'dfs) (equal (profun-no no) maxProfundidade)) nil)
         (t (remover-se #'(lambda (x) (null x)) (cons (new-sucessor no 'jogar-bispo posicao obj heuristica) (sucessores no algoritmo (cdr lista) maxProfundidade obj heuristica))))))))
)          

;******************* BOARDS *******************

;Problema A
(defun board-a()
 '((NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL)
(34 NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL 78 NIL NIL NIL)
(NIL NIL 54 NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL 23 NIL NIL 45 NIL NIL NIL))
)

;Problema B
(defun board-b()
 '((NIL NIL NIL 78 77 NIL NIL NIL)
(NIL NIL NIL NIL NIL 31 NIL NIL)
(NIL NIL 11 NIL NIL NIL 55 NIL)
(34 NIL NIL NIL NIL NIL NIL 66)
(NIL NIL NIL NIL 22 NIL NIL NIL)
(NIL NIL 54 NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL 13 NIL NIL 45 NIL NIL NIL))
)

;Problema C
(defun board-c()
 '((NIL NIL NIL 71 77 NIL NIL NIL)
(NIL NIL NIL 42 NIL 31 NIL NIL)
(NIL NIL 11 NIL NIL 17 55 NIL)
(34 NIL NIL NIL NIL NIL NIL 66)
(NIL NIL NIL NIL 22 NIL NIL NIL)
(NIL NIL 54 NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL 13 NIL NIL 43 NIL NIL NIL))
)

;Problema D
(defun board-d()
 '((NIL NIL NIL 71 77 NIL 43 NIL)
(45 NIL NIL 42 NIL 31 NIL NIL)
(NIL 54 11 NIL NIL 17 55 NIL)
(34 NIL NIL NIL NIL NIL NIL 66)
(NIL 32 NIL NIL 22 NIL 34 88)
(NIL NIL 56 NIL NIL NIL NIL NIL)
(NIL NIL 23 NIL NIL NIL NIL NIL)
(NIL 13 NIL NIL 43 NIL 78 NIL))
)

;Problema E
(defun board-e()
 '((51 52 53 71 77 NIL 43 63)
(45 NIL NIL 42 NIL 31 NIL 64)
(NIL 54 11 NIL NIL 17 55 65)
(34 NIL NIL 7 NIL NIL NIL 66)
(NIL 32 NIL NIL 22 NIL 34 88)
(NIL NIL 56 81 82 83 84 85)
(NIL NIL 23 NIL NIL NIL NIL NIL)
(NIL 13 NIL NIL 43 NIL 78 NIL))
)

;Função que recebe um número positivon e cria uma lista com todos os nº entre 11 e o número passado como argumento
;;(lista-numeros)
(defun lista-numeros(&optional (n 88))
  (cond
   ((< n 11) nil)
   ((> n 88) nil)
   (t (cons n (lista-numeros (- n 1))))
   )
)

;Função que recebe uma lista e irá mudar aleatoriamente os seus números
;;(baralhar (lista-numeros))
(defun baralhar(l)
  (cond
   ((null l) nil)
   (t (let ((n (nth (random (length l)) l)))
        (cons n (baralhar (remover-se #'(lambda (num) (= num n)) l)))
        )))
)

;Função que remove da lista, o número que for escolhido
;;(remover-se #'(lambda (num) (= num 5)) '(1 3 5 2 4 5 6 5))
(defun remover-se(lamb l)
  (cond
   ((null l) nil)
   ((funcall lamb (car l)) (remover-se lamb (cdr l)))
   (t (cons (car l) (remover-se lamb (cdr l))))
   )
)

;Função que remove os numeros que contenham o valor 0 e 9
;;(baralhar-0-9 (baralhar (lista-numeros)))
(defun baralhar-0-9(lista)
  (cond
   ((null lista) nil)
    (t (let* ((valor (car lista))
          (separa (separa-num valor)))
      (cond
       ((null separa) nil)
       ((or (equal 0 (cadr separa)) (equal 9 (cadr separa))) (baralhar-0-9 (remover-se #'(lambda (n) (= n valor)) lista)))
       (t (cons valor (baralhar-0-9 (cdr lista))))))))
)
       
;Função que recebe uma lista e cria sublistas de n elementos
;;(board-random)
(defun board-random (&optional (l (baralhar-0-9 (baralhar (lista-numeros)))) (n 8))
  (cond
   ((null l) nil)
   (t (cons (subseq l 0 n) (board-random (subseq l n) n)))
   )
)

;Problema F
(defun board-f()
'((37 62 55 31 73 58 51 54) 
  (35 44 38 12 41 83 74 68) 
  (36 33 24 27 18 75 88 61)
  (52 43 32 34 81 78 87 77) 
  (56 13 15 86 42 71 21 46) 
  (85 28 45 11 64 14 63 22) 
  (17 57 16 66 82 47 72 48) 
  (53 67 23 76 25 84 65 26))
)

;Problema G
(defun board-g()
 '((NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL))
)

;******************* PRINT BOARDS *******************

;Mostra um tabuleiro formatado
(defun print-board(board &optional (stream t))
   (not (null (mapcar #'(lambda(l) (format stream "~%~t~t ~a" l)) board)))
)

;Imprime uma lista de boards
(defun print-boards (boards &optional (stream t))
  (not (null (mapcar #'(lambda(board) (format stream "~%~t~t ~a" (print-board board))) boards)))
)