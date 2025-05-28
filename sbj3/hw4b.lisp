(setq tc1 (list 11 33 23 45 13 25 8 135))
(setq tc2 (list 83 72 65 54 47 33 29 11))

(defun insertsort (lst)
	(loop for i from 1 to 7 do ; 인덱스 1부터 7까지
		(setq tmplist lst)
		(setq retlist '())
		(loop for j from 1 to i do   ; 원하는 인덱스 숫자 얻기
			(setq tmplist (cdr tmplist)))
		(setq target (car tmplist))
		; 이제 해당 숫자와 이전의 인덱스와 비교하면서 정렬 수행하기
		(setq tmplist lst)
		(setq flag 0)
		(loop for j from 0 below i do ; 내 인덱스 이전 숫자와 비교
			(setq lstnum (car tmplist))
			(setq tmplist (cdr tmplist))
			;; tmplist의 원소들을 하나씩 빼면서 target과 비교하면서 정렬
			(if (= flag 0)
				(cond
					(( < lstnum target)
				 	(setq retlist (append retlist (list lstnum)))) ;; 타겟보다 작다면 리스트의 앞에 차례로 추가
					(t
				 	(setq retlist (append retlist (list target lstnum))) ;; 타겟보다 크면 타겟을 리스트에 추가한 후 뒤에 tmplist의 원소를 추가 -> 크기순 정렬됨
				 	(setq flag 1))) ;; 이후 뒷 부분은 비교할 필요없이 retlist에 append하면 되니까 flag처리
				(setq retlist (append retlist (list lstnum))) ;;flag가 1이면 이미 target은 retlist에 들어간 상태이므로 비교하지않고 바로 append(이전 정렬에서 정렬되었음을 보장)
			);;여기서 c++와 같은 언어들은 pop을 실행하면 알아서 해당 자료구조의 크기도 변경되는데 lisp도 그럴거라고 생각하고 작성했는데 그게 아니라서 버그수정에 시간이 걸렸다.
		)
		(if (= flag 1) (setq tmplist (cdr tmplist))) ;; target이 이미 retlist에 들어있는경우 target을 tmplist에서 제외
		(setq retlist (append retlist tmplist))
		(setq lst retlist)
		;; 한 단계 끝나고 출력부분 -> 개행 없이 출력을 위해 princ활용
		(princ "step ")
		(princ i)
		(princ " : ")
		(princ retlist)
		(terpri) ;;개행 출력
	)

	(terpri)
	(princ "finish : ")
	(princ lst)
	(terpri)
)
(terpri)
(princ "Insertsort")
(terpri)(terpri)

(princ "TC1 : ")
(princ tc1)
(terpri)(terpri)

(insertsort tc1)
(setq retlist '())
(terpri)(terpri)(terpri)

(princ "TC2 : ")
(princ tc2)
(terpri)(terpri)

(insertsort tc2)
(terpri)(terpri)
