;; 문제 접근
;; 크기가 4인 list를 만들어서 i번째 인덱스에 들어가는 값 j는 (i, j)에 queen이 들어있다는 것을 나타냄
;; 세로, (좌 + 우) 위쪽 대각선 방향만 확인
;; 가로는 어짜피 한 줄에 하나의 queen만 들어갈 수 있으므로 각 인덱스는 한 줄을 나타내도록 지정

(setq queen '())

(defun NQueen(lst)
	(if (= (length lst) 4) 
		(progn
			(print lst)
			(return-from NQueen nil)
		)
	)
	(loop for append_j from 0 below 4 ;; 현재 추가할 queen의 j_idx (0~3)
		do
		(progn
		(setq flag 0) ;; 다른 queen과 겹치는 경우 flag 1로 변경해서 다음 함수 호출 X
		(setq pre_i 0)
		(setq append_i (length lst)) ;; 현재 추가할 queen의 i_idx
		;; 이 append_i의 값이 전역변수라서 내부에서 함수를 호출하면 해당 호출 함수에서 append_i의 값을 바꿔 제대로된 값이 리턴되지 않았다.
		;; 처음 출력 정답 내용
		;; (1 3 0 2) 
		;; (1 3 2 0) 
		;; (2 0 3 1) 
		;; (2 1 3 0) 
		;; 처음 값은 이것처럼 대각선 값을 제대로 확인 X
		;; 모두 지역변수로 선언할 수도 있지만 매번 초기화하는 방법을 선택했습니다.
		;; append_j의 값은 함수 호출로인한 side effect로 바뀔 수 있지만, 어짜피 루프 돌면 새로운 다음 값으로 초기화돼서 로직에 영향을 끼치지 않습니다.
		(if (> append_i 0)
			(progn
			(loop for pre_j in lst ;; 비교할 queen의 i 인덱스 범위 지정
				do
				(progn
				(setq comp_i (abs (- pre_i append_i)))
				(setq comp_j (abs (- pre_j append_j))) ; i, j의 차가 0이거나 |i| == |j|면 겹치는 경우이므로 flag를 1로 설정
				(if (= comp_i comp_j) ; 대각선 확인
					(setq flag 1))
				(if (= pre_j append_j) ; 위쪽 확인
					(setq flag 1))
				(setq pre_i (+ pre_i 1))
				)
		)))
		(if (= flag 0) ;; 모든 조건을 통과했다면
			(progn
				(setq retlist (append lst (list append_j))) ;;전달용 리스트에 해당 인덱스 추가
				(NQueen retlist) ;; 재귀 호출 -> 얘는 이 list 자
			)
		)
		)
	)
)
(terpri)
(princ "NQueen size = 4")
(terpri)
(NQueen queen)
;; 배운점 : lisp의 변수 scope에 대해 생각해볼 수 있어서 좋았다.
;; call by value : 참조를 담은 값 (진짜 값이든, 포인터 변수이든 아무튼 그 변수 자체는 아님)
;; call by reference : 동일한 메모리 (&로 객체 넘기면 그거 수정하면 그 객체도 수정되듯이 진짜 변수 그자체)
;; call by == pass by 거의 같은 의미다