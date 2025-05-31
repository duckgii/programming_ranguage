n_queen(N, X):- backtracking(N, X).
abs(X, Ret):-
	Ret is (X * -1).

checkoneQueen(X, Y, Same):-
	X == 0 ->
		Same is 1;
	Y == 0 ->
		Same is 1;
	X == Y ->
		Same is 1;
	(
		abs(X, Ret),
		Ret == Y 
	) ->
		Same is 1;
		Same is (0).

checkallQueen(PreX_idx, [], NowX_idx, NowY_idx, Flag, Flag):-!. % 모든 리스트 다 검사했으면 0 반환
checkallQueen(PreX_idx, [PreY_idx|Tail], NowX_idx, NowY_idx, 0, 0):-!. % 겹치면 0으로 flag 수정하고 바로 종료

%조사할 퀸의 x 인덱스, 조사할 퀸의 y 인덱스, 현재 push할 퀸의 x 인덱스, 현재 push할 퀸의 y인덱스, flag, return)
checkallQueen(PreX_idx, [PreY_idx|Tail], NowX_idx, NowY_idx, Flag, R):- % Flag 무조건 1로 시작 -> 1이면 겹치지 않는다는 의미
	(
		Check_X_idx is (PreX_idx - NowX_idx),
		Check_Y_idx is (PreY_idx - NowY_idx),
		checkoneQueen(Check_X_idx, Check_Y_idx, Same),
		Same == 1 
	)->
		Next_Xidx is (PreX_idx + 1),
		checkallQueen(Next_Xidx, Tail, NowX_idx, NowY_idx, 0, R); % -> 같으면 겹치는 경우
		Next_Xidx is (PreX_idx + 1),
		checkallQueen(Next_Xidx, Tail, NowX_idx, NowY_idx, Flag, R).

push_back([], Num, [Num]):-!.
push_back([Head|Tail], Num, [Head|Tail2]):-
	push_back(Tail, Num, Tail2).

	% write(List), nl.

% insertQueen(N, List, Idx_x, Idx_y, Ret):-

% reverse([], []):-!.
% reverse([H1|T1], Rtemp):-
% 	reverse(T1, [H1|Rtemp]).
reverse([], Rev, Rev):-!.
reverse([H1|T1], Rev, Rtemp):-
	reverse(T1, [H1|Rev], Rtemp).

insertQueen(0, List, 0, 1, List):-!.
insertQueen(N, List, Idx_x, Idx_y, Ret):-
	Next_Xidx is (Idx_x + 1),
	Next_Yidx is (Idx_y + 1),
	checkallQueen(1, List, Idx_x, Idx_y, 1, Flag),
	(
		Flag == 1 , Idx_x == N ->
			push_back(List, Idx_y, Temp),
			reverse(Temp, [], RTemp),
			insertQueen(0, RTemp, 0, 1, Ret);
		Flag == 1 , Idx_y < N ->
			push_back(List, Idx_y, Temp),
			(insertQueen(N, Temp, Next_Xidx, 1, Ret);
			insertQueen(N, List, Idx_x, Next_Yidx, Ret)) %';'은 or연산!!
			;
		Flag == 1 , Idx_y == N ->
			push_back(List, Idx_y, Temp),
			insertQueen(N, Temp, Next_Xidx, 1, Ret);
		Idx_y < N ->
			insertQueen(N, List, Idx_x, Next_Yidx, Ret)
	).

backtracking(N, X):-
	insertQueen(N, [], 1, 1, X).
% 1->N까지 숫자 하나씩 올려가면서 인덱스 넣어보기
%처음에는 순차적으로 값을 보여주는 과정이 어떻게 저렇게 나와야하는지 이해가 되지 않았는데, 
%호출한 함수의 모든 매개변수가 바인딩되면 REPL이 감지해서 출력
% 이후 다음 재귀함수 실행됨.

