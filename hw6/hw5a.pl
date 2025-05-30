sorting(A, X):- sortFunc(A, X).
% 재귀 호출의 출력 인자는 “아직 값이 없는 새로운 변수”여야 하고, 
% 그 변수를 통해 돌아오는 값을 머리 패턴([...])과 통일하면서 결과가 채워지는 겁니다. 
% 변수 이름은 자유지만, 각 호출마다 독립된 빈 변수를 써야 제대로 동작해요.

% -> 기껏 만들었지만 짜고보니까 안 씀(insert와 거의 동일함)
push_back(Elem, [], [Elem]) :- !.
push_back(Elem, [H|T1], [H|T2]) :-
	push_back(Elem, T1, T2).
% 두 번째 인자의 리스트가 빌 때까지(T1 == []까지) 뽑아서 세 번째 인자의 H로 저장
% 마지막 인자는 두 번째 인자가 텅 비면 그때 elem을 넣어서 반환 -> 역순으로 차니까 가장 뒤에 append됨.

insert([], Num, [Num]):- !. % 현재 정렬된 list 숫자들보다 num이 더 큰 경우 가장 뒤에 추가
insert(List, [], List):- !. % list를 전체 순회하기 전에 Num이 삽입된경우 남은 list 그대로 반환
insert([H1|T1], Num, [H2|T2]):-
	H1 < Num ->
	(	% 넣고자하는 Num이 정렬된 리스트의 숫자보다 크다면 정렬된 숫자를 반환 List의 앞에 넣는다.
		H2 = H1,
		insert(T1, Num, T2)
	);
	(	% 넣고자하는 Num이 정렬된 리스트의 숫자보다 작다면 Num을 반환 List의 앞에 넣는다.
		H2 = Num,
		insert([H1|T1], [], T2)
	).

listLength([], 0):- !.
listLength([Head|Rest], Length):-
	listLength(Rest, LengthT),
	Length is (LengthT + 1).

printList([], Temp):- !.
printList(R, Temp):-
	listLength(R, Len),
	Len > 1 ->
		write("X = "), write(R), write("."), nl;
		printList([], Temp).


sortList([], Sorted, Sorted):-!.
sortList([H|T], Sorted, Return):-
	insert(Sorted, H, R),
	printList(R, Temp),
	sortList(T, R, Return).
	

sortFunc([], Return):-!.
sortFunc(List, Return):-
	sortList(List, [], Return),
	sortFunc([], Return).



