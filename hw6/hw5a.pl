sorting(A, X):- sortList(A, [], X).
% 재귀 호출의 출력 인자는 “아직 값이 없는 새로운 변수”여야 하고, 
% 그 변수를 통해 돌아오는 값을 머리 패턴([...])과 통일하면서 결과가 채워진다. 
% 변수 이름은 자유지만, 각 호출마다 독립된 빈 변수를 써야 제대로 동작한다.
% 이 부분이 헷갈려서 변수를 선언하지 않고 매개변수로 넘어간 값을 대입해서 반환한다는 부분이 이해가 어려웠다.
% 프롤로그의 변수는 선언과 동시에 초기화되기 때문에 호출할 때 잘 고려해서 선언하는것이 중요

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

listLength([], 0):- !. %사용하지 않는 변수는 _로 처리하여 warning를 없앴다. 완전익명변수 '_'는 컴파일러가 무시함
listLength([_|Rest], Length):- % 리스트의 길이 확인 함수
	listLength(Rest, LengthT),
	Length is (LengthT + 1).

printList([], _):- !.
printList(R, Temp):- % 출력을 위한 함수(길이가 2보다 클 때만 출력)
	listLength(R, Len),
	Len > 1 ->
		write("X = "), write(R), write("."), nl;
		printList([], Temp).


sortList([], Sorted, Sorted):-!. % 정렬이 완료되면 반환에 정렬된 리스트를 넣음
sortList([H|T], Sorted, Return):-
	insert(Sorted, H, R), % 정렬된 리스트에 새 값을 집어넣고
	printList(R, _), % 정렬된 리스트 출력하고
	sortList(T, R, Return). % 다음값 집어넣기

%처음에 X = []이게 왜 출력되는지 몰라서 시간이 좀 걸렸다.
%알고보니 swipl 여기서 알아서 반환형인 X를 출력해줬다.
% 그래서 처음에는 X에 아무것도 반환되지 않았는데 다시 바꾸느라 시간이 좀 걸렸다.