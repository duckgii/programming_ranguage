#include <stdio.h>

#define AA 3

typedef struct t_list {
	int	a, b;
} s_list;

/*
16, 8진수
구조체 자료형 함수 파라미터로 들어감
*/

char W[3] = "tf";
char *answer;

int main()
{
	int n, i;
    int arr[7];
    int sum = 0;
    i=0;
    scanf("%d", &n);
    for (i = 0; i < n; i++)
    {
        arr[i] = i;
    }
    for (i = 0; i < n; i++)
    {
		
        sum += arr[i];
    }
    if (sum >= 10)
    {
        answer = W;
    }
    if (sum < 10)
    {
        answer = (W + 1);
    }
    printf("%c\n", answer[0]);
    return (1);
}
