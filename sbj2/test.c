#include <stdio.h>

#define a 3123

struct t_list;

struct k_list {
	int aa;
};

typedef struct t_list {
	int aa;
} s_list;

typedef enum Point {
	RED
} Point;

typedef union qwe qwe;

typedef int iii;

char W[3] = "tf";
char *answer;

int main()
{
	int n, i; // 이부분만 수정되면 될듯
    int arr[7] = {0};
	int arr[2][3];
    int sum = a;
	int* arr[9]; // 이걸 어케 처리해야할까...
	struct k_list	kk;
	int *****tet;
	iii iiiii;
	s_list	sdflkj;
    scanf("%d", &n);
	int (*p)(int, int);

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
    else if (sum >= 100)
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
