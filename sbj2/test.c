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
	int n, i;
    int arr[7];
    int sum = a;
	struct k_list	kk;
    i=075;
    i=0X7A5F;
	iii iiiii;
	s_list	sdflkj;
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
