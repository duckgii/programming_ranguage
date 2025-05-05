
char W[3] = "tf";
char *answer;

int main()
{
	int n, i;
    i=0;
    int arr[7];
    int sum = 0;
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
    return 1;
}
