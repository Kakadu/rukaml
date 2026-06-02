int fact_rec(int n)
{
    if (n < 1)
    {
        return 1;
    }

    return n * fact_rec(n - 1);
}

int fact_iter(int n)
{
    int acc = 1;

    for (int i = 2; i <= n; i = i + 1)
    {
        acc = acc * i;
    }

    return acc;
}

int main()
{
    return 144 - fact_rec(4) - fact_iter(5);
}
