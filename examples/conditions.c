int main(void) {
    int a = 3;
    int b = 7;

    if (a > 0 ? a * b == 21 : b - a == a + b)
        return 3;
    else
        return 7;
}