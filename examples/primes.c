extern int putchar(int c);
int limit = 1000000;

int print_int(int n) {
    if (n < 0) {
        putchar(45);
        n = -n;
    }

    if (n > 9) {
        print_int(n / 10);
    }

    putchar(48 + n % 10);
}

int is_prime(int n) {
    if (n < 2 || n % 2 == 0) return 0;
    if (n == 2) return 1;

    for (int i = 3; i * i <= n; i += 2) {
        if (n % i == 0) {
            return 0;
        }
    }

    return 1;
}

int main(void) {
    int count = 0;

    for (int i = 0; i < limit; i++) {
        count += is_prime(i);
    }

    print_int(count);
    putchar(10);
}
