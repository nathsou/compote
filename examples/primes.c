extern int putchar(int c);
long limit = 1000000l;

int print_long(long n) {
    if (n < 0) {
        putchar(45);
        n = -n;
    }

    if (n > 9) {
        print_long(n / 10l);
    }

    putchar(48 + n % 10);
}

int is_prime(long n) {
    if (n < 2l || n % 2l == 0) return 0;
    if (n == 2l) return 1;

    for (long i = 3l; i * i <= n; i += 2l) {
        if (n % i == 0l) {
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

    print_long(count);
    putchar(10);
}
