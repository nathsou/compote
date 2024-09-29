
int count_calls(void) {
    static int calls = 0;
    return ++calls;
}

int is_even(int n);

int is_odd(int n) {
    count_calls();

    if (n == 0) {
        return 0;
    }
    
    return is_even(n - 1);
}

int is_even(int n) {
    count_calls();

    if (n == 0) {
        return 1;
    }

    return is_odd(n - 1);
}

int main(void) {
    is_odd(71);
    int calls = count_calls() - 1;
    
    return calls;
}
