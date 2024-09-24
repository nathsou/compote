int main(void) {
    int i = 0;

    while (i < 10) {
        i++;
    }

    for (int j = 0; j < 10; j++) {
        i++;
    }

    int k = 0;
    for (k = 0; k < 10; k++) {
        k++;
    }

    for (;;) {
        break;
    }

    i = 0;
    do {
        i++;
    } while (i < 10);

    return i;
}