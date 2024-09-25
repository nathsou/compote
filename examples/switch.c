int main(void) {
    int res = 0;

    switch (3 * 7) {
        case 1:
        case 2:
            res = 1;
            break;
        case 21:
            res = 2;
        case 11:
            res += 7;
            break;
        default:
            res *= 2;
    }

    return res;
}