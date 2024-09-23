int main(void) {
    if (1) goto label1;
    else goto label2;

    label1:
        return 3 * 7;
    
    label2:
        return 0;
}