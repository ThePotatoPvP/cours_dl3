#include <fcntl.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <unistd.h>

// Attention ce PC est petit boutiste.
union swag {
    uint32_t witness;
    unsigned char foo[4];
};

int ex1(int n) {
    union swag bar = {0};
    bar.witness = n;
    for(int i = 0; i < 4; i++){
        printf("%d", bar.foo[i]);
        if (i<3) printf("-");
    }
    printf("\n");
}

int main() {
    ex1(1036487236);
    return 0;
}
