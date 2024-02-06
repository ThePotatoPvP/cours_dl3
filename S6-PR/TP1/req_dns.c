#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <arpa/inet.h>
#include <time.h>
#include <string.h>

char* formatString(char* str) {
    int s = 0; int p = 0;
    char* name = calloc(strlen(str)+2, sizeof(char));
    for (;;) {
        if (str[s] == '\0' || str[s] == '.') {
            name[p] = (s-p);
            while (p < s+1) {
                name[p+1] = str[p];
                p++;
            }
            if (!str[s]) break;
        }
        s++;
    }
    return name;
}

int requete(int argc, char** argv) {
    uint16_t header[6];

    srand((unsigned int)time(NULL));
    header[0] = (uint16_t)rand() % 65536;

    header[1] = 0;  // FLAGS
    header[2] = 1;  // QDCOUNT
    header[3] = 0;  // ANCOUNT
    header[4] = 0;  // NSCOUNT
    header[5] = 0;  // ARCOUNT

    // Conversion en ordre big-endian
    for (int i = 0; i < 6; ++i) {
        header[i] = htons(header[i]);
    }

    FILE *file = fopen("entete", "wb");
    if (file == NULL) {
        perror("Erreur lors de l'ouverture du fichier");
        return 1;
    }

    fwrite(header, sizeof(uint16_t), 6, file);

    if (!argc) {argv[1] = "foobar";}
    char* name = formatString(argv[1]);

    uint16_t* _name = calloc(strlen(name), sizeof(uint8_t));
    for (int i = 0; name[i]; i+=2){
        _name[i/2] = htons((name[i]<<8) + name[i+1]);
    }

    fwrite(_name, sizeof(uint8_t), strlen(name), file);
    free(name); free(_name);

    uint16_t footer[2] = {0, 1};
    fwrite(footer, sizeof(uint16_t), 2, file);

    fclose(file);
    printf("Entête DNS écrite dans le fichier 'entete'\n");

    return 0;
}



int main(int argc, char** argv) {
    requete(argc, argv);
    //printf("%s\n", formatString("www.foo"));
    return 0;
}

