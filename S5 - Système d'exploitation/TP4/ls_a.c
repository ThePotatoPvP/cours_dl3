#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

int main(int argc, char *argv[]) {
    // Le répertoire à lister (le répertoire courant par défaut)
    const char *repertoire = (argc > 1) ? argv[1] : ".";
    struct stat info;
    int taille;

    // Ouvre le répertoire
    DIR *dp = opendir(repertoire);

    // Vérifie si l'ouverture du répertoire a réussi
    if (dp == NULL) {
        perror("Erreur lors de l'ouverture du répertoire");
        exit(EXIT_FAILURE);
    }

    // Lit le contenu du répertoire
    struct dirent *entry;
    while ((entry = readdir(dp)) != NULL) {
        if (entry->d_name[0] != '.'){
            if (lstat(entry->d_name, &info) != 0) {
                perror("Erreur lors de la vérification du fichier");
                return 1;
            }
            printf("%ld\t%ld\t%s\n", (long)info.st_ino, (long)info.st_size, entry->d_name);
        }
    }

    // Ferme le répertoire
    closedir(dp);

    return 0;
}
