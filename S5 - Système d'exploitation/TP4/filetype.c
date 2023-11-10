#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

void afficher_informations_fichier(const char *chemin) {
    struct stat info_fichier;

    // Utilise lstat() pour obtenir des informations sur le fichier (y compris les liens symboliques)
    if (lstat(chemin, &info_fichier) != 0) {
        perror("Erreur lors de la vérification du fichier");
        return;
    }

    // Affiche le numéro d'inœud
    printf("Numéro d'inœud : %ld\n", (long)info_fichier.st_ino);

    // Affiche le type de fichier
    if (S_ISREG(info_fichier.st_mode)) {
        printf("Type de fichier : Fichier ordinaire\n");
    } else if (S_ISDIR(info_fichier.st_mode)) {
        printf("Type de fichier : Répertoire\n");
    } else if (S_ISLNK(info_fichier.st_mode)) {
        printf("Type de fichier : Lien symbolique\n");
    } else if (S_ISFIFO(info_fichier.st_mode)) {
        printf("Type de fichier : Tube nommé\n");
    } else {
        printf("Type de fichier : Inconnu\n");
    }

    // Vérifie si le fichier est exécutable par au moins une catégorie d'utilisateurs
    if (info_fichier.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) {
        printf("Le fichier est exécutable par au moins une catégorie d'utilisateurs.\n");
    } else {
        printf("Le fichier n'est pas exécutable par au moins une catégorie d'utilisateurs.\n");
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Utilisation : %s <chemin_du_fichier>\n", argv[0]);
        return 1;
    }

    const char *reference = argv[1];

    afficher_informations_fichier(reference);

    return 0;
}
