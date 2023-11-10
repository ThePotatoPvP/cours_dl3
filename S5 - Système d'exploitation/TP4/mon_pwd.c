#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <limits.h>

//ATTENTION: si le répertoire courant est toujours ./
//la variable courant elle correspond à un chemin du style "./../.."
//qui évoluera au fur et au mesure qu'on remonte à la racine.

//On limitera les tailles des chemins PATH_MAX



//retourne le nom du répertoire correspondant au chemin courant
char *nom_du_repertoire(char * courant) {
  char *chemin = (char *)malloc(PATH_MAX + 1);

  if (chemin == NULL) {
    perror("Allocation mémoire échouée");
    exit(1);
  }

  // Utilise realpath() pour obtenir le chemin du répertoire courant
  if (realpath(courant, chemin) == NULL) {
    perror("Erreur lors de l'obtention du chemin du répertoire courant");
    free(chemin);
    exit(1);
  }

  return chemin;
}


//est-ce que le répertoire correspondant au chemin courant
//est la racine
int est_racine(char * courant) {
  struct stat punto1, punto2;

  lstat(strcat(courant, "."), &punto1);
  courant[strlen(courant)-1]='\0';
  lstat(strcat(courant, ".."), &punto2);
  courant[strlen(courant)-1]='\0';
  courant[strlen(courant)-1]='\0';
  return (punto1.st_ino = punto2.st_ino);
}



//c'est là qu'on fait la récursion
char *  construit_chemin_aux(char * courant, char * pwd) {
  if (est_racine(courant)){
    return pwd;
  }
  char* turfu = nom_du_repertoire(courant);
  return construit_chemin_aux(strcat(courant, "/.."), strcat(strcat(pwd, "/"), turfu));
}

//cette fonction retournera la référence absolue du répertoire courant ./
char *  construit_chemin() {
  //pwd est un chemin de style "/bidule/truc" où "bidule/truc" est la fin du pwd, à la fin ce sera le pwd
  char *  pwd = malloc (PATH_MAX* sizeof(char));
  // courant est un chemin du style "./../.."
  char *courant = malloc (PATH_MAX* sizeof(char));

  sprintf(courant,".");
  sprintf(pwd,"/");

  pwd = construit_chemin_aux(courant, pwd);

  free(courant);

  return pwd;
}

int main(int argc, char **argv) {
  char * pwd;

  pwd = nom_du_repertoire(".");
  //pwd = construit_chemin();
  printf("%s\n", pwd);
  free(pwd);
  return 0;
}
